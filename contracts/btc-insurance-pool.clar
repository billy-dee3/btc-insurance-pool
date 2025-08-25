;; ------------------------------------------------------------
;; BTC-Pegged Insurance Pool (Stacks / Clarity v2)
;; ------------------------------------------------------------

;; Summary:
;; - Liquidity providers (LPs) contribute STX and receive pool shares.
;; - Users buy coverage by paying a premium in STX.
;; - Policies specify a BTC price floor; if BTC <= floor during coverage,
;;   the policyholder can file a claim.
;; - Community voting (by LP share weight) approves/rejects claims.
;; - Oracle updates BTC price (trusted signer).
;;
;; Notes:
;; - This is a self-contained STX-denominated MVP. You can swap STX for
;;   a SIP-010 token by replacing stx-transfer? with token transfers.
;; - Arithmetic uses integer math. Basis points (BPS) = parts per 10_000.
;; ------------------------------------------------------------

(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-BAD-ARGS       (err u400))
(define-constant ERR-NOT-FOUND      (err u404))
(define-constant ERR-INSUFFICIENT   (err u402))
(define-constant ERR-DOUBLE-VOTE    (err u409))
(define-constant ERR-NOT-ELIGIBLE   (err u412))
(define-constant ERR-TOO-EARLY      (err u425))

;; --- Config (can be made mutable via governance) ---------------------------
(define-constant PREMIUM_BPS u200)        ;; 2% premium on coverage purchased
(define-constant QUORUM_BPS  u2000)       ;; 20% of total shares must vote
(define-constant PASS_BPS    u6000)       ;; >=60% YES (by shares) to pass
(define-constant VOTE_WINDOW u1440)       ;; ~1 day (blocks) to reach quorum
(define-constant GRACE_BLOCKS u1440)      ;; grace after policy end for claim

(define-data-var oracle principal tx-sender) ;; set at deploy; updatable
(define-data-var last-btc-price uint u0)
(define-data-var last-btc-height uint u0)

;; Pool accounting
(define-data-var total-shares uint u0)     ;; LP shares
(define-data-var pool-balance uint u0)     ;; STX tracked balance (accounting)
(define-data-var reserved uint u0)         ;; reserved for active policies

;; LP balances (shares)
(define-map lp-shares
  { lp: principal }
  { shares: uint })

;; Policies
(define-data-var next-policy-id uint u1)
(define-map policies
  { id: uint }
  {
    owner: principal,
    coverage: uint,
    premium: uint,
    btc-floor: uint,
    start: uint,
    end: uint,
    active: bool,
    claimed: bool
  })

;; Claims
(define-data-var next-claim-id uint u1)
(define-map claims
  { id: uint }
  {
    policy-id: uint,
    claimant: principal,
    requested: uint,
    yes: uint,       ;; yes votes (share-weight)
    no: uint,        ;; no votes (share-weight)
    start: uint,     ;; vote start (block)
    resolved: bool,
    approved: bool
  })

;; To prevent double voting
(define-map claim-votes
  { claim-id: uint, voter: principal }
  { voted: bool })

;; --- Helpers ---------------------------------------------------------------

(define-read-only (get-contract-principal) (as-contract tx-sender))

(define-read-only (min (a uint) (b uint))
  (if (< a b) a b))

(define-read-only (mul-div (x uint) (num uint) (den uint))
  (if (is-eq den u0) u0 (/ (* x num) den)))

;; Block height tracking
(define-data-var current-height uint u0)

;; Using internal height counter for block-height tracking
(define-read-only (now)
  (var-get current-height))

;; Safe operations for data handling
(define-private (verify-claim (id uint))
  (match (map-get? claims { id: id })
    claim (ok claim)
    ERR-NOT-FOUND))

(define-private (verify-policy (id uint))
  (match (map-get? policies { id: id })
    policy (ok policy)
    ERR-NOT-FOUND))

(define-private (update-height)
  (var-set current-height (+ (var-get current-height) u1)))

(define-read-only (get-lp-shares (who principal))
  (default-to u0 (get shares (map-get? lp-shares { lp: who })) ))

(define-read-only (get-total-shares) (var-get total-shares))
(define-read-only (get-pool-balance) (var-get pool-balance))
(define-read-only (get-reserved) (var-get reserved))

(define-read-only (available-liquidity)
  (let ((bal (var-get pool-balance)) (res (var-get reserved)))
    (if (> bal res) (- bal res) u0)))

(define-read-only (policy-exists? (pid uint))
  (is-some (map-get? policies { id: pid })))

(define-read-only (claim-exists? (cid uint))
  (is-some (map-get? claims { id: cid })))

;; --- Admin / Oracle --------------------------------------------------------

(define-public (set-oracle (who principal))
  (begin
    (asserts! (is-eq tx-sender (var-get oracle)) ERR-NOT-AUTHORIZED)
    (asserts! (not (is-none (some who))) ERR-BAD-ARGS)
    (var-set oracle who)
    (ok who)))

(define-public (submit-btc-price (price uint))
  (begin
    (asserts! (is-eq tx-sender (var-get oracle)) ERR-NOT-AUTHORIZED)
    (asserts! (> price u0) ERR-BAD-ARGS)
    (var-set last-btc-price price)
    (var-set last-btc-height (now))
    (ok price)))

(define-read-only (get-btc-price)
  { price: (var-get last-btc-price), height: (var-get last-btc-height) })

;; --- Liquidity: deposit / withdraw ----------------------------------------

(define-public (deposit (amount uint))
  (let (
        (ts (var-get total-shares))
        (bal (var-get pool-balance))
        (recipient (get-contract-principal))
       )
    (begin
      (asserts! (> amount u0) ERR-BAD-ARGS)
      (asserts! (is-ok (stx-transfer? amount tx-sender recipient)) ERR-INSUFFICIENT)

      (let ((minted (if (is-eq ts u0)        ;; first LP: 1:1 shares
                         amount
                         (mul-div amount ts bal))))
        ;; credit shares
        (let ((prev (get-lp-shares tx-sender)))
          (map-set lp-shares { lp: tx-sender } { shares: (+ prev minted) })
          (var-set total-shares (+ ts minted))
          (var-set pool-balance (+ bal amount))
          (ok minted)
        )))))

(define-public (withdraw (share-amount uint))
  (let (
        (ts (var-get total-shares))
        (bal (var-get pool-balance))
        (res (var-get reserved))
       )
    (begin
      (asserts! (and (> share-amount u0) (> ts u0)) ERR-BAD-ARGS)

      (let ((my (get-lp-shares tx-sender)))
        (asserts! (>= my share-amount) ERR-INSUFFICIENT)

        (let ((payout (mul-div share-amount bal ts)))
          ;; ensure sufficient reserve coverage
          (asserts! (>= (- bal payout) res) ERR-INSUFFICIENT)

          ;; update state
          (map-set lp-shares { lp: tx-sender } { shares: (- my share-amount) })
          (var-set total-shares (- ts share-amount))
          (var-set pool-balance (- bal payout))

          ;; transfer out
          (stx-transfer? payout (get-contract-principal) tx-sender)
        )))))

;; --- Policies: buy coverage / status --------------------------------------

(define-public (buy-coverage (coverage uint) (duration uint) (btc-floor uint))
  (let ((start (now)))
    (begin
      (asserts! (and (> coverage u0) (> duration u0) (> btc-floor u0)) ERR-BAD-ARGS)
      ;; premium = coverage * PREMIUM_BPS / 10_000
      (let ((premium (mul-div coverage PREMIUM_BPS u10000)))
        ;; collect premium in STX
        (asserts! (is-ok (stx-transfer? premium tx-sender (get-contract-principal))) ERR-INSUFFICIENT)

        ;; update pool balance (premiums add liquidity, not reserved)
        (var-set pool-balance (+ (var-get pool-balance) premium))

        ;; create policy
        (let ((pid (var-get next-policy-id)))
          (map-set policies
            { id: pid }
            {
              owner: tx-sender,
              coverage: coverage,
              premium: premium,
              btc-floor: btc-floor,
              start: start,
              end: (+ start duration),
              active: true,
              claimed: false
            })
          ;; reserve coverage
          (var-set reserved (+ (var-get reserved) coverage))
          (var-set next-policy-id (+ pid u1))
          (ok pid)
        )))))

(define-read-only (get-policy (pid uint))
  (match (map-get? policies { id: pid })
    policy (ok policy)
    ERR-NOT-FOUND))

;; --- Claims: file / vote / resolve ----------------------------------------

(define-public (file-claim (pid uint) (requested uint) (reason-hash (buff 32)))
  (let (
        (policy (try! (verify-policy pid)))
        (price (var-get last-btc-price))
        (nowh (now))
       )
    (begin
      (asserts! (is-eq (get owner policy) tx-sender) ERR-NOT-AUTHORIZED)
      (asserts! (and (get active policy) (not (get claimed policy))) ERR-NOT-ELIGIBLE)
      ;; within coverage or grace
      (asserts! (<= (get start policy) nowh) ERR-TOO-EARLY)
      (asserts! (<= nowh (+ (get end policy) GRACE_BLOCKS)) ERR-NOT-ELIGIBLE)
      ;; trigger: BTC price at or below floor
      (asserts! (<= price (get btc-floor policy)) ERR-NOT-ELIGIBLE)
      (asserts! (> requested u0) ERR-BAD-ARGS)

      (let ((cid (var-get next-claim-id)))
        (map-set claims
          { id: cid }
          {
            policy-id: pid,
            claimant: tx-sender,
            requested: requested,
            yes: u0,
            no: u0,
            start: nowh,
            resolved: false,
            approved: false
          })
        (var-set next-claim-id (+ cid u1))
        (ok cid)
      ))))

(define-public (vote-claim (cid uint) (support bool))
  (let (
        (claim (try! (verify-claim cid)))
        (sh (get-lp-shares tx-sender))
        (ts (var-get total-shares))
        (nowh (now))
       )
    (begin
      (asserts! (> sh u0) ERR-NOT-AUTHORIZED) ;; must be LP
      (asserts! (not (get resolved claim)) ERR-NOT-ELIGIBLE)
      (asserts! (<= nowh (+ (get start claim) VOTE_WINDOW)) ERR-NOT-ELIGIBLE)

      ;; no double voting
      (asserts! (is-none (map-get? claim-votes { claim-id: cid, voter: tx-sender })) ERR-DOUBLE-VOTE)
      (map-set claim-votes { claim-id: cid, voter: tx-sender } { voted: true })

      (if support
        (map-set claims { id: cid } (merge claim { yes: (+ (get yes claim) sh) }))
        (map-set claims { id: cid } (merge claim { no: (+ (get no claim) sh) })))
      (ok { my-shares: sh, total-shares: ts })
    )))

(define-public (resolve-claim (cid uint))
  (let (
        (claim (try! (verify-claim cid)))
        (ts (var-get total-shares))
        (bal (var-get pool-balance))
        (res (var-get reserved))
        (nowh (now))
       )
    (begin
      (asserts! (not (get resolved claim)) ERR-NOT-ELIGIBLE)
      (let (
            (votes (+ (get yes claim) (get no claim)))
            (quorum (mul-div ts QUORUM_BPS u10000))
            (policy-id (get policy-id claim))
           )
        (asserts!
          (or (>= votes quorum) (> nowh (+ (get start claim) VOTE_WINDOW)))
          ERR-NOT-ELIGIBLE)

        (let ((policy (try! (verify-policy policy-id)))
              (max-pay (get coverage policy))
              (req (get requested claim))
              (payout (min req max-pay)))
          
          ;; check pass threshold
          (let ((passed
                   (if (is-eq votes u0)
                       false
                       (>= (mul-div (get yes claim) u10000 votes) PASS_BPS))))
            (if passed
              (begin
                ;; ensure liquidity: pool-balance - payout >= reserved - payout
                (asserts! (>= bal payout) ERR-INSUFFICIENT)

                ;; Update accounting: reduce reserved and pool-balance
                (var-set reserved (if (>= res payout) (- res payout) u0))
                (var-set pool-balance (- bal payout))

                ;; pay claimant
                (asserts! (is-ok (stx-transfer? payout (get-contract-principal) (get claimant claim))) ERR-INSUFFICIENT)

                ;; finalize claim + deactivate policy
                (map-set claims { id: cid } (merge claim { resolved: true, approved: true }))
                (map-set policies { id: policy-id } (merge policy { active: false, claimed: true }))

                (ok { status: "approved", paid: payout })
              )
              ;; rejected
              (begin
                (map-set claims { id: cid } (merge claim { resolved: true, approved: false }))
                ;; if policy window is over, free reserve (no payout)
                (if (> nowh (+ (get end policy) GRACE_BLOCKS))
                  (var-set reserved (if (>= res (get coverage policy)) (- res (get coverage policy)) u0))
                  (var-set reserved res))
                (ok { status: "rejected", paid: u0 })
              ))))))))

;; --- Views -----------------------------------------------------------------

(define-read-only (get-claim (cid uint))
  (match (map-get? claims { id: cid })
    claim (ok claim)
    ERR-NOT-FOUND))

(define-read-only (get-lp (who principal))
  { shares: (get-lp-shares who) })

(define-read-only (pool-stats)
  {
    total-shares: (var-get total-shares),
    pool-balance: (var-get pool-balance),
    reserved: (var-get reserved),
    available: (available-liquidity),
    btc: { price: (var-get last-btc-price), height: (var-get last-btc-height) }
  })
