# BTC Insurance Pool Smart Contract

A decentralized insurance protocol built on Stacks blockchain that enables BTC price protection through STX-denominated coverage policies.

## Overview

This smart contract implements a decentralized insurance pool where:
- Liquidity providers can deposit STX to earn premiums
- Users can purchase coverage against BTC price drops
- Claims are resolved through community voting
- Policies and payouts are denominated in STX

## Key Features

- **Liquidity Provision**
  - Deposit/withdraw STX for pool shares
  - Earn premiums from policy sales
  - Share-weighted voting rights on claims

- **Insurance Policies**
  - Specify BTC price floor and coverage amount
  - Fixed 2% premium on coverage amount
  - Grace period for claim filing

- **Claims Process**
  - Community-governed through share-weighted voting
  - 20% quorum requirement
  - 60% approval threshold for claims to pass

## Technical Details

- **Platform**: Stacks blockchain
- **Language**: Clarity v2
- **Math**: Integer arithmetic with basis points (BPS)

### Constants

```clarity
PREMIUM_BPS:   200  (2% premium)
QUORUM_BPS:    2000 (20% quorum)
PASS_BPS:      6000 (60% to pass)
VOTE_WINDOW:   1440 (~1 day in blocks)
GRACE_BLOCKS:  1440 (grace period)
```

## Usage

### For Liquidity Providers
```clarity
(deposit <amount>)        ;; Deposit STX for pool shares
(withdraw <share-amount>) ;; Withdraw STX
```

### For Users
```clarity
(buy-coverage <amount> <duration> <btc-floor>) ;; Purchase coverage
(file-claim <policy-id> <amount> <reason>)     ;; File claim
```

### For Voters
```clarity
(vote-claim <claim-id> <support>)  ;; Vote on claims
```

## Security

- Oracle-based BTC price feeds
- Share-weighted voting system
- Reserved liquidity for active policies
- Grace periods for claim resolution

## Development

### Prerequisites
- Clarinet
- Stacks blockchain development environment

### Testing
```bash
clarinet test
```

## License

MIT

## Contributing

Pull requests are welcome. For major changes, please open an issue first to discuss what you would like to change.

## Authors

[Your Name]

## Acknowledgments

- Stacks Foundation
- Clarity Lang Team
