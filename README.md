# Cardano Plutus Contracts

Here is a contracts on Cardano Plutus for swapping seal with ada. 

## Setup Playground

This demo is working on a local playground.
> The playground docker-compose from [maccam912/ppp](https://github.com/maccam912/ppp)，
> The playground is using the code about 3-4 month ago, but is newer than the public playground.

Start the playgroundy 
```
cd playground
docker-compose up
```

Wait a while, then open <https://localhost:8009/> in your favorite browser.


## Simulation

Copy the content of `src/swapToken.hs` to play ground. Click "Compile" then "Simulate" button.

## Contracts

### Swap Token

User can exchange token "Seal" with "Lovelace".
> This is not fully complated

## TOOD

- [ ] Off-Chain
  - [x] Send Seal
  - [x] Send Lovelace
  - [ ] Reeder Seal
  - [ ] Reeder Lovelace
- [ ] On-Chain
  - [x] Declare Seal
  - [x] Declare Loveace
  - [ ] Exchange Pool
  - [ ] Validator
- [ ] Upgrade to latest plutus (v1.0.6)


## Referenece

- https://github.com/input-output-hk/plutus/blob/master/README.adoc

