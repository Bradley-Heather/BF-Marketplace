<h1 align="center">Bora Finance Marketplace</h1>

<p align="center"> The Bora marketplace DApp provides a direct connection between property developers and investors of any size. <br />
 Drastically diminishing the barriers of entry into the property market </p>

## Links

- [Repo](https://github.com/Bradley-Heather/Bora-Finance-Property-Sale "<project-name> Repo")

- [Bora Finance website](<Homepage url> "https://borafinance.io/")

## Brief descrtiption of current MVP 
Trusted developers or property owners may mint a fixed number of 'Property Tokens' to represent the real world value of a property. This is a one shot policy locked via the consumption of a eUTXO to ensure no additional tokens can ever enter the market. These Tokens may then be listed on the Bora marketplace and sold to investors.

**Users categories** <br />
Bora Marketplace - Run by the BoraMarket smart contract <br />
Property developer/owner - Trusted party that is given the permision to mint and list property tokens <br />
Investor - Party that purchases property tokens

**Monetization** <br />
Bora Market currently receives a fee from:

Fixed fee for listing a Property <br />
Fixed fee for the sale of any Property Tokens - _This will ultimately be changed to a percentage_ 

_**Disclaimer:** This purely an MVP and proof of concept that has not been audited and is not to be used as production code. Fringe cases may well exist._ 

## Running the DApp

Clone the The Plutus repository:

`git clone https://github.com/input-output-hk/plutus.git`

check out the correct commit as specified in cabal.project:

`git checkout 2f11c28bd8f6d630daab582255e16d8408075bd7`

Enter a nix-shell inside the Plutus directory:

`nix-shell`

Clone this repository and navigate to its root:

`git clone https://github.com/Bradley-Heather/Bora-Finance-Property-Sale.git`

You may need to update Cabal:

`cabal update` 

Build the project:

`cabal build`

Run the executable: 

`cabal run PAB`

## To run the simulator Trace

Start the repl 

`cabal repl`

Load Proprty Sale Test

`:l PropertySaleTest`

Run the trace

`runBoraTrace`

## Running the Web Client

Ensure you have go installed and the executable is available

Ensure the DApp is running as per above

Open a new terminal and change directory to bora-finance-svc

Run the main executable:

`go run main.go`

Open a browser and navigate to localhost:9000

Within the browser you can interact with the Marketplace

## Future Rollouts

- [ ] Automated monthly dividend payouts to all addresses holding relevant 'Property Tokens'
- [ ] NFT proof of ownership 
- [ ] Property Price Oracle that references realworld property price data for secondary token market

_A special thank you to Lars Brunjes as large amount of the Plutus code in this repository draws inspiration from and is heavely influenced by the Plutus Pioneer Program_ 
