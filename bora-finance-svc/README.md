This service provides a web app and frontend to interact with the Plutus Application Backend.

The following can be done:
- List(tokenize) a property
- Set the price for the tokens
- Tokens can be bought by a buyer
- Funds from sales can be withdrawn by the seller
- Seller can withdraw unsold tokens
- Seller can close the contract


To run this application:
- Ensure golang installed on your system
- Have the PAB running on and listening on the port specified on local.sh (https://github.com/Bradley-Heather/Bora-Finance-Property-Sale)
- Ensure the path to the PAB directory is correct (will need to be update to reflect the path on your system)
- Export environment variables in local.sh to you local env
- From the root directory run: go run main.go
- Open your browser on localhost:9000