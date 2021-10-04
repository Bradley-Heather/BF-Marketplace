package models

type PropertyDetails struct {
	Id					string		`json:"id"`
	TokenName			string		`json:"tokenName"`
	Address 			string		`json:"address"`
	Owners				[]string	`json:"owners"`
	ExpectedYield		int			`json:"yield"`
	TokenPrice 			int			`json:"tokenPrice"`
	NumTokens			int			`json:"numTokens"`
	PictureUrl			string		`json:"-"`
	Picture				[]byte		`json:"-"`
	TokensSold			int			`json:"tokensSold"`
	SellerFunds			int			`json:"sellerFunds"`
}

type TokenName struct {
	TokenName			string		`json:"unTokenName"`
}

type MintParams struct {
	MpTokenName			TokenName		`json:"mpTokenName"`
	MpAmount			int				`json:"mpAmount"`
}

type PageVariables struct {
	Properties 	[]PropertyDetails
}
