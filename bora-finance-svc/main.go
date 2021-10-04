package main

import (
	"log"
	"net/http"
	"github.com/hilgardvr/bora-finance-svc/controllers"
)

const port = ":9000"


func main() {
	log.Println("server running on port", port)
	http.HandleFunc("/", controllers.HomePageController)
	http.HandleFunc("/home", controllers.HomePageController)
	http.HandleFunc("/api/logo", controllers.ServeFlavicon)
	http.HandleFunc("/api/css", controllers.ServeCss)
	http.HandleFunc("/api/images", controllers.ServeImages)
	http.HandleFunc("/api/submitProperty", controllers.Mint)
	http.HandleFunc("/api/buy", controllers.BuyTokens)
	http.HandleFunc("/api/listProperty", controllers.ListProperty)
	http.HandleFunc("/api/withdrawTokens", controllers.WithdrawTokens)
	http.HandleFunc("/api/withdrawFunds", controllers.WithdrawFunds)
	http.HandleFunc("/api/close", controllers.Close)
	log.Fatal(http.ListenAndServe(port, nil))
}