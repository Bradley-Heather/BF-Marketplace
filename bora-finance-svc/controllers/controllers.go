package controllers

import (
	"html/template"
	"log"
	"net/http"

	"github.com/hilgardvr/bora-finance-svc/service"
)

//todo dynamic populate
func ServeImages(w http.ResponseWriter, r *http.Request) {
    p := "./uploads/mansion.jpg"
    http.ServeFile(w, r, p)
}

func ServeFlavicon(w http.ResponseWriter, r *http.Request) {
    http.ServeFile(w, r, "./static/BoraLogo.png")
}

func ServeCss(w http.ResponseWriter, r *http.Request) {
    http.ServeFile(w, r, "./static/styles.css")
}

func HomePageController(w http.ResponseWriter, r *http.Request) {
	allProperties := service.GetProperties()

	t, err := template.ParseFiles("./static/index.html")
	if err != nil {
		log.Fatalln("template parsing err", err)
	}

	err = t.Execute(w, allProperties)
	
	if err != nil {
		log.Fatalln("template executing err", err)
	}
}

func BuyTokens(w http.ResponseWriter, r *http.Request) {
	if r.Method == http.MethodPost {
		service.BuyTokens(r)
		ref := r.Header.Get("Referer")
		http.Redirect(w, r, ref, http.StatusSeeOther)
	} else {
		http.Error(w, "Expecting POST request", http.StatusMethodNotAllowed)
	}
}

func WithdrawTokens(w http.ResponseWriter, r *http.Request) {
	if r.Method == http.MethodPost {
		service.WithdrawTokens(r)
		ref := r.Header.Get("Referer")
		http.Redirect(w, r, ref, http.StatusSeeOther)
	} else {
		http.Error(w, "Expecting POST request", http.StatusMethodNotAllowed)
	}
}

func WithdrawFunds(w http.ResponseWriter, r *http.Request) {
	if r.Method == http.MethodPost {
		service.WithdrawFund(r)
		ref := r.Header.Get("Referer")
		http.Redirect(w, r, ref, http.StatusSeeOther)
	} else {
		http.Error(w, "Expecting POST request", http.StatusMethodNotAllowed)
	}
}

func Close(w http.ResponseWriter, r *http.Request) {
	if r.Method == http.MethodPost {
		service.Close()
		ref := r.Header.Get("Referer")
		http.Redirect(w, r, ref, http.StatusSeeOther)
	} else {
		http.Error(w, "Expecting POST request", http.StatusMethodNotAllowed)
	}
}

func ListProperty(w http.ResponseWriter, r *http.Request) {
	if r.Method == http.MethodPost {
		err := service.ListProperty(r)
		if err != nil {
			http.Error(w, err.Error(), http.StatusInternalServerError)
		} else {
			ref := r.Header.Get("Referer")
			http.Redirect(w, r, ref, http.StatusSeeOther)
		}
	} else {
		http.Error(w, "Expecting POST request", http.StatusMethodNotAllowed)
	}
}

func Mint(w http.ResponseWriter, r *http.Request) {
	if r.Method == http.MethodPost {
		service.Mint(r)
		ref := r.Header.Get("Referer")
		http.Redirect(w, r, ref, http.StatusSeeOther)
	} else {
		http.Error(w, "Expecting POST request", http.StatusMethodNotAllowed)
	}
}

