package service

import (
	"os"
	"strconv"
	"strings"

	"errors"
	"fmt"
	"io/ioutil"
	"log"
	"net/http"

	"github.com/hilgardvr/bora-finance-svc/models"
)

//todo - onchain state is simulated here, in future will need to sync with chain state 
var properties 	[]models.PropertyDetails
const URL = "localhost:9000/uploads/"

func CheckErr(e error) {
	if e != nil {
		log.Println("Error", e)
	}
}

func uploadFile(r *http.Request) (string, []byte, error) {
	file, handler, err := r.FormFile("picture")
	if err != nil {
		log.Println("Error Retrieving the File")
		log.Println(err)
		return "", []byte{}, err
	}
	err = os.MkdirAll("./uploads", os.ModePerm)
	if err != nil {
		log.Println("Error creating directory", err)
		panic(err)
	}
	log.Printf("Uploaded File: %+v\n", handler.Filename)
	log.Printf("File Size: %+v\n", handler.Size)
	log.Printf("MIME Header: %+v\n", handler.Header)
	dst, err := os.Create(fmt.Sprintf("./uploads/%s", handler.Filename))
	if err != nil {
		log.Println("Error creating file", err)
	}
	defer dst.Close()
	fileBytes, err := ioutil.ReadAll(file)
	if err != nil {
		log.Println(err)
		fileBytes = nil
	}
	// write this byte array to our temporary file
	dst.Write(fileBytes)
	// return that we have successfully uploaded our file!
	log.Printf("Successfully Uploaded File\n")
	return handler.Filename, fileBytes, err
}

//todo validation of form values eg duplicates etc
func parseTokenizeForm(r *http.Request) (models.PropertyDetails, error) {
	err := r.ParseMultipartForm(10 << 20)
	properyDetails := models.PropertyDetails{}
	if err != nil {
		return properyDetails, err
	}
	propName 	:= r.FormValue("tokenName")
	address 	:= r.FormValue("address")
	owner 		:= strings.Split(r.FormValue("owner"), ",")
	yield, err	:= strconv.Atoi(r.FormValue("yield"))
	if err != nil {
		return properyDetails, err
	}
	// value to be set on listing of property
	// value, err	:= strconv.Atoi(r.FormValue("value"))
	// if err != nil {
	// 	return properyDetails, err
	// }
	numNfts, err 	:= strconv.Atoi(r.FormValue("numTokens"))
	if err != nil {
		return properyDetails, err
	}
	name, bytes, err := uploadFile(r)
	if err != nil {
		log.Println("Error uploading file", err)
	}
	propDetails := models.PropertyDetails{
		Id				: fmt.Sprintf("%s%s", propName, address),
		TokenName		: propName,
		Address 		: address,
		Owners			: owner,
		ExpectedYield	: yield,
		TokenPrice		: 0,
		NumTokens		: numNfts,
		Picture			: bytes,
		PictureUrl		: name,
		TokensSold		: 0,
		SellerFunds		: 0,
	}
	return propDetails, nil
}

func checkMintingValues(pd models.PropertyDetails) bool {
	if pd.NumTokens < 0 || pd.TokenName == "" {
		return false
	}
	return true
}

func Mint(r *http.Request) error {
	//only one property can be listed at this time
	//todo expand
	if len(properties) == 0 {
		propertyDetails, err := parseTokenizeForm(r)
		if err != nil {
			log.Println("Error parsing form: ", err)
			return err
		}
		isValid := checkMintingValues(propertyDetails)
		if isValid {
			resp := mintTokens(propertyDetails)
			if resp != nil && resp.StatusCode == http.StatusOK {
				properties = append(properties, propertyDetails)
				return nil
			} else {
				return handleResponseError(resp)
			}
		} else {
			return nil
		}
	} else {
		return nil
	}
}

func ListProperty(r *http.Request) error {
	amount, err := strconv.Atoi(r.FormValue("amount"))
	if err != nil {
		log.Println("Could not parse list amount", err)
		return err
	}
	resp := listProperty(amount)
	if resp.StatusCode == http.StatusOK {
		if len(properties) == 1{
			properties[0].TokenPrice = amount
		}
		return nil
	} else {
		return handleResponseError(resp)
	}
}

func BuyTokens(r *http.Request) error {
	tokenAmount, err	:= strconv.Atoi(r.FormValue("buyAmount"))
	if err != nil {
		log.Println("could not parse buy amount to int", err)
		return err
	}
	 //todo
	// tokenName 	:= r.FormValue("tokenName")
	// buyer 		:= r.FormValue("buyer")
	resp := buyTokens(tokenAmount)
	if resp != nil && resp.StatusCode == http.StatusOK {
		//we get a 200 response even though the buy amount goes over the
		//available amount of tokens
		if (properties[0].TokensSold + tokenAmount <= properties[0].NumTokens &&
		properties[0].TokenPrice > 0) {
			properties[0].TokensSold += tokenAmount
			properties[0].SellerFunds += tokenAmount * properties[0].TokenPrice
		}
		return nil
	} else {
		return handleResponseError(resp)
	}
}

func WithdrawTokens(r *http.Request) error {
	amount, err	:= strconv.Atoi(r.FormValue("withdrawAmount"))
	if err != nil {
		log.Println("could not parse withdraw tokens to int", err)
		return err
	}
	resp := withdrawTokens(amount)
	if resp != nil && resp.StatusCode == http.StatusOK {
		if properties[0].NumTokens - properties[0].TokensSold >= amount {
			properties[0].NumTokens -= amount
		}
		return nil
	} else {
		return handleResponseError(resp)
	}
}

func WithdrawFund(r *http.Request) error {
	amount, err	:= strconv.Atoi(r.FormValue("withdrawAmount"))
	if err != nil {
		log.Println("could not parse withdraw tokens to int", err)
		return err
	}
	resp := withdrawFunds(amount)
	if resp != nil && resp.StatusCode == http.StatusOK {
		if properties[0].SellerFunds >= amount {
			properties[0].SellerFunds -= amount
		}
		return nil
	} else {
		return handleResponseError(resp)
	}	
}

func handleResponseError(resp *http.Response) error {
	if resp == nil {
		msg := "unsuccessfull call to withdraw funds pab - no response"
		log.Println(msg)
		return errors.New(msg)
	} else if resp.StatusCode != http.StatusOK {
		msg := fmt.Sprintf("unsuccessfull call to funds pab - reponse %d: %s", resp.StatusCode, resp.Body)
		log.Println(msg)
		return errors.New(msg)
	}
	return nil
}

func Close() error {
	close()
	return nil
}

func GetProperties() []models.PropertyDetails {
	tmp := make([]models.PropertyDetails, len(properties))
	copy(tmp, properties)
	return tmp
}