#update to bora url
pabUrl=http://127.0.0.1:9080/api/new/contract/instance/
echo "setting BORA_PAB_URL to ${pabUrl}"
export BORA_PAB_URL=$pabUrl

#update to path to bora contract file
minterPwd=/home/hilgard/workspace/Bora-Finance-Property-Sale/Minter.cid
echo "setting BORA_CID_MINTER_FILE to ${minterPwd}"
export BORA_CID_MINTER_FILE=$minterPwd

#update to path to bora contract file
sellerPwd=/home/hilgard/workspace/Bora-Finance-Property-Sale/Seller.cid
echo "setting BORA_CID_SELLER_FILE to ${sellerPwd}"
export BORA_CID_SELLER_FILE=$sellerPwd

#update to path to bora contract file
buyer2Pwd=/home/hilgard/workspace/Bora-Finance-Property-Sale/Buyer2.cid
echo "setting BORA_CID_BUYER2_FILE to ${buyer2Pwd}"
export BORA_CID_BUYER2_FILE=$buyer2Pwd

minterCid=
echo "setting BORA_MINTER_CID to ${minterCid}"
export BORA_MINTER_CID=$minterCid

sellerCid=
echo "setting BORA_SELLER_CID to ${sellerCid}"
export BORA_SELLER_CID=$sellerCid

buyer2Cid=
echo "setting BORA_BUYER2_CID to ${buyer2Cid}"
export BORA_BUYER2_CID=$buyer2Cid

