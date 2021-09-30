{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module MarketPlace where

import qualified Data.Aeson              as J
import           GHC.Generics            (Generic)
import           Ledger
import           Plutus.Types.Percentage (Percentage)
import qualified PlutusTx
import           Prelude

data Marketplace =
  Marketplace
    { marketplaceOperator :: PubKeyHash,
      marketplaceMintFee  :: Value,  -- fixed fee by minting and bundling
      marketplaceSaleFee  :: Percentage -- percentage by selling on the Sale or Auction
    }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (J.ToJSON, J.FromJSON)

PlutusTx.makeLift ''Marketplace
PlutusTx.unstableMakeIsData ''Marketplace

data StartMarketplaceParams = StartMarketplaceParams {
    nftFee  :: Integer,
    saleFee :: Ratio Integer
}
    deriving stock    (Haskell.Eq, Haskell.Show, Haskell.Generic)
    deriving anyclass (J.ToJSON, J.FromJSON, Schema.ToSchema)

-- | Starts the NFT Marketplace protocol: minting protocol NFT, creating empty nft storage
start :: StartMarketplaceParams -> Contract w s Text Core.Marketplace
start StartMarketplaceParams {..} = do
    pkh <- pubKeyHash <$> ownPubKey
    saleFeePercentage <- maybe (throwError "Operator's fee value should be in [0, 100]") pure $ mkPercentage saleFee


type MarketplaceOwnerSchema =
    Endpoint "start" StartMarketplaceParams

data OwnerContractState = Started Core.Marketplace
    deriving stock (Haskell.Eq, Haskell.Show, Haskell.Generic)
    deriving anyclass (J.ToJSON, J.FromJSON)

Lens.makeClassyPrisms ''OwnerContractState

ownerEndpoints :: Promise (ContractResponse Text OwnerContractState) MarketplaceOwnerSchema Void ()
ownerEndpoints = withContractResponse (Proxy @"start") Started (start) <> ownerEndpoints
