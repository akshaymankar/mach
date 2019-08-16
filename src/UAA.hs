{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module UAA where

import Data.Aeson
import Data.Aeson.Casing
import Data.Aeson.TH
import Data.Text
import GHC.Generics
import Lens.Micro
import Network.HTTP.Client
import Network.HTTP.Types.Header
import URI.ByteString            as B
import Utils.HTTP
import Data.Text.Encoding (encodeUtf8)

import qualified Data.ByteString.Char8 as B

data UAA = UAA { baseURI      :: B.URI
               , manager      :: Manager
               , client       :: B.ByteString
               , clientSecret :: B.ByteString
               }

data Token = Token { tokenTokenType    :: Text
                   , tokenAccessToken  :: Text
                   }
  deriving (Show, Eq, Generic)

$(deriveJSON (aesonPrefix snakeCase) ''Token)

fetchToken :: UAA -> IO Token
fetchToken UAA{..} =
  parseRequest (baseURI
                & pathL <>~ "/oauth/token"
                & queryL <>~ (Query [("grant_type", "client_credentials")])
                & serializeURIRef'
                & B.unpack
               )
  & (fmap $ requestHeadersL <>~ [ (hAccept, "application/json")
                                , (hContentType, "application/x-www-form-urlencoded")
                                ])
  & (fmap $ applyBasicAuth client clientSecret)
  & (>>= flip httpPost manager)

applyUAAAuth :: UAA -> Request -> IO Request
applyUAAAuth uaa req = do
  Token{..} <- fetchToken uaa
  req
    & (requestHeadersL <>~ [(hAuthorization, encodeUtf8 $ tokenTokenType <> " " <> tokenAccessToken)])
    & pure
