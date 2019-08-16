{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Bosh.Client where

import Control.Exception.Safe
import Data.Aeson
import Data.Aeson.Casing
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Default.Class         (def)
import Data.Either
import Data.PEM
import Data.String
import Data.Text                  (Text)
import Data.Text.Encoding         (encodeUtf8)
import Data.X509
import Data.X509.CertificateStore (makeCertificateStore)
import GHC.Generics
import Network.Connection         (TLSSettings (..))
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.OAuth.OAuth2
import Network.TLS                as TLS
import Network.TLS.Extra          as TLS
import System.Environment
import URI.ByteString
import Lens.Micro
import Network.OAuth.OAuth2.TokenRequest
import Utils.HTTP
import UAA

import qualified Data.ByteString.Char8 as B

-- TODO: Implement socks proxy
data BoshClientInputs = BoshClientInputs { bciEnv    :: String
                                         , bciClient :: Text
                                         , bciSecret :: Text
                                         , bciCACert :: B.ByteString
                                         }

data BoshClient = BoshClient { baseUrl      :: String
                             , manager      :: Manager
                             , client       :: Text
                             , clientSecret :: Text
                             , auth         :: Auth
                             }

data BoshInfo = BoshInfo { biName               :: Text
                         , biUserAuthentication :: AuthInfo
                         }
              deriving (Show, Eq, Generic)

data AuthInfo = AuthInfo { aiType    :: Text
                         , aiOptions :: Object}
              deriving (Show, Eq, Generic)

data Auth = BasicAuth B.ByteString B.ByteString
          | UAAAuth UAA

parseUAA :: Manager -> Text -> Text -> Value -> Parser UAA
parseUAA mgr client clientSecret =
  withObject "UAA" $
  \v -> do
    url <- B.pack <$> v .: "url"
    case parseURI strictURIParserOptions url of
      Left e    -> Prelude.error $ show e --TODO: Make Parser error
      Right baseURI -> return $ UAA baseURI mgr (encodeUtf8 client) (encodeUtf8 clientSecret)

boshClientInputsFromEnv :: IO BoshClientInputs
boshClientInputsFromEnv = BoshClientInputs
                          <$> lookupEnv' "BOSH_ENVIRONMENT"
                          <*> lookupEnv' "BOSH_CLIENT"
                          <*> lookupEnv' "BOSH_CLIENT_SECRET"
                          <*> lookupEnv' "BOSH_CA_CERT"

mkClient :: BoshClientInputs -> IO BoshClient
mkClient BoshClientInputs{..} = do
    managerSettings <- (mkManagerSettings <$> (tlsSettings bciCACert) <*> pure Nothing)
                       & either throwM pure
    manager <- newManager managerSettings
    info <- httpGet manager =<< parseRequest (infoUrl bciEnv)
    let authInfo = biUserAuthentication info
    auth <-
      if aiType authInfo  == "basic"
      then pure $ BasicAuth (encodeUtf8 bciClient) (encodeUtf8 bciSecret)
      else parseEither (parseUAA manager bciClient bciSecret) (Object $ aiOptions authInfo)
               & either (throwM . InvalidUAAOptions) (pure . UAAAuth)
    return $ BoshClient bciEnv manager bciClient bciSecret  auth

infoUrl :: String -> String
infoUrl base = base ++ "/info"

boshClientFromEnv :: IO BoshClient
boshClientFromEnv = boshClientInputsFromEnv >>= mkClient

lookupEnv' :: IsString s => String -> IO s
lookupEnv' s = do
  maybeVal <- lookupEnv s
  maybe (throwM $ EnvVarNotFound s) (return . fromString) maybeVal

tlsSettings :: B.ByteString -> Either PEMParseException TLSSettings
tlsSettings caCert =
  case pemParseBS caCert of
    Left e     -> Left $ PEMParseException e
    Right pems -> Right $
      let caStore = map (decodeSignedCertificate . pemContent) pems
                    & rights
                    & makeCertificateStore
          defParams = defaultParamsClient "" ""
          tlsParams = defParams{ TLS.clientSupported = def
                                                       { TLS.supportedCiphers = TLS.ciphersuite_strong}
                               , TLS.clientShared    = (TLS.clientShared defParams)
                                                       { TLS.sharedCAStore = caStore}
                               }
       in (TLSSettings tlsParams)

applyAuth :: Auth -> Request -> IO Request
applyAuth (BasicAuth user pass) r = pure $ applyBasicAuth user pass r
applyAuth (UAAAuth uaa) r = applyUAAAuth uaa r

authenticatedReq :: FromJSON a => BoshClient -> Request -> IO a
authenticatedReq BoshClient{..} initialReq = do
  req <- applyAuth auth initialReq
  withResponse req manager jsonResponse

addAuthentication :: BoshClient -> Request -> IO Request
addAuthentication BoshClient{..} initialReq = applyAuth auth initialReq

data EnvVarNotFound = EnvVarNotFound String
  deriving (Show)
instance Exception EnvVarNotFound

data PEMParseException = PEMParseException String
  deriving (Show)
instance Exception PEMParseException

data InvalidUAAOptions = InvalidUAAOptions String
  deriving (Show)
instance Exception InvalidUAAOptions

data OAuthError = OAuthFetchTokenFailed (OAuth2Error Errors)
  deriving (Show)
instance Exception OAuthError

$(deriveJSON (aesonPrefix snakeCase) ''BoshInfo)
$(deriveJSON (aesonPrefix snakeCase) ''AuthInfo)
