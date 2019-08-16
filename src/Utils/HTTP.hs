module Utils.HTTP where

import Control.Exception.Safe
import Data.Aeson
import Data.ByteString           (ByteString)
import Lens.Micro
import Network.HTTP.Client
import Network.HTTP.Types.Header
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status

-- Convinience functions

httpGet :: FromJSON a => Manager -> Request -> IO a
httpGet mgr req = do
  withResponse req mgr jsonResponse

httpPost :: FromJSON a => Request -> Manager -> IO a
httpPost req mgr = withResponse req mgr jsonResponse

jsonResponse :: FromJSON a => Response BodyReader -> IO a
jsonResponse res =
    if responseStatus res /= status200
    then do
      body <- mconcat <$> (brConsume $ responseBody res)
      throwM $ InvalidHTTPStatus (responseStatus res) body
    else do
      body <- mconcat <$> (brConsume $ responseBody res)
      case eitherDecodeStrict body of
        Left e  -> throwM $ InvalidJSON e body
        Right b -> return b

-- Lenses

requestHeadersL :: Lens' Request RequestHeaders
requestHeadersL f req = f (requestHeaders req) <&> \rh' -> req { requestHeaders = rh' }

requestBodyL :: Lens' Request RequestBody
requestBodyL f req = f (requestBody req) <&> \rb' -> req { requestBody = rb' }

methodL :: Lens' Request Method
methodL f req = f (method req) <&> \m' -> req { method = m' }

redirectCountL :: Lens' Request Int
redirectCountL f req = f (redirectCount req) <&> \m' -> req { redirectCount = m' }

portL :: Lens' Request Int
portL f req = f (port req) <&> \p' -> req {port = p'}

queryStringL :: Lens' Request ByteString
queryStringL f req = f (queryString req) <&> \q' -> req {queryString = q'}

requestPathL :: Lens' Request ByteString
requestPathL f req = f (path req) <&> \q' -> req {path = q'}

-- Exceptions

data InvalidHTTPResponse = InvalidHTTPStatus Status ByteString
                         | InvalidJSON String ByteString
                         | MissingHeader HeaderName
  deriving (Show)
instance Exception InvalidHTTPResponse

