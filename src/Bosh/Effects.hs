{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}
module Bosh.Effects where

import Bosh.Client
import Control.Concurrent
import Control.Exception.Safe
import Control.Monad
import Data.Aeson
import Data.Aeson.Casing
import Data.Aeson.TH
import Data.ByteString.UTF8
import Data.Text
import GHC.Generics
import Lens.Micro
import Network.HTTP.Client
import Network.HTTP.Types
import Polysemy
import Utils.HTTP

import qualified Codec.Archive.Tar          as Tar
import qualified Codec.Compression.GZip     as GZ
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as LBS

data ExportReleaseOpts = ExportReleaseOpts { eroRelease        :: Text
                                           , eroReleaseVersion :: Text
                                           , eroOS             :: Text
                                           , eroOSVersion      :: Text
                                           }
data ExportReleaseResponse = ExportReleaseResponse { errBlobstoreId :: Text
                                                   , errSha1        :: Text
                                                   }
                             deriving (Show, Eq, Generic)

data TaskShortResponse = TaskShortResponse { tsrId    :: Int
                                           , tsrState :: Text
                                           }

type Deployment = Text

data Release = Release { relName            :: Text
                       , relReleaseVersions :: [ReleaseVersion]
                       }
               deriving (Show, Eq, Generic)

data ReleaseVersion = ReleaseVersion { rvVersion            :: Text
                                     , rvCommitHash         :: Text
                                     , rvUncommittedChanges :: Bool
                                     , rvCurrentlyDeployed  :: Bool
                                     , rvJobNames           :: [Text]
                                     }
                      deriving (Show, Eq, Generic)

data Task = Task {taskState :: Text}
  deriving (Show, Eq, Generic)

data BlobstoreObject = BlobstoreObject {boSha1 :: Text, boBlobstoreId :: String}
  deriving (Show, Eq, Generic)

data Bosh m r where
  -- Bosh :: [Text] -> Bosh m (Text, Text)
  ExportRelease :: Deployment -> ExportReleaseOpts -> Bosh m BlobstoreObject
  ListReleases :: Bosh m [Release]
  DownloadBlobTar :: BlobstoreObject -> Bosh m (Tar.Entries Tar.FormatError)

makeSem ''Bosh

runHTTPBosh :: Member (Embed IO) r => BoshClient -> Sem (Bosh ': r) a -> Sem r a
runHTTPBosh b =
  interpret $ embed . httpOut b

httpOut :: BoshClient -> Bosh m r -> IO r
httpOut b ListReleases = do
  req <- parseRequest (baseUrl b <> "/releases")
  authenticatedReq b req
httpOut b (ExportRelease deployment opts@(ExportReleaseOpts{..})) =
  httpExportRelease b deployment opts
httpOut b (DownloadBlobTar BlobstoreObject{..}) = do
  downloadReq <- parseRequest (baseUrl b <> "/resources/" <> boBlobstoreId)
                 & (>>= addAuthentication b)
  withResponse downloadReq (manager b) $ \res -> do
    gzipped <- download res --mconcat <$> (brConsume $ responseBody res)
    return $ Tar.read $ GZ.decompress $ LBS.fromStrict gzipped


download :: Response BodyReader -> IO ByteString
download res = do
  case lookup hContentLength (responseHeaders res) of
    Nothing -> error "no content length!!"
    Just len -> do
      BS.putStrLn $ "Bytes: " <> len
      mconcat <$> go id
      where
        go front = do
          x <- responseBody res
          if BS.null x
            then return $ front []
            else (putStrLn $ "Downloaded: " ++ (show $ BS.length x)) >> go (front . (x:))

httpExportRelease
  :: BoshClient -> Deployment -> ExportReleaseOpts -> IO BlobstoreObject
httpExportRelease b deployment (ExportReleaseOpts{..}) = do
  let body = object [ "deployment_name" .= deployment
                    , "release_name" .= eroRelease
                    , "release_version" .= eroReleaseVersion
                    , "stemcell_os" .= eroOS
                    , "stemcell_version" .= eroOSVersion
                    , "sha2" .= True
                    , "jobs" .= ([] :: [()])
                    ]
  req <-
    parseRequest (baseUrl b <> "/releases/export")
    & (mapped . requestBodyL) .~ (RequestBodyBS $ LBS.toStrict $ encode body)
    & (mapped . requestHeadersL) <>~ [(hContentType, "application/json")]
    & (mapped . redirectCountL) .~ 0
    & (mapped . methodL) .~ methodPost
    & (>>= addAuthentication b)
  taskLoc <- withResponse req (manager b) extractTaskRedirect
  waitForTask b taskLoc

extractTaskRedirect :: MonadThrow m => Response body -> m ByteString
extractTaskRedirect res = do
  when (responseStatus res /= status302)
    $ throwM $ InvalidHTTPStatus (responseStatus res) ""
  case lookup hLocation (responseHeaders res) of
    Nothing          -> throwM $ MissingHeader hLocation
    Just redirectUrl -> return redirectUrl

waitForTask :: FromJSON a => BoshClient -> ByteString -> IO a
waitForTask b redirectUrl = do
  req <- parseRequest (baseUrl b)
  let origPort = port req
  taskReq <- parseRequest (toString redirectUrl)
             & (mapped . portL) .~ origPort -- https://github.com/cloudfoundry/bosh/issues/1253
             & (mapped . redirectCountL) .~ 0
             & (>>= addAuthentication b)
  wait $ httpGet (manager b) taskReq
  let outputReq = taskReq
                  & requestPathL <>~ "/output"
                  & queryStringL .~ "type=result"
  httpGet (manager b) outputReq
  -- LBS.putStrLn $ encode v
  where
    wait :: IO Task -> IO ()
    wait action = do
      t <- action
      if taskState t == "done"
      then return ()
      else do
        when (taskState t == "error") $ throwM $ TaskFailed t
        putStrLn "still waiting"
        threadDelay $ 1 * seconds
        wait action

seconds :: Int
seconds = 1_000_000

data TaskFailed = TaskFailed Task
  deriving (Show)
instance Exception TaskFailed


$(deriveJSON (aesonPrefix snakeCase) ''ExportReleaseResponse)
$(deriveJSON (aesonPrefix snakeCase) ''Release)
$(deriveJSON (aesonPrefix snakeCase) ''ReleaseVersion)
$(deriveJSON (aesonPrefix snakeCase) ''Task)
$(deriveJSON (aesonPrefix snakeCase) ''BlobstoreObject)
