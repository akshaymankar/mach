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
import Data.Text as T
import GHC.Generics
import Lens.Micro
import Network.HTTP.Client
import Network.HTTP.Types
import Polysemy
import Utils.HTTP
import Utils.Tar
import System.ProgressBar

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

data Task = Task {taskState :: Text, taskDescription :: Text}
  deriving (Show, Eq, Generic)

data BlobstoreObject = BlobstoreObject {boSha1 :: Text, boBlobstoreId :: String}
  deriving (Show, Eq, Generic)

data Bosh m r where
  -- Bosh :: [Text] -> Bosh m (Text, Text)
  ExportRelease :: Deployment -> ExportReleaseOpts -> Bosh m BlobstoreObject
  ListReleases :: Bosh m [Release]
  DownloadBlobTar :: BlobstoreObject -> Bosh m (Tar.Entries Tar.FormatError)
  UploadRelease :: [Tar.Entry] -> Bosh m ()

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
httpOut b (UploadRelease tar) = do
  let tgz = writeTgzToMemory tar
  _ <- putStrLn "uploading"
  req <- parseRequest (baseUrl b  <> "/releases")
         & (>>= addAuthentication b)
         & (fmap $ requestHeadersL <>~ [(hContentType, "application/x-compressed")])
         & (fmap $ methodL .~ methodPost)
         & (fmap $ requestBodyL .~ RequestBodyLBS tgz)
         & (fmap $ redirectCountL .~ 0)
  taskLoc <- withResponse req (manager b) extractTaskRedirect
  waitForTask b taskLoc

download :: Response BodyReader -> IO ByteString
download res = do
  case lookup hContentLength (responseHeaders res) of
    Nothing -> error "no content length!!"
    Just len -> do
      BS.putStrLn $ "Bytes: " <> len
      pb <- newProgressBar defStyle 10 (Progress 0 (read $ BS.unpack len) ())
      x <- mconcat <$> go pb id
      BS.putStrLn "Done"
      return x
      where
        go pb front = do
          x <- responseBody res
          if BS.null x
            then return $ front []
            else incProgress pb (BS.length x) >> go pb (front . (x:))
            --(putStr $ "Downloaded: " ++ (show $ d + BS.length x) ++ "\r") >> go (d + BS.length x) (front . (x:))

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
  downloadTaskOutput b taskLoc

extractTaskRedirect :: MonadThrow m => Response body -> m ByteString
extractTaskRedirect res = do
  when (responseStatus res /= status302)
    $ throwM $ InvalidHTTPStatus (responseStatus res) ""
  case lookup hLocation (responseHeaders res) of
    Nothing          -> throwM $ MissingHeader hLocation
    Just redirectUrl -> return redirectUrl

waitForTask :: BoshClient -> ByteString -> IO ()
waitForTask b redirectUrl = do
  origPort <- port <$> parseRequest (baseUrl b)
  taskReq <- parseRequest (toString redirectUrl)
             & (mapped . portL) .~ origPort -- https://github.com/cloudfoundry/bosh/issues/1253
             & (mapped . redirectCountL) .~ 0
             & (>>= addAuthentication b)
  wait $ httpGet (manager b) taskReq
  where
    wait :: IO Task -> IO ()
    wait action = do
      t@Task{..} <- action
      if taskState == "done"
      then return ()
      else do
        when (taskState == "error") $ throwM $ TaskFailed t
        putStrLn $ "still waiting for task '"  <> T.unpack taskDescription <> "' state: '" <> T.unpack taskState <> "'"
        threadDelay $ 1 * seconds
        wait action

downloadTaskOutput :: FromJSON a => BoshClient -> ByteString -> IO a
downloadTaskOutput b redirectUrl = do
  taskReq <- parseRequest (toString redirectUrl)
             >>= addAuthentication b
  origPort <- port <$> parseRequest (baseUrl b)
  let outputReq = taskReq
                  & portL .~ origPort -- https://github.com/cloudfoundry/bosh/issues/1253
                  & redirectCountL .~ 0
                  & requestPathL <>~ "/output"
                  & queryStringL .~ "type=result"
  httpGet (manager b) outputReq

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
