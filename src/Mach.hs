{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
module Mach where

import Bosh.Effects
import Data.Aeson
import Data.Aeson.Casing
import Data.Aeson.TH
import Bosh.Types      hiding (Release)
import Data.Function   ((&))
import Data.List       (isPrefixOf)
import Lens.Micro
import Polysemy
import System.FilePath
import Data.Text as T (unpack, Text)
import Utils.Tar
import Codec.Archive.Tar.Entry as Tar
import Codec.Archive.Tar       as Tar
import Numeric (readOct)
import System.Posix.Types
import System.Process.Typed
import System.IO
import Text.StringRandom
import GHC.Generics
import Lens.Micro.TH

import qualified Data.Yaml as Yaml
import qualified Data.ByteString.Lazy.Char8 as LBS

{-|
1. Download all releases
1. Get all compiled packages
1. Put all the compile package files in the given tar
-}

patchStemcellImage :: Member Bosh r => Manifest -> Tar.Entries Tar.FormatError -> Sem r [Tar.Entry]
patchStemcellImage m osTar = do
  let rs = head (releases m)
      s = head (stemcells m)
      os = maybe "ubuntu-xenial" id (stemcellOs s)
      d = name m
  blob <- exportRelease d (ExportReleaseOpts (releaseName rs) "0+dev.2" os "315.64")
  packageEntries <- downloadBlobTar blob
                    & (fmap extractCompiledPkgs)
                    & (fmap dropDirectories)
                    & (fmap $ map entryToTuple)
                    & (mapped . mapped . _2 %~ readTgzFromMemory)
                    & (fmap $ map toStemcellEntry)
                    & (fmap $ map entriesToList)
                    & (fmap $ mconcat)
  let packagesDir = mkDir $ either (error) id $ toTarPath True "./var/vcap/packages/"
  return $ packagesDir : entriesToList osTar ++ packageEntries
  where
    accCompiledPackages e es = if "./compiled_packages/" `isPrefixOf` Tar.entryPath e
                                 then e:es
                                 else es
    extractCompiledPkgs = Tar.foldEntries accCompiledPackages [] (error "empty!!")
    isDirectory = (\p -> Tar.entryContent p == Tar.Directory)
    dropDirectories = filter (not . isDirectory)
    entryToTuple x = (takeBaseName $ Tar.entryPath x, extractFile $ Tar.entryContent x)
    changePath basePath e = case Tar.toTarPath (isDirectory e) $ basePath ++ (drop 1 $ Tar.entryPath e) of
                              Left x  -> error x
                              Right p -> e & entryTarPathL .~ p
    mkDir p = Tar.Entry { entryTarPath = p
                        , entryContent = Tar.Directory
                        , entryPermissions = CMode $ fst $ head $ readOct "0755"
                        , entryOwnership = Ownership "" "" 0 0
                        , entryTime = 0
                        , entryFormat = GnuFormat
                        }
    toStemcellEntry (x,y) = let newBasePath = "./var/vcap/packages/" ++ x
                            in Tar.mapEntriesNoFail (changePath newBasePath) y

{-
1. run rake task in given docker container
1. Open stemcell tar
1. Update release.MF
1. Save patched tar
-}

buildStemcell :: String -> IO String
buildStemcell container = do
  _ <- putStrLn "building stemcell"
  version <- stringRandomIO "[a-z]{5}"
  let cmd = proc "docker" ["exec", container
                          , "bundle", "exec"
                          , "rake"
                          , "stemcell:build_with_local_os_image[google,kvm,ubuntu,xenial,/opt/bosh/tmp/patched-base-image.tgz,"
                            ++ T.unpack version
                            ++ "]"
                          ]
            & setStdout (useHandleClose stdout)
            & setStderr (useHandleClose stderr)
  runProcess_ cmd
  return $ "bosh-stemcell-" ++ T.unpack version ++ "-google-kvm-ubuntu-xenial-go_agent.tgz"

data StemcellManifest = StemcellManifest { smName :: Text
                                        , smVersion :: Text
                                        , smBoshProtocol :: Int
                                        , smApiVersion :: Int
                                        , smSha1 :: Text
                                        , smOperatingSystem :: Text
                                        , smStemcellFormats :: [Text]
                                        , smCloudProperties :: Value
                                        }
  deriving (Show, Eq, Generic)
makeLensesFor [ ("smName", "nameLens")
              , ("smOperatingSystem", "osLens")
              ] ''StemcellManifest


-- patchStemcellManifest :: Tar.Entries Tar.FormatError -> [Tar.Entry]
patchStemcellManifest :: Entries FormatError -> [Entry]
patchStemcellManifest es =
  let
    newManifest :: StemcellManifest
    newManifest = getFile "stemcell.MF" es
            & maybe (error "manifest not found") id
            & entryContent
            & getFileContents
            & maybe (error "stemcell.MF is not a file") id
            & Yaml.decodeEither' . LBS.toStrict
            & either (error . show) id
            & nameLens .~ "bosh-google-kvm-ubuntu-xenial-mach-go_agent"
            & osLens .~ "ubuntu-xenial-mach"
    newManifestContents = LBS.fromStrict $ Yaml.encode newManifest
    replaceManifest :: Entry -> Entry
    replaceManifest e = if Tar.entryPath e == "stemcell.MF"
                        then e & entryContentL .~ (NormalFile newManifestContents (LBS.length newManifestContents))
                        else e
    newEntries = Tar.mapEntriesNoFail replaceManifest
  in entriesToList $ newEntries es

$(deriveJSON (aesonPrefix snakeCase) ''StemcellManifest)
