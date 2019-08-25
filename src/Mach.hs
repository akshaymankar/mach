{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Mach where

import Bosh.Effects
import Bosh.Types              hiding (Release)
import Codec.Archive.Tar       as Tar
import Codec.Archive.Tar.Entry as Tar
import Crypto.Hash
import Data.Aeson
import Data.Aeson.Casing
import Data.Aeson.TH
import Data.Function           ((&))
import Data.List               (isPrefixOf)
import Data.List               (find, maximumBy)
import Data.Text               as T (Text, pack, unpack)
import Data.Text.Encoding      as T (encodeUtf8)
import Data.Versions           hiding (Lens')
import GHC.Generics
import Lens.Micro
import Lens.Micro.TH
import Logging
import Numeric                 (readOct)
import Polysemy
import Polysemy.Async
import Prelude                 hiding (log)
import System.FilePath
import System.IO
import System.Posix.Types
import System.Process.Typed
import Text.StringRandom
import Utils.Tar

import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Yaml                  as Yaml

type ReleaseTar = [Tar.Entry]
type OSTar = [Tar.Entry]

data NamedReleaseTar = NamedReleaseTar { nrtName :: Text
                                       , nrtTar  :: [Entry]}

makeLensesFor [("nrtTar", "nrtTarL")] ''NamedReleaseTar


{-|
1. Download all releases
1. Get all compiled packages
1. Put all the compile package files in the given tar
-}
patchStemcellImage ::
  (Member Bosh r, Member Log r)
  => Manifest -> Entries FormatError -> Sem r ([NamedReleaseTar], OSTar)
patchStemcellImage m osTar = do
  let os = maybe "ubuntu-xenial" id (stemcellOs $ head $ stemcells m)
      osVer = stemcellVersion $ head $ stemcells m
      d = name m
  allReleases <- listReleases
  log $ (show $ length allReleases) <> " releases found"
  releaseTars <- map (toExportReleaseOpts allReleases os osVer) (releases m)
                 & map (getReleaseTar d)
                 & sequence
  log "Patching Stemcell"
  let newImage = foldr (addPackagesToOS . nrtTar) (entriesToList osTar) releaseTars
      packagesDir = mkDir $ either (error) id $ toTarPath True "./var/vcap/packages/"
  return (releaseTars, packagesDir : newImage)
  where
    toExportReleaseOpts allReleases stemcell sv r =
      let rv = getReleaseVersion (releaseName r) (releaseVersion r) allReleases
      in ExportReleaseOpts (releaseName r) rv stemcell sv
    mkDir p = Tar.Entry { entryTarPath = p
                        , entryContent = Tar.Directory
                        , entryPermissions = CMode $ fst $ head $ readOct "0755"
                        , entryOwnership = Ownership "" "" 0 0
                        , entryTime = 0
                        , entryFormat = GnuFormat
                        }

uploadReleaseTar ::
  (Member Bosh r, Member Log r) => NamedReleaseTar -> Sem r ()
uploadReleaseTar (NamedReleaseTar n tar) = do
  log $ "uploading release: " <> n
  uploadRelease tar

getReleaseTar :: Member Bosh r => Deployment -> ExportReleaseOpts -> Sem r NamedReleaseTar
getReleaseTar d exportOpts = do
  tar <- exportRelease d exportOpts
         >>= downloadBlobTar
         & (fmap entriesToList)
  return $ NamedReleaseTar (eroRelease exportOpts) tar

getReleaseVersion :: Text -> Text -> [Release] -> Text
getReleaseVersion name "latest"rs =
  let release = find (\r -> relName r == name) rs
                & maybe (error $ "release " <> T.unpack name <> " not found") id
  in maximumBy compareVersions (relReleaseVersions release)
     & rvVersion
     where
       compareVersions :: ReleaseVersion -> ReleaseVersion -> Ordering
       compareVersions v1 v2 = either (error . show) id $ do
         m1 <- mess $ rvVersion v1
         m2 <- mess $ rvVersion v2
         return $ compare m1 m2
getReleaseVersion _ v _ = v

addPackagesToOS :: ReleaseTar -- Release Tar
                -> OSTar -- Stemcell Tar
                -> OSTar
addPackagesToOS releaseTar osTar =
  let packageEntries = releaseTar
                       & (extractCompiledPkgs)
                       & (dropDirectories)
                       & (map entryToTuple)
                       & (mapped . _2 %~ readTgzFromMemory)
                       & (map toStemcellEntry)
                       & (map entriesToList)
                       & (mconcat)
      newImage = packageEntries ++ osTar
  in newImage
  where
    accCompiledPackages e es = if "./compiled_packages/" `isPrefixOf` Tar.entryPath e
                                 then e:es
                                 else es
    extractCompiledPkgs = foldr accCompiledPackages []
    isDirectory = (\p -> Tar.entryContent p == Tar.Directory)
    dropDirectories = filter (not . isDirectory)
    entryToTuple x = (takeBaseName $ Tar.entryPath x, extractFile $ Tar.entryContent x)
    changePath basePath e = case Tar.toTarPath (isDirectory e) $ basePath ++ (drop 1 $ Tar.entryPath e) of
                              Left x  -> error x
                              Right p -> e & entryTarPathL .~ p

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
  v <- stringRandomIO "[a-z]{5}"
  stdoutFile <- openFile "/tmp/build-stemcell.stdout" WriteMode
  stderrFile <- openFile "/tmp/build-stemcell.stderr" WriteMode
  _ <- putStrLn "Building Stemcell"
  let cmd = proc "docker" ["exec", container
                          , "bundle", "exec"
                          , "rake"
                          , "stemcell:build_with_local_os_image[google,kvm,ubuntu,xenial,/opt/bosh/tmp/patched-base-image.tgz,"
                            ++ T.unpack v
                            ++ "]"
                          ]
            & setStdout (useHandleClose stdoutFile)
            & setStderr (useHandleClose stderrFile)
  runProcess_ cmd
  _ <- putStrLn "Done building stemcell"
  return $ T.unpack v

data StemcellManifest = StemcellManifest { smName            :: Text
                                         , smVersion         :: Text
                                         , smBoshProtocol    :: Int
                                         , smApiVersion      :: Int
                                         , smSha1            :: Text
                                         , smOperatingSystem :: Text
                                         , smStemcellFormats :: [Text]
                                         , smCloudProperties :: Value
                                         }
  deriving (Show, Eq, Generic)
makeLensesFor [ ("smName", "nameL")
              , ("smOperatingSystem", "operatingSystemL")
              ] ''StemcellManifest

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
            & nameL .~ "bosh-google-kvm-ubuntu-xenial-mach-go_agent"
            & operatingSystemL .~ "ubuntu-xenial-mach"
    newManifestContents = LBS.fromStrict $ Yaml.encode newManifest
    replaceManifest :: Entry -> Entry
    replaceManifest e = if Tar.entryPath e == "stemcell.MF"
                        then e & entryContentL .~ (NormalFile newManifestContents (LBS.length newManifestContents))
                        else e
    newEntries = Tar.mapEntriesNoFail replaceManifest
  in entriesToList $ newEntries es

data ReleaseManifest = ReleaseManifest { rmName               :: Text
                                       , rmVersion            :: Text
                                       , rmUncommittedChanges :: Bool
                                       , rmCommitHash         :: Text
                                       , rmJobs               :: [RMJob]
                                       }
                       deriving (Show, Eq, Generic)

data RMJob = RMJob { rmjName        :: Text
                   , rmjVersion     :: Text
                   , rmjFingerprint :: Text
                   , rmjSha1        :: Text
                   }
             deriving (Show, Eq, Generic)

makeLensesFor [ ("rmName", "rmNameL")
              , ("rmJobs", "rmJobsL")
              ] ''ReleaseManifest

makeLensesFor [ ("rmjVersion", "rmjVersionL")
              , ("rmjFingerprint", "rmjFingerprintL")
              , ("rmjSha1", "rmjSha1L")
              ] ''RMJob

{-
1. Remove compiled releases entries
1. Open each job remove packages from specs, replace the spec file entry, gzip the tar, replace the jobs tgz
1. Calculate new shasum for each job, replace the shasum in release.MF, replace the entry
-}
patchRelease :: [Entry] -> [Entry]
patchRelease es =
  map patchIfJob es
  & patchReleaseMF
  where
    isJobTGZ e = takeDirectory (entryPath e) == "./jobs" && takeExtension (entryPath e) == ".tgz"
    patchIfJob e = if isJobTGZ e
                   then patchJob e
                   else e

calculateFileFingerprint :: Entry -> Text
calculateFileFingerprint entry =
  let path = T.pack $ entryPath entry
      contents = extractFile $ entryContent entry
      fileDigest = (hash (LBS.toStrict contents) :: Digest SHA256)
                   & T.pack . show
      modeStr = entryPermissions entry
                & T.pack . show
  in path <> fileDigest <> modeStr

calculateJobFingerprint :: Entry -> Text
calculateJobFingerprint entry =
  case entryContent entry of
    NormalFile jobTGZ _ ->
      let jobFiles = readTgzFromMemory jobTGZ
                     & entriesToList
                     & filter isFile
      in map calculateFileFingerprint jobFiles
         & mconcat
         & T.encodeUtf8
         & hash
         & (show :: Digest SHA256 -> String)
         & T.pack
    _ -> error "not a file"

calculateJobSha :: Entry -> Text
calculateJobSha entry =
  case entryContent entry of
    NormalFile jobTGZ _ -> LBS.toStrict jobTGZ
                           & hash
                           & (show :: Digest SHA1 -> String)
                           & T.pack
    _ -> error "not a file"

patchReleaseMF :: [Entry] -> [Entry]
patchReleaseMF es =
  let releaseMF = find isReleaseMF es
                  & maybe (error "release.MF not found!!") id
                  & entryContent
                  & extractFile
                  & Yaml.decodeEither' . LBS.toStrict
                  & either (error . show) id
      isJob e = takeDirectory (entryPath e) == "./jobs"
      jobEntries = filter isJob es
      jobFingerprints = map (\j -> (T.pack $ takeBaseName $ entryPath j, calculateJobFingerprint j)) jobEntries
      jobShas = map (\j ->  (T.pack $ takeBaseName $ entryPath j, calculateJobSha j)) jobEntries
      updateJob j = let fingerPrint = lookup (rmjName j) jobFingerprints
                                      & maybe (error "job not found") id
                        sha = lookup (rmjName j) jobShas
                              & maybe (error "job not found") id
                    in j
                       & rmjFingerprintL .~ fingerPrint
                       & rmjVersionL .~ fingerPrint
                       & rmjSha1L .~ sha
      newReleaseMF = releaseMF
                     & rmNameL <>~ "-mach"
                     & rmJobsL . each %~ updateJob
      replaceReleaseMF e = if isReleaseMF e
                           then e
                                & entryContentL . normalFileContentL .~ (Yaml.encode newReleaseMF & LBS.fromStrict)
                           else e
  in map replaceReleaseMF es
  where isReleaseMF e = takeFileName (entryPath e) == "release.MF"

patchJob :: Entry -> Entry
patchJob e =
  case entryContent e of
    NormalFile jobTGZ _ ->
      let newJobTGZ = jobTGZ
                      & readTgzFromMemory
                      & mapEntriesNoFail patchJobManifest
                      & entriesToList
                      & writeTgzToMemory
      in e & entryContentL .~ NormalFile newJobTGZ (LBS.length newJobTGZ)
    _ -> error "entry for job is not a file!!"
  where patchJobManifest entry =
          if takeFileName (entryPath entry) /= "job.MF"
          then entry
          else
            case entryContent entry of
              NormalFile manifest _ ->
                let newManifest =  Yaml.decodeEither' (LBS.toStrict manifest)
                                   & either (\x -> error $ "error: " <> show x <> "\nin \n" <> (LBS.unpack manifest)) id
                                   & jsPackagesL .~ (Just $ toJSON ([] :: [Text]))
                                   & Yaml.encode
                                   & LBS.fromStrict
                in entry & entryContentL .~ NormalFile newManifest (LBS.length newManifest)
              _ -> error "job.MF is not a file"

data JobSpec = JobSpec { jsName       :: Text
                       , jsPackages   :: Maybe Value
                       , jsProperties :: Maybe Value
                       , jsTemplates  :: Maybe Value
                       , jsConsumes   :: Maybe Value
                       , jsProvides   :: Maybe Value
                       }
               deriving (Show, Eq, Generic)

jsPackagesL :: Lens' JobSpec (Maybe Value)
jsPackagesL f j = f (jsPackages j) <&> \ps' -> j {jsPackages = ps'}

patchManifest :: Manifest -> Manifest
patchManifest = undefined

$(deriveJSON (aesonPrefix snakeCase) ''StemcellManifest)
$(deriveJSON (aesonPrefix snakeCase) ''JobSpec)
$(deriveJSON (aesonPrefix snakeCase) ''RMJob)
$(deriveJSON (aesonPrefix snakeCase) ''ReleaseManifest)
