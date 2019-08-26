{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import Bosh.Client
import Bosh.Effects
import Bosh.Types
import Data.Yaml           hiding (Parser)
import Data.Yaml.Pretty
import Lens.Micro
import Logging
import Mach
import Options.Applicative
import Polysemy
import System.FilePath
import Utils.Tar

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text             as T

data Options = Options { manifestFile             :: FilePath
                       , baseImageFile            :: FilePath
                       , stemcellBuilderContainer :: String
                       , stemcellVersion          :: String
                       , newDeployment            :: T.Text
                       }

parseOpts :: Parser Options
parseOpts = Options
            <$> strOption ( long "manifest"
                          <> short 'f'
                          <> metavar "FILE")
            <*> strOption ( long "base-os-tar"
                          <> short 'b'
                          <> metavar "FILE")
            <*> strOption  (long "stemcell-builder-container"
                           <> short 'c'
                           <> metavar "CONTAINER_SHA")
            <*> strOption (long "stemcell-version"
                          <> metavar "STEMCELL_VERSION")
            <*> strOption (long "new-deployment-name"
                          <> short 'd'
                          <> metavar "NAME")

main :: IO ()
main = do
  let opts = info (parseOpts <**> helper)
           (fullDesc
           <> progDesc "Project Mach")
  Options{..} <- execParser opts
  m <- decodeFileEither manifestFile :: IO (Either ParseException Manifest)
  case m of
    Left e -> error $ show e
    Right manifest -> do
      boshClient <- boshClientFromEnv
      baseImage <- readTgz baseImageFile
      (releases, patchedImage) <- runM
                                  . runHTTPBosh boshClient
                                  . runLogStdout
                                  $ patchStemcellImage manifest baseImage
      let tmpDir = takeDirectory baseImageFile
      writeTgz (tmpDir </> "patched-base-image.tgz") patchedImage
      _ <- buildStemcell stemcellVersion stemcellBuilderContainer
      let stemcellFileName = "bosh-stemcell-" ++ stemcellVersion ++ "-google-kvm-ubuntu-xenial-go_agent.tgz"
      stemcell <- readTgz $ tmpDir </> stemcellFileName
      let newStemcell = patchStemcellManifest stemcell
          newStemcellLoc = tmpDir </> "bosh-stemcell-" ++ stemcellVersion ++ "-google-kvm-ubuntu-xenial-mach-go_agent.tgz"
      writeTgz newStemcellLoc newStemcell
      let patchedReleases = map (nrtTarL %~ patchRelease) releases
      _ <- runM
           . runHTTPBosh boshClient
           . runLogStdout
           $ mapM uploadReleaseTar patchedReleases
      _ <- encodePretty (setConfDropNull True defConfig) (patchManifest newDeployment (T.pack stemcellVersion) manifest)
           & BS.writeFile "patched-manifest.yml"
      return ()
