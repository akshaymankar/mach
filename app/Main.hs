{-# LANGUAGE RecordWildCards #-}
module Main where

import Bosh.Client
import Bosh.Effects
import Bosh.Types
import Data.Yaml           hiding (Parser)
import Lens.Micro
import Logging
import Mach
import Options.Applicative
import System.FilePath
import Utils.Tar

import Polysemy

data Options = Options { manifestFile             :: FilePath
                       , baseImageFile            :: FilePath
                       , stemcellBuilderContainer :: String
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
      version <- buildStemcell stemcellBuilderContainer
      let stemcellFileName = "bosh-stemcell-" ++ version ++ "-google-kvm-ubuntu-xenial-go_agent.tgz"
      stemcell <- readTgz $ tmpDir </> stemcellFileName
      let newStemcell = patchStemcellManifest stemcell
          newStemcellLoc = tmpDir </> "bosh-stemcell-" ++ version ++ "-google-kvm-ubuntu-xenial-mach-go_agent.tgz"
      let patchedReleases = map (nrtTarL %~ patchRelease) releases
      writeTgz newStemcellLoc newStemcell
      _ <- runM
           . runHTTPBosh boshClient
           . runLogStdout
           $ mapM uploadReleaseTar patchedReleases
      undefined
