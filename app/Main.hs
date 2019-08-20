{-# LANGUAGE RecordWildCards #-}
module Main where

import Data.Yaml hiding (Parser)
import Options.Applicative
import Bosh.Types
import Bosh.Effects
import Bosh.Client
import Mach
import Utils.Tar
import System.FilePath

import Polysemy

data Options = Options { manifestFile :: FilePath
                       , baseImageFile :: FilePath
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
      -- boshClient <- boshClientFromEnv
      -- baseImage <- readTgz baseImageFile
      -- es <- runM . runHTTPBosh boshClient $ patchStemcellImage manifest baseImage
      let tmpDir = takeDirectory baseImageFile
      -- writeTgz (tmpDir </> "patched-base-image.tgz") es
      -- stemcellFilename <- buildStemcell stemcellBuilderContainer
      let stemcellFileName = "bosh-stemcell-feeul-google-kvm-ubuntu-xenial-go_agent.tgz"
          version = "feeul"
      stemcell <- readTgz $ tmpDir </> stemcellFileName
      let newStemcell = patchStemcellManifest stemcell
          newStemcellLoc = tmpDir </> "bosh-stemcell-" ++ version ++ "-google-kvm-ubuntu-xenial-mach-go_agent.tgz"
      writeTgz newStemcellLoc newStemcell
