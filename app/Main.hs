module Main where

import Data.Yaml hiding (Parser)
import Options.Applicative
import Bosh.Types
import Bosh.Effects
import Bosh.Client
import Mach
import Utils.Tar

import Polysemy

data Options = Options { manifestFile :: FilePath
                       , baseImageFile :: FilePath
                       }

parseOpts :: Parser Options
parseOpts = Options
            <$> strOption ( long "manifest"
                          <> short 'f'
                          <> metavar "FILE")
            <*> strOption ( long "base-os-tar"
                          <> metavar "FILE")

main :: IO ()
main = do
  let opts = info (parseOpts <**> helper)
           (fullDesc
           <> progDesc "Project Mach")
  o <- execParser opts
  m <- decodeFileEither (manifestFile o) :: IO (Either ParseException Manifest)
  case m of
    Left e -> error $ show e
    Right manifest -> do
      boshClient <- boshClientFromEnv
      baseImage <- readTgz $ baseImageFile o
      es <- runM . runHTTPBosh boshClient $ foo manifest baseImage
      writeTgz "/tmp/foo.tgz" es
      -- runM . shellOutBosh $ foo manifest
      -- B.putStrLn $ encode rs
      -- B.putStrLn $ encode manifest
