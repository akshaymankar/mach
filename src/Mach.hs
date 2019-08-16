{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
module Mach where

import Bosh.Effects
import Bosh.Types      hiding (Release)
import Data.Function   ((&))
import Data.List       (isPrefixOf)
import Lens.Micro
import Polysemy
import System.FilePath
import Utils.Tar
import Codec.Archive.Tar.Entry as Tar
import Codec.Archive.Tar       as Tar
import Numeric (readOct)
import System.Posix.Types

{-|
1. Download all releases
1. Get all compiled packages
1. Put all the compile package files in the given tar
-}

foo :: Member Bosh r => Manifest -> Tar.Entries Tar.FormatError -> Sem r [Tar.Entry]
foo m osTar = do
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
