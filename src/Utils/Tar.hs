{-# LANGUAGE OverloadedStrings #-}
module Utils.Tar where

import Codec.Archive.Tar       as Tar
import Codec.Archive.Tar.Entry as Tar
import Lens.Micro

import qualified Codec.Compression.GZip     as GZ
import qualified Data.ByteString.Lazy.Char8 as LBS

readTgz :: FilePath -> IO (Entries Tar.FormatError)
readTgz f = do
  LBS.readFile f
  & (fmap readTgzFromMemory)

writeTgz :: FilePath -> [Entry] -> IO ()
writeTgz f es = do
  LBS.writeFile f $ GZ.compress $ Tar.write es

readTgzFromMemory :: LBS.ByteString -> Entries Tar.FormatError
readTgzFromMemory = Tar.read . GZ.decompress

entryTarPathL :: Lens' Entry TarPath
entryTarPathL f e = f (entryTarPath e) <&> \p' -> e {entryTarPath = p'}

extractFile :: EntryContent -> LBS.ByteString
extractFile (NormalFile bs _) = bs
extractFile (Directory)       = ""
extractFile _                 = error "not a file"

entriesToList :: Entries FormatError -> [Entry]
entriesToList = foldEntries (:) [] (error . show)

