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
writeTgz f = LBS.writeFile f . writeTgzToMemory

readTgzFromMemory :: LBS.ByteString -> Entries Tar.FormatError
readTgzFromMemory = Tar.read . GZ.decompress

writeTgzToMemory :: [Entry] -> LBS.ByteString
writeTgzToMemory = GZ.compress . Tar.write

entryTarPathL :: Lens' Entry TarPath
entryTarPathL f e = f (entryTarPath e) <&> \p' -> e {entryTarPath = p'}

entryContentL :: Lens' Entry EntryContent
entryContentL f e = f (entryContent e)  <&> \c' -> e {entryContent = c'}

normalFileContentL :: Lens' EntryContent LBS.ByteString
normalFileContentL f (NormalFile c _) = f c <&> \c' -> NormalFile c' (LBS.length c')
normalFileContentL _ _ =  error "not a normal file"

extractFile :: EntryContent -> LBS.ByteString
extractFile (NormalFile bs _) = bs
extractFile (Directory)       = ""
extractFile _                 = error "not a file"

entriesToList :: Entries FormatError -> [Entry]
entriesToList = foldEntries (:) [] (error . show)

getFile :: FilePath -> Entries FormatError -> Maybe Entry
getFile _ Done = Nothing
getFile _ (Fail e) = error $ show e
getFile name (Next e es) = if entryPath e == name
                           then Just e
                           else getFile name es

isFile :: Entry -> Bool
isFile e = case entryContent e of
             NormalFile _ _ -> True
             _ -> False

getFileContents :: EntryContent -> Maybe LBS.ByteString
getFileContents (NormalFile c _) = Just c
getFileContents _  = Nothing
