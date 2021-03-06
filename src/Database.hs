{-# OPTIONS -w #-}
module Database () where

import Types
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad
import System.Directory
import System.FilePath
import qualified Codecs.Mp3.ID3 as Id3
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BLazy
import qualified Data.Binary.Get as Bin
import Data.List

addDir :: FilePath -> IO Database
addDir path = do
    allFiles <- getDirectoryContents path
    let allFiles' = map (\f -> path </> f) $ filter (\(x:xs) -> x /= '.') allFiles
    (dirs,files) <- splitFiles allFiles'
    subDirs       <- mapM addDir dirs
    let subDirs'  = filter nonEmpty subDirs
    contents <- mapM BLazy.readFile files
    return $ Dir path (Set.fromList $ map mkSong contents)
                      (Map.fromList $ map (\d@(Dir f _ _) -> (f,d)) subDirs')
  where
    nonEmpty (Dir _ d f) | Set.null d && Map.null f = False
                         | otherwise                = True
    
    splitFiles :: [FilePath] -> IO ([FilePath],[FilePath])
    splitFiles []             = return ([],[])
    splitFiles (x:xs)         = do
        dir <- doesDirectoryExist x
        (dirs,files) <- splitFiles xs
        if dir then return $ (x:dirs,files)
               else if isMedia x then return $ (dirs,x:files)
                                 else return $ (dirs,files)

-- Checks if the a file is a music file
isMedia :: FilePath -> Bool
isMedia f = takeExtension f == ".mp3"

-- Reads the file, and computes ID3 and such
-- Currently we only check for Id3v1 and print the Id3v1 content
mkSong :: BLazy.ByteString -> String
mkSong =  p . (Bin.runGet Id3.getId3v1)
    where p (title, artist, album, year, comment, genre)
              = foldr ((++) . (++) " " . B.unpack) ""
                [title,artist,album,year,comment,genre]