module Database where

import Types
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad
import System.Directory
import System.FilePath (takeExtension)

-- Recursively add directories to the database
-- TODO change path to be relative to "root"
addDir :: FilePath -> IO Database
addDir dir = do
    fs <- getDirectoryContents dir
    let fs' = map (dir++) $ filter (\(x:xs) -> x /= '.') fs

    subdirs <- filterM doesDirectoryExist fs' 
    files   <- filterM doesFileExist fs'
    subDB   <- mapM addDir $ map (++"/") subdirs

    return $ Dir dir
        (Set.fromList $ map mkSong $ filter isMedia files)
        (Map.fromList $ map (\d@(Dir f _ _) -> (f,d)) subDB)

-- Checks if the a file is a music file
isMedia :: FilePath -> Bool
isMedia f = takeExtension f == ".mp3"

-- Reads the file, and computes ID3 and such
mkSong :: FilePath -> Song
mkSong f = undefined
