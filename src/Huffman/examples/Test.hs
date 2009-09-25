{-# LANGUAGE BangPatterns #-}
{-# OPTIONS -Wall #-}

module Main where

--
-- This implementation of Huffman codes is designed to be very specific
-- and is suitable for when working with files, or other data that concists
-- of bytes.
--

import Huffman
import Huffman8
import Data.Word
import System.Environment
import qualified Data.ByteString as B

-- Simple main function, for testing purposes
main :: IO ()
main = do
    args <- getArgs
    case args of
        [m,f] -> case m of
            "encode" -> encoder f
            "decode" -> decoder f >>= print
            _        -> print "valid actions are \"encode\" and \"decode\""
        [_] -> print "you need to supply a file"
        []  -> print "you need to supply action and file"
        _   -> print "usage: program <action> <file>"

decoder :: FilePath -> IO [Word8]
decoder file = do
    treeFile <- readFile $ file++".tree"
    let !tree = read treeFile :: HuffTree Word8
    enc <- B.readFile file
    let res = decode (B.unpack enc) tree tree 7 0
    return res

encoder :: FilePath -> IO ()
encoder file = do
    f <- B.readFile file
    f `seq` print "Read file"
    let tree = mkTree f 
        arr  = tree2arr tree
        (res, _)  = encode arr f
    B.writeFile (file ++ ".huff") res
    writeFile (file ++ ".huff.tree") (show tree)

