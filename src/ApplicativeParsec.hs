-- This part is taken from http://book.realworldhaskell.org/read/using-parsec.html

module ApplicativeParsec
    (
      module Control.Applicative
    , module Text.ParserCombinators.Parsec
    , (<||>)
    ) where

import Control.Applicative
import Control.Monad (MonadPlus(..), ap)
-- Hide a few names that are provided by Applicative.
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))

-- The Applicative instance for every Monad looks like this.
instance Applicative (GenParser s a) where
    pure  = return
    (<*>) = ap

-- The Alternative instance for every MonadPlus looks like this.
instance Alternative (GenParser s a) where
    empty = mzero
    (<|>) = mplus

-- However, this one we wrote ourselves ;-)
(<||>) :: GenParser a b c -> GenParser a b c -> GenParser a b c
p <||> q = try p <|> try q
