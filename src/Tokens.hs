{-# LANGUAGE OverloadedStrings #-}

module Tokens (
    Token (..),
    tokenize
) where

import Data.Char (isSpace, isDigit)
import Data.Bifunctor (first)
import Data.Functor (($>))
import Data.Function ((&))
import Data.List (stripPrefix)
import Control.Applicative ((<|>), some)
import Data.Text (Text)
import qualified Data.Text as T

data Token = Colon 
           | Eq
           | OpenParen
           | CloseParen
           | OpenCurly
           | CloseCurly
           | Name Text
    deriving (Show, Eq)

tokenize :: Text -> [Token]
tokenize str = reverse $ snd $ tokenizeLoop (str, [])

type TokenizeState = (Text, [Token])
tokenizeLoop :: TokenizeState -> TokenizeState
tokenizeLoop st = if continue then tokenizeLoop nextSt else nextSt
    where (continue, nextSt) = tokenizeOnce st

tokenizeOnce :: TokenizeState -> (Bool, TokenizeState)
tokenizeOnce st@(str', acc) = maybe (False, st) newSt result
    where str = T.dropWhile isSpace str'
          result = tokenizeKey str <|> tokenizeName str <|> tokenizePunct str
          newSt (s, t) = (True, (s, t : acc))

puncts :: [(Char, Token)]
puncts = [ ('(', OpenParen)
         , (')', CloseParen)
         , ('{', OpenCurly)
         , ('}', CloseCurly)
         ]

isDelim :: Char -> Bool
isDelim c = isSpace c || c `elem` puncts'
    where puncts' = fst <$> puncts

startsDelim :: Text -> Bool
startsDelim str = T.null str || isDelim (T.head str)

tokenizeKey :: Text -> Maybe (Text, Token)
tokenizeKey str = case T.uncons str of
    Just (':', rest) | startsDelim rest -> Just (rest, Colon)
    Just ('=', rest) | startsDelim rest -> Just (rest, Eq)
    _ -> Nothing

tokenizePunct :: Text -> Maybe (Text, Token)
tokenizePunct str = if T.null str 
    then Nothing 
    else (T.tail str,) <$> lookup (T.head str) puncts

tokenizeName :: Text -> Maybe (Text, Token)
tokenizeName str = if T.null name then Nothing else Just (rest, Name name)
    where (name, rest) = T.break isDelim str