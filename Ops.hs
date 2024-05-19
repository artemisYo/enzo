{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Ops (
    registerOps, OpsError, Ops (..), OpName
) where

import Data.IntMap (IntMap)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.IntMap as M
import Data.Char (ord)
import Data.List (intersperse, length)

import Tokens (Token(..))

data Ops = Ops
    { none :: IntMap [OpName]
    , left :: IntMap [OpName]
    , right :: IntMap [OpName]
    , closed :: IntMap [OpName]
    , prefix :: IntMap [OpName]
    , postfix :: IntMap [OpName]
    , maxPrec :: Int
    } deriving (Show)
type OpName = [Maybe Text]

defaultOps :: Ops
defaultOps = Ops M.empty M.empty M.empty M.empty M.empty M.empty 0

data OpsError = InvalidOpName Text
              | InvalidPrecLvl Text
              | WrongArgTypes [Token]
        deriving (Show)

registerOps :: [Token] -> Either OpsError (Ops, [Token])
registerOps ts = (fmap reverse) <$> registerOps' defaultOps [] ts 

data Assoc = AssocLeft | AssocRight | AssocNone deriving (Show)
data Fixity = Postfix | Prefix | Closed | Infix deriving (Show)

registerOps' :: Ops -> [Token] -> [Token] -> Either OpsError (Ops, [Token])
registerOps' ops acc [] = Right (ops, acc)
registerOps' ops acc (Name n : ts) = maybe 
    (registerOps' ops (Name n : acc) ts) 
    (\assoc -> case ts of
        (Name name : Name prec : rest) -> 
            addOp ops assoc (parseOpName name) (parseNum prec) >>= \o -> registerOps' o acc rest
        _ -> Left $ WrongArgTypes $ take 5 ts
    )
    $ parseAssoc n
registerOps' ops acc (t : ts) = registerOps' ops (t : acc) ts

addOp :: Ops -> Assoc -> Either Text (Fixity, OpName) -> Either Text Int -> Either OpsError Ops
addOp _ _ (Left t) _ = Left $ InvalidOpName t
addOp _ _ _ (Left t) = Left $ InvalidPrecLvl t
addOp ops' assoc (Right (fixity, name)) (Right prec) = Right $ case fixity of
    Postfix -> ops { postfix = addName (postfix ops) }
    Prefix -> ops { prefix = addName (prefix ops) }
    Closed -> ops { closed = addName (closed ops) }
    Infix -> case assoc of
        AssocRight -> ops { right = addName (right ops) }
        AssocLeft -> ops { left = addName (left ops) }
        AssocNone -> ops { none = addName (none ops) }
    where addName map = M.alter (\names -> Just $ maybe [name] (name:) names) prec map
          ops = ops' { maxPrec = max prec (maxPrec ops') }

parseNum :: Text -> Either Text Int
parseNum n = T.foldl (\acc c -> pushAcc acc <*> decDigit c) (Right 0) n
    where pushAcc = fmap (+) . fmap (*10)
          decDigit c = if '0' <= c && c <= '9' 
            then Right (ord c - ord '0')
            else Left n

parseOpName :: Text -> Either Text (Fixity, OpName)
parseOpName n = maybe (Left n) Right $ fixity $ validate $ parseName n
    where removeEmpty = filter $ maybe True (not . T.null)
          makeHoles = intersperse Nothing . fmap Just
          parseName = removeEmpty . makeHoles . T.splitOn "_"
          validate = boolWith Nothing Just (\l -> length l > 1 && not (repeats l))
          fixity = fmap $ \n -> case (head n, last n) of 
                (Nothing, Nothing) -> (Infix, tail $ init n)
                (Nothing, Just _) -> (Postfix, tail n)
                (Just _, Nothing) -> (Prefix, init n)
                (Just _, Just _) -> (Closed, n) -- remove leading and trailing holes for parsing

boolWith :: b -> (a -> b) -> (a -> Bool) -> a -> b
boolWith b f p a = if p a then f a else b 

repeats :: Eq a => [a] -> Bool
repeats [] = False
repeats (x : []) = False;
repeats (x : y : rest) = x == y || repeats (y : rest) 

parseAssoc :: Text -> Maybe Assoc
parseAssoc "opl" = Just AssocLeft
parseAssoc "opr" = Just AssocRight
parseAssoc "op" = Just AssocNone
parseAssoc _ = Nothing