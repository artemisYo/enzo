module Parse (
    expr, Expr (..)
) where

import ParsComb (Parser (..), predP, prefixP, getP)
import Ops (Ops (..), OpName)
import Tokens (Token (..))

import qualified Data.IntMap as M
import Data.Text (Text)
import Control.Applicative (some, empty, (<|>))
import Data.List (foldl')

import Debug.Trace (trace)

data Expr = Call [Expr]
          | Infix { lhs :: Expr, op :: Expr, rhs :: Expr }
          | Pfix { op :: Expr, e :: Expr } -- works for both sides
          | Prim [Either Text Expr]
          | Var Text
    deriving (Show)

orElse :: Maybe a -> a -> a
orElse m d = maybe d id m

expr :: Ops -> Parser Maybe Token Expr
expr ops = parseK ops 0

-- right: a <> (b <> c)
-- ([Expr -> Expr], Expr) -> foldr (flip (&)) snd fst
-- left: (a <> b) <> c
-- (Expr, [Expr -> Expr]) -> foldl (&) fst snd
parseK :: Ops -> Int -> Parser Maybe Token Expr
parseK ops p
    | p == (maxPrec ops + 1) = Call <$> some (Var <$> parseAnyName <|> parseK')
    | p == (maxPrec ops + 2) = getP OpenParen *> expr ops <* getP CloseParen
    | otherwise = parseClosed
              <|> Infix <$> parseK' <*> parseNone <*> parseK'
              <|> flip (foldr appl) <$> some (parseR ops p) <*> parseK'
              <|> foldl' (appr) <$> parseK' <*> some (parseL ops p)
              <|> parseK'
    where parseK' = parseK ops (p+1)
          parseNone = parseOps ops $ M.lookup p (none ops) `orElse` empty
          parseClosed = parseOps ops $ M.lookup p (closed ops) `orElse` empty
          isName (Name _) = True
          isName _ = False
          appl f a = f a
          appr a f = f a

parseR :: Ops -> Int -> Parser Maybe Token (Expr -> Expr)
parseR ops p = Pfix <$> parsePrefix
           <|> Infix <$> parseK ops (p+1) <*> parseRight
    where parsePrefix = parseOps ops $ M.lookup p (prefix ops) `orElse` empty
          parseRight = parseOps ops $ M.lookup p (right ops) `orElse` empty

parseL :: Ops -> Int -> Parser Maybe Token (Expr -> Expr)
parseL ops p = Pfix <$> parsePostfix
           <|> (shift Infix) <$> parseLeft <*> parseK ops (p+1)
    where parsePostfix = parseOps ops $ M.lookup p (postfix ops) `orElse` empty
          parseLeft = parseOps ops $ M.lookup p (left ops) `orElse` empty
          shift f b c a = f a b c

parseOps :: Ops -> [OpName] -> Parser Maybe Token Expr
parseOps ops names = foldr (<|>) empty $ parseOp ops <$> names

parseOp :: Ops -> OpName -> Parser Maybe Token Expr
parseOp ops parts = Prim <$> sequenceA (constructOp <$> parts)
    where constructOp = maybe (Right <$> expr ops) ((fmap Left) . parseName)

parseName :: Text -> Parser Maybe Token Text
parseName text = Parser $ \i -> case i of
    ((Name t) : rest) | t == text -> Just (rest, t)
    _ -> Nothing

parseAnyName :: Parser Maybe Token Text
parseAnyName = Parser $ \i -> case i of
    ((Name t) : rest) -> Just (rest, t)
    _ -> Nothing