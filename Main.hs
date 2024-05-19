{-# LANGUAGE OverloadedStrings #-}

import Tokens (tokenize, Token (..))
import Ops (registerOps)
import ParsComb (Parser (..), predP, prefixP, getP)
import Parse (expr, Expr)

import Control.Monad.Zip
import Data.Text (Text)

input = 
    "opl _+_ 6\n\
    \op if_then_else_ 1\n\
    \opl _-_ 6\n\
    \op _:=_ 0\n\
    \op _≤_ 4\n\
    \fib n := if 2 ≤ n then fib (n - 1) + fib (n - 2) else 2"

main = do
    let (ops, toks) = fromRight $ registerOps $ tokenize input
    print ops
    print toks
    print $ runP (expr ops) toks

fromRight :: Either a b -> b
fromRight (Right b) = b
fromRight _ = head []