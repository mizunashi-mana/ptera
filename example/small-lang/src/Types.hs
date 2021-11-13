module Types where

import Data.ByteString (ByteString)

data Token
    = TokPlus
    | TokMulti
    | TokParenOpen
    | TokParenClose
    | TokLitInteger Integer
    | TokIdentifier ByteString
    deriving (Eq, Show)

data Ast
    = Sum Ast Ast
    | Product Ast Ast
    | Var ByteString
    | Value Integer
    deriving (Eq, Show)
