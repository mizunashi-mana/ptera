{-# LANGUAGE TemplateHaskell #-}

module Types where

import           Data.ByteString (ByteString)
import           Language.Parser.Ptera.TH (LiftType (..))

data Token
    = TokPlus
    | TokMulti
    | TokParenOpen
    | TokParenClose
    | TokLitInteger Integer
    | TokIdentifier ByteString
    | TokEndOfInput
    deriving (Eq, Show)

data Ast
    = Sum Ast Ast
    | Product Ast Ast
    | Var ByteString
    | Value Integer
    deriving (Eq, Show)

instance LiftType Token where
    liftType _ = [t|Token|]

instance LiftType Ast where
    liftType _ = [t|Ast|]
