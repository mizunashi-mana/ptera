module Language.Parser.Ptera.Machine.PEG where

import Language.Parser.Ptera.Prelude

import qualified Data.EnumMap.Strict as EnumMap


type T = PEG

newtype StartPoint = StartPoint Int
    deriving (Eq, Show)
    deriving Enum via Int

newtype VarNum = VarNum Int
    deriving (Eq, Show)
    deriving Enum via Int

data PEG a = PEG
    {
        pegInitials :: EnumMap.EnumMap StartPoint VarNum,
        pegRules :: ()
    }
