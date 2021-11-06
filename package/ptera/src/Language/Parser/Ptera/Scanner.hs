module Language.Parser.Ptera.Scanner where

import           Language.Parser.Ptera.Prelude


type T = Scanner

class (Enum e, Monad m) => Scanner p e m | m -> p, m -> e where
    consumeInput :: m (Maybe e)
    getMark :: m p
    seekToMark :: p -> m ()


newtype ListScanner e a = ListScanner
    {
        runListScanner :: State [e] a
    }
    deriving (Functor, Applicative, Monad) via State [e]

instance Enum e => Scanner [e] e (ListScanner e) where
    consumeInput = ListScanner do
        get >>= \case
            [] ->
                pure Nothing
            x:xs -> do
                put xs
                pure do Just x

    getMark = ListScanner get

    seekToMark xs = ListScanner do put xs