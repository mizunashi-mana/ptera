module Language.Parser.Ptera.Scanner where

import           Language.Parser.Ptera.Prelude


type T = Scanner

class Monad m => Scanner posMark elem m | m -> posMark, m -> elem where
    consumeInput :: m (Maybe elem)
    getPosMark :: m posMark
    seekToPosMark :: posMark -> m ()
    scanMode :: ScanMode posMark -> m ()

data ScanMode posMark
    = ScanModeNoBack
    | ScanModeNeedBack posMark
    deriving (Eq, Show)


newtype ListScanner e a = ListScanner
    {
        unListScanner :: State [e] a
    }
    deriving (Functor, Applicative, Monad) via State [e]

runListScanner :: ListScanner e a -> [e] -> a
runListScanner (ListScanner scanner) xs = evalState scanner xs

instance Scanner [e] e (ListScanner e) where
    consumeInput = ListScanner do
        get >>= \case
            [] ->
                pure Nothing
            x:xs -> do
                put xs
                pure do Just x

    getPosMark = ListScanner get

    seekToPosMark xs = ListScanner do put xs

    scanMode _ = pure ()
