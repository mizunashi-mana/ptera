module Language.Parser.Ptera.Scanner where

import           Language.Parser.Ptera.Prelude


type T = Scanner

class Monad m => Scanner p e m | m -> p, m -> e where
    consumeInput :: m (Maybe e)
    getMark :: m p
    seekToMark :: p -> m ()
    scanMode :: ScanMode p -> m ()

data ScanMode p
    = ScanModeNoBack
    | ScanModeNeedBack p
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

    getMark = ListScanner get

    seekToMark xs = ListScanner do put xs

    scanMode _ = pure ()
