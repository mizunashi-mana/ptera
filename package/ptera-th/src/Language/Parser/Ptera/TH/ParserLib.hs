module Language.Parser.Ptera.TH.ParserLib (
    module Language.Parser.Ptera.Runner.Parser,
    module Data.Proxy,
    Parser,
    pteraTHTokenToTerminal,
    pteraTHArrayIndex,
    pteraTHArrayFromList,
    pteraTHLookupTable8,
    pteraTHLookupTable16,
    pteraTHLookupTable32,
    pteraTHUnsafeCoerce,
    pteraTHUnsafeRunner,
    pteraTHAction,
) where

import           Language.Parser.Ptera.Prelude

import           Data.Proxy                          (Proxy (..))
import qualified Data.Array                          as Array
import qualified Data.Bits                           as Bits
import qualified GHC.Prim                            as Prim
import qualified GHC.ST                              as ST
import qualified GHC.Types                           as Types
import           Language.Parser.Ptera.Runner.Parser (AltKind (..), RunnerParser (..),
                                                      Trans (..), TransOp (..), Action)
import qualified Unsafe.Coerce                       as Unsafe
import qualified Language.Parser.Ptera.Data.HEnum as HEnum
import qualified Language.Parser.Ptera.Syntax.GrammarToken as GrammarToken
import qualified Language.Parser.Ptera.Runner as Runner
import qualified Language.Parser.Ptera.Runner.Parser as RunnerParser

type Parser = Runner.T

pteraTHTokenToTerminal :: GrammarToken.GrammarToken e q => Proxy q -> e -> Int
pteraTHTokenToTerminal p t = HEnum.unsafeHEnum do GrammarToken.tokenToTerminal p t

pteraTHArrayIndex :: Array.Array Int e -> Int -> e
pteraTHArrayIndex arr i = arr Array.! i

pteraTHArrayFromList :: Int -> [e] -> Array.Array Int e
pteraTHArrayFromList b l = Array.listArray (0, b) l

pteraTHLookupTable8 :: Int -> Prim.Addr# -> Int -> Int -> Int
pteraTHLookupTable8 offset table# s c = do
    let !(Types.I# i#) = s `Bits.shiftL` offset + c
    ST.runST do
        ST.ST \s0# -> do
            let !(# s1#, r# #) = Prim.readWord8OffAddr# table# i# s0#
            case r# of
                255## -> (# s1#, -1 #)
                _     -> (# s1#, Types.I# do Prim.word2Int# r# #)

pteraTHLookupTable16 :: Int -> Prim.Addr# -> Int -> Int -> Int
pteraTHLookupTable16 offset table# s c = do
    let !(Types.I# i#) = s `Bits.shiftL` offset + c
    ST.runST do
        ST.ST \s0# -> do
            let !(# s1#, r# #) = Prim.readWord16OffAddr# table# i# s0#
            case r# of
                65535## -> (# s1#, -1 #)
                _       -> (# s1#, Types.I# do Prim.word2Int# r# #)

pteraTHLookupTable32 :: Int -> Prim.Addr# -> Int -> Int -> Int
pteraTHLookupTable32 offset table# s c = do
    let !(Types.I# i#) = s `Bits.shiftL` offset + c
    ST.runST do
        ST.ST \s0# -> do
            let !(# s1#, r# #) = Prim.readInt32OffAddr# table# i# s0#
            (# s1#, Types.I# r# #)

pteraTHUnsafeCoerce :: a -> b
pteraTHUnsafeCoerce = Unsafe.unsafeCoerce

pteraTHUnsafeRunner :: RunnerParser e -> Parser s h e
pteraTHUnsafeRunner p = Runner.UnsafeRunner p

pteraTHAction :: ([a] -> b) -> Action
pteraTHAction f = RunnerParser.Action do Unsafe.unsafeCoerce f
