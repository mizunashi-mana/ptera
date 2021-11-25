module Language.Parser.Ptera.TH.ParserLib (
    module Language.Parser.Ptera.Runner.Parser,
    pteraTHArrayIndex,
    pteraTHArrayFromList,
    pteraTHLookupTable8,
    pteraTHLookupTable16,
    pteraTHLookupTable32,
    pteraTHUnsafeCoerce,
) where

import           Language.Parser.Ptera.Prelude

import qualified Data.Array                          as Array
import qualified Data.Bits                           as Bits
import qualified GHC.Prim                            as Prim
import qualified GHC.ST                              as ST
import qualified GHC.Types                           as Types
import           Language.Parser.Ptera.Runner.Parser (AltKind (..), Parser (..),
                                                      Trans (..), TransOp (..))
import qualified Unsafe.Coerce as Unsafe


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
