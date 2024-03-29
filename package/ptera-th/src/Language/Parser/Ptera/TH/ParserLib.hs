module Language.Parser.Ptera.TH.ParserLib (
    module Language.Parser.Ptera.Runner.Parser,
    module Data.Proxy,
    Parser,
    PteraTHArray,
    pteraTHTokenToTerminal,
    pteraTHArrayIndex,
    pteraTHArrayFromList,
    pteraTHLookupTable8,
    pteraTHLookupTable16,
    pteraTHLookupTable32,
    pteraTHUnsafeExtractReduceArgument,
    pteraTHUnsafeRunner,
    pteraTHAction,
    pteraTHActionTaskPure,
) where

import           Language.Parser.Ptera.Prelude

import qualified Data.Array                          as Array
import qualified Data.Bits                           as Bits
import           Data.Proxy                          (Proxy (..))
import qualified GHC.Prim                            as Prim
import qualified GHC.ST                              as ST
import qualified GHC.Types                           as Types
import qualified Language.Parser.Ptera.Data.HEnum    as HEnum
import qualified Language.Parser.Ptera.Runner        as Runner
import           Language.Parser.Ptera.Runner.Parser (ActionM (..), ActionTask,
                                                      AltKind (..),
                                                      GrammarToken (..),
                                                      ReduceArgument (..),
                                                      RunnerParser (..),
                                                      Trans (..), TransOp (..),
                                                      failAction, getAction,
                                                      modifyAction)
import qualified Unsafe.Coerce                       as Unsafe

type Parser = Runner.T

type PteraTHArray = Array.Array

pteraTHTokenToTerminal :: GrammarToken tokens elem => Proxy tokens -> elem -> Int
pteraTHTokenToTerminal p t = HEnum.unsafeHEnum do tokenToTerminal p t

pteraTHArrayIndex :: PteraTHArray Int e -> Int -> e
pteraTHArrayIndex arr i = arr Array.! i

pteraTHArrayFromList :: Int -> [e] -> PteraTHArray Int e
pteraTHArrayFromList b l = Array.listArray (0, b) l

pteraTHLookupTable8 :: Int -> Prim.Addr# -> Int -> Int -> Int
pteraTHLookupTable8 offset table# s c = do
    let !(Types.I# i#) = s `Bits.shiftL` offset + c
    ST.runST do
        ST.ST \s0# -> do
            let !(# s1#, r# #) = Prim.readWord8OffAddr# table# i# s0#
            case Prim.word2Int# do Prim.word8ToWord# r# of
                255# -> (# s1#, -1 #)
                ri#  -> (# s1#, Types.I# ri# #)

pteraTHLookupTable16 :: Int -> Prim.Addr# -> Int -> Int -> Int
pteraTHLookupTable16 offset table# s c = do
    let !(Types.I# i#) = s `Bits.shiftL` offset + c
    ST.runST do
        ST.ST \s0# -> do
            let !(# s1#, r# #) = Prim.readWord16OffAddr# table# i# s0#
            case Prim.word2Int# do Prim.word16ToWord# r# of
                65535# -> (# s1#, -1 #)
                ri#    -> (# s1#, Types.I# ri# #)

pteraTHLookupTable32 :: Int -> Prim.Addr# -> Int -> Int -> Int
pteraTHLookupTable32 offset table# s c = do
    let !(Types.I# i#) = s `Bits.shiftL` offset + c
    ST.runST do
        ST.ST \s0# -> do
            let !(# s1#, r# #) = Prim.readInt32OffAddr# table# i# s0#
            (# s1#, Types.I# do Prim.int32ToInt# r# #)

pteraTHUnsafeExtractReduceArgument :: ReduceArgument -> a
pteraTHUnsafeExtractReduceArgument = \case
    ReduceArgument x -> Unsafe.unsafeCoerce x

pteraTHUnsafeRunner :: RunnerParser ctx elem () -> Parser ctx rules elem initials
pteraTHUnsafeRunner p = Runner.UnsafeRunnerM p

pteraTHAction :: ([ReduceArgument] -> ActionTask ctx b) -> ActionM ctx
pteraTHAction f = ActionM do \args -> ReduceArgument <$> f args

pteraTHActionTaskPure :: a -> ActionTask ctx a
pteraTHActionTaskPure x = pure x
