module Parser.Debug where

import           Language.Parser.Ptera.Prelude

import qualified Language.Parser.Ptera.Machine.LAPEG        as LAPEG
import qualified Language.Parser.Ptera.Machine.PEG          as PEG
import qualified Language.Parser.Ptera.Machine.SRB          as SRB
import qualified Language.Parser.Ptera.Pipeline.Grammar2PEG as Grammar2PEG
import qualified Language.Parser.Ptera.Pipeline.LAPEG2SRB   as LAPEG2SRB
import qualified Language.Parser.Ptera.Pipeline.PEG2LAPEG   as PEG2LAPEG
import qualified Language.Parser.Ptera.Runner.Parser        as RunnerParser
import qualified Language.Parser.Ptera.Syntax               as Syntax
import qualified Parser.Rules                               as Rules
import qualified Types

fixedGrammar :: Syntax.FixedGrammar Rules.ParsePoints Rules.NonTerminal Types.Token
fixedGrammar = runIdentity $ Syntax.fixedT Rules.grammar

peg :: PEG.T Int RunnerParser.Action
peg = case fixedGrammar of
    Syntax.UnsafeFixedGrammar g -> Grammar2PEG.grammar2Peg g

laPeg :: LAPEG.T Int RunnerParser.Action
laPeg = case runExcept $ PEG2LAPEG.peg2LaPeg peg of
    Right x -> x
    Left vs -> error $ "unreachable: " ++ show vs

srb :: SRB.T Int RunnerParser.Action
srb = LAPEG2SRB.laPeg2Srb laPeg

{-
LAPEG {
    initials = fromList [(0,Var 0)],
    alts = Array (array (0,7) [
        (0,Alt {
            altVar = Var 3,
            altKind = AltSeq,
            altUnitSeq = Array (array (0,2) [(0,UnitTerminal 2),(1,UnitNonTerminal (Var 0)),(2,UnitTerminal 3)]),
            altAction = ()
        }),
        (1,Alt {
            altVar = Var 3,
            altKind = AltSeq,
            altUnitSeq = Array (array (0,0) [(0,UnitTerminal 4)]),
            altAction = ()
        }),
        (2,Alt {
            altVar = Var 3,
            altKind = AltSeq,
            altUnitSeq = Array (array (0,0) [(0,UnitTerminal 5)]),
            altAction = ()
        }),
        (3,Alt {
            altVar = Var 2,
            altKind = AltSeq,
            altUnitSeq = Array (array (0,2) [(0,UnitNonTerminal (Var 3)),(1,UnitTerminal 1),(2,UnitNonTerminal (Var 2))]),
            altAction = ()
        }),
        (4,Alt {
            altVar = Var 2,
            altKind = AltSeq,
            altUnitSeq = Array (array (0,0) [(0,UnitNonTerminal (Var 3))]),
            altAction = ()
        }),
        (5,Alt {
            altVar = Var 1,
            altKind = AltSeq,
            altUnitSeq = Array (array (0,2) [(0,UnitNonTerminal (Var 2)),(1,UnitTerminal 0),(2,UnitNonTerminal (Var 1))]),
            altAction = ()
        }),
        (6,Alt {
            altVar = Var 1,
            altKind = AltSeq,
            altUnitSeq = Array (array (0,0) [(0,UnitNonTerminal (Var 2))]),
            altAction = ()
        }),
        (7,Alt {
            altVar = Var 0,
            altKind = AltSeq,
            altUnitSeq = Array (array (0,0) [(0,UnitNonTerminal (Var 1))]),
            altAction = ()
        })
    ]),
    rules = Array (array (0,3) [
        (0,Rule {ruleRange = StraightSet (fromList [2,4,5]), ruleAlts = [AltNum 7]}),
        (1,Rule {ruleRange = StraightSet (fromList [2,4,5]), ruleAlts = [AltNum 5,AltNum 6]}),
        (2,Rule {ruleRange = StraightSet (fromList [2,4,5]), ruleAlts = [AltNum 3,AltNum 4]}),
        (3,Rule {ruleRange = StraightSet (fromList [2,4,5]), ruleAlts = [AltNum 0,AltNum 1,AltNum 2]})
    ])
}
-}

{-
SRB {
    initials = fromList [],
    states = Array (array (0,0) [
        (0,MState {
            stateNum = StateNum 0,
            stateTrans = IntMap {
                intMapStraight = fromList [
                    (2,Just (TransWithOps [TransOpEnter (Var 0) Nothing] (StateNum 1))),
                    (4,Just (TransWithOps [TransOpEnter (Var 0) Nothing] (StateNum 1))),
                    (5,Just (TransWithOps [TransOpEnter (Var 0) Nothing] (StateNum 1)))
                ],
                intMapNegative = Nothing
            },
            stateAltItems = []
        })
    ]),
    alts = Array (array (0,7) [
        (0,Alt {
            altVar = Var 3,
            altKind = AltSeq,
            altUnitSeq = Array (array (0,2) [(0,UnitTerminal 2),(1,UnitNonTerminal (Var 0)),(2,UnitTerminal 3)]),
            altAction = ()
        }),
        (1,Alt {
            altVar = Var 3,
            altKind = AltSeq,
            altUnitSeq = Array (array (0,0) [(0,UnitTerminal 4)]),
            altAction = ()
        }),
        (2,Alt {
            altVar = Var 3,
            altKind = AltSeq,
            altUnitSeq = Array (array (0,0) [(0,UnitTerminal 5)]),
            altAction = ()
        }),
        (3,Alt {
            altVar = Var 2,
            altKind = AltSeq,
            altUnitSeq = Array (array (0,2) [(0,UnitNonTerminal (Var 3)),(1,UnitTerminal 1),(2,UnitNonTerminal (Var 2))]),
            altAction = ()
        }),
        (4,Alt {
            altVar = Var 2,
            altKind = AltSeq,
            altUnitSeq = Array (array (0,0) [(0,UnitNonTerminal (Var 3))]),
            altAction = ()
        }),
        (5,Alt {
            altVar = Var 1,
            altKind = AltSeq,
            altUnitSeq = Array (array (0,2) [(0,UnitNonTerminal (Var 2)),(1,UnitTerminal 0),(2,UnitNonTerminal (Var 1))]),
            altAction = ()
        }),
        (6,Alt {
            altVar = Var 1,
            altKind = AltSeq,
            altUnitSeq = Array (array (0,0) [(0,UnitNonTerminal (Var 2))]),
            altAction = ()
        }),
        (7,Alt {
            altVar = Var 0,
            altKind = AltSeq,
            altUnitSeq = Array (array (0,0) [(0,UnitNonTerminal (Var 1))]),
            altAction = ()
        })
    ])}
-}
