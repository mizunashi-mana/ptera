module Language.Parser.Ptera.Runner (
    T,

    RunnerM (..),
    RunT.Result (..),
    runParserM,
    runParser,
) where

import           Language.Parser.Ptera.Prelude

import qualified Language.Parser.Ptera.Syntax.SafeGrammar as SafeGrammar
import qualified Language.Parser.Ptera.Runner.Parser as Parser
import qualified Language.Parser.Ptera.Runner.RunT   as RunT
import qualified Language.Parser.Ptera.Scanner       as Scanner
import qualified Language.Parser.Ptera.Syntax               as Syntax
import qualified Type.Membership as Membership
import qualified Type.Membership.Internal as MembershipInternal

type T = RunnerM

type RunnerM :: Type -> Type -> [t] -> Type -> [Symbol] -> Type
newtype RunnerM ctx rules tokens elem initials = UnsafeRunnerM (Parser.T ctx elem)

type Runner = RunnerM ()

runParserM :: forall v initials ctx posMark m a rules tokens elem proxy.
    a ~ Syntax.RuleExprReturnType (Syntax.SemActM ctx) rules tokens elem v
    => Membership.Member initials v => Scanner.T posMark elem m
    => proxy v -> RunnerM ctx rules tokens elem initials -> ctx
    -> m (RunT.Result a)
runParserM _ (UnsafeRunnerM p) customCtx0 =
    case RunT.initialContext p customCtx0 pos of
        Nothing ->
            pure do RunT.ParseFail
        Just initialCtx ->
            evalStateT runner initialCtx
    where
        runner :: StateT (RunT.Context ctx posMark elem) m (RunT.Result a)
        runner = RunT.unRunT RunT.runT

        pos = SafeGrammar.genStartPoint
            do MembershipInternal.membership @initials @v

runParser :: forall v initials posMark m a rules tokens elem proxy.
    a ~ Syntax.RuleExprReturnType Syntax.SemAct rules tokens elem v
    => Membership.Member initials v => Scanner.T posMark elem m
    => proxy v -> Runner rules tokens elem initials
    -> m (RunT.Result a)
runParser p r = runParserM p r ()
