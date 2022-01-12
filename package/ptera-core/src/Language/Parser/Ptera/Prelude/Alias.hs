module Language.Parser.Ptera.Prelude.Alias (
    StringLit,
    debugTrace,
    debugTraceShow,
    debugTraceShowId,
) where

import qualified Debug.Trace as Debug
import qualified Prelude

type StringLit = Prelude.String

debugTrace :: StringLit -> a -> a
debugTrace = Debug.trace

debugTraceShow :: Prelude.Show a => a -> b -> b
debugTraceShow = Debug.traceShow

debugTraceShowId :: Prelude.Show a => a -> a
debugTraceShowId = Debug.traceShowId
