module Language.Parser.Ptera.Prelude.Debug (
    debugTrace,
    debugTraceShow,
    debugTraceShowId,
) where

import Prelude
import qualified Debug.Trace as Debug

debugTrace :: String -> a -> a
debugTrace = Debug.trace

debugTraceShow :: Show a => a -> b -> b
debugTraceShow = Debug.traceShow

debugTraceShowId :: Show a => a -> a
debugTraceShowId = Debug.traceShowId
