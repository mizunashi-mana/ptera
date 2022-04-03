{-# LANGUAGE TemplateHaskell #-}

module Language.Parser.Ptera.TH.Class.LiftType (
    T,
    LiftType (..),
    typeOf,
) where

import Language.Parser.Ptera.Prelude

import qualified Language.Haskell.TH as TH
import           Data.Sequence
import           GHC.Real


type T = LiftType

class LiftType a where
    liftType :: proxy a -> TH.Q TH.Type

typeOf :: LiftType a => a -> TH.Q TH.Type
typeOf x = liftType do Identity x


instance LiftType () where
    liftType _ = [t|()|]

instance LiftType Int where
    liftType _ = [t|Int|]

instance LiftType Char where
    liftType _ = [t|Char|]

instance LiftType Integer where
    liftType _ = [t|Integer|]

instance LiftType a => LiftType (Ratio a) where
    liftType _ = [t|Ratio $(liftType do Proxy @a)|]

instance LiftType a => LiftType [a] where
    liftType _ = [t|[] $(liftType do Proxy @a)|]

instance LiftType a => LiftType (Maybe a) where
    liftType _ = [t|Maybe $(liftType do Proxy @a)|]

instance (LiftType a, LiftType b) => LiftType (a, b) where
    liftType _ = [t|(,) $(liftType do Proxy @a) $(liftType do Proxy @b)|]

instance (LiftType a, LiftType b, LiftType c) => LiftType (a, b, c) where
    liftType _ =
        [t|(,,) $(liftType do Proxy @a) $(liftType do Proxy @b) $(liftType do Proxy @c)|]

instance LiftType a => LiftType (Seq a) where
    liftType _ = [t|Seq $(liftType do Proxy @a)|]
