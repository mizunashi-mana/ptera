module Language.Parser.Ptera.Syntax.SafeRule where

data Rule n t e f r where
    Rule :: n -> [Alt n t e f r] -> Rule n t e f r

data Alt n t e f r where
    Alt :: Expr n t e us -> f us r -> Alt n t e f r

data Expr n t e us where
    Eps :: Expr n t e '[]
    (:^) :: Unit n t e u -> Expr n t e us -> Expr n t e (u ': us)

infixr 5 :^

data Unit n t e u where
    UnitToken :: t -> Unit n t e e
    UnitVar :: n -> Unit n t e u
