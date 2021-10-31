# Ptera: A Generator for Parsers

[![Hackage](https://img.shields.io/hackage/v/ptera.svg)](https://hackage.haskell.org/package/ptera)

## Installation

Add dependencies on `package.cabal`:

```
build-depends:
    base,
    bytestring,
    ptera,          -- main
    ptera-th,       -- for outputing parser with Template Haskell
    charset,
    template-haskell,
```

## Usage

Write parser rules:

```haskell
data Terminal
    = Digit
    | SymPlus
    | SymMulti
    deriving (Eq, Show, Enum)

data NonTerminal
    | Expr
    | Sum
    | Product
    | Value
    deriving (Eq, Show, Enum)

type ParseRule = Rule Terminal NonTerminal


data Ast
    = GenValue
    | GenSum (NonEmpty Ast)
    | GenProduct Ast Ast

rExpr :: ParseRule Ast
rExpr = rule Expr rSum

rSum :: ParseRule Ast
rSum = rule Sum do
    (rProduct <,> manyP do token SymPlus *> rProduct) <&> \(e, es) -> GenSum do e :| es

rProduct :: ParseRule Ast
rProduct = rule Product do
    orP
        [
            (rValue <* token SymMulti <,> rProduct) <&> \(e1, e2) -> GenProduct e1 e2,
            rValue
        ]

rValue :: ParseRule Ast
rValue = rule Value do token Digit *> pure GenValue
```

## Examples

* Small language: https://github.com/mizunashi-mana/ptera/tree/master/example/small-lang
* Haskell2010: https://github.com/mizunashi-mana/ptera/tree/master/example/haskell2010
