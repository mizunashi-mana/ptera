module Lexer.CodeUnit where

import qualified Data.Char    as Char
import qualified Data.EnumSet as EnumSet


type T = CodeUnit

data CodeUnit
    -- https://www.compart.com/en/unicode/
    = LcU0009 -- '\t'
    | LcU000A -- '\n'
    | LcU000B -- '\v'
    | LcU000C -- '\f'
    | LcU000D -- '\r'
    | LcU0020 -- ' '
    | LcU0021 -- '!'
    | LcU0022 -- '"'
    | LcU0023 -- '#'
    | LcU0024 -- '$'
    | LcU0025 -- '%'
    | LcU0026 -- '&'
    | LcU0027 -- '\''
    | LcU0028 -- '('
    | LcU0029 -- ')'
    | LcU002A -- '*'
    | LcU002B -- '+'
    | LcU002C -- ','
    | LcU002D -- '-'
    | LcU002E -- '.'
    | LcU002F -- '/'
    | LcU0030 -- '0'
    | LcU0031 -- '1'
    | LcU0032 -- '2'
    | LcU0033 -- '3'
    | LcU0034 -- '4'
    | LcU0035 -- '5'
    | LcU0036 -- '6'
    | LcU0037 -- '7'
    | LcU0038 -- '8'
    | LcU0039 -- '9'
    | LcU003A -- ':'
    | LcU003B -- ';'
    | LcU003C -- '<'
    | LcU003D -- '='
    | LcU003E -- '>'
    | LcU003F -- '?'
    | LcU0040 -- '@'
    | LcU0041 -- 'A'
    | LcU0042 -- 'B'
    | LcU0043 -- 'C'
    | LcU0044 -- 'D'
    | LcU0045 -- 'E'
    | LcU0046 -- 'F'
    | LcU0047 -- 'G'
    | LcU0048 -- 'H'
    | LcU0049 -- 'I'
    | LcU004A -- 'J'
    | LcU004B -- 'K'
    | LcU004C -- 'L'
    | LcU004D -- 'M'
    | LcU004E -- 'N'
    | LcU004F -- 'O'
    | LcU0050 -- 'P'
    | LcU0051 -- 'Q'
    | LcU0052 -- 'R'
    | LcU0053 -- 'S'
    | LcU0054 -- 'T'
    | LcU0055 -- 'U'
    | LcU0056 -- 'V'
    | LcU0057 -- 'W'
    | LcU0058 -- 'X'
    | LcU0059 -- 'Y'
    | LcU005A -- 'Z'
    | LcU005B -- '['
    | LcU005C -- '\\'
    | LcU005D -- ']'
    | LcU005E -- '^'
    | LcU005F -- '_'
    | LcU0060 -- '`'
    | LcU0061 -- 'a'
    | LcU0062 -- 'b'
    | LcU0063 -- 'c'
    | LcU0064 -- 'd'
    | LcU0065 -- 'e'
    | LcU0066 -- 'f'
    | LcU0067 -- 'g'
    | LcU0068 -- 'h'
    | LcU0069 -- 'i'
    | LcU006A -- 'j'
    | LcU006B -- 'k'
    | LcU006C -- 'l'
    | LcU006D -- 'm'
    | LcU006E -- 'n'
    | LcU006F -- 'o'
    | LcU0070 -- 'p'
    | LcU0071 -- 'q'
    | LcU0072 -- 'r'
    | LcU0073 -- 's'
    | LcU0074 -- 't'
    | LcU0075 -- 'u'
    | LcU0076 -- 'v'
    | LcU0077 -- 'w'
    | LcU0078 -- 'x'
    | LcU0079 -- 'y'
    | LcU007A -- 'z'
    | LcU007B -- '{'
    | LcU007C -- '|'
    | LcU007D -- '}'
    | LcU007E -- '~'
    | LcU200E -- Left-to-Right Mark
    | LcU200F -- Right-to-Left Mark

    -- https://www.compart.com/en/unicode/category
    | LcOtherCatCf -- Format
    | LcOtherCatLl -- Lowercase Letter
    | LcOtherCatLm -- Modifier Letter
    | LcOtherCatLo -- Other Letter
    | LcOtherCatLt -- Titlecase Letter
    | LcOtherCatLu -- Uppercase Letter
    | LcOtherCatM  -- Mark
    | LcOtherCatNd -- Decimal Number
    | LcOtherCatNl -- Letter Number
    | LcOtherCatNo -- Other Number
    | LcOtherCatPc -- Connector Punctuation
    | LcOtherCatPd -- Dash Punctuation
    | LcOtherCatPe -- Close Punctuation
    | LcOtherCatPf -- Final Punctuation
    | LcOtherCatPi -- Initial Punctuation
    | LcOtherCatPo -- Other Punctuation
    | LcOtherCatPs -- Open Punctuation
    | LcOtherCatS  -- Symbol
    | LcOtherCatZl -- Line Separator
    | LcOtherCatZp -- Paragraph Separator
    | LcOtherCatZs -- Space Separator

    | LcOther
    deriving (Eq, Ord, Enum, Bounded, Show)

fromCharPoint :: Char -> Maybe CodeUnit
fromCharPoint c = case fromChar c of
    LcOtherCatCf -> Nothing
    LcOtherCatLl -> Nothing
    LcOtherCatLm -> Nothing
    LcOtherCatLo -> Nothing
    LcOtherCatLt -> Nothing
    LcOtherCatLu -> Nothing
    LcOtherCatM  -> Nothing
    LcOtherCatNd -> Nothing
    LcOtherCatNl -> Nothing
    LcOtherCatNo -> Nothing
    LcOtherCatPc -> Nothing
    LcOtherCatPd -> Nothing
    LcOtherCatPe -> Nothing
    LcOtherCatPf -> Nothing
    LcOtherCatPi -> Nothing
    LcOtherCatPo -> Nothing
    LcOtherCatPs -> Nothing
    LcOtherCatS  -> Nothing
    LcOtherCatZl -> Nothing
    LcOtherCatZp -> Nothing
    LcOtherCatZs -> Nothing
    LcOther      -> Nothing
    lc           -> Just lc

fromChar :: Char -> CodeUnit
fromChar c = case fromEnum c of
    0x0009 -> LcU0009
    0x000A -> LcU000A
    0x000B -> LcU000B
    0x000C -> LcU000C
    0x000D -> LcU000D
    0x0020 -> LcU0020
    0x0021 -> LcU0021
    0x0022 -> LcU0022
    0x0023 -> LcU0023
    0x0024 -> LcU0024
    0x0025 -> LcU0025
    0x0026 -> LcU0026
    0x0027 -> LcU0027
    0x0028 -> LcU0028
    0x0029 -> LcU0029
    0x002A -> LcU002A
    0x002B -> LcU002B
    0x002C -> LcU002C
    0x002D -> LcU002D
    0x002E -> LcU002E
    0x002F -> LcU002F
    0x0030 -> LcU0030
    0x0031 -> LcU0031
    0x0032 -> LcU0032
    0x0033 -> LcU0033
    0x0034 -> LcU0034
    0x0035 -> LcU0035
    0x0036 -> LcU0036
    0x0037 -> LcU0037
    0x0038 -> LcU0038
    0x0039 -> LcU0039
    0x003A -> LcU003A
    0x003B -> LcU003B
    0x003C -> LcU003C
    0x003D -> LcU003D
    0x003E -> LcU003E
    0x003F -> LcU003F
    0x0040 -> LcU0040
    0x0041 -> LcU0041
    0x0042 -> LcU0042
    0x0043 -> LcU0043
    0x0044 -> LcU0044
    0x0045 -> LcU0045
    0x0046 -> LcU0046
    0x0047 -> LcU0047
    0x0048 -> LcU0048
    0x0049 -> LcU0049
    0x004A -> LcU004A
    0x004B -> LcU004B
    0x004C -> LcU004C
    0x004D -> LcU004D
    0x004E -> LcU004E
    0x004F -> LcU004F
    0x0050 -> LcU0050
    0x0051 -> LcU0051
    0x0052 -> LcU0052
    0x0053 -> LcU0053
    0x0054 -> LcU0054
    0x0055 -> LcU0055
    0x0056 -> LcU0056
    0x0057 -> LcU0057
    0x0058 -> LcU0058
    0x0059 -> LcU0059
    0x005A -> LcU005A
    0x005B -> LcU005B
    0x005C -> LcU005C
    0x005D -> LcU005D
    0x005E -> LcU005E
    0x005F -> LcU005F
    0x0060 -> LcU0060
    0x0061 -> LcU0061
    0x0062 -> LcU0062
    0x0063 -> LcU0063
    0x0064 -> LcU0064
    0x0065 -> LcU0065
    0x0066 -> LcU0066
    0x0067 -> LcU0067
    0x0068 -> LcU0068
    0x0069 -> LcU0069
    0x006A -> LcU006A
    0x006B -> LcU006B
    0x006C -> LcU006C
    0x006D -> LcU006D
    0x006E -> LcU006E
    0x006F -> LcU006F
    0x0070 -> LcU0070
    0x0071 -> LcU0071
    0x0072 -> LcU0072
    0x0073 -> LcU0073
    0x0074 -> LcU0074
    0x0075 -> LcU0075
    0x0076 -> LcU0076
    0x0077 -> LcU0077
    0x0078 -> LcU0078
    0x0079 -> LcU0079
    0x007A -> LcU007A
    0x007B -> LcU007B
    0x007C -> LcU007C
    0x007D -> LcU007D
    0x007E -> LcU007E
    0x200E -> LcU200E
    0x200F -> LcU200F
    _      -> case Char.generalCategory c of
        Char.Format               -> LcOtherCatCf
        Char.LowercaseLetter      -> LcOtherCatLl
        Char.ModifierLetter       -> LcOtherCatLm
        Char.OtherLetter          -> LcOtherCatLo
        Char.TitlecaseLetter      -> LcOtherCatLt
        Char.UppercaseLetter      -> LcOtherCatLu
        Char.NonSpacingMark       -> LcOtherCatM
        Char.SpacingCombiningMark -> LcOtherCatM
        Char.EnclosingMark        -> LcOtherCatM
        Char.DecimalNumber        -> LcOtherCatNd
        Char.LetterNumber         -> LcOtherCatNl
        Char.OtherNumber          -> LcOtherCatNo
        Char.ConnectorPunctuation -> LcOtherCatPc
        Char.DashPunctuation      -> LcOtherCatPd
        Char.ClosePunctuation     -> LcOtherCatPe
        Char.FinalQuote           -> LcOtherCatPf
        Char.InitialQuote         -> LcOtherCatPi
        Char.OtherPunctuation     -> LcOtherCatPo
        Char.OpenPunctuation      -> LcOtherCatPs
        Char.MathSymbol           -> LcOtherCatS
        Char.CurrencySymbol       -> LcOtherCatS
        Char.ModifierSymbol       -> LcOtherCatS
        Char.OtherSymbol          -> LcOtherCatS
        Char.LineSeparator        -> LcOtherCatZl
        Char.ParagraphSeparator   -> LcOtherCatZp
        Char.Space                -> LcOtherCatZs
        _                         -> LcOther

catFormat :: EnumSet.EnumSet CodeUnit
catFormat = EnumSet.fromList
    [
        LcU200E, LcU200F,
        LcOtherCatCf
    ]

catLowercaseLetter :: EnumSet.EnumSet CodeUnit
catLowercaseLetter = EnumSet.fromList
    [
        LcU0061, LcU0062, LcU0063, LcU0064, LcU0065,
        LcU0066, LcU0067, LcU0068, LcU0069, LcU006A,
        LcU006B, LcU006C, LcU006D, LcU006E, LcU006F,
        LcU0070, LcU0071, LcU0072, LcU0073, LcU0074,
        LcU0075, LcU0076, LcU0077, LcU0078, LcU0079,
        LcU007A,
        LcOtherCatLl
    ]

catModifierLetter :: EnumSet.EnumSet CodeUnit
catModifierLetter = EnumSet.fromList
    [
        LcOtherCatLm
    ]

catOtherLetter :: EnumSet.EnumSet CodeUnit
catOtherLetter = EnumSet.fromList
    [
        LcOtherCatLo
    ]

catTitlecaseLetter :: EnumSet.EnumSet CodeUnit
catTitlecaseLetter = EnumSet.fromList
    [
        LcOtherCatLt
    ]

catUppercaseLetter :: EnumSet.EnumSet CodeUnit
catUppercaseLetter = EnumSet.fromList
    [
        LcU0041, LcU0042, LcU0043, LcU0044, LcU0045,
        LcU0046, LcU0047, LcU0048, LcU0049, LcU004A,
        LcU004B, LcU004C, LcU004D, LcU004E, LcU004F,
        LcU0050, LcU0051, LcU0052, LcU0053, LcU0054,
        LcU0055, LcU0056, LcU0057, LcU0058, LcU0059,
        LcU005A,
        LcOtherCatLu
    ]

catMark :: EnumSet.EnumSet CodeUnit
catMark = EnumSet.fromList
    [
        LcOtherCatM
    ]

catDecimalNumber :: EnumSet.EnumSet CodeUnit
catDecimalNumber = EnumSet.fromList
    [
        LcU0030, LcU0031, LcU0032, LcU0033, LcU0034,
        LcU0035, LcU0036, LcU0037, LcU0038, LcU0039,
        LcOtherCatNd
    ]

catLetterNumber :: EnumSet.EnumSet CodeUnit
catLetterNumber = EnumSet.fromList
    [
        LcOtherCatNl
    ]

catOtherNumber :: EnumSet.EnumSet CodeUnit
catOtherNumber = EnumSet.fromList
    [
        LcOtherCatNo
    ]

catConnectorPunctuation :: EnumSet.EnumSet CodeUnit
catConnectorPunctuation = EnumSet.fromList
    [
        LcU005F,
        LcOtherCatPc
    ]

catDashPunctuation :: EnumSet.EnumSet CodeUnit
catDashPunctuation = EnumSet.fromList
    [
        LcU002D,
        LcOtherCatPd
    ]

catClosePunctuation :: EnumSet.EnumSet CodeUnit
catClosePunctuation = EnumSet.fromList
    [
        LcU0029, LcU005D, LcU007D,
        LcOtherCatPe
    ]

catFinalPunctuation :: EnumSet.EnumSet CodeUnit
catFinalPunctuation = EnumSet.fromList
    [
        LcOtherCatPf
    ]

catInitialPunctuation :: EnumSet.EnumSet CodeUnit
catInitialPunctuation = EnumSet.fromList
    [
        LcOtherCatPi
    ]

catOtherPunctuation :: EnumSet.EnumSet CodeUnit
catOtherPunctuation = EnumSet.fromList
    [ LcU0021, LcU0022, LcU0023, LcU0025, LcU0026
    , LcU0027, LcU002A, LcU002C, LcU002E, LcU002F
    , LcU003A, LcU003B, LcU003F, LcU0040, LcU005C
    , LcOtherCatPo
    ]

catOpenPunctuation :: EnumSet.EnumSet CodeUnit
catOpenPunctuation = EnumSet.fromList
    [
        LcU0028, LcU005B, LcU007B,
        LcOtherCatPs
    ]

catPunctuation :: EnumSet.EnumSet CodeUnit
catPunctuation = mconcat
    [
        catClosePunctuation,
        catConnectorPunctuation,
        catDashPunctuation,
        catFinalPunctuation,
        catInitialPunctuation,
        catOpenPunctuation,
        catOtherPunctuation
    ]

catSymbol :: EnumSet.EnumSet CodeUnit
catSymbol = EnumSet.fromList
    [
        LcU0024, LcU002B, LcU003C, LcU003D, LcU003E,
        LcU005E, LcU0060, LcU007C, LcU007E,
        LcOtherCatS
    ]

catLineSeparator :: EnumSet.EnumSet CodeUnit
catLineSeparator = EnumSet.fromList
    [
        LcOtherCatZl
    ]

catParagraphSeparator :: EnumSet.EnumSet CodeUnit
catParagraphSeparator = EnumSet.fromList
    [
        LcOtherCatZp
    ]

catSpaceSeparator :: EnumSet.EnumSet CodeUnit
catSpaceSeparator = EnumSet.fromList
    [
        LcU0020,
        LcOtherCatZs
    ]
