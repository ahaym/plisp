module Parse where

import Text.ParserCombinators.ReadP
import Text.Read.Lex

parensed :: ReadP a -> ReadP a
parensed p = char '(' *> skipSpaces *> p <* skipSpaces <* char ')'

