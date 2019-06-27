module Parse where

import Control.Applicative hiding (many)
import Control.Monad (guard)
import Data.Char
import Text.ParserCombinators.ReadP
import Text.Read.Lex hiding (Ident)

import Lang

parensed :: ReadP a -> ReadP a
parensed p = char '(' *> skipSpaces *> p <* skipSpaces <* char ')'

wsp :: ReadP ()
wsp = do
    many1 $ satisfy (\c -> c == ' ' || c == '\n')
    return ()

libWords :: [(String, StdFunc)]
libWords =
    [ ("+",Plus)
    , ("-",Minus)
    , ("/",Div)
    , ("*",Times)
    , ("atos",Atos)
    , ("stoa",Stoa)
    , ("car",Car)
    , ("cdr",Cdr)
    , ("cons",Cons)
    , ("trace",Trace)
    ]

keywords :: [String]
keywords = map fst libWords ++ 
    [ "quote"
    , "if"
    , "let"
    , "define"
    , "defun"
    ]

parseAtom :: ReadP String
parseAtom = do
    a <- (:) <$> satisfy isAlpha <*> munch (\c -> isAlphaNum c || c == '_')
    guard . not $ elem a keywords
    return a

parseProgram :: ReadP [Stmt]
parseProgram = sepBy parseStmt skipSpaces <* skipSpaces <* eof

parseStmt :: ReadP Stmt
parseStmt = parseDefine <|> parseDefun <|> (LV <$> parseLV)

parseDefine :: ReadP Stmt
parseDefine = parensed $ do
    string "define" >> wsp
    lhs <- parseAtom
    wsp
    rhs <- parseLV 
    return $ Define lhs rhs

parseDefun :: ReadP Stmt
parseDefun = parensed $ do
    string "defun"
    wsp
    lhs <- parseAtom
    wsp
    args <- parensed $ sepBy parseAtom wsp
    wsp
    rhs <- parseLV
    return $ Defun lhs args rhs

parseLV :: ReadP LVal
parseLV = (Ident <$> parseAtom) 
    <|> parseSF
    <|> (LNum <$> readDecP)
    <|> (LString <$> between (char '"') (char '"') (many $ satisfy (/='"'))) --TODO: quotes and escapes
    <|> parseL
    <|> parseQ
    <|> parseIf
    <|> parseLet

parseSF :: ReadP LVal
parseSF = SF <$> foldl1 (<|>) parsers
    where
        parsers = flip map libWords $ \(w, f) -> string w >> return f

parseL :: ReadP LVal
parseL = parensed (LList <$> sepBy parseLV wsp)

parseQ :: ReadP LVal
parseQ = parensed $ fmap Quote (string "quote" >> wsp >> parseLV) 

parseIf :: ReadP LVal
parseIf = parensed $ do
    string "if"
    wsp
    cond <- parseLV
    wsp
    tb <- parseLV
    wsp
    fb <- parseLV
    return $ If cond tb fb

parseLet :: ReadP LVal
parseLet = parensed $ do
    string "let"
    wsp
    n <- parseAtom
    wsp
    x <- parseLV
    wsp
    y <- parseLV
    return $ Let n x y
