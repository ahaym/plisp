module Parse where

import Control.Applicative hiding (many)
import Control.Monad (guard, void)
import Data.Char
import Data.Maybe
import Text.ParserCombinators.ReadP
import Text.Read.Lex hiding (Ident)

import Lang

parensed :: ReadP a -> ReadP a
parensed p = char '(' *> skipSpaces *> p <* skipSpaces <* char ')'

wsp :: ReadP ()
wsp = do
    many1 $ void (satisfy (\c -> c == ' ' || c == '\n'))
        <|> void parseComment
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
    , ("eq", Equals)
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
parseProgram = fmap catMaybes $ 
    sepBy parseStmt (many1 $ char '\n')<* skipSpaces <* eof

parseStmt :: ReadP (Maybe Stmt)
parseStmt = (Just <$> parseDefine) 
    <|> (Just <$> parseDefun) 
    <|> (Just . LV <$> parseLV) 
    <|> parseComment

parseComment :: ReadP (Maybe Stmt)
parseComment = do
    string ";;"
    munch (/='\n')
    return Nothing

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
parseQ = parensed (fmap Quote (string "quote" >> wsp >> parseLV))
    <|> (char '\'' >> Quote <$> parseLV)

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

runP :: String -> Maybe [Stmt]
runP s = case readP_to_S parseProgram s of
    [lv] -> Just $ fst lv
    _ -> Nothing

runPF:: FilePath -> IO (Maybe [Stmt])
runPF f = runP <$> readFile f
