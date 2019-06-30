{-# LANGUAGE LambdaCase #-}

module Interpret where

import Control.Monad
import Control.Monad.State.Strict
import Data.Char
import qualified Data.Map.Strict as M

import IFreer
import Lang

exec :: Stmt -> IM ()
exec (LV l) = eval l >>= putLV
exec (Define n l) = do
    rhs <- eval l
    modify $ M.insert n rhs
    tab <- get
    putLV $ LString (show tab)
exec (Defun n args l) = do
    tab <- get
    modify' $ M.insert n (Func tab args l)

execProgram :: [Stmt] -> IM ()
execProgram = mapM_ exec

eval :: LVal -> IM LVal
eval (Ident x) = gets $ \t -> case M.lookup x t of
    Just v -> v
    _ -> error $ x ++ " used when not defined!"
eval f@SF{} = return f
eval f@Func{} = return f
eval i@LNum{} = return i
eval s@LString{} = return s
eval (LList ls) = case ls of
    [] -> return $ LList []
    (l:ls) -> eval l >>= \case
        SF sf -> mapM eval ls >>= stdFunc sf
        v -> mapM eval ls >>= call v
eval (Quote l) = return l
eval (If cond tb fb) = do
    c <- eval cond
    eval $ if c == LNum 0 then fb else tb 
eval (Let n x y) = do
    modify' $ M.insert n x
    eval y

stdFunc :: StdFunc -> [LVal] -> IM LVal
stdFunc Plus ls = stdBin (+) ls
stdFunc Minus ls = stdBin (-) ls
stdFunc Times ls = stdBin (*) ls
stdFunc Div ls = stdBin div ls
stdFunc Atos ls = case ls of
    [LList is] -> return . LString . map (chr . assertNum) $ is
    _ -> error "Atos takes a single list of numbers"
stdFunc Stoa ls = case ls of
    [LString s] -> return . LList . map (LNum . ord) $ s
    _ -> error "Atos takes a single string"
stdFunc Car ls = case ls of
    [LList ls'] -> case ls' of
        (l:_) -> return l
        _ -> error "Car takes a single list"
    _ -> error "Car takes a single list"
stdFunc Cdr ls = case ls of
    [LList ls'] -> case ls' of
        (_:rest) -> return (LList rest)
        _ -> error "Cdr takes a single list"
stdFunc Cons ls = case ls of
    [x, LList rest] -> return $ LList (x : rest)
    _ -> error "Cons takes a value and a list"
stdFunc Trace ls = case ls of
    [x, y] -> trace x >> return y
    _ -> error "Trace takes exactly two values"
stdFunc equals ls = case ls of
    [x, y] -> do
        x' <- eval x
        y' <- eval y
        if x == y then return (LNum 1) else return (LNum 0)
    _ -> error "equals takes exactly two values"

stdBin :: (Int -> Int -> Int) -> [LVal] -> IM LVal
stdBin f = return . LNum . foldl1 f . map assertNum

assertNum :: LVal -> Int
assertNum v = case v of
    LNum x -> x
    e -> error $ show e ++ " not a number!"

call :: LVal -> [LVal] -> IM LVal
call (Func table args f) ls = do
    when (length args /= length ls) $ error "different numbers of args!"
    orig <- get
    put table
    forM_ (zip args ls) $ \(n, x) -> modify' $ M.insert n x
    ans <- eval f
    put orig
    return ans
call f _ = error $ show f ++ " not a valid function!"
