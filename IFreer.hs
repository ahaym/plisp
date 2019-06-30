{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module IFreer where

import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import qualified Data.Map.Strict as M

import Freer
import Lang

type SymTab = M.Map String LVal

-- Setting up the monad

data Interpreter a where
    GetTab :: Interpreter SymTab
    PutTab :: SymTab -> Interpreter ()
    PutLV :: LVal -> Interpreter ()
    TraceLV :: LVal -> Interpreter ()

type IM = Freer Interpreter

getTab :: IM SymTab
getTab = liftFreer GetTab

putTab :: SymTab -> IM ()
putTab = liftFreer . PutTab

putLV :: LVal -> IM ()
putLV = liftFreer . PutLV

trace :: LVal -> IM ()
trace = liftFreer . TraceLV

instance MonadState (M.Map String LVal) (Freer Interpreter) where
    get = getTab
    put = putTab

interpIO :: Bool -> Interpreter a -> StateT SymTab IO a
interpIO pv = go
    where
        go :: Interpreter a -> StateT SymTab IO a
        go GetTab = get
        go (PutTab t) = put t
        go (PutLV x) = when pv $ liftIO (print x)
        go (TraceLV x) = liftIO $ print x

interpPure :: Interpreter a -> StateT SymTab (Writer [Either LVal LVal]) a
interpPure = go
    where
        go :: Interpreter a -> StateT SymTab (Writer [Either LVal LVal]) a
        go GetTab = get
        go (PutTab t) = put t
        go (PutLV x) = tell [Left x]
        go (TraceLV x) = tell [Right x]
