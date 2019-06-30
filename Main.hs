module Main where

import Control.Monad.State.Strict
import qualified Data.Map.Strict as M

import Freer
import IFreer
import Interpret
import Parse

runProgramIO :: FilePath -> IO ()
runProgramIO fp = do
    ast <- runPF fp
    case ast of
        Nothing -> error "No valid parse"
        Just t -> evalStateT (runFreer (execProgram t) (interpIO True)) M.empty

main :: IO ()
main = runProgramIO "tests/test.plisp"
