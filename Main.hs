module Main where

import IFreer
import Interpret
import Parse

runProgramIO :: FilePath -> IO ()
runProgramIO fp = do
    ast <- runPF fp
    case ast of
        Nothing -> error "No valid parse"
        Just t ->  interpIO True $ execProgram t

main :: IO ()
main = runProgramIO "tests/test.plisp"
