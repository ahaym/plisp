module Lang where

import Data.Map.Strict (Map)

data LVal
    = Ident String
    | SF StdFunc
    | Func (Map String LVal) [String] LVal
    | LNum Int
    | LString String
    | LList [LVal]
    | Quote LVal
    -- | Lambda [String] LVal
    | If LVal LVal LVal
    | Let String LVal LVal -- TODO: Multi-name let
    deriving (Eq, Show)

data Stmt
    = LV LVal
    | Define String LVal
    | Defun String [String] LVal
    deriving (Eq, Show)

data StdFunc
    = Plus
    | Minus
    | Div
    | Times
    | Atos
    | Stoa
    | Car
    | Cdr
    | Cons
    | Trace
    | Equals
    deriving (Eq, Show)
