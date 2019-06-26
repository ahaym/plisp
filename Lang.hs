module Lang where

data LVal
    = Ident String
    | SF StdFunc
    | LNum Int
    | LString String
    | LList [LVal]
    | Quote LVal
    | Lambda [String] LVal
    | If LVal LVal LVal
    | Let String LVal LVal
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
    deriving (Eq, Show)
