module Term where

data Term = Lambda Char Term | Apply Term Term | Var Char

termShow :: Term -> String
termShow (Var s)        = showChar s ""
termShow (Apply t1 t2)  = "(" ++ termShow t1 ++ " " ++ termShow t2 ++ ")"
termShow (Lambda s t)   = "(L " ++ showChar s " . " ++ termShow t ++ ")"
