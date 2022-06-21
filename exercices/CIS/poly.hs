data ExprT = Lit Integer
    | Add ExprT ExprT
    | Mul ExprT ExprT
    deriving (Show, Eq)

eval :: ExprT -> Integer
eval (Lit value) = value
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Mul e1 e2) = (eval e1) * (eval e2) 

evalStr :: String -> Maybe Integer
evalStr [] = Nothing
evalStr (x:xs) = xs

    -- "(4+9)*2" 