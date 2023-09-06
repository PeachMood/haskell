import Data.Map

type Variable = String
data Expr = Const Double | Variable String | Add Expr Expr |Sub Expr Expr | Mult Expr Expr | Div Expr Expr | Pow Expr Expr | Ln Expr deriving (Show, Eq)

simplify:: Expr -> Expr
simplify (Add (Const val1) (Const val2)) = Const (val1 + val2)
simplify (Add (Const 0) expr) = simplify expr
simplify (Add expr (Const 0)) = simplify expr
simplify (Add expr1 expr2)
    | expr1 == expr2 = simplify $ Mult (Const 2) (simplify expr1)
simplify (Add (Mult expr1 expr2) (Mult expr3 expr4))
    |expr2 == expr4 = simplify $ Mult (simplify $ Add (simplify expr1) (simplify expr3)) (simplify expr2)
    |expr1 == expr3 = simplify $ Mult (simplify $ Add (simplify expr2) (simplify expr4)) (simplify expr1)
simplify (Add (Mult (Const val1) expr2) (Mult (Const val2) expr4))
    |val2 < 0 = simplify $ Sub (simplify $ Mult (Const val1) (simplify expr2)) (simplify $ Mult (Const (-val2)) (simplify expr4))
    |val1 < 0 = simplify $ Sub (simplify $ Mult (Const val2) (simplify expr4)) (simplify $ Mult (Const (-val1)) (simplify expr2))

simplify (Sub (Const val1) (Const val2)) = Const (val1 - val2)
simplify (Sub expr (Const 0)) = simplify expr
simplify (Sub expr1 expr2)
    | expr1 == expr2 = Const 0
simplify (Sub (Mult expr1 expr2) (Mult expr3 expr4))
    |expr2 == expr4 = Mult (simplify $ Sub (simplify expr1) (simplify expr3)) (simplify expr2)
    |expr1 == expr3 = Mult (simplify $ Sub (simplify expr2) (simplify expr4)) (simplify expr1)
simplify (Sub (Mult (Const val1) expr2) (Mult (Const val2) expr4))
    |val1 > 0 && val2 < 0 = simplify $ Add (simplify $ Mult (Const val1) (simplify expr2)) (simplify $ Mult (Const (-val2)) (simplify expr4))
    |val1 < 0 && val2 < 0  = simplify $ Sub (simplify $ Mult (Const (-val2)) (simplify expr2)) (simplify $ Mult (Const (-val1)) (simplify expr2))

simplify (Mult (Const val1) (Const val2)) = Const (val1 * val2)
simplify (Mult expr (Const val)) = simplify $ Mult (Const val) (simplify expr)
simplify (Mult (Const 0) _) = Const 0
simplify (Mult (Const 1) expr) = simplify expr
simplify (Mult expr1 expr2)
    | expr1 == expr2 = simplify $ Pow (simplify expr1) (Const 2)
simplify (Mult (Pow expr1 expr2) (Pow expr3 epxr4))
    |expr1 == expr3 = simplify $ Pow (simplify expr1) (simplify $ Add (simplify expr3) (simplify epxr4))

simplify (Div _ (Const 0)) = error "Division by zero is undefined"
simplify (Div (Const val1) (Const val2)) = Const (val1 / val2)
simplify (Div expr (Const 1)) = simplify expr
simplify (Div (Const 0) _) = Const 0
simplify (Div (Pow expr1 expr2) (Pow expr3 epxr4))
    |expr1 == expr3 = simplify $ Pow (simplify expr1) (simplify $ Sub (simplify expr3) (simplify epxr4))

simplify (Pow (Const val) _ )
    | val <= 0 || val == 1 = error "The exponential function is undefined"
simplify (Pow (Const val1) (Const val2)) = Const (val1 ** val2)
simplify (Pow _ (Const 0)) = Const 1
simplify (Pow expr (Const 1)) = simplify expr

simplify (Ln (Const val))
    | val <= 0 = error "Logarithmic function is undefined"
simplify (Ln (Const val)) = Const (log val)
simplify (Ln (Pow expr1 epxr2)) = simplify $ Mult (simplify epxr2) (simplify $ Ln expr1)
simplify epxr = epxr


diff:: Variable -> Expr -> Expr
diff _ (Const _) = Const 0
diff var1 (Variable var2) = if var1==var2 then Const 1 else Const 0

diff var (Add expr1 expr2) = simplify $ Add (simplify $ diff var expr1) (simplify $ diff var expr2)
diff var (Sub expr1 expr2) = simplify $ Sub (simplify $ diff var expr1) (simplify $ diff var expr2)
diff var (Mult expr1 expr2) = simplify $ Add (Mult (simplify $ diff var expr1) (simplify expr2) ) (Mult (simplify $ diff var expr2) (simplify expr1) )
diff var (Div expr1 expr2) = simplify $ Div(Sub (Mult (simplify $ diff var expr1) (simplify expr2)) (Mult (simplify $ diff var expr2) (simplify expr1))) (simplify $ Pow (simplify expr2) (Const 2))

diff var1 (Pow (Variable var2) (Const val))
    |var1 /= var2 = Const 0
    |otherwise = simplify $ Mult (Const val) (simplify $ Pow (Variable var2) (Const (val-1)))
diff var (Pow expr (Const val)) = simplify $ Mult (simplify $ Mult (Const val) (simplify $ Pow (simplify expr) (Const (val-1)))) (simplify $ diff var expr)
diff var1 (Pow (Const val) (Variable var2))
    |var1 /= var2 = Const 0
    |val <= 0 || val == 1 = error "The exponential function is undefined"
    |otherwise = simplify $ Mult (simplify $ Pow (Const val) (Variable var2)) (simplify $ Ln (Const val))
diff var (Pow (Const val) expr)
    |val <= 0 || val == 1 = error "The exponential function is undefined"
    |otherwise = simplify $ Mult (simplify $ Mult (simplify $ Pow (Const val) (simplify expr)) (simplify $ Ln (Const val))) (simplify $ diff var expr)
diff var (Pow expr1 expr2) = simplify $ Mult (simplify $ Pow (simplify expr1) (simplify expr2)) (simplify $ Add (simplify $ Mult (simplify $ diff var expr1) (simplify $ Div (simplify expr2) (simplify expr1))) (simplify $ Mult (simplify $ diff var expr2) (simplify $ Ln (simplify expr1))))

diff _ (Ln (Const val))
    | val <= 0 = error "Logarithmic function is undefined"
    | otherwise = Const 0
diff var1 (Ln (Variable var2))
    | var1 /= var2 = Const 0
    | otherwise = simplify $ Div (Const 1) (Variable var2)
diff var (Ln expr) = simplify $ Div (simplify $ diff var expr) (simplify expr)


substitute :: Map [Char] Double-> Expr -> Expr
substitute _ (Const val) = Const val
substitute map v@(Variable var) = maybe v Const (Data.Map.lookup var map)
substitute map (Add expr1 expr2) = Add (substitute map expr1 ) (substitute map expr2)
substitute map (Mult expr1 expr2) = Mult (substitute map expr1 ) (substitute map expr2)
substitute map (Div expr1 expr2) = Div (substitute map expr1 ) (substitute map expr2)
substitute map (Pow expr1 expr2) = Pow(substitute map expr1 ) (substitute map expr2)
substitute map (Ln expr) = Ln (substitute map expr)
