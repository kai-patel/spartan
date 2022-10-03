module Main where

type Var = String
data Term = Variable Var | Lambda Var Term | Apply Term Term deriving (Show)

alphaEq :: Term -> Bool
alphaEq = undefined

free :: Term -> [Var]
free (Variable v) = [v]
free (Lambda x m) = filter (\i -> i /= x) (free m)
free (Apply m n) = free(m) ++ free(n)

substitute :: Term -> Var -> Term -> Term
substitute (Variable y) x (n)
    | y == x = n
    | y /= x = Variable y
substitute (Apply m1 m2) x n = Apply (substitute m1 x n) (substitute m2 x n)
substitute (Lambda y m) x n
    | y /= x && y `notElem` free n = Lambda y (substitute m x n)
    | y == x = Lambda x m

betaReduce :: Term -> Term
betaReduce (Apply (Lambda v e) e') = substitute e v e'
betaReduce _ = error "Cannot beta-reduce a non-application"

alphaConvert :: Term -> Term
alphaConvert = undefined




main :: IO ()
main = undefined
