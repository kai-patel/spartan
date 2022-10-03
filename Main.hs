module Main where

type Var = String
data Term = Variable Var | Lambda Var Term | Apply Term Term deriving (Show)

alphaEq :: Term -> Bool
alphaEq = undefined

-- Find all free variables in a given expression
free :: Term -> [Var]
free (Variable v) = [v]
free (Lambda x m) = filter (\i -> i /= x) (free m)
free (Apply m n) = free(m) ++ free(n)

-- Get a fresh variable i.e. the first variable not in the given list
fresh :: [Var] -> Var
fresh xs = head $ dropWhile (\x -> x `elem` xs) variables
    where variables = [l : [] | l <- ['a' .. 'z']] ++ [l : show x | x <- [1 ..], l <- ['a' .. 'z']]

-- Substitution with explicit alpha conversion
substitute :: Term -> Var -> Term -> Term
substitute (Variable y) x (n)
    | y == x = n
    | y /= x = Variable y
substitute (Apply m1 m2) x n = Apply (substitute m1 x n) (substitute m2 x n)
substitute (Lambda y t) x t'
    | x == y = Lambda y t
    | x /= y = Lambda z (substitute (substitute t y (Variable z)) x t')
    where
    z = fresh (free t ++ free t')


betaReduce :: Term -> Term
betaReduce (Apply (Lambda v e) e') = substitute e v e'
betaReduce _ = error "Cannot beta-reduce a non-application"

main :: IO ()
main = undefined
