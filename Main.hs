module Main where

import Control.Applicative
import Control.Monad
import Data.Char
import qualified Data.Bifunctor as Bifunctor

type Var = String
data Term = Variable Var | Lambda Var Term | Apply Term Term deriving (Show)

newtype Parser a = Parser { parse :: String -> [(a, String)] }

instance Functor Parser where
    fmap f p = Parser (fmap (Bifunctor.first f) . parse p)

instance Applicative Parser where
    pure = result
    p1 <*> p2 = Parser $ \input -> do
        (f, input') <- parse p1 input
        (a, input'') <- parse p2 input'
        return (f a, input'')

instance Monad Parser where
    p >>= f = Parser $ \input -> concat [parse (f n) input' | (n, input') <- parse p input]

instance MonadPlus Parser where
    mzero = zero
    mplus = plus

instance MonadFail Parser where
    fail _ = mzero

instance Alternative Parser where
    empty = zero
    p1 <|> p2 = Parser $ \input -> case parse (p1 `plus` p2) input of
        [] -> []
        (x:_) -> [x]

result :: a -> Parser a
result value = Parser $ \input -> [(value, input)]

plus :: Parser a -> Parser a -> Parser a
p1 `plus` p2 = Parser $ \input -> parse p1 input ++ parse p2 input

zero :: Parser a
zero = Parser (const [])

item :: Parser Char
item = Parser p
    where
    p [] = []
    p (x:xs) = [(x, xs)]

satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = item >>= \x -> if predicate x then result x else zero

char :: Char -> Parser Char
char c = satisfy (== c)

digit :: Parser Char
digit = satisfy isDigit

lower :: Parser Char
lower = satisfy isLower

upper :: Parser Char
upper = satisfy isUpper

letter :: Parser Char
letter = satisfy (isVar)
    where isVar x = (isLower x || isUpper x) && x /= 'λ' && x /= '\\'

alphanumeric :: Parser Char
alphanumeric = letter <|> digit

string :: String -> Parser String
string "" = result ""
string (x:xs) = char x >> string xs >> result (x:xs)

manyP :: Parser a -> Parser [a]
manyP p = do
    x <- p
    xs <- manyP p
    return (x:xs)
    <|> return []

many1P :: Parser a -> Parser [a]
many1P p = do
    x <- p
    xs <- manyP p
    return (x:xs)

thenP :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
thenP combi p1 p2 = do
    x <- p1
    xs <- p2
    return $ combi x xs

spaces :: Parser ()
spaces = void $ manyP $ satisfy isSpace

token :: Parser a -> Parser a
token p = p <* spaces

parse' :: Parser a -> Parser a
parse' p = spaces >> p

bracketed :: Parser a -> Parser b -> Parser a -> Parser b
bracketed open p close = open >> p <* close

-- BNF Form for Lambda Calculus Syntax
-- ABSTRACTION:
-- expr ::= \ variable . expr
-- APPLICATION TERM:
-- expr ::= application_term
-- APPLICATION:
-- application_term ::= application_term item
-- ITEM:
-- application_term ::= item
-- VARIABLE:
-- item ::= variable
-- GROUPING:
-- item ::= ( expr )
--
-- variable ::= alpha extension
-- extension ::=
-- extension ::= extension_char extension
-- extension_char ::= alpha | digit | _

charTok :: Char -> Parser Char
charTok = token <$> char

variable :: Parser Term
variable = do
    x <- token letter
    pure $ Variable [x]

abstraction :: Parser Term
abstraction = do
    _ <- charTok '\\' <|> charTok 'λ'
    (Variable v) <- variable
    _ <- charTok '.'
    rest <- expr
    pure $ (Lambda v rest)

application :: Parser Term
application = do
    _ <- charTok '('
    expr1 <- expr
    _ <- spaces <|> empty
    expr2 <- expr
    _ <- charTok ')'
    pure $ (Apply expr1 expr2)

expr :: Parser Term
expr = variable <|> abstraction <|> application

-- Find all free variables in a given expression
free :: Term -> [Var]
free (Variable v) = [v]
free (Lambda x m) = filter (\i -> i /= x) (free m)
free (Apply m n) = free(m) ++ free(n)

-- Find all variables in a given expression
used :: Term -> [Var]
used (Variable v) = [v]
used (Lambda a b) = [a] ++ used b
used (Apply a b) = used a ++ used b

-- Get a fresh variable i.e. the first variable not in the given list
fresh :: [Var] -> Var
fresh xs = head $ dropWhile (\x -> x `elem` xs) variables
    where variables = [l : [] | l <- ['a' .. 'z']] ++ [l : show x | x <- [1 :: Int ..], l <- ['a' .. 'z']]

-- Substitution arg 3 with arg 2 in arg 1 with explicit alpha conversion
substitute :: Term -> Term -> Var -> Term
substitute (Variable y) m x
    | x == y = m
    | x /= y = Variable y

substitute (Apply a b) m x = Apply (substitute a m x) (substitute b m x)

substitute l@(Lambda y e) n x
    | y == x = Lambda x e
    | y /= x && (y `notElem` (free n)) = Lambda y (substitute e n x)
    | y /= x && (y `elem` (free n)) = Lambda y' (substitute (substitute e (Variable y') y) n x)
    where y' = fresh (used l)

substitute _ _ _ = error "Substitution failure"

betaReduce :: Term -> Term
betaReduce (Apply (Lambda v e) e') = betaReduce $ substitute e e' v
betaReduce (Apply a b) = Apply (betaReduce a) (betaReduce b)
betaReduce t = t

main :: IO ()
main = putStrLn . show $ betaReduce . fst . head $ parse expr "\\x.xz"
