module Main where

import Control.Monad (forM_)
import Text.Trifecta
import Text.Trifecta.Delta(Delta(..))
import Text.PrettyPrint.ANSI.Leijen(putDoc)
import Control.Applicative ((<|>))
import qualified Data.Map as M
import System.IO
import Data.Maybe (fromJust)

-----------------------------------------------------------------------
--
-- Types
--
-----------------------------------------------------------------------

data Expr = Literal Integer
          | Identifier String
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Rem Expr Expr
          | Eq Expr Expr
          | Neq Expr Expr
          | Lt Expr Expr
          | Gt Expr Expr
          | Def String Expr    -- "def" (id)+ "=" expr
          | App Expr Expr      -- (id | Clos) "@" param
          | If Expr Expr Expr
          | Lambda String Expr -- "fun" arg "->" body
          deriving Show

data Value = Number Integer
           | Clos String Expr Env -- (arg, body, frozenEnv)
           | NewEnv Env           -- for REPL
           | ReplMsg String       -- for REPL
           deriving Show

type Env = M.Map String Value

type ReplVal = (Value, Env)

-----------------------------------------------------------------------
--
-- Parsing
--
-----------------------------------------------------------------------

number :: Parser Expr
number = integer >>= return . Literal

parenExpr :: Parser Expr
parenExpr = symbol "(" *> expr <* symbol ")"

identifier :: Parser Expr
identifier = Identifier <$> some identifierChar <* spaces

identifierChar :: Parser Char
identifierChar = satisfy $ \c -> c `elem` ['a' .. 'z']

factor :: Parser Expr
factor = number <|> if_ <|> lambda <|> identifier <|> parenExpr

app :: Parser Expr
app = factor `chain` appop

term :: Parser Expr
term = app `chain` mulop

comp :: Parser Expr
comp = term `chain` addop

expr :: Parser Expr
expr = comp `chain` cmpop

appop :: Parser (Expr -> Expr -> Expr)
appop = infixOp "@" App

mulop :: Parser (Expr -> Expr -> Expr)
mulop = (infixOp "*" Mul) <|> (infixOp "/" Div) <|> (infixOp "%" Rem)

addop :: Parser (Expr -> Expr -> Expr)
addop = (infixOp "+" Add) <|> (infixOp "-" Sub)

cmpop :: Parser (Expr -> Expr -> Expr)
cmpop = (infixOp "==" Eq) <|> (infixOp "!=" Neq)
    <|> (infixOp "<" Lt) <|> (infixOp ">" Gt)

infixOp :: String -> (a -> a -> a) -> Parser (a -> a -> a)
infixOp x f = symbol x >> return f

chain :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chain` op = do
    a <- p
    rest a
    where
        rest a = do
                f <- op
                b <- p
                rest (f a b)
            <|> return a

arglist :: Parser [String] -> Parser [String]
arglist args = do
        args' <- args
        Identifier arg <- identifier
        arglist $ return (arg : args')
    <|> args

def :: Parser Expr
def = do
    symbol "def"
    Identifier id <- identifier
    args <- arglist $ return []
    symbol "="
    e <- expr
    return $ Def id $ if length args == 0
        then e
        else foldl (flip Lambda) e args

if_ :: Parser Expr
if_ = do
    symbol "if"
    cond <- expr
    symbol "then"
    true <- expr
    symbol "else"
    false <- expr
    return $ If cond true false

lambda :: Parser Expr
lambda = do
    symbol "fun"
    Identifier arg <- identifier
    symbol "->"
    e <- expr
    return $ Lambda arg e

arithmeticExpr :: Parser Expr
arithmeticExpr = def <|> expr

-----------------------------------------------------------------------
--
-- Evaluation
--
-----------------------------------------------------------------------

calc :: Expr -> Expr -> (Integer -> Integer -> Integer) -> Env -> Value
calc x y op env =
    let Number x' = eval x env
        Number y' = eval y env
    in  Number $ x' `op` y'

eval :: Expr -> Env -> Value
eval (Literal n) env = Number n
eval (Add x y) env = calc x y (+) env
eval (Sub x y) env = calc x y (-) env
eval (Mul x y) env = calc x y (*) env
eval (Div x y) env = calc x y div env
eval (Rem x y) env = calc x y rem env
eval (Eq x y) env = calc x y (\ x y -> toInteger . fromEnum $ x == y) env
eval (Neq x y) env = calc x y (\ x y -> toInteger . fromEnum $ x /= y) env
eval (Lt x y) env = calc x y (\ x y -> toInteger . fromEnum $ x < y) env
eval (Gt x y) env = calc x y (\ x y -> toInteger . fromEnum $ x > y) env
eval (Identifier id) env = case M.lookup id env of
    Just val -> val
    Nothing -> error $ "No such a variable: " ++ id
eval (Def id expr) env =
    let val = eval expr env
        newEnv = M.insert id val env
    in  NewEnv newEnv
eval (App expr param) env =
    let (clos@(Clos arg body frozenEnv), funcName) = apply expr env
        paramVal = eval param env
        frozenEnv' = M.insert arg paramVal frozenEnv
        frozenEnv'' = M.insert funcName clos frozenEnv'
    in  eval body frozenEnv''
eval (If cond true false) env = case eval cond env of
    Number 0 -> eval false env
    otherwise -> eval true env
eval (Lambda arg expr) env = Clos arg expr env

apply :: Expr -> Env -> (Value, String)
apply expr env = case expr of
    Identifier id -> (fromJust $ M.lookup id env, id)
    otherwise -> (eval expr env, "")

replEval :: Expr -> Env -> ReplVal
replEval expr env = case expr of
    Def id _ ->
        let NewEnv newEnv = eval expr env
        in  (ReplMsg $ "Set: " ++ id, newEnv)
    otherwise -> (eval expr env, env)

run :: Env -> IO ()
run env = do
    putStr "> "
    hFlush stdout
    input <- getLine
    case parseString arithmeticExpr (Columns 0 0) input of
        Failure doc -> do
            putDoc doc
            putStrLn "Parse error."
        Success expr -> do
            print expr
            let (value, newEnv) = replEval expr env
            print value
            run newEnv

main :: IO ()
main = putStrLn "Welcome to Haskell Calculator!!" >> run M.empty
