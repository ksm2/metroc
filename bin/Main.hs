module Main where

import System.Environment
import Data.Map

import MetroLang.Test.AST
import MetroLang.Test.Parser

-- | evalArith evaluate an arithmetic expression to an integer.
evalArith :: Map String Integer -> AExpr -> Integer
evalArith v (Var string) = v ! string
evalArith v (IntConst a) = a
evalArith v (Neg aExpr) = -(evalArith v aExpr)
evalArith v (ABinary Add a b) = (evalArith v a) + (evalArith v b)
evalArith v (ABinary Subtract a b) = (evalArith v a) - (evalArith v b)
evalArith v (ABinary Multiply a b) = (evalArith v a) * (evalArith v b)

-- | evalBool evaluate a boolean expression to a bool.
evalBool :: Map String Integer -> BExpr -> Bool
evalBool v (BoolConst bool) = bool
evalBool v (Not bExpr) = not $ evalBool v bExpr
evalBool v (BBinary And a b) = (evalBool v a) && (evalBool v b)
evalBool v (BBinary Or a b) = (evalBool v a) || (evalBool v b)
evalBool v (RBinary Greater a b) = (evalArith v a) > (evalArith v b)
evalBool v (RBinary Less a b) = (evalArith v a) < (evalArith v b)

-- | run executes a program
run :: Map String Integer -> Stmt -> IO (Map String Integer)
run variables (Seq []) = return variables
run variables (Seq (x:xs)) =
    do  vars2 <- run variables x
        outer <- run vars2 (Seq xs)
        return outer
run variables (Assign string aExpr) = return $ insert string (evalArith variables aExpr) variables
run variables (While bExpr stmt) =
    if (evalBool variables bExpr)
        then do inner <- run variables stmt
                outer <- run inner $ While bExpr stmt
                return outer
        else return variables
run variables Skip = return variables
run variables (Print aExpr) =
    do putStr "Print:"
       print (evalArith variables aExpr)
       return variables


-- | main, do nothing
main :: IO ()
main =
    do  args <- getArgs
        let arg1:_ = args
        ast <- parseFile arg1
        print ast

        run empty ast
        return ()
