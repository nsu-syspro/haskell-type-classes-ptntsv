-- The above pragma enables all warnings
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall #-}

module Task2 where

import Data.List (nub)
import Task1 (Parse (..))

-- * Expression data type

-- | Generalized representation of expressions comprising
-- - Literals of type 'a'
-- - Variables with arbitrary 'String' names
-- - Binary operations of type 'op'
data Expr a op
  = Lit a
  | Var String
  | BinOp op (Expr a op) (Expr a op)
  deriving (Show)

getVars :: Expr a op -> [String]
getVars = nub . getVars'
  where
    getVars' (Var name) = [name]
    getVars' (BinOp _ l r) = getVars l ++ getVars r
    getVars' _ = []

-- | Integer binary operations
data IntOp = Add | Mul | Sub
  deriving (Show)

instance Parse IntOp where
  parse :: String -> Maybe IntOp
  parse "*" = Just Mul
  parse "+" = Just Add
  parse "-" = Just Sub
  parse _ = Nothing

-- * Parsing

-- | Parses given expression in Reverse Polish Notation
-- wrapped in 'Maybe' with 'Nothing' indicating failure to parse
--
-- Usage example:
--
-- >>> parse "2" :: Maybe (Expr Integer IntOp)
-- Just (Lit 2)
-- >>> parse "2 3 -" :: Maybe (Expr Integer IntOp)
-- Just (BinOp Sub (Lit 2) (Lit 3))
-- >>> parse "3 2 * 3 +" :: Maybe (Expr Integer IntOp)
-- Just (BinOp Add (BinOp Mul (Lit 3) (Lit 2)) (Lit 3))
-- >>> parse "2 +" :: Maybe (Expr Integer IntOp)
-- Nothing
-- >>> parse "2 3" :: Maybe (Expr Integer IntOp)
-- Nothing
instance (Parse a, Parse op) => Parse (Expr a op) where
  parse input = go (words input) []
    where
      go :: [String] -> [Expr a op] -> Maybe (Expr a op)
      go [] [result] = Just result
      go [] _ = Nothing
      go (t : ts) stack =
        case (parse t :: Maybe op) of
          Just oper -> case stack of
            (e1 : e2 : es) -> go ts (BinOp oper e2 e1 : es)
            _ -> Nothing
          _ -> case (parse t :: Maybe a) of
            Just n -> go ts (Lit n : stack)
            _ -> go ts (Var t : stack)

-- * Evaluation

-- | Class of evaluatable types
class Eval a op where
  -- \| Evaluates given binary operation with provided arguments
  evalBinOp :: op -> a -> a -> a

instance Eval Integer IntOp where
  evalBinOp :: IntOp -> Integer -> Integer -> Integer
  evalBinOp Add l r = l + r
  evalBinOp Mul l r = l * r
  evalBinOp Sub l r = l - r

-- | Evaluates given 'Expr' using given association list of variable values
--
-- Returns 'Nothing' in case appropriate variable value is missing.
--
-- Usage example:
--
-- >>> evalExpr [] (Lit 2 :: Expr Integer IntOp)
-- Just 2
-- >>> evalExpr [("x", 3)] (BinOp Add (Lit 2) (Var "x")) :: Maybe Integer
-- Just 5
-- >>> evalExpr [("x", 3)] (BinOp Add (Lit 2) (Var "y")) :: Maybe Integer
-- Nothing
evalExpr :: (Eval a op) => [(String, a)] -> Expr a op -> Maybe a
evalExpr table (BinOp oper le re) = case (evalExpr table le, evalExpr table re) of
  (Just lv, Just rv) -> Just $ evalBinOp oper lv rv
  _ -> Nothing
evalExpr _ (Lit n) = Just n
evalExpr table (Var name) = lookup name table

-- | Parses given integer expression in Reverse Polish Notation and evaluates it
-- using given association list of variable values
--
-- Returns 'Nothing' in case the expression could not be parsed
-- or in case appropriate variable value is missing.
--
-- Usage example:
--
-- >>> evaluateInteger [] "2"
-- Just 2
-- >>> evaluateInteger [("x", 3)] "2 x -"
-- Just (-1)
-- >>> evaluateInteger [("x", 3)] "2 y -"
-- Nothing
-- >>> evaluateInteger [] "3 2 * 3 +"
-- Just 9
-- >>> evaluateInteger [] "2 +"
-- Nothing
-- >>> evaluateInteger [] "2 3"
-- Nothing
evaluateInteger :: [(String, Integer)] -> String -> Maybe Integer
evaluateInteger = evaluate @_ @IntOp

-- | Parses given expression in Reverse Polish Notation and evaluates it
-- using given association list of variable values
--
-- Returns 'Nothing' in case the expression could not be parsed
-- or in case appropriate variable value is missing.
--
-- The 'forall a op.' part is required to define generic type
-- of intermediate 'Expr' expression that uses scoped type variables 'a' and 'op'.
evaluate :: forall a op. (Eval a op, Parse a, Parse op) => [(String, a)] -> String -> Maybe a
evaluate m s = case parse s of
  Just e -> evalExpr m (e :: Expr a op)
  Nothing -> Nothing
