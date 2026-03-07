{-# OPTIONS_GHC -Wall #-}

-- The above pragma enables all warnings

module Task1 where

import Text.Read (readMaybe)

-- * Expression data type

-- | Representation of integer arithmetic expressions comprising
-- - Literals of type 'a'
-- - Binary operations 'Add' and 'Mul'
data IExpr
  = Lit Integer
  | Add IExpr IExpr
  | Mul IExpr IExpr
  deriving (Show)

-- * Evaluation

-- | Evaluates given 'IExpr'
--
-- Usage example:
--
-- >>> evalIExpr (Lit 2)
-- 2
-- >>> evalIExpr (Add (Lit 2) (Lit 3))
-- 5
-- >>> evalIExpr (Add (Mul (Lit 3) (Lit 2)) (Lit 3))
-- 9
evalIExpr :: IExpr -> Integer
evalIExpr (Lit n) = n
evalIExpr (Add lexpr rexpr) = evalIExpr lexpr + evalIExpr rexpr
evalIExpr (Mul lexpr rexpr) = evalIExpr lexpr * evalIExpr rexpr

-- * Parsing

-- | Class of parseable types
class Parse a where
  -- | Parses value 'a' from given string
  -- wrapped in 'Maybe' with 'Nothing' indicating failure to parse
  parse :: String -> Maybe a

instance Parse Integer where
  parse = readMaybe

instance Parse Bool where
  parse = readMaybe

-- | Parses given expression in Reverse Polish Notation
-- wrapped in 'Maybe' with 'Nothing' indicating failure to parse
--
-- Usage example:
--
-- >>> parse "2" :: Maybe IExpr
-- Just (Lit 2)
-- >>> parse "2 3 +" :: Maybe IExpr
-- Just (Add (Lit 2) (Lit 3))
-- >>> parse "3 2 * 3 +" :: Maybe IExpr
-- Just (Add (Mul (Lit 3) (Lit 2)) (Lit 3))
-- >>> parse "2 +" :: Maybe IExpr
-- Nothing
-- >>> parse "2 3" :: Maybe IExpr
-- Nothing
instance Parse IExpr where
  parse input = go (words input) []
    where
      go :: [String] -> [IExpr] -> Maybe IExpr
      go [] [e] = Just e
      go [] _ = Nothing
      go ("*" : ts) (e1 : e2 : es) = go ts (Mul e2 e1 : es)
      go ("+" : ts) (e1 : e2 : es) = go ts (Add e2 e1 : es)
      go (t : ts) acc = case (readMaybe t :: Maybe Integer) of
        Just n -> go ts (Lit n : acc)
        _ -> Nothing

-- * Evaluation with parsing

-- | Parses given expression in Reverse Polish Notation and evaluates it
--
-- Returns 'Nothing' in case the expression could not be parsed.
--
-- Usage example:
--
-- >>> evaluateIExpr "2"
-- Just 2
-- >>> evaluateIExpr "2 3 +"
-- Just 5
-- >>> evaluateIExpr "3 2 * 3 +"
-- Just 9
-- >>> evaluateIExpr "2 +"
-- Nothing
-- >>> evaluateIExpr "2 3"
-- Nothing
evaluateIExpr :: String -> Maybe Integer
evaluateIExpr input = do
  e <- parse input
  Just (evalIExpr e)
