-- The above pragma enables all warnings
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall #-}

module Task3 where

import Control.Monad (replicateM)
import Data.Maybe (fromMaybe)
import Task1 (Parse (parse))
import Task2

data BoolOp = And | Or | Xor
  deriving (Show)

instance Parse BoolOp where
  parse "and" = Just And
  parse "or" = Just Or
  parse "xor" = Just Xor
  parse _ = Nothing

instance Eval Bool BoolOp where
  evalBinOp :: BoolOp -> Bool -> Bool -> Bool
  evalBinOp And l r = l && r
  evalBinOp Or l r = l || r
  evalBinOp Xor l r = l /= r

-- | Solves SAT problem for given boolean formula written in Reverse Polish Notation
--
-- Returns whether given formula is satifiable
-- wrapped into 'Maybe' with 'Nothing' indicating parsing failure.
--
-- Only following binary operations are allowed:
-- - and
-- - or
-- - xor
--
-- All other words are considered as variables.
--
-- Usage example:
--
-- >>> solveSAT "x y and"
-- Just True
-- >>> solveSAT "x y xor x y and and"
-- Just False
-- >>> solveSAT "x y xor x y and or"
-- Just True
-- >>> solveSAT "x xor"
-- Nothing
solveSAT :: String -> Maybe Bool
solveSAT s = case parse s of
  Just (e :: Expr Bool BoolOp) -> Just $ any (fromMaybe False . evalBoolExpr) possibleValues
    where
      evalBoolExpr m = evaluate @Bool @BoolOp m s
      names = getVars e
      possibleValues = [zip names vals | vals <- replicateM (length names) [True, False]]
  _ -> Nothing
