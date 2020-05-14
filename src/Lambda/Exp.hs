module Lambda.Exp
  ( Exp(..)
  , eval
  , vars
  , isEqual
  ) where

import Utils

type Var = Char

data Exp = V Var
         | F Var Exp
         | A Exp Exp
         deriving (Eq, Show)

eval :: Int -> Exp -> Maybe Exp
eval 0 _ = Nothing
eval _ (V c) = Just (V c)
eval d (F v bod) = F v <$> eval (d-1) bod
eval d (A f a) = do
  ea <- eval (d-1) a
  ef <- eval (d-1) f
  case ef of
    (F v bod) -> eval (d-1) (sub v ea bod)
    _ -> Just (A ef ea)

-- Assumes unique variables
-- Variable, Argument, Function Body -> Reduced Expression
sub :: Var -> Exp -> Exp -> Exp
sub param arg (V c)
    | param == c = arg
    | otherwise  = V c
sub param arg (F v e) = F v (sub param arg e)
sub param arg (A f a) = A (sub param arg f) (sub param arg a)

vars :: Exp -> String
vars (V v) = [v]
vars (F c e) = merge [c] (vars e)
vars (A e1 e2) = merge (vars e1) (vars e2)

isEqual :: Exp -> Exp -> Maybe Bool
isEqual e1 e2 = do
  v1 <- eval maxDepth e1
  v2 <- eval maxDepth e2
  Just (v1 == v2)
  where maxDepth = 10
