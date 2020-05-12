module Exp
  ( Exp(..)
  , depth
  , vars
  , eval
  , isEqual
  ) where

import qualified Data.Map as M

data Exp = Term Bool
         | Var Char
         | Not Exp
         | And Exp Exp
         deriving (Show, Eq)

depth :: Exp -> Int
depth (Term _) = 0
depth (Var _) = 0
depth (Not e) = 1 + depth e
depth (And e1 e2) = 1 + max (depth e1) (depth e2)

vars :: Exp -> String
vars (Term _) = []
vars (Var v) = [v]
vars (Not e) = vars e
vars (And e1 e2) = merge (vars e1) (vars e2)

merge :: Ord a => [a] -> [a] -> [a]
merge vs1 [] = vs1
merge [] vs2 = vs2
merge (v1:vs1) (v2:vs2) =
  case compare v1 v2 of
    EQ -> v1 : merge vs1 vs2
    LT -> v1 : merge vs1 (v2:vs2)
    GT -> v2 : merge (v1:vs1) vs2

type Bind = M.Map Char Bool

bindings :: String -> [Bind]
bindings [] = []
bindings cs = M.fromList . zip cs <$> bindings' (length cs)

bindings' :: Int -> [[Bool]]
bindings' 0 = [[]]
bindings' n = ((True :) <$> bs) ++ ((False :) <$> bs)
  where bs = bindings' (n-1)

eval :: Exp -> Bind -> Maybe Bool
eval (Term t) _ = Just t
eval (Var v) b = M.lookup v b
eval (Not e) b = not <$> eval e b
eval (And e1 e2) b = (&&) <$> eval e1 b <*> eval e2 b

isEqual :: Exp -> Exp -> Bool
isEqual e1 e2 =
  case bindings (merge (vars e1) (vars e2)) of
    [] -> eval e1 mempty == eval e2 mempty
    bs -> map (eval e1) bs == map (eval e2) bs