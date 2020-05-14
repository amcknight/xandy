module AndNot.Exp
  ( Exp(..)
  , eval
  , vars
  , isEqual
  ) where

import qualified Data.Map as M
import Utils

data Exp = Term Bool
         | Var Char
         | Not Exp
         | And Exp Exp
         deriving (Show, Eq)

vars :: Exp -> String
vars (Term _) = []
vars (Var v) = [v]
vars (Not e) = vars e
vars (And e1 e2) = merge (vars e1) (vars e2)

type Bind = M.Map Char Bool

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

bindings :: String -> [Bind]
bindings [] = []
bindings cs = M.fromList . zip cs <$> bindings' (length cs)

bindings' :: Int -> [[Bool]]
bindings' 0 = [[]]
bindings' n = ((True :) <$> bs) ++ ((False :) <$> bs)
  where bs = bindings' (n-1)