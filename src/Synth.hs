module Synth
  ( synth
  ) where

import Exp
import Data.Foldable (find)

synth :: Exp -> Maybe Exp
synth oracle = synth' [CEx oracle] oracle

synth' :: [Res] -> Exp -> Maybe Exp
synth' rs oracle =
  case guess rs (vars oracle) of
    Nothing -> Nothing
    Just g ->
      case check oracle g of
        Nothing -> Just g
        Just r -> synth' (r:rs) oracle

newtype Res = CEx Exp

allowed :: [Res] -> Exp -> Bool
allowed rs g = and (fmap (allowedOne g) rs)

allowedOne :: Exp -> Res -> Bool
allowedOne ex (CEx rEx) = ex /= rEx

progs :: Int -> String -> [Exp]
progs 0 vs = Term True : Term False : (Var <$> vs)
progs n vs = progs (n-1) vs ++ (Not <$> progs (n-1) vs) ++ (And <$> progs (n-1) vs <*> progs (n-1) vs)

guess :: [Res] -> String -> Maybe Exp
guess rs vs = find (allowed rs) (progs maxDepth vs)
  where maxDepth = 8

check :: Exp -> Exp -> Maybe Res
check oracle e =
  if isEqual oracle e
  then Nothing
  else Just (CEx e)

