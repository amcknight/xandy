module Lambda.Synth
  ( synth
  ) where

import Lambda.Exp
import Data.Foldable (find)
import Control.Exception.Base (throw, Exception)

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
progs 0 vs = V <$> vs
progs n vs = progs (n-1) vs ++ (F <$> vs <*> progs (n-1) vs) ++ (A <$> progs (n-1) vs <*> progs (n-1) vs)

guess :: [Res] -> String -> Maybe Exp
guess rs vs = find (allowed rs) (progs maxDepth vs)
  where maxDepth = 7

check :: Exp -> Exp -> Maybe Res
check oracle e =
  case isEqual oracle e of
    Nothing -> throw $ Unequal "CANNAT DETERMINE EQUALITY"
    Just False -> Just (CEx e)
    Just True -> Nothing

newtype Unequal = Unequal String
instance Exception Unequal
instance Show Unequal where
  show (Unequal s) = s
