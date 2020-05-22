module Lambda.Progs
  ( progs
  ) where

import Lambda.Exp
import Lambda.Peano
import Lambda.Eval
import Data.List (nub)

-- All programs that are functions with the right number of params and no unbound terms
progs :: Peano -> [Exp]
progs n = filter isBound $ allBinds =<< filter (isSig one) (progShapes' =<< (toInt <$> range one n))

progShapes' :: Int -> [Exp]
progShapes' 0 = [H]
progShapes' n
    | n < 0 = []
    | n > 0 = (F <$> progShapes' m) ++ concatMap ps [0..m]
  where ps i = A <$> progShapes' i <*> progShapes' (m-i-1)
        m = n-1

isBetaNormal :: Exp -> Bool
isBetaNormal e = case step e of
  Nothing -> True
  _ -> False

isSig :: Peano -> Exp -> Bool
isSig Z _ = True
isSig (S p) (F e) = isSig p e
isSig _ _ = False

allBinds :: Exp -> [Exp]
allBinds = allBinds' Z

allBinds' :: Peano -> Exp -> [Exp]
allBinds' mi H       = H : (V <$> range one mi)
allBinds' _  (V i)   = [V i]
allBinds' mi (F e)   = F <$> allBinds' (S mi) e
allBinds' mi (A f a) = A <$> allBinds' mi f <*> allBinds' mi a