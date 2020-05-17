module Lambda.Progs
  ( progs
  ) where

import Lambda.Exp
import Lambda.Peano

-- All programs that are functions with the right number of params and no unbound terms
progs :: Peano -> [Exp]
progs n = filter isBound $ allBinds =<< filter (isSig one) (progShapes' =<< range one n)

progShapes' :: Peano -> [Exp]
progShapes' Z = [H]
progShapes' (S p) = (F <$> subShapes) ++ (A <$> subShapes <*> subShapes)
  where subShapes = progShapes' p

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