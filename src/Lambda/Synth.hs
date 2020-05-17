module Lambda.Synth
  ( synth
  ) where

import Lambda.Exp
import Data.Foldable (find)
import Control.Exception.Base (throw, Exception)

newtype Constraint = CEx Exp

data Result = Success
            | Con Constraint
            | Error Unequal

synth :: Exp -> Maybe Exp
synth oracle = synth' [CEx oracle] oracle

synth' :: [Constraint] -> Exp -> Maybe Exp
synth' cs oracle =
  case guess cs of
    Nothing -> Nothing
    Just g ->
      case check oracle g of
        Success -> Just g
        Con c -> synth' (c:cs) oracle
        Error err -> throw err

allowed :: [Constraint] -> Exp -> Bool
allowed rs g = and (fmap (allowedOne g) rs)

allowedOne :: Exp -> Constraint -> Bool
allowedOne ex (CEx rEx) = ex /= rEx

progs :: Int -> [Exp]
progs n = filter bound $ allBinds =<< filter (isSig 1) (progShapes' =<< [1 .. n])

progShapes' :: Int -> [Exp]
progShapes' 0 = [H]
progShapes' n = (F <$> pn) ++ (A <$> pn <*> pn)
  where pn = progShapes' (n-1)

--(allBinds =<<

isSig :: Int -> Exp -> Bool
isSig 0 _ = True
isSig n (F e) = isSig (n-1) e
isSig _ _ = False

allBinds :: Exp -> [Exp]
allBinds = allBinds' 0

allBinds' :: Int -> Exp -> [Exp]
allBinds' mi H       = H : (V <$> [1..mi])
allBinds' _  (V i)   = [V i]
allBinds' mi (F e)   = F <$> allBinds' (mi+1) e
allBinds' mi (A f a) = A <$> allBinds' mi f <*> allBinds' mi a

guess :: [Constraint] -> Maybe Exp
guess rs = find (allowed rs) (progs maxDepth)
  where maxDepth = 7

check :: Exp -> Exp -> Result
check oracle e =
  case isEqual oracle e of
    Left err -> Error $ Unequalable $ "CANNAT DETERMINE EQUALITY because of: " ++ show err
    Right False -> Con $ CEx e
    Right True -> Success

newtype Unequal = Unequalable String
instance Exception Unequal
instance Show Unequal where
  show (Unequalable s) = s
