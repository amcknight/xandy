module Lambda.Synth.Cegis
  ( synth
  ) where

import Lambda.Exp
import Lambda.Peano
import Lambda.Eval
import Lambda.Progs
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

guess :: [Constraint] -> Maybe Exp
guess rs = find (allowed rs) (progs maxDepth)
  where maxDepth = toPeano 10

check :: Exp -> Exp -> Result
check oracle e =
  case isEqual oracle e of
    Left (TooLong _) -> Con $ CEx e -- Isn't really a counter example, but we want to avoid this
    Right False -> Con $ CEx e
    Right True -> Success

newtype Unequal = Unequalable String
instance Exception Unequal
instance Show Unequal where
  show (Unequalable s) = s
