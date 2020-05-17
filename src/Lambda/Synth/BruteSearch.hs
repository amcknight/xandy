module Lambda.Synth.BruteSearch
  ( synth
  ) where

import Lambda.Exp
import Lambda.Peano
import Lambda.Progs
import Lambda.Eval
import Data.Foldable (find)

synth :: Exp -> Maybe Exp
synth oracle = find (isProvenEqual oracle) (progs maxDepth)
  where maxDepth = toPeano 20
