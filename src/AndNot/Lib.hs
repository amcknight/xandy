{-# LANGUAGE ScopedTypeVariables #-}
module AndNot.Lib (synthOr) where

import AndNot.Synth
import AndNot.Exp

--quat :: IO SatResult
--quat = sat . forAll ["x"] $ \(x::SWord8) -> x * 2 .== x + x

synthOr :: Maybe Exp
synthOr = synth stupidOr

stupidOr :: Exp
stupidOr =
  Not (Not (Not (And
    (Not (Not (Not (Var 'x'))))
    (And (Term True) (Not (Var 'y')))
  )))
