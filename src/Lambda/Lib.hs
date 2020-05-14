module Lambda.Lib (synthS) where

import Lambda.Synth
import Lambda.Exp

synthS :: Maybe Exp
synthS = synth (stupid s)

stupid :: Exp -> Exp
stupid = A (F 's' (V 's'))

s :: Exp
s =
  F 'a' (F 'b' (F 'c'
    (A
      (A (V 'a') (V 'c'))
      (A (V 'b') (V 'c'))
    )
  ))

k :: Exp
k = F 'l' (F 'r' (V 'l'))