module Lambda.Lib (synthS) where

import Lambda.Synth
import Lambda.Exp

synthS :: Maybe Exp
synthS = synth (stupid s)

stupid :: Exp -> Exp
stupid = A i

s :: Exp
s =
  F (F (F
    (A
      (A (V 3) (V 1))
      (A (V 2) (V 1))
    )
  ))

k :: Exp
k = F (F (V 2))

i :: Exp
i = F (V 1)
