module Lambda.Lib (synthS) where

import Lambda.Synth.BruteSearch
import Lambda.Exp
import Lambda.Peano

synthS :: Maybe Exp
synthS = synth (stupid s)

stupid :: Exp -> Exp
stupid = A i

s :: Exp
s =
  F (F (F
    (A
      (A (V three) (V one))
      (A (V two) (V one))
    )
  ))

k :: Exp
k = F (F (V two))

i :: Exp
i = F (V one)

y :: Exp
y =
  F (A
    (F (A (V two) (A (V one) (V one))))
    (F (A (V two) (A (V one) (V one))))
  )
