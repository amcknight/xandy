module Lambda.Peano
  ( Peano(..)
  , toPeano, toInt
  , range
  , zero, one, two, three, four
  , infinity
  ) where

data Peano = Z | S Peano deriving (Eq, Ord)

instance Show Peano where
  show = show . toInt

toInt :: Peano -> Int
toInt Z = 0
toInt (S p) = 1 + toInt p

toPeano :: Int -> Peano
toPeano 0 = Z
toPeano i = S $ toPeano (i-1)

range :: Peano -> Peano -> [Peano]
range l r
    | l > r = []
    | l == r = [l]
range l r = l : range (S l) r

zero :: Peano
zero = Z
one :: Peano
one = S zero
two :: Peano
two = S one
three :: Peano
three = S two
four :: Peano
four = S three

infinity :: Peano
infinity = S infinity