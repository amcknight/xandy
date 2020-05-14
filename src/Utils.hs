module Utils
  ( merge
  ) where

-- Merge two sorted lists
merge :: Ord a => [a] -> [a] -> [a]
merge vs1 [] = vs1
merge [] vs2 = vs2
merge (v1:vs1) (v2:vs2) =
  case compare v1 v2 of
    EQ -> v1 : merge vs1 vs2
    LT -> v1 : merge vs1 (v2:vs2)
    GT -> v2 : merge (v1:vs1) vs2
