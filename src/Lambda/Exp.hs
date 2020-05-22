{-# LANGUAGE TypeSynonymInstances #-}

module Lambda.Exp
  ( Exp(..)
  , isBound
  , depth
  ) where

import Lambda.Peano

type Var = Peano

data Exp = H
         | V Var
         | F Exp
         | A Exp Exp
         deriving Eq

instance Show Exp where
  show H = "_"
  show (V n) = show n
  show (F e) = "\\" ++ show e
  show (A f a) = "(" ++ show f ++ " " ++ show a ++ ")"

isBound :: Exp -> Bool
isBound H = False
isBound (V _) = True
isBound (F e) = isBound e
isBound (A f a) = isBound f && isBound a

depth :: Exp -> Peano
depth H = Z
depth (V _) =   S Z
depth (F e) =   S $ depth e
depth (A f a) = S $ toPeano $ toInt (depth f) + toInt (depth a)
