{-# LANGUAGE TypeSynonymInstances #-}

module Lambda.Exp
  ( Exp(..)
  , isBound
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
  show (F e) = "\\ " ++ show e
  show (A f a) = "(" ++ show f ++ " " ++ show a ++ ")"

isBound :: Exp -> Bool
isBound H = False
isBound (V _) = True
isBound (F e) = isBound e
isBound (A f a) = isBound f && isBound a
