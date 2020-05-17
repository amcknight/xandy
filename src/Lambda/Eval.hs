module Lambda.Eval
  ( eval
  , step
  , isEqual
  , isProvenEqual
  , EvalError(..)
  ) where

import Lambda.Exp
import Lambda.Peano

newtype EvalError = TooLong Exp deriving Show
type Eval = Either EvalError Exp

eval :: Exp -> Eval
eval e = case eval' maxDepth e of
  Nothing -> Left $ TooLong e
  Just reducedE -> Right reducedE
  where maxDepth = 30

-- Rewrite in terms of step
eval' :: Int -> Exp -> Maybe Exp
eval' 0 _ = Nothing
eval' _ H = Just H
eval' _ (V i) = Just $ V i
eval' d (F bod) = F <$> eval' (d-1) bod
eval' d (A f a) = do
  ea <- eval' (d-1) a
  ef <- eval' (d-1) f
  case ef of
    (F bod) -> eval' (d-1) (sub ea bod)
    _ -> Just (A ef ea)


step :: Exp -> Maybe Exp
step H       = Nothing
step (V _)   = Nothing
step (F bod) = F <$> step bod
step (A f a) =
  case step f of
    Just ef -> Just $ A ef a
    Nothing -> case step a of
      Just ea -> Just $ A f ea
      Nothing -> case f of
        F bod -> Just $ sub a bod
        _ -> Nothing


-- Substitute the Argument into the Function Body
sub :: Exp -> Exp -> Exp
sub = sub' one

-- De Brujin Index, Argument, Function Body -> Reduced Expression
sub' :: Peano -> Exp -> Exp -> Exp
sub' _ _ H = H
sub' index arg (V i)
    | index == i = arg
    | otherwise  = V i
sub' index arg (F e) = F (sub' (S index) arg e)
sub' index arg (A f a) = A (sub' index arg f) (sub' index arg a)

isEqual :: Exp -> Exp -> Either EvalError Bool
isEqual e1 e2 = do
  v1 <- eval e1
  v2 <- eval e2
  Right (v1 == v2)

isProvenEqual :: Exp -> Exp -> Bool
isProvenEqual e1 e2 =
  case isEqual e1 e2 of
    Right b -> b
    Left _ -> False