module Lambda.Exp
  ( Exp(..)
  , Error(..)
  , eval
  , step
  , isEqual
  , bound
  ) where

--import Data.Type.Nat

type Var = Int

data Exp = H
         | V Var
         | F Exp
         | A Exp Exp
         deriving (Eq, Show)

newtype Error = TooLong Exp deriving Show

type Eval = Either Error Exp

eval :: Exp -> Eval
eval e = case eval' maxDepth e of
  Nothing -> Left $ TooLong e
  Just reducedE -> Right reducedE
  where maxDepth = 10

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
sub = sub' 1

-- De Brujin Index, Argument, Function Body -> Reduced Expression
sub' :: Int -> Exp -> Exp -> Exp
sub' _ _ H = H
sub' index arg (V i)
    | index == i = arg
    | otherwise  = V i
sub' index arg (F e) = F (sub' (index+1) arg e)
sub' index arg (A f a) = A (sub' index arg f) (sub' index arg a)

isEqual :: Exp -> Exp -> Either Error Bool
isEqual e1 e2 = do
  v1 <- eval e1
  v2 <- eval e2
  Right (v1 == v2)

bound :: Exp -> Bool
bound H = False
bound (V _) = True
bound (F e) = bound e
bound (A f a) = bound f && bound a
