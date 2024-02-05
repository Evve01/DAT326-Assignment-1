{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use infix" #-}
module Lab1 where

import Data.List

type Env var dom = [(var, dom)]

data TERM v
  = Empty
  | Singleton (TERM v)
  | Union (TERM v) (TERM v)
  | Intersection (TERM v) (TERM v)
  | Var v
  deriving (Show, Eq)

data PRED v
  = Elem (TERM v) (TERM v)
  | Subset (TERM v) (TERM v)
  | And (PRED v) (PRED v)
  | Or (PRED v) (PRED v)
  | Implies (PRED v) (PRED v)
  deriving (Show)

newtype Set = S [Set]
  deriving (Ord)

instance Eq Set where
  (S s1) == (S s2) = sort (nub s1) == sort (nub s2)

instance Show Set where
  show set = show (len set)

len :: Set -> Int
len (S set) = length set

get :: Set -> [Set]
get (S set) = nub set

check :: Eq v => Env v Set -> PRED v -> Bool
check e (Elem x y) = elem (eval e x) (get (eval e y))
check e (Subset x y) = subset (eval e x) (eval e y)
check e (And x y) = check e x && check e y
check e (Or x y) = check e x || check e y
check e (Implies x y) = check e x ==> check e y

(==>) :: Bool -> Bool -> Bool
False ==> _ = True
True ==> p = p

subset :: Set -> Set -> Bool
subset (S []) _ = True
subset set1 set2 = (len inter /= 0) && inter == set1
  where
    inter = intersectS set1 set2

varVal :: Eq v => Env v Set -> v -> Set
varVal e v = head [d | (v', d) <- e, v == v']

eval :: Eq v => Env v Set -> TERM v -> Set
eval e Empty = S []
eval e (Singleton x) = S [eval e x]
eval e (Union x y) = unionS (eval e x) (eval e y)
eval e (Intersection x y) = intersectS (eval e x) (eval e y)
eval e (Var var) = varVal e var

unionS :: Set -> Set -> Set
unionS set1 set2 = S (union (get set1) (get set2))

intersectS :: Set -> Set -> Set
intersectS set1 set2 = S (intersect (get set1) (get set2))

type Von v = TERM v

vonNeumann :: Integer -> Von v
vonNeumann 0 = Empty
vonNeumann x = Union (vonNeumann (x - 1)) (Singleton (vonNeumann (x - 1)))

n1 = vonNeumann 1

n2 = vonNeumann 2

e = [] :: Env Set Set

-- claim1: if n1 <= n2 then subset n1 n2
claim1 :: Integer -> Integer -> Bool
claim1 n1 n2 = (n1 <= n2) ==> subset (eval e v1) (eval e v2)
  where
    (v1, v2) = (vonNeumann n1, vonNeumann n2)

-- claim2: n = {0, 1,..., n - 1}
--         v = vn 
claim2 :: Integer -> Bool
claim2 0 = vonNeumann 0 == Empty
claim2 n = check e (Subset vn v)
  where
    v = vonNeumann n
    vn = vonNeumann (n - 1)
-- claim2 0 = vonNeumann 0 == Empty
-- claim2 n = check e (Subset vn v) && claim2 (n - 1)
--   where
--     v = vonNeumann n
--     vn = vonNeumann (n - 1)

{-
Elem should input two terms - OK
Empty should be a subset of empty.  - OK
Subset uses the wrong equality. 

claim1 doesn't work - OK
claim2 hardcodes true and fails for 0, and is difficult to read.
-}