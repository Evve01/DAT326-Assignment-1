module Lab1 where

type Env var dom = [(var, dom)]

data TERM v =   Empty
                | Singleton (TERM v)
                | Union (TERM v) (TERM v)
                | Intersection (TERM v) (TERM v)
                | Var v

data PRED v =   Elem (v) (TERM v)
                | Subset (TERM v) (TERM v)
                | And (PRED v) (PRED v)
                | Or (PRED v) (PRED v)
                | Implies (PRED v) (PRED v)

newtype Set = S [Set]
instance Eq Set where
    (S []) == (S [])  = True
    (S []) == set     = len set == 0
    set == (S [])     = len set == 0

len :: Set -> Int
len (S set) = length set

get :: Set -> [Set]
get (S set) = set

check :: Eq v => Env v Set -> PRED v -> Bool
check e (Elem var x)  = elemS (varVal e var) (eval e x)
check e (Subset x y)  = subsetS (eval e x) (eval e y)
check e (And x y)     = (check e x) && (check e y)
check e (Or x y)      = (check e x) || (check e y)
check e (Implies x y) = (check e x) ==> (check e y)

(==>) :: Bool -> Bool -> Bool
False ==> _ = True
True ==> p = p

subsetS :: Set -> Set -> Bool
subsetS (S xs) (S ys)   | len (S xs) == 0 = True
                        | len (S ys) == 0 = False
                        | len (S (tail xs)) == 0 = False
                        | otherwise = (elem (head xs) ys) && (subset (S (tail xs)) (S ys))

eval :: Eq v => Env v Set -> TERM v -> Set
eval e (Empty)            = S []
eval e (Singleton x)      = S [eval e x]
--eval e (Union x y)        = union (eval e x) (eval e y)
--eval e (Intersection x y) = intersection (eval e x) (eval e y)
eval e (Var var)          = varVal e var

--union :: Set -> Set

--intersection :: Set -> Set

varVal :: Eq v => Env v Set -> v -> Set  
varVal vss x = findS vss
    where findS ((v, s):vss)| x==v = s
                            | otherwise = findS vss


a = ([] :: [] Set)
