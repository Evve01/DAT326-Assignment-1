module Lab1 where
import           Data.List

type Env var dom = [(var, dom)]

data TERM v =   Empty
                | Singleton (TERM v)
                | Union (TERM v) (TERM v)
                | Intersection (TERM v) (TERM v)
                | Var v

data PRED v =   Elem v (TERM v)
                | Subset (TERM v) (TERM v)
                | And (PRED v) (PRED v)
                | Or (PRED v) (PRED v)
                | Implies (PRED v) (PRED v)

newtype Set = S [Set]
    deriving(Eq)

instance Show Set where
    show set = show (len set)

len :: Set -> Int
len (S set) = length set

get :: Set -> [Set]
get (S set) = set

check :: Eq v => Env v Set -> PRED v -> Bool
check e (Elem var x)  = elem (varVal e var) (nub (get (eval e x)))
check e (Subset x y)  = subset (eval e x) (eval e y)
check e (And x y)     = check e x && check e y
check e (Or x y)      = check e x || check e y
check e (Implies x y) = check e x ==> check e y

(==>) :: Bool -> Bool -> Bool
False ==> _ = True
True ==> p  = p

subset :: Set -> Set -> Bool
subset set1 set2 = (len inter /= 0) && inter == set1
  where
    inter = intersectS set1 set2


eval :: Eq v => Env v Set -> TERM v -> Set
eval e Empty              = S []
eval e (Singleton x)      = S [eval e x]
eval e (Union x y)        = unionS (eval e x) (eval e y)
eval e (Intersection x y) = intersectS (eval e x) (eval e y)
eval e (Var var)          = varVal e var

unionS :: Set -> Set -> Set
unionS set1 set2 = S (union (get set1) (get set2))

intersectS :: Set -> Set -> Set
intersectS set1 set2 = S (intersect (get set1) (get set2))

varVal :: Eq v => Env v Set -> v -> Set
varVal vss x = findS vss
    where findS ((v, s):vss)| x==v = s
                            | otherwise = findS vss

type Von v = TERM v
vonNeumann :: Integer -> Von v
vonNeumann 0 = Empty
vonNeumann x = Union (vonNeumann (x - 1)) (Singleton (vonNeumann (x - 1)))

-- claim1: if n1 <= n2 then subset n1 n2
-- claim2: n = {0, 1,..., n - 1}
--

n1 = vonNeumann 1
n2 = vonNeumann 2

claim1 :: Integer -> Integer -> Bool
claim1 n1 n2 = n1 <= n2 && subset (eval [] v1) (eval [] v2) 
  where 
    v1 = vonNeumann n1
    v2 = vonNeumann n2
  
