module Test where
import           Lab1

a, b :: TERM Set
a = Union Empty (Singleton Empty)
b = Union Empty Empty
c = Union (Singleton Empty) (Singleton Empty)
d = Union (Singleton Empty) (Singleton (Singleton Empty))

-- e :: Env Set Set
-- e = []

v = vonNeumann 1
vn = vonNeumann 0

v1 = vonNeumann 1
v2 = vonNeumann 2

s1 = eval e v1
s2 = eval e v2