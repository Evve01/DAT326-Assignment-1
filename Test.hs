module Test where
import Lab1 

a, b :: TERM Set
a = Union Empty (Singleton (Empty))
b = Union Empty Empty
c = Union (Singleton Empty) (Singleton Empty)
d = Union (Singleton Empty) (Singleton (Singleton (Empty)))

e :: Env Set Set
e = []
