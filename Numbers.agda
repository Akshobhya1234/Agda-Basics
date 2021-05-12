module Numbers where

open import Agda.Primitive

data NaturalNumbers : Set where
 zero : NaturalNumbers
 suc : NaturalNumbers -> NaturalNumbers

data Empty : Set where

{-# BUILTIN NATURAL NaturalNumbers #-}

_+_ : NaturalNumbers -> NaturalNumbers -> NaturalNumbers
zero + y = y
suc x + y = suc(x + y)

--abstract equal method
data Equal {a : Level} {X : Set a} : X -> X -> Set where
 equal : {x : X} -> Equal x x

{-# BUILTIN EQUALITY Equal #-}

_==_ = Equal

0+x : (x : NaturalNumbers) -> (0 + x) ==  x
0+x x = equal 

x+0 : (x : NaturalNumbers) -> (x + 0) == x
x+0 zero = equal
x+0 (suc x) rewrite x+0 x = equal --rewrite requires builtin equality

--proof by negation. mapping is not possible to empty set
one=two : 1 == 2 -> Empty
one=two ()
