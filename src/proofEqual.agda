module proofEqual where

data Boolean : Set where
 true : Boolean
 false : Boolean

not : Boolean -> Boolean
not true = false
not false = true

_and_ : Boolean -> Boolean -> Boolean
false and _ = false
true and x  = x

_or_ : Boolean -> Boolean -> Boolean
false or x = x
true or y = y

data Equal : Boolean -> Boolean -> Set where
 equal : (x : Boolean) -> Equal x x

proof : Equal (not false) true
proof = equal true

proofAnd : Equal (true and true) true
proofAnd = equal true
