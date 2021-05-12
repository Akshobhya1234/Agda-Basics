module BooleanFunctions  where

--Define Boolean type
data Boolean : Set where
 true : Boolean
 false : Boolean

--takes one 
data Unit : Set where
 unit : Unit

--takes none
data Empty : Set where

--identity function
identity : Boolean → Boolean
identity true = true
identity false = false

--NOT function
not : Boolean -> Boolean
not true = false
not false = true

--and function "_" takes arguments
_and_ : Boolean -> Boolean -> Boolean
false and  false = false
true and false = false
false and true = false
true and true = true

--or function simplified
_or_ : Boolean -> Boolean -> Boolean
false or y = y
true or _ = true

--if then else logic
if_then_else_ : Boolean -> Boolean -> Boolean -> Boolean
if false then _ else y = y
if true then x else _ = x

--Not using if then else
notAgain : Boolean -> Boolean
notAgain x = if x then false else true

--Logical implication function
_implies_ : Boolean -> Boolean -> Boolean
true implies true = true
true implies false = false
false implies true = true
false implies false = true

--Equivalent function
_equivalent_ : Boolean -> Boolean -> Boolean
true equivalent true = true
true equivalent false = false
false equivalent true = false
false equivalent false = true

--identity function 
identityAgain : (X : Set) -> X  -> X
identityAgain X x = x

-- type \+e+l+l for ℓ
open import Agda.Primitive
AgainIf_then_else_ : {ℓ : Level} -> {X : Set ℓ} -> Boolean -> X -> X -> X
AgainIf false then _ else x = x
AgainIf true then y else _  = y
