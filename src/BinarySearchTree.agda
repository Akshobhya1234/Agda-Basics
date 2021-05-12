module BinarySearchTree where

data Bool : Set where
 true : Bool
 false : Bool

not : Bool -> Bool
not true = false
not false = true

_and_ : (x y : Bool) -> Bool
true and x = x
false and y = false

data Natural : Set where
 zero : Natural
 suc : Natural -> Natural
{-# BUILTIN NATURAL Natural #-}

_LE_ : (x y : Natural) -> Bool
zero LE y = true
suc x LE zero = false
suc x LE suc y = x LE y

data List : Set where
 null : List
 _::_ : Natural -> List -> List

infix 5 _::_


data BinaryTree : Set where
 leaf : BinaryTree
 node : (n : Natural) (lt rt : BinaryTree) -> BinaryTree


BTree : BinaryTree
BTree = node 3 (node 1 leaf leaf) (node 5 leaf leaf)

append : List -> List -> List
append null ys = ys
append (x :: xs) ys = x :: append xs ys

toList : BinaryTree -> List
toList leaf = null
toList (node p lt rt) = append (toList lt) (p :: toList rt)

insert : Natural -> BinaryTree -> BinaryTree
insert p leaf = node p leaf leaf
insert p (node x lt rt) with  p LE x -- use with to call as function
insert p (node x lt rt) | true = {!!}
insert p (node x lt rt) | false = {!!}
