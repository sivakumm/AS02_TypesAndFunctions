{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-# HLINT ignore "Evaluate" #-}
{-# HLINT ignore "Use list literal" #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# HLINT ignore "Eta reduce" #-}

module Main (main) where

import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck (Testable (property))
import Prelude hiding (Foldable)

todo :: a
todo = error "TODO"

-------------------------------------------------------------------------------
-- 0. Types
-------------------------------------------------------------------------------

-- Implement the two functions with the given types.
-- There is only one solution each which is a total and terminating.
-- Follow the types!
riddleA :: ((a, b) -> c) -> (c -> d) -> (a, b) -> d
riddleA f1 f2 tuple = (f2.f1) tuple -- I first came up with `riddleA f1 f2 tuple = f2 (f1 tuple)`

riddleB :: a -> ((a -> b) -> c) -> (a -> a -> b) -> c
riddleB a f1 f2 = (f1.f2) a -- again, I wrote first `f1 (f2 a)` and then resolved it to (f1.f2) a

-------------------------------------------------------------------------------
-- 1. Recursion on Lists
-------------------------------------------------------------------------------

-- Implement the function `myLength`. It returns the length of a list:
myLength :: [a] -> Int
myLength [] = 0
myLength (_:as) = 1 + myLength as

myLengthSpec :: Spec
myLengthSpec =
  describe "myLength" $ do
    it "myLength [] == 0" $ myLength [] `shouldBe` 0
    it "myLength [1,2] == 2" $ myLength ([1, 2] :: [Int]) `shouldBe` 2
    it "behaves like length" $ property $ \(l :: [Int]) -> myLength l == myLength l

-- Implement the function myReverse. It reverses a list:
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (a:as) = myReverse as ++ [a]

myReverseSpec :: Spec
myReverseSpec =
  describe "myReverse" $ do
    it "myReverse [] == []" $ myReverse [] `shouldBe` ([] :: [Int])
    it "myReverse [1,2] == [2,1]" $ myReverse [1, 2] `shouldBe` ([2, 1] :: [Int])
    it "behaves like reverse" $ property $ \(l :: [Int]) -> myReverse l == reverse l

-- Implement the function drop. It drops the first n elements.
-- It returns the list unchanged for negative n.
myDrop :: Int -> [a] -> [a]
myDrop 0 as = as
myDrop n (a:as)
  | n >= 0 = myDrop (n-1) as
  | otherwise = (a:as)

myDropSpec :: Spec
myDropSpec =
  describe "myDrop" $ do
    it "myDrop 0 [] == []" $ myDrop 0 [] `shouldBe` ([] :: [Int])
    it "myDrop 2 [] == []" $ myDrop 2 [] `shouldBe` ([] :: [Int])
    it "myDrop 2 [1,2] == []" $ myDrop 2 [1, 2] `shouldBe` ([] :: [Int])
    it "myDrop 2 [1,2,3] == [3]" $ myDrop 2 [1, 2, 3] `shouldBe` ([3] :: [Int])
    it "myDrop 7 [1,2,3] == []" $ myDrop 7 [1, 2, 3] `shouldBe` ([] :: [Int])
    it "behaves like drop" $ property $ \(n :: Int, l :: [Int]) -> myDrop n l == drop n l

-------------------------------------------------------------------------------
-- 2. Recursion on Trees (https://en.wikipedia.org/wiki/Tree_traversal)
-------------------------------------------------------------------------------

data Bin a = Leaf a | Fork (Bin a) a (Bin a) deriving (Eq, Show)

-- Example tree with Chars as elements.
-- It may help to draw it on paper.
exTree :: Bin Char
exTree =
  Fork
    ( Fork
        (Leaf 'a')
        'b'
        (Leaf 'c')
    )
    'd'
    ( Fork
        (Leaf 'e')
        'f'
        (Leaf 'g')
    )

-- Implement the function preorder. It traverses a binary tree pre-order:
preorder :: Bin a -> [a]
preorder (Leaf a) = [a]
preorder (Fork b a c) = [a] ++ preorder b ++ preorder c

preorderSpec :: Spec
preorderSpec =
  describe "preorder" $ do
    it "preorder (Leaf 1) == [1]" $ preorder (Leaf 1) `shouldBe` ([1] :: [Int])
    it "preorder (Fork (Leaf 'a') 'b' (Leaf 'c')) == \"bac\"" $ preorder (Fork (Leaf 'a') 'b' (Leaf 'c')) `shouldBe` "bac"
    it "preorder exTree == \"dbacfeg\"" $ preorder exTree `shouldBe` "dbacfeg"

-- Implement the function inorder. It traverses a binary tree in-order:
inorder :: Bin a -> [a]
inorder (Leaf a) = [a]
inorder (Fork a b c) = inorder a ++ [b] ++ inorder c

inorderSpec :: Spec
inorderSpec =
  describe "inorder" $ do
    it "inorder (Leaf 1) == [1]" $ inorder (Leaf 1) `shouldBe` ([1] :: [Int])
    it "inorder (Fork (Leaf 'a') 'b' (Leaf 'c')) == \"abc\"" $ inorder (Fork (Leaf 'a') 'b' (Leaf 'c')) `shouldBe` "abc"
    it "inorder exTree == \"abcdefg\"" $ inorder exTree `shouldBe` "abcdefg"

-- Implement the function postorder. It traverses a binary tree post-order:
postorder :: Bin a -> [a]
postorder (Leaf a) = [a]
postorder (Fork a b c) = postorder a ++ postorder c ++ [b]

postorderSpec :: Spec
postorderSpec =
  describe "postorder" $ do
    it "postorder (Leaf 1) == [1]" $ postorder (Leaf 1) `shouldBe` ([1] :: [Int])
    it "postorder (Fork (Leaf 'a') 'b' (Leaf 'c')) == \"acb\"" $ postorder (Fork (Leaf 'a') 'b' (Leaf 'c')) `shouldBe` "acb"
    it "postorder exTree == \"acbegfd\"" $ postorder exTree `shouldBe` "acbegfd"

-- Now we build a small calculator for rationals:
data Rat = Int :/: Int deriving (Show, Eq)

-- Supported operations:
data Op = ADD | SUB | MUL | DIV deriving (Show, Eq)

-- Type for expressions:
data Expr
  = Val Rat
  | Bin Op Expr Expr
  deriving (Show, Eq)

-- Helper function to simplify/shorten a rational:
shorten :: Rat -> Rat
shorten (n :/: d) = (n `div` f) :/: (d `div` f)
  where
    f = gcd n d

-- The following function evaluates an operation on two rationals:
-- Complete the cases for SUB, MUL and DIV
-- Hint: DIV can easy be implemented by MUL with the reciprocal
evalOp :: Op -> Rat -> Rat -> Rat
evalOp ADD (ln :/: ld) (rn :/: rd) = shorten (((ln * rd) + (rn * ld)) :/: (ld * rd))
evalOp SUB (ln :/: ld) (rn :/: rd) = shorten (((ln * rd) - (rn * ld)) :/: (ld * rd))
evalOp MUL (ln :/: ld) (rn :/: rd) = shorten ((ln * rn) :/: (ld * rd))
evalOp DIV (ln :/: ld) (rn :/: rd) = evalOp MUL (ln :/: ld) (rd :/: rn)

evalOpSpec :: Spec
evalOpSpec =
  describe "evalOp" $ do
    it "evalOp ADD (1:/:2) (1:/:4) == (3:/:4)" $ evalOp ADD (1 :/: 2) (1 :/: 4) `shouldBe` (3 :/: 4)
    it "evalOp SUB (1:/:2) (1:/:4) == (1:/:4)" $ evalOp SUB (1 :/: 2) (1 :/: 4) `shouldBe` (1 :/: 4)
    it "evalOp MUL (1:/:2) (1:/:4) == (1:/:8)" $ evalOp MUL (1 :/: 2) (1 :/: 4) `shouldBe` (1 :/: 8)
    it "evalOp DIV (1:/:2) (1:/:4) == (1:/:8)" $ evalOp DIV (1 :/: 2) (1 :/: 4) `shouldBe` (2 :/: 1)

-- Now implement the `eval` function, which evaluates an expression:
eval :: Expr -> Rat
eval (Val a) = a
eval (Bin op exp1 exp2) = evalOp op (eval exp1) (eval exp2)

-- Example expression:
-- ((1/2) + (1/4)) * ((1/6) / (2/1))
e :: Expr
e =
  Bin
    MUL
    ( Bin
        ADD
        (Val (1 :/: 2))
        (Val (1 :/: 4))
    )
    ( Bin
        DIV
        (Val (1 :/: 6))
        (Val (2 :/: 1))
    )

evalSpec :: Spec
evalSpec =
  describe "eval" $ do
    it "eval (Val (1:/:2)) == (1:/:2)" $ eval (Val (1 :/: 2)) `shouldBe` (1 :/: 2)
    it "eval e == (1:/:16)" $ eval e `shouldBe` (1 :/: 16)

-------------------------------------------------------------------------------
-- 3. Higher order functions on lists
-------------------------------------------------------------------------------

-- Data type for examples
data Paradigm = Functional | Imperative deriving (Eq, Show)

data Language = Language {name :: String, paradigm :: Paradigm} deriving (Eq, Show)

languages :: [Language]
languages =
  [ Language "Haskell" Functional,
    Language "Java" Imperative,
    Language "Lisp" Functional,
    Language "Python" Imperative
  ]

-- Implement your own version of the function `map`.
-- It applies a function to every element in a list.
myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f [a] = [f a]
myMap f (a:as) = [f a] ++ myMap f as

myMapSpec :: Spec
myMapSpec =
  describe "myMap" $ do
    it "myMap (\\x -> x + 1) [] == []" $ myMap (\x -> x + 1) [] `shouldBe` ([] :: [Int])
    it "myMap (\\x -> x + 1) [1,2,3] == [2,3,4]" $ myMap (\x -> x + 1) [1, 2, 3] `shouldBe` ([2, 3, 4] :: [Int])

-- Implement your own version of the function `filter`.
-- It keeps only the elements which satisfy the predicate.
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter f (a:as) 
  | f a = [a] ++ myFilter f as
  | otherwise = myFilter f as

myFilterSpec :: Spec
myFilterSpec =
  describe "myFilter" $ do
    it "myFilter even [] == []" $ myFilter even [] `shouldBe` ([] :: [Int])
    it "myFilter even [1,2,3,4] == [2,4]" $ myFilter even [1, 2, 3, 4] `shouldBe` ([2, 4] :: [Int])

-- Implement the function `squares`. It squares every element in a list.
-- Make use of the predefined function `map`:
squares :: [Int] -> [Int]
squares [] = []
squares (i:is) = [i * i] ++ squares is

squaresSpec :: Spec
squaresSpec =
  describe "squares" $ do
    it "squares [] == []" $ squares [] `shouldBe` ([] :: [Int])
    it "squares [1,2,3] == [1,4,9]" $ squares [1, 2, 3] `shouldBe` ([1, 4, 9] :: [Int])

-- Implement the function `names`. It extracts the names of the languages.
-- Make use of the predefined function `map`:
names :: [Language] -> [String]
names [] = [] -- QUESTION: is it necessary to delcare this? This scenario should be defined in `map` already and therefore unnecessary, right?
names l = map name l

namesSpec :: Spec
namesSpec =
  describe "names" $ do
    it "names [] == []" $ names [] `shouldBe` ([] :: [String])
    it "names languages == [\"Haskell\", \"Java\", \"Lisp\", \"Python\"]" $ names languages `shouldBe` ["Haskell", "Java", "Lisp", "Python"]

-- Implement the function `evens`. It keeps only the even values of a list.
-- Use the function `filter` and the `even` function:
evens :: [Int] -> [Int]
evens [] = []
evens is = filter even is

evensSpec :: Spec
evensSpec =
  describe "evens" $ do
    it "evens [] == []" $ evens [] `shouldBe` ([] :: [Int])
    it "evens [1,2,3,4] == [2,4]" $ evens [1, 2, 3, 4] `shouldBe` ([2, 4] :: [Int])

-- Implement the function `likes`. It keeps only the functional languages:
-- Use the function `filter` and write the predicate as a lambda expression:
likes :: [Language] -> [Language]
likes [] = []
likes ls = filter (\l -> (paradigm l) == Functional) ls -- QUESTION: I first tried `likes = filter (\l -> (paradigm l) == Functional)`, but that ended up in an error while compiling. Why?

likesSpec :: Spec
likesSpec =
  describe "likes" $ do
    it "likes [] == []" $ likes [] `shouldBe` ([] :: [Language])
    it "likes languages == [Language \"Haskell\" Functional, Language \"Lisp\" Functional]" $
      likes languages `shouldBe` [Language "Haskell" Functional, Language "Lisp" Functional]

-- Implement the function `foldrLength`. It computes the lengths of a list.
-- Use the function `foldr`:
foldrLength :: [a] -> Int
foldrLength [] = 0
foldrLength [_] = 1
foldrLength (_:is) = foldr (+) (foldrLength is) [1] -- this one was difficult

foldrLengthSpec :: Spec
foldrLengthSpec =
  describe "foldrLength" $ do
    it "behaves like length" $ property $ \(l :: [Int]) -> foldrLength l == length l

-- Implement the function `foldrMap`. It has the same behavior like `map`.
-- Use the function `foldr`:
foldrMap :: (a -> b) -> [a] -> [b] 
-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- plugin the resulting type expectation of `foldrMap` (b => [b]):
-- foldr :: (a -> [b] -> [b]) -> [b] -> [a] -> [b]
foldrMap f as = foldr (\x bs -> (f x):bs) [] as -- This was very very very difficult! I had aid from AI to solve this.



foldrMapSpec :: Spec
foldrMapSpec =
  describe "foldrMap" $ do
    it "foldrMap (+1) [] == []" $ foldrMap (+ 1) [] `shouldBe` ([] :: [Int])
    it "foldrMap (+1) [1,2,3] == [2,3,4]" $ foldrMap (+ 1) [1, 2, 3] `shouldBe` ([2, 3, 4] :: [Int])
    it "foldrMap even [1,2,3] == [2]" $ foldrMap even ([1, 2, 3] :: [Int]) `shouldBe` [False, True, False]

-------------------------------------------------------------------------------
-- 4. Combinators
-------------------------------------------------------------------------------

-- Implement the "pipe operator" `|>` which allows to combine functions from left to right:
-- The following should compile when uncommented and evaluate to 5.

res :: Int
res = (fst |> head |> length) (["hallo", "bla"], True)

-- Hints:
-- 1. First write down the type signature.
-- 2. The operator needs to be surrounded by parenthesis in the type signature:
-- Example: (<+>) :: Int -> Int -> Int

(|>) :: (a -> b) -> (b -> c) -> (a -> c)
(|>) f g = g . f

-- Implement the function `flip'`.
-- It takes a function and flips its first two arguments.
flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f a b = f b a

flip'Spec :: Spec
flip'Spec =
  describe "flip'" $ do
    it "flip' take \"abcd\" 2 == \"ab\"" $ flip' take "abcd" 2 `shouldBe` "ab"

-- Implement the function `curry'`.
-- It converts a function which takes a pair to a function which takes the arguments one after another.
curry' :: ((a, b) -> c) -> (a -> b -> c) -- QUESTION: This is kind of difficult to understand for me, how this type signature actually matches with the implementation below.
curry' f a b = f (a, b)

curry'Spec :: Spec
curry'Spec =
  describe "curry'" $ do
    it "curry' fst 'a' True == 'a'" $ curry' fst 'a' True `shouldBe` 'a'

-- Implement the function `uncurry' :: (a -> b -> c) -> ((a,b) -> c)`.
-- It is the inverse of `curry'`: `curry' . uncurry' == id`:
uncurry' :: (a -> b -> c) -> ((a, b) -> c)
uncurry' f (a, b) = f a b -- It was only possible solve this, because I was thinking ov "reversing" curry' implementation. But I think I did not understand how and why this actually works like this.

uncurry'Spec :: Spec
uncurry'Spec =
  describe "uncurry'" $ do
    it "uncurry' (&&) (True,False) == False" $ uncurry' (&&) (True, False) `shouldBe` False

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "1. Recursion on lists" $ do
    myLengthSpec
    myReverseSpec
    myDropSpec
  describe "2. Recursion on trees" $ do
    preorderSpec
    inorderSpec
    postorderSpec
    evalOpSpec
    evalSpec
  describe "3. Higher order functions on lists" $ do
    myMapSpec
    myFilterSpec
    squaresSpec
    namesSpec
    evensSpec
    likesSpec
    foldrLengthSpec
    foldrMapSpec
  describe "4. Combinators" $ do
    flip'Spec
    curry'Spec
    uncurry'Spec
