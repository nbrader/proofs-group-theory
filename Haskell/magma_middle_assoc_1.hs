#!/usr/bin/env stack
-- stack --resolver lts-20.5 ghci --package QuickCheck

import Test.QuickCheck

-- Define a data type for the set of elements
data M = A | B | C | D deriving (Eq, Show, Enum, Bounded)

-- Define the binary operation for the magma
magmaOp :: M -> M -> M
magmaOp A A = B
magmaOp A B = A
magmaOp A C = C
magmaOp A D = D
magmaOp B A = A
magmaOp B B = C
magmaOp B C = D
magmaOp B D = B
magmaOp C A = C
magmaOp C B = D
magmaOp C C = B
magmaOp C D = A
magmaOp D A = D
magmaOp D B = C
magmaOp D C = A
magmaOp D D = B

-- Predicate: All elements of the magma are valid
validM :: M -> Bool
validM _ = True  -- All elements are valid in this magma

-- Property: middle associative function
-- A middle associative function satisfies:
-- f (f a m) b = f a (f m b), when `P m` holds.
middle_assoc :: (Arbitrary a, Eq a, Show a) => (a -> Bool) -> (a -> a -> a) -> Property
middle_assoc p f = 
  forAll arbitrary $ \a -> 
    forAll arbitrary $ \m -> 
      forAll arbitrary $ \b -> 
        p m ==> (f (f a m) b == f a (f m b))

-- Property: fold_left_combine_middle_assoc
fold_left_combine_middle_assoc :: (Arbitrary a, Eq a, Show a) => (a -> Bool) -> (a -> a -> a) -> Property
fold_left_combine_middle_assoc p f =
  forAll arbitrary $ \x -> 
    forAll arbitrary $ \y -> 
      forAll arbitrary $ \xs -> 
        forAll arbitrary $ \ys -> 
          p x && p y && all p xs && all p ys ==>
          f (foldl f x xs) (foldl f y ys) == foldl f x (xs ++ [y] ++ ys)

-- Arbitrary instance for M to generate random test data
instance Arbitrary M where
  arbitrary = elements [A, B, C, D]

-- Main function to run the tests
main :: IO ()
main = do
  -- Test the magma operation with the properties
  putStrLn "Testing middle associative property for magma:"
  quickCheck (middle_assoc validM magmaOp)

  putStrLn "\nTesting fold left combine middle associative property for magma:"
  quickCheck (fold_left_combine_middle_assoc validM magmaOp)
