#!/usr/bin/env stack
-- stack --resolver lts-20.5 ghci --package QuickCheck
import Test.QuickCheck
import Data.List (nub)
import Control.Monad

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

-- Check if middle associativity holds for a specific middle element
isMiddleAssoc :: M -> Bool
isMiddleAssoc m = and [magmaOp (magmaOp a m) b == magmaOp a (magmaOp m b) | 
                      a <- [A .. D], b <- [A .. D]]

-- Check if fold-left-combine middle associativity holds for a specific middle element
isFoldLeftCombineMiddleAssoc :: M -> Bool
isFoldLeftCombineMiddleAssoc m = and [
    magmaOp (foldl magmaOp x xs) (foldl magmaOp y ys) == 
    foldl magmaOp x (xs ++ [y] ++ ys) |
    x <- [A .. D], y <- [A .. D],
    xs <- sequence [[A .. D]], -- Generate all possible lists of length 1
    ys <- sequence [[A .. D]]  -- We'll use small lists for testing
    ]

-- Check if an element is a generator
isGenerator :: M -> Bool
isGenerator m = length (generateElements m) == 4
  where
    generateElements :: M -> [M]
    generateElements x = nub $ concat $ take 4 $ iterate genStep [x]
    genStep elements = [magmaOp a b | a <- elements, b <- [A ..]]

-- Property: For any generator that satisfies middle associativity,
-- it should also satisfy fold-left-combine middle associativity
prop_middle_assoc_implies_fold :: Property
prop_middle_assoc_implies_fold = 
    forAll arbitrary $ \m ->
        isGenerator m ==>
        classify (isGenerator m) "generator" $
        (isMiddleAssoc m) ==>
        isFoldLeftCombineMiddleAssoc m

-- Arbitrary instance for M
instance Arbitrary M where
    arbitrary = elements [A, B, C, D]

-- Main function to run the tests
main :: IO ()
main = do
    putStrLn "Testing which elements are generators:"
    mapM_ (\m -> putStrLn $ show m ++ " is " ++ 
           (if isGenerator m then "" else "not ") ++ "a generator") [A .. D]
    
    putStrLn "\nTesting which generators satisfy middle associativity:"
    mapM_ (\m -> when (isGenerator m) $
           putStrLn $ show m ++ " does " ++ 
           (if isMiddleAssoc m then "" else "not ") ++ 
           "satisfy middle associativity") [A .. D]
    
    putStrLn "\nTesting if middle associativity implies fold-left-combine middle associativity for generators:"
    quickCheck prop_middle_assoc_implies_fold
