#!/usr/bin/env stack
-- stack --resolver lts-20.5 ghci --package QuickCheck

import Test.QuickCheck
import Data.List (nub)
import Control.Monad

-- Define data types for both magmas
data M1 = A | B | C | D deriving (Eq, Show, Enum, Bounded)
data M2 = P | Q | R | S deriving (Eq, Show, Enum, Bounded)
data M3 = W | X | Y | Z deriving (Eq, Show, Enum, Bounded)
newtype M4 = M4 {fromM4 :: [M3]} deriving (Eq, Show)

-- First magma operation (non-associative)
magmaOp1 :: M1 -> M1 -> M1
magmaOp1 A A = B
magmaOp1 A B = A
magmaOp1 A C = C
magmaOp1 A D = D
magmaOp1 B A = A
magmaOp1 B B = C
magmaOp1 B C = D
magmaOp1 B D = B
magmaOp1 C A = C
magmaOp1 C B = D
magmaOp1 C C = B
magmaOp1 C D = A
magmaOp1 D A = D
magmaOp1 D B = C
magmaOp1 D C = A
magmaOp1 D D = B

-- Second magma operation (associative - Klein four-group)
magmaOp2 :: M2 -> M2 -> M2
magmaOp2 P P = P
magmaOp2 P Q = Q
magmaOp2 P R = R
magmaOp2 P S = S
magmaOp2 Q P = Q
magmaOp2 Q Q = P
magmaOp2 Q R = S
magmaOp2 Q S = R
magmaOp2 R P = R
magmaOp2 R Q = S
magmaOp2 R R = P
magmaOp2 R S = Q
magmaOp2 S P = S
magmaOp2 S Q = R
magmaOp2 S R = Q
magmaOp2 S S = P

-- Third magma operation (associative - Klein four-group)
magmaOp3 :: M3 -> M3 -> M3
magmaOp3 W W = W
magmaOp3 W X = X
magmaOp3 W Y = Y
magmaOp3 W Z = Z
magmaOp3 X W = X
magmaOp3 X X = Y
magmaOp3 X Y = Z
magmaOp3 X Z = W
magmaOp3 Y W = Y
magmaOp3 Y X = Z
magmaOp3 Y Y = W
magmaOp3 Y Z = X
magmaOp3 Z W = Z
magmaOp3 Z X = W
magmaOp3 Z Y = X
magmaOp3 Z Z = Y

-- Third magma operation (associative - Klein four-group)
magmaOp4 :: M4 -> M4 -> M4
magmaOp4 (M4 x) (M4 y) = M4 (x ++ y) 

-- Generic functions that work with either magma
class (Eq a, Show a) => MagmaElement a where
    carrier :: [a]
    op :: a -> a -> a

instance MagmaElement M1 where
    carrier = [A .. D]
    op = magmaOp1

instance MagmaElement M2 where
    carrier = [P .. S]
    op = magmaOp2

instance MagmaElement M3 where
    carrier = [W .. Z]
    op = magmaOp3

instance MagmaElement (M4) where
    carrier = map (M4 . (:[])) [W .. Z]
    op = magmaOp4

-- Check if middle associativity holds for a specific middle element
isMiddleAssoc :: MagmaElement a => a -> Bool
isMiddleAssoc m = and [op (op a m) b == op a (op m b) | 
                      a <- carrier, b <- carrier]

-- Helper function for folding with the magma operation
foldMagma :: MagmaElement a => a -> [a] -> a
foldMagma = foldl op

-- Check if fold-left-combine middle associativity holds
isFoldLeftCombineMiddleAssoc :: forall a. MagmaElement a => [a] -> Bool
isFoldLeftCombineMiddleAssoc elems = and [
    (combined1 == combined2) |
    x <- elems,
    y <- elems,
    xs <- concat [sequence (replicate i elems) | i <- [0..2]],
    ys <- concat [sequence (replicate i elems) | i <- [0..2]],
    let result1 = foldMagma x xs,
    let result2 = foldMagma y ys,
    let combined1 = op result1 result2,
    let combined2 = foldMagma x (xs ++ [y] ++ ys),
    combined1 /= combined2
    ]

-- Replace specific functions with instances of the generic function
isFoldLeftCombineMiddleAssocM1 :: Bool
isFoldLeftCombineMiddleAssocM1 = isFoldLeftCombineMiddleAssoc (carrier :: [M1])

isFoldLeftCombineMiddleAssocM2 :: Bool
isFoldLeftCombineMiddleAssocM2 = isFoldLeftCombineMiddleAssoc (carrier :: [M2])

isFoldLeftCombineMiddleAssocM3 :: Bool
isFoldLeftCombineMiddleAssocM3 = isFoldLeftCombineMiddleAssoc (carrier :: [M3])

isFoldLeftCombineMiddleAssocM4 :: Bool
isFoldLeftCombineMiddleAssocM4 = isFoldLeftCombineMiddleAssoc (carrier :: [M4])

-- Check if an element is a generator
--
-- NOTE:
-- 	I previosuly wasn't considering the possible generating sets but instead only declaring an element
--	as a generator when it was a generator in a generating set by itself.
--

-- Generic generator function works for all magma types
isGeneratorByItself :: (MagmaElement a) => a -> Bool
isGeneratorByItself m = length (generateElements m) == length (specificCarrier m)
  where
    specificCarrier :: MagmaElement a => a -> [a]
    specificCarrier _ = carrier

    generateElements x = nub $ concat $ take (length (specificCarrier x)) $ iterate genStep [x]
    genStep elems = nub $ elems ++ [op a b | a <- elems, b <- elems]

-- Properties for both magmas
class (MagmaElement a, Arbitrary a) => TestMagma a where
    testMagma :: String -> a -> IO ()

instance TestMagma M1 where
    testMagma name _ = testMagmaGeneric name (carrier :: [M1])

instance TestMagma M2 where
    testMagma name _ = testMagmaGeneric name (carrier :: [M2])

instance TestMagma M3 where
    testMagma name _ = testMagmaGeneric name (carrier :: [M3])

instance TestMagma M4 where
    testMagma name _ = testMagmaGeneric name (carrier :: [M4])

-- Generalized testing function
testMagmaGeneric :: (MagmaElement a) => String -> [a] -> IO ()
testMagmaGeneric name elems = do
    putStrLn $ "\nTesting " ++ name ++ ":"
    putStrLn "Testing which elements are generators:"
    mapM_ (\m -> putStrLn $ show m ++ " is " ++ 
           (if isGeneratorByItself m then "" else "not ") ++ "a generator by itself") elems
    
    putStrLn "\nTesting which elements satisfy middle associativity:"
    mapM_ (\m -> putStrLn $ show m ++
           (if isMiddleAssoc m then " satisfies " else " does not satisfy ") ++ 
           "middle associativity") elems
    
    putStrLn "\nTesting which elements satisfy fold-left-combine middle associativity:"
    putStrLn $ (\b -> if b then "Satisfies " else "Does not satisfy ") (case name of
        "First Magma (Non-associative)" -> isFoldLeftCombineMiddleAssocM1
        "Second Magma (Klein four-group)" -> isFoldLeftCombineMiddleAssocM2
        "Third Magma" -> isFoldLeftCombineMiddleAssocM3
        "Free Monoid" -> isFoldLeftCombineMiddleAssocM4
        _ -> False) ++ "fold-left-combine middle associativity"

-- Arbitrary instances
instance Arbitrary M1 where
    arbitrary = elements carrier

instance Arbitrary M2 where
    arbitrary = elements carrier

instance Arbitrary M3 where
    arbitrary = elements carrier

instance Arbitrary M4 where
    arbitrary = elements carrier

-- Main function to test both magmas
main :: IO ()
main = do
    testMagma "First Magma (Non-associative)" (undefined :: M1)
    testMagma "Second Magma (Klein four-group)" (undefined :: M2)
    testMagma "Third Magma" (undefined :: M3)
    testMagma "Free Monoid" (undefined :: M4)