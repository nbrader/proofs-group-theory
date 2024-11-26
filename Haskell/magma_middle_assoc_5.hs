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
isFoldLeftCombineMiddleAssoc :: MagmaElement a => a -> Bool
isFoldLeftCombineMiddleAssoc m = and [
    let result1 = foldMagma x xs
        result2 = foldMagma y ys
        combined1 = op result1 result2
        combined2 = foldMagma x (xs ++ [m] ++ ys)
    in combined1 == combined2 |
    x <- carrier,
    y <- carrier,
    xs <- concat [sequence (replicate i carrier) | i <- [0..4]],
    ys <- concat [sequence (replicate i carrier) | i <- [0..4]]
    ]

-- Check if fold-left-combine middle associativity holds
isFoldLeftCombineMiddleAssocM1 = and [
    (combined1 == combined2) |
    x <- carrier :: [M1],
    y <- carrier,
    xs <- concat [sequence (replicate i carrier) | i <- [0..2]],
    ys <- concat [sequence (replicate i carrier) | i <- [0..2]],
    let result1 = foldMagma x xs,
    let result2 = foldMagma y ys,
    let combined1 = op result1 result2,
    let combined2 = foldMagma x (xs ++ [y] ++ ys),
    combined1 /= combined2
    ]

-- Check if fold-left-combine middle associativity holds
isFoldLeftCombineMiddleAssocM2 = and [
    (combined1 == combined2) |
    x <- carrier :: [M2],
    y <- carrier,
    xs <- concat [sequence (replicate i carrier) | i <- [0..2]],
    ys <- concat [sequence (replicate i carrier) | i <- [0..2]],
    let result1 = foldMagma x xs,
    let result2 = foldMagma y ys,
    let combined1 = op result1 result2,
    let combined2 = foldMagma x (xs ++ [y] ++ ys),
    combined1 /= combined2
    ]

-- Check if fold-left-combine middle associativity holds
isFoldLeftCombineMiddleAssocM3 = and [
    (combined1 == combined2) |
    x <- carrier :: [M3],
    y <- carrier,
    xs <- concat [sequence (replicate i carrier) | i <- [0..2]],
    ys <- concat [sequence (replicate i carrier) | i <- [0..2]],
    let result1 = foldMagma x xs,
    let result2 = foldMagma y ys,
    let combined1 = op result1 result2,
    let combined2 = foldMagma x (xs ++ [y] ++ ys),
    combined1 /= combined2
    ]

-- Check if fold-left-combine middle associativity holds
isFoldLeftCombineMiddleAssocM4 = and [
    (combined1 == combined2) |
    x <- carrier :: [M4],
    y <- carrier,
    xs <- concat [sequence (replicate i carrier) | i <- [0..2]],
    ys <- concat [sequence (replicate i carrier) | i <- [0..2]],
    let result1 = foldMagma x xs,
    let result2 = foldMagma y ys,
    let combined1 = op result1 result2,
    let combined2 = foldMagma x (xs ++ [y] ++ ys),
    combined1 /= combined2
    ]

-- Check if an element is a generator
--
-- NOTE:
-- 	I previosuly wasn't considering the possible generating sets but instead only declaring an element
--	as a generator when it was a generator in a generating set by itself.
--
isGeneratorByItselfM1 :: M1 -> Bool
isGeneratorByItselfM1 m = length (generateElements m) == length (carrier :: [M1])
  where
    generateElements x = nub $ concat $ take 4 $ iterate genStep [x]
    genStep elems = nub $ elems ++ [op a b | a <- elems, b <- elems]

isGeneratorByItselfM2 :: M2 -> Bool
isGeneratorByItselfM2 m = length (generateElements m) == length (carrier :: [M2])
  where
    generateElements x = nub $ concat $ take 4 $ iterate genStep [x]
    genStep elems = nub $ elems ++ [op a b | a <- elems, b <- elems]

isGeneratorByItselfM3 :: M3 -> Bool
isGeneratorByItselfM3 m = length (generateElements m) == length (carrier :: [M3])
  where
    generateElements x = nub $ concat $ take 4 $ iterate genStep [x]
    genStep elems = nub $ elems ++ [op a b | a <- elems, b <- elems]

isGeneratorByItselfM4 :: M4 -> Bool
isGeneratorByItselfM4 m = length (generateElements m) == length (carrier :: [M4])
  where
    generateElements x = nub $ concat $ take 4 $ iterate genStep [x]
    genStep elems = nub $ elems ++ [op a b | a <- elems, b <- elems]

-- Properties for both magmas
class (MagmaElement a, Arbitrary a) => TestMagma a where
    testMagma :: String -> a -> IO ()

instance TestMagma M1 where
    testMagma name _ = testMagmaGenericM1 name (carrier :: [M1])

instance TestMagma M2 where
    testMagma name _ = testMagmaGenericM2 name (carrier :: [M2])

instance TestMagma M3 where
    testMagma name _ = testMagmaGenericM3 name (carrier :: [M3])

instance TestMagma M4 where
    testMagma name _ = testMagmaGenericM4 name (carrier :: [M4])

testMagmaGenericM1 :: String -> [M1] -> IO ()
testMagmaGenericM1 name elems = do
    putStrLn $ "\nTesting " ++ name ++ ":"
    putStrLn "Testing which elements are generators by themselves:"
    mapM_ (\m -> putStrLn $ show m ++ " is " ++ 
           (if isGeneratorByItselfM1 m then "" else "not ") ++ "a generator by itself") elems
    
    putStrLn "\nTesting which elements satisfy middle associativity:"
    mapM_ (\m -> putStrLn $ show m ++
           (if isMiddleAssoc m then " satisfies " else " does not satisfy ") ++ 
           "middle associativity") elems
    
    putStrLn "\nTesting which elements satisfy fold-left-combine middle associativity:"
    putStrLn $ (if isFoldLeftCombineMiddleAssocM1 then "Satisfies " else "Does not satisfy ") ++ "fold-left-combine middle associativity"

testMagmaGenericM2 :: String -> [M2] -> IO ()
testMagmaGenericM2 name elems = do
    putStrLn $ "\nTesting " ++ name ++ ":"
    putStrLn "Testing which elements are generators:"
    mapM_ (\m -> putStrLn $ show m ++ " is " ++ 
           (if isGeneratorByItselfM2 m then "" else "not ") ++ "a generator by itself") elems
    
    putStrLn "\nTesting which elements satisfy middle associativity:"
    mapM_ (\m -> putStrLn $ show m ++
           (if isMiddleAssoc m then " satisfies " else " does not satisfy ") ++ 
           "middle associativity") elems
    
    putStrLn "\nTesting which elements satisfy fold-left-combine middle associativity:"
    putStrLn $ (if isFoldLeftCombineMiddleAssocM2 then "Satisfies " else "Does not satisfy ") ++ "fold-left-combine middle associativity"

testMagmaGenericM3 :: String -> [M3] -> IO ()
testMagmaGenericM3 name elems = do
    putStrLn $ "\nTesting " ++ name ++ ":"
    putStrLn "Testing which elements are generators:"
    mapM_ (\m -> putStrLn $ show m ++ " is " ++ 
           (if isGeneratorByItselfM3 m then "" else "not ") ++ "a generator by itself") elems
    
    putStrLn "\nTesting which elements satisfy middle associativity:"
    mapM_ (\m -> putStrLn $ show m ++
           (if isMiddleAssoc m then " satisfies " else " does not satisfy ") ++ 
           "middle associativity") elems
    
    putStrLn "\nTesting which elements satisfy fold-left-combine middle associativity:"
    putStrLn $ (if isFoldLeftCombineMiddleAssocM3 then "Satisfies " else "Does not satisfy ") ++ "fold-left-combine middle associativity"

testMagmaGenericM4 :: String -> [M4] -> IO ()
testMagmaGenericM4 name elems = do
    putStrLn $ "\nTesting " ++ name ++ ":"
    putStrLn "Testing which elements are generators:"
    mapM_ (\m -> putStrLn $ show m ++ " is " ++ 
           (if isGeneratorByItselfM4 m then "" else "not ") ++ "a generator by itself") elems
    
    putStrLn "\nTesting which elements satisfy middle associativity:"
    mapM_ (\m -> putStrLn $ show m ++
           (if isMiddleAssoc m then " satisfies " else " does not satisfy ") ++ 
           "middle associativity") elems
    
    putStrLn "\nTesting which elements satisfy fold-left-combine middle associativity:"
    putStrLn $ (if isFoldLeftCombineMiddleAssocM4 then "Satisfies " else "Does not satisfy ") ++ "fold-left-combine middle associativity"

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