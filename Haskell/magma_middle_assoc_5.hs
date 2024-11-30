#!/usr/bin/env stack
-- stack --resolver lts-20.5 ghci --package QuickCheck

import Test.QuickCheck
import Data.List (nub, null)
import Control.Monad

-- Define data types for both magmas
data M1 = A | B | C | D deriving (Eq, Show, Enum, Bounded)
data M2 = P | Q | R | S deriving (Eq, Show, Enum, Bounded)
data M3 = W | X | Y | Z deriving (Eq, Show, Enum, Bounded)

-- I'm making this carrier type finite for testing purposes but it's a place-holder for
-- an infinite type (otherwise M4 would not be closed as is required to be a magma.
data C4 = I | J | K deriving (Eq, Show, Enum, Bounded) 
newtype M4 = M4 [C4] deriving (Eq, Show)

-- Define a data type for the different Magma options
data MagmaType = FirstMagma | SecondMagma | ThirdMagma | FreeMonoid
    deriving (Eq, Show, Enum, Bounded)

-- Pretty names for each MagmaType
prettyMagmaName :: MagmaType -> String
prettyMagmaName FirstMagma  = "First Magma (Non-associative)"
prettyMagmaName SecondMagma = "Second Magma (Klein four-group)"
prettyMagmaName ThirdMagma  = "Third Magma"
prettyMagmaName FreeMonoid  = "Append Magma"

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

instance MagmaElement M4 where
    carrier = map M4 $ [] : map (:[]) [I .. K]
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
isFoldLeftCombineMiddleAssocM1, isFoldLeftCombineMiddleAssocM2,
                                isFoldLeftCombineMiddleAssocM3,
                                isFoldLeftCombineMiddleAssocM4 :: Bool

isFoldLeftCombineMiddleAssocM1 = isFoldLeftCombineMiddleAssoc (carrier :: [M1])
isFoldLeftCombineMiddleAssocM2 = isFoldLeftCombineMiddleAssoc (carrier :: [M2])
isFoldLeftCombineMiddleAssocM3 = isFoldLeftCombineMiddleAssoc (carrier :: [M3])
isFoldLeftCombineMiddleAssocM4 = isFoldLeftCombineMiddleAssoc (carrier :: [M4])

-- Check if an element is a generator
--
-- NOTE:
-- 	I previosuly wasn't considering the possible generating sets but instead only declaring an element
--	as a generator when it was a generator in a generating set by itself.
--

-- Generic generator function works for all magma types
isGeneratorByItself :: (MagmaElement a) => a -> Bool
isGeneratorByItself g = all (`elem` generateElements g) (specificCarrier g)
  where
    specificCarrier :: MagmaElement a => a -> [a]
    specificCarrier _ = carrier

    generateElements x = nub $ concat $ take (length (specificCarrier x)) $ iterate genStep [x]
    genStep elems = nub $ elems ++ [op a b | a <- elems, b <- elems]

-- Generic generator function works for all magma types
isGeneratingSet :: (MagmaElement a) => [a] -> Bool
isGeneratingSet gs@(g:_) = all (`elem` generateElements gs) (specificCarrier g)
  where
    specificCarrier :: MagmaElement a => a -> [a]
    specificCarrier _ = carrier

    generateElements gs = nub $ concat $ take (length (specificCarrier g)) $ iterate genStep gs
    genStep elems = nub $ elems ++ [op a b | a <- elems, b <- elems]

subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x:xs) = map (x:) subs ++ subs
  where subs = subsets xs

-- Generalized testing function
testMagmaGeneric :: (MagmaElement a) => String -> [a] -> IO ()
testMagmaGeneric name elems = do
    putStrLn $ "\nTesting " ++ name ++ ":"
    putStrLn "Testing which elements are generators by themselves:"
    mapM_ (\m -> putStrLn $ show m ++ " is " ++ 
           (if isGeneratorByItself m then "" else "not ") ++ "a generator by itself") elems
    
    putStrLn $ "\nTesting " ++ name ++ ":"
    putStrLn "Testing which subsets are generating sets:"
    mapM_ (\subset -> putStrLn $ show subset ++ " is " ++ 
           (if isGeneratingSet subset then "" else "not ") ++ "a generating set") (filter (not . null) $ subsets elems)
    
    putStrLn "\nTesting which elements satisfy middle associativity:"
    mapM_ (\m -> putStrLn $ show m ++
           (if isMiddleAssoc m then " satisfies " else " does not satisfy ") ++ 
           "middle associativity") elems
    
    putStrLn "\nTesting if elements satisfy fold-left-combine middle associativity:"
    putStrLn $ (\b -> if b then "Satisfies " else "Does not satisfy ") (case name of
        "First Magma (Non-associative)" -> isFoldLeftCombineMiddleAssocM1
        "Second Magma (Klein four-group)" -> isFoldLeftCombineMiddleAssocM2
        "Third Magma" -> isFoldLeftCombineMiddleAssocM3
        "Free Monoid" -> isFoldLeftCombineMiddleAssocM4
        _ -> False) ++ "fold-left-combine middle associativity"


-- Function to test a specific MagmaType
testMagma :: MagmaType -> IO ()
testMagma magmaType = case magmaType of
    FirstMagma  -> testMagmaGeneric (prettyMagmaName FirstMagma) (carrier :: [M1])
    SecondMagma -> testMagmaGeneric (prettyMagmaName SecondMagma) (carrier :: [M2])
    ThirdMagma  -> testMagmaGeneric (prettyMagmaName ThirdMagma) (carrier :: [M3])
    FreeMonoid  -> testMagmaGeneric (prettyMagmaName FreeMonoid) (carrier :: [M4])

-- Arbitrary instances
instance Arbitrary M1 where arbitrary = elements carrier
instance Arbitrary M2 where arbitrary = elements carrier
instance Arbitrary M3 where arbitrary = elements carrier
instance Arbitrary M4 where arbitrary = elements carrier

-- Function to generate a grid for a magma operation
printMagmaGrid :: (Show a, MagmaElement a) => String -> [a] -> IO ()
printMagmaGrid name elems = do
    putStrLn $ "\n" ++ name ++ " Operation Table:"
    let elemCombinations = elems ++ [row `op` col | row <- elems, col <- elems]
        maxElemLength = maximum (map (length . show) elemCombinations)
        padding = ((maxElemLength `div` 4) + 1) * 4 -- Adjust padding to be a multiple of 4
        padElement = padRight padding
    -- Print the header
    putStr $ replicate padding ' '
    mapM_ (putStr . padElement . show) elems
    putStrLn ""
    -- Print each row
    forM_ elems $ \row -> do
        putStr (padElement (show row))
        mapM_ (\col -> putStr (padElement (show $ op row col))) elems
        putStrLn ""

-- Utility function to pad a string to a fixed width
padRight :: Int -> String -> String
padRight n s = s ++ replicate (n - length s) ' '

-- Print grids for each magma type
printAllMagmaGrids :: MagmaType -> IO ()
printAllMagmaGrids magmaType = case magmaType of
    FirstMagma  -> printMagmaGrid (prettyMagmaName FirstMagma) (carrier :: [M1])
    SecondMagma -> printMagmaGrid (prettyMagmaName SecondMagma) (carrier :: [M2])
    ThirdMagma  -> printMagmaGrid (prettyMagmaName ThirdMagma) (carrier :: [M3])
    FreeMonoid  -> printMagmaGrid (prettyMagmaName FreeMonoid) (carrier :: [M4])


-- Main function to iterate over all magma types
main :: IO ()
main = do
    mapM_ printAllMagmaGrids [minBound .. maxBound :: MagmaType]
    mapM_ testMagma [minBound .. maxBound :: MagmaType]
