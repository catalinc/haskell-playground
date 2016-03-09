-- Haskell problems from HaskellWiki book
-- http://en.wikibooks.org/wiki/Haskell

{- 
        HASKELL BASICS 
-}

{- Functions -}

module HaskellWikiProblems where

-- 0
-- Circle area
areaCircle r = pi * r ^ 2

-- 1
-- Write a function to calculate the volume of a box. 
-- A box has width, height and depth. You have to multiply them all to get the volume.
volumeBox w h d = w * h * d

-- 2
-- Triangle area
areaTriangle b h = (b * h) / 2

-- 3 
-- Rectangle & square area
areaRect l w = l * w
-- Using rect area, square area can be written as follow
areaSquare l = areaRect l l

-- 4
-- Write a function to calculate the volume of a cylinder. 
-- The volume of a cylinder is the area of the base, which is a circle by the height.
volumeCylinder r h = h * areaCircle r

{- Lists -}

-- 5
-- lisp like cons
consThing :: [a] -> a -> [a]
consThing list thing = thing:list

-- 6
-- a 'dedicated' cons
cons8 :: [Integer] -> [Integer]
cons8 list = 8 : list

{- Simple I/O -}

-- 7
-- Compute triangle area according to user input
computeAreaTriangle = do
  putStrLn ("The height ?")
  h <- getLine -- '<-' can be seen as 'take the value out of an action'
  putStrLn ("The base ?")
  b <- getLine
  putStrLn ("The area of that triangle is " ++ (show (areaTriangle (read b) (read h))))

-- 8
-- Write a program that asks the user for his or her name. 
-- If the name is one of Simon, John or Phil, tell the user that you think Haskell is a great programming language. 
-- If the name is Koen, tell them that you think debugging Haskell is fun (Koen Classen is one of the people who works on Haskell debugging); 
-- otherwise, tell the user that you don't know who he or she is.
nameGame :: IO ()
nameGame = do
  putStrLn ("Give me a name")
  name <- getLine
  case name of 
    "Simon" -> putStrLn ("Haskell is a great language")
    "John"  -> putStrLn ("Haskell is a great language")
    "Phil"  -> putStrLn ("Haskell is a great language")
    "Koen"  -> putStrLn ("Debugging Haskell is fun :)")
    _ -> putStrLn ("I don't know shit about " ++ name)


{- Type declarations -}

type Name = String -- type alias
data Date = Date Int Int Int -- DD/MM/YYYY
type AnniversaryBook = [Anniversary]

showDate :: Date -> String
showDate (Date d m y) = 
    (show d) ++ "/" ++ (show m) ++ "/" ++ (show y) 

data Anniversary = -- Birthday and Wedding = type constructors
    Birthday Name Date
    | Wedding Name Name Date

-- a function operating on Anniversary type
showAnniversary :: Anniversary -> String

showAnniversary (Birthday name date) =  -- pattern matching according to type constructor
    name ++ " was born on " ++ showDate date

showAnniversary (Wedding hisName herName date) =
    hisName ++ " married " ++ herName ++ " on " ++ showDate date

{-
      ELEMENTARY HASKELL
-}

{- Recursion -}

-- 9
-- Define a recursive function power such that power x y raises x to the y power 
power :: Integer -> Integer -> Integer
power x 0 = 1
power x y = x * (power x (y -1))

-- 10
-- You are given a function plusOne x = x + 1. Without using any other (+)s, define a recursive 
-- function addition such that addition x y adds x and y together.
plusOne :: Integer -> Integer
plusOne x = x + 1

addition :: Integer -> Integer -> Integer
addition x 0 = x
addition x y = addition (plusOne x) (y - 1)

-- 11
-- replicate :: Int -> a -> [a], which takes an element and a count and returns the list which is that element repeated that many times. 
-- E.g. replicate 3 'a' = "aaa". 
-- (Hint: think about what replicate of anything with a count of 0 should be; a count of 0 is your 'base case'.)
clone :: Int -> a -> [a]
clone 0 e = []
clone n e = e : (clone (n - 1) e)

-- 12
-- atIndex :: [a] -> Int -> a, which returns the element at the given 'index'. 
-- The first element is at index 0, the second at index 1, and so on. 
-- Note that with this function, you're recurring both numerically and down a list.
atIndex :: [a] -> Int -> a
atIndex (x:xs) 0 =  x
atIndex (x:xs) i = atIndex xs (i-1)

-- 13
-- (A bit harder.) zip :: [a] -> [b] -> [(a, b)], which takes two lists and 'zips' them together, 
-- so that the first pair in the resulting list is the first two elements of the two lists, and so on. 
-- E.g. zip [1,2,3] "abc" = [(1, 'a'), (2, 'b'), (3, 'c')]. 
-- If either of the lists is shorter than the other, you can stop once either list runs out. 
-- E.g. zip [1,2] "abc" = [(1, 'a'), (2, 'b')]
myzip :: [a] -> [b] -> [(a, b)]
myzip _ [] = []
myzip [] _ = []
myzip (x:xs) (y:ys) = (x,y) : (myzip xs ys)

{- Pattern matching -}

-- NOTE: pattern matching is a way of assigning names to things (or binding those names to those things), 
-- and possibly breaking down expressions into subexpressions at the same time
-- the only functions allowed in patterns are type constructors

data Foo = Bar | Baz Int
-- we can write something like
f :: Foo -> Int
f Bar = 1 -- pattern matching on type constructors
f (Baz x) = x - 1

data Foo2 = Bar2 | Baz2 {barNumber::Int, barName::String}
-- if only the relevant info is constructor type we can use {} pattern
g :: Foo2 -> Bool
g Bar2 {} = True
g Baz2 {} = False
-- or it helps to use records for a constructor with many elements
h :: Foo2 -> Int
h Bar2 = 0
h Baz2 {barName=name} = length name

{- Lists -}

multiplyList :: (Num a) => a -> [a] -> [a]
multiplyList _ [] = []
multiplyList m (n:ns) = (m*n) : multiplyList m ns

doubleList = multiplyList 2 -- partial application

-- 14
-- takeInt returns the first n items in a list. So takeInt 4 [11,21,31,41,51,61] returns [11,21,31,41]
takeInt :: Int -> [Int] -> [Int]
takeInt 0 _ = []
takeInt _ [] = []
takeInt n (x:xs) = x : takeInt (n-1) xs 

-- 15
-- dropInt drops the first n items in a list and returns the rest. so dropInt 3 [11,21,31,41,51] returns [41,51].
dropInt :: Int -> [Int] -> [Int]
dropInt _ [] = []
dropInt 0 l = l
dropInt n (x:xs) = dropInt (n-1) xs

-- 16
-- sumInt returns the sum of the items in a list.
sumInt :: [Int] -> Int
sumInt [] = 0
sumInt (x:xs) = x + sumInt (xs)

-- 17
-- scanSum adds the items in a list and returns a list of the running totals. So scanSum [2,3,4,5] returns [2,5,9,14]. 
scanSum :: [Int] -> [Int]
scanSum [] = []
scanSum (x:y:ys) = x : scanSum ((x + y) : ys)
scanSum (x:y:[]) = x : (x + y) : []
scanSum (x:[]) = x:[]

-- 18
-- diffs returns a list of the differences between adjacent items. So diffs [3,5,6,8] returns [2,1,2]. 
diffs :: [Int] -> [Int]
diffs [] = []
diffs (x:y:ys) = (y - x) : diffs (y:ys)
diffs (x:y:[]) = (y - x) : []
diffs (x:[]) = [] 

-- diff between two lists
diffL :: [Int] -> [Int] -> [Int]
diffL _ [] = []
diffL [] _ = []
diffL (x:xs) (y:ys) =  (y - x) : (diffL xs ys)

-- 19
-- Write a function which takes a list and a number and returns the given element; use head or tail, and not !!.
nth :: [a] -> Int -> a
nth l 0 = head l
nth l n = nth (tail l ) (n - 1)

-- pattern maching in list comprehension
-- example: list of tuples [(Integer, Integer)]. what we would like to do is return the first element of every tuple whose second element is even. 
isEven y 
    | ((mod y 2) == 0) = True
    | otherwise = False
firstOfEvens xys = [ x | (x,y) <- xys, isEven y ]

{- Control structures -}

-- IF
-- 'if' is an expression (returning a value) rather than a statement (to be executed) in Haskell
-- 'else' branch always required
message42 :: Integer -> String
message42 n =
   if n == 42
      then "The Answer is forty two."
      else "The Answer is not forty two."

-- CASE
-- 'case' expressions are a generalization of if expressions
data Colour = Black | White | RGB Int Int Int
describeColour c = 
   "This colour is "
   ++ (case c of
          Black -> "black"
          White -> "white"
          RGB _ _ _ -> "freaky, man, sort of in between")
   ++ ", yeah?"

-- GUARDS
-- guards are simple boolean conditions
describeLetter :: Char -> String
describeLetter c
   | c >= 'a' && c <= 'z' = "Lower case"
   | c >= 'A' && c <= 'Z' = "Upper case"
   | otherwise            = "Not a letter"

-- It's worth noting that there is a fundamental difference between if-expressions and case-expressions. 
-- if-expressions, and guards, only check to see if a boolean expression evaluated to True. 
-- case-expressions, and multiple equations for the same function, pattern match against the input.

{- List processing -}

-- 20
-- Use map to build functions that, given a list l of Ints, returns:

--     * A list that is the element-wise negation of l.
--     * A list of lists of Ints ll that, for each element of l, contains the factors of l. It will help to know that

factors p = [ f | f <- [1..p], p `mod` f == 0 ]

--     * The element-wise negation of ll.

negX :: Int -> Int
negX x = -x -- not really bitwise negation ...

negList :: [Int] -> [Int]
negList = map negX

factorsList :: [Int] -> [[Int]]
factorsList = map factors

negFactorsList :: [[Int]] -> [[Int]]
negFactorsList = map negList

{- folds -}

addStr :: String -> Float -> Float
addStr str x = read str + x

sumStr :: [String] -> Float
sumStr = foldr addStr 0.0

echoes = foldr (\x xs -> (replicate x x) ++ xs) []

-- 21
-- Define and :: [Bool] -> Bool, which returns True if a list of Bools are all True, and False otherwise.
andL :: [Bool] -> Bool
andL = foldl1 (&&) -- or foldl (&&) True

-- 22
-- Define or :: [Bool] -> Bool, which returns True if any of a list of Bools are True, and False otherwise.
orL :: [Bool] -> Bool
orL = foldl1 (||) -- or foldl (||) False

-- 23
-- maximum :: Ord a => [a] -> a, which returns the maximum element of a list 
-- (hint: max :: Ord a => a -> a -> a returns the maximum of two values).
maxL :: Ord a => [a] -> a
maxL = foldl1 max

-- 24
-- minimum :: Ord a => [a] -> a, which returns the minimum element of a list 
-- (hint: min :: Ord a => a -> a -> a returns the minimum of two values).
minL :: Ord a => [a] -> a
minL = foldl1 min

{- scans -}

-- 25
-- factList :: Int -> [Int], which returns a list of factorials from 1 up to Int.
factList :: Int -> [Int]
factList n = scanl1 (*) [1..n]

{-
      INTERMEDIATE HASKELL
-}

{- More on datatypes -}

{- Enumerations -}

-- Simply a data type where none of the constructor functions have any arguments

data Month = January | February | March | April | May | June | July
             | August | September | October | November | December

{- Named Fields (Record Syntax) -}

data Configuration =
    Configuration { username      :: String,
                    localhost     :: String,
                    remotehost    :: String,
                    isguest       :: Bool,
                    issuperuser   :: Bool,
                    currentdir    :: String,
                    homedir       :: String,
                    timeconnected :: Integer
                  }

-- username :: Configuration -> String
-- localhost :: Configuration -> String
-- ...

-- in general, to update the field x in a datatype y to z, you write y{x=z}
-- eg:
-- changeDir :: Configuration -> String -> Configuration
-- changeDir cfg newDir =
--     -- make sure the directory exists
--     if directoryExists newDir
--       then -- change our current directory
--            cfg{currentdir = newDir} -- (!)
--       else error "directory does not exist"

-- postWorkingDir :: Configuration -> String
--   -- retrieve our current directory
-- postWorkingDir cfg = currentdir cfg

{- Parameterised Types (a.k.a templates) -}

-- eitherExample :: Int -> Either Int String
-- eitherExample a | even a = Left (a/2)
--                 | a `mod` 3 == 0 = Right "three"
--                 | otherwise = Right "neither two or three"

-- otherFunction :: Int -> String
-- otherFunction a = case eitherExample a of
--   Left c -> "Even: " ++ show a ++ " = 2*" ++ show c ++ "."
--   Right s -> show a ++ " is divisible by " ++ s ++ "."

{- Trees -}

data Tree a = Leaf a | Branch a (Tree a) (Tree a)

-- tree as a list

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap f (Leaf x) = Leaf (f x)
treeMap f (Branch x firstSub secondSub) = Branch (f x) (treeMap f firstSub) (treeMap f secondSub)

treeFold :: (a -> b -> b -> b) -> b -> Tree a -> b
treeFold f z (Leaf x) = f x z z
treeFold f z (Branch x firstSub secondSub) = f x (treeFold f z firstSub) (treeFold f z secondSub) 

{- Class declarations -}

-- type Foo is an instance of class Eq 
-- in Haskell classes are merely interfaces: define a set of operations on a type and optionally provide an implementation
-- a class is a template for types

data FooXXX = FooXXX {x :: Integer, str :: String}
   deriving (Eq, Ord, Show)

{- Classes and Types -}

-- limits on type signatures
fooTypeLimits :: (Num a, Show a, Show b) => a -> a -> b -> String -- as must be numbers and printables, b must be only printable
fooTypeLimits x y t = 
   show x ++ " plus " ++ show y ++ " is " ++ show (x+y) ++ ".  " ++ show t

-- type contraints on record declaration
data (Num a) => FooX a = F1 a | F2 Integer
          
main = do
  putStrLn (show (areaCircle 10.0))
  putStrLn (show (volumeBox 10.0 10.0 10.0))
  putStrLn (show (areaTriangle 10.0 20.0))
  putStrLn (show (areaRect 10.0 10.0))
  putStrLn (show (areaSquare 10.0))
  putStrLn (show (volumeCylinder 10.0 10.0))
  putStrLn (show (cons8 [1,2,3]))
  putStrLn (show (cons8 []))
  putStrLn (show (consThing ["hello"] "thing"))
