module Main where

import Char
import IO

-- 1
-- Use map to convert a string into a list of booleans, each element in the new list 
-- representing whether or not the original element was a lower-case character. 
-- That is, it should take the string "aBCde" and return [True,False,False,True,True].

lowerToBool :: String -> [Bool]
lowerToBool = map Char.isLower 

-- 2
-- Use the functions mentioned in this section  (you will need two of them) 
-- to compute the number of lower-case letters in a string. 
-- For instance, on "aBCde" it should return 3.

lowerToInt :: Char -> Int
lowerToInt c 
           | Char.isLower c = 1
           | otherwise = 0

countLowerChars :: String -> Int
countLowerChars = foldr (+) 0 . map lowerToInt

-- 3
-- Given that the function max returns the maximum of two numbers, write a function 
-- using a fold that will return the maximum value in a list (and zero if the list is empty). 
-- So, when applied to [5,10,2,8,1] it will return 10.  
-- Assume that the values in the list are always >= 0. 

maxOfList :: [Int] -> Int
maxOfList = foldr (max) 0

-- 4
-- Write a function that takes a list of pairs of length at least 2 and returns 
-- the first component of the second element in the list. 
-- So, when provided with [(5,'b'),(1,'c'),(6,'a')], it will return 1.

firstOfSecond :: [(a,b)] -> a
firstOfSecond (x:y:ys) = fst y 

-- 5
-- Write a program that will repeatedly ask the user for numbers 
-- until she types in zero, at which point it will tell her the sum of all the numbers, 
-- the product of all the numbers, and, for each number, its factorial.
askForNumbers :: IO [Int] -- an IO action producing an integer list
askForNumbers = do
  putStrLn "Enter a number (or 0 to stop): "
  x <- getLine
  if (read x) == 0  -- convert string to number
  then return []
  else do
    rest <- askForNumbers
    return (read x:rest)

playWithNumbers :: [Int] -> IO ()
playWithNumbers ns = do 
  putStrLn ("Product of all numbers is: " ++ (show (product ns)))

numberProgram :: IO ()
numberProgram = do 
  askForNumbers >>= playWithNumbers

-- 6
-- Write a data type declaration for Triple, a type which contains three elements, 
-- all of different types. Write functions tripleFst, tripleSnd and tripleThr to extract
-- respectively the first, second and third elements.

data Triple a b c = Triple a b c

tripleFst :: Triple a b c -> a
tripleFst (Triple a b c) = a

tripleSnd :: Triple a b c -> b
tripleSnd (Triple a b c) = b

tripleThr :: Triple a b c -> c
tripleThr (Triple a b c) = c

-- 7
-- Write a datatype Quadruple which holds four elements. 
-- However, the first two elements must be the same type and the 
-- last two elements must be the same type. Write a function 
-- firstTwo which returns a list containing the first two elements 
-- and a function lastTwo which returns a list containing the last two elements. 
-- Write type signatures for these functions

data Quadruple a b = Quadruple a a b b

firstTwo :: Quadruple a b -> [a]
firstTwo (Quadruple x y z t) = [x,y]

secondTwo :: Quadruple a b -> [b]
secondTwo (Quadruple  x y z t) = [z,t]

-- 8
-- Write a datatype Tuple which can hold one, two, three or four elements, depending on the constructor 
-- (that is, there should be four constructors, one for each number of arguments). 
-- Also provide functions tuple1 through tuple4 which take a tuple and return 
-- Just the value in that position, or Nothing if the number is invalid 
-- (i.e., you ask for the tuple4 on a tuple holding only two elements).

data Tuple a b c d = One a 
                   | Two a b 
                   | Three a b c 
                   | Four a b c d

tuple1 (One   a      ) = Just a
tuple1 (Two   a b    ) = Just a
tuple1 (Three a b c  ) = Just a
tuple1 (Four  a b c d) = Just a

tuple2 (One   a      ) = Nothing
tuple2 (Two   a b    ) = Just b
tuple2 (Three a b c  ) = Just b
tuple2 (Four  a b c d) = Just b

tuple3 (One   a      ) = Nothing
tuple3 (Two   a b    ) = Nothing
tuple3 (Three a b c  ) = Just c
tuple3 (Four  a b c d) = Just c

tuple4 (One   a      ) = Nothing
tuple4 (Two   a b    ) = Nothing
tuple4 (Three a b c  ) = Nothing
tuple4 (Four  a b c d) = Just d

-- 9
-- Based on our definition of Tuple from the previous exercise, 
-- write a function which takes a Tuple and returns either the value (if it's a one-tuple), 
-- a Haskell-pair (i.e., ('a',5)) if it's a two-tuple, 
-- a Haskell-triple if it's a three-tuple or a Haskell-quadruple if it's a four-tuple. 
-- You will need to use the Either type to represent this.

tuple2HaskellTuple :: Tuple a b c d -> Either (Either a (a,b)) (Either (a,b,c) (a,b,c,d)) -- all 4 possible combinations
tuple2HaskellTuple (One   a      ) = Left (Left a)
tuple2HaskellTuple (Two   a b    ) = Left (Right (a,b))
tuple2HaskellTuple (Three a b c  ) = Right (Left (a,b,c))
tuple2HaskellTuple (Four  a b c d) = Right (Right (a,b,c,d))

-- 10

data MyList a = Nil
              | Cons a (MyList a)

-- Write functions listHead, listTail, listFoldl and listFoldr 
-- which are equivalent to their Prelude twins, but function on our MyList datatype. 

listHead :: MyList a -> Maybe a
listHead Nil = Nothing
listHead (Cons x xs) = Just x

listTail :: MyList a -> Maybe (MyList a)
listTail Nil = Nothing
listTail (Cons x xs) = Just xs

listFoldr :: (a -> b -> b) -> b -> (MyList a) -> b
listFoldr f b Nil = b
listFoldr f b (Cons x xs) = f x (listFoldr f b xs)

-- 11

data BinaryTree a = Leaf a
                  | Branch (BinaryTree a) a (BinaryTree a)

-- Write a function elements which returns the elements in a BinaryTree in a 
-- bottom-up, left-to-right manner 
-- (i.e., the first element returned in the left-most leaf, followed by its parent's value, 
-- followed by the other child's value, and so on). 
-- The result type should be a normal Haskell list.

treeElements :: (BinaryTree a) -> [a]
treeElements (Leaf a) = [a]
treeElements (Branch leftSub a rightSub) = (treeElements leftSub) ++ [a] ++ (treeElements rightSub)

-- 12
-- Write a foldr function treeFoldr for BinaryTrees and rewrite elements
-- in terms of it (call the new one elements2).

treeFoldr :: (a -> b -> b) -> b -> (BinaryTree a) -> b
treeFoldr f b (Leaf a) = f a b
treeFoldr f b (Branch leftSub a rightSub) = treeFoldr f (f a (treeFoldr f b rightSub)) leftSub

treeElements2 :: (BinaryTree a) -> [a]
treeElements2 = treeFoldr (:) []

-- 13
-- Write a foldl function treeFoldl for BinaryTrees and rewrite elements
-- in terms of it (call the new one elements3).

treeFoldl :: (a -> b -> a) -> a -> BinaryTree b -> a
treeFoldl f i (Leaf x) = f i x
treeFoldl f i (Branch left x right) = treeFoldl f (f (treeFoldl f i left) x) right

treeElements3 t = treeFoldl (\i a -> i ++ [a]) [] t

-- 14
-- Write a program that first asks whether the user wants to read from a file, write to a file or quit. 
-- If the user responds quit, the program should exit. If he responds read, the program should ask him 
-- for a file name and print that file to the screen (if the file doesn't exist, the program may crash). 
-- If he responds write, it should ask him for a file name and then ask him for text to write to the file, 
-- with "." signaling completion. All but the "." should be written to the file.

doFileProgram = do
  putStrLn "Enter command: [read/write/quit]"
  command <- getLine
  case command of
    "quit"  -> return ()
    "read"  -> do doReadFile
                  doFileProgram
    "write" -> do doWriteFile
                  doFileProgram
    _       -> do doFileProgram
                  
doReadFile = do
    putStrLn "File to read: "
    filename <- getLine 
    bracket (openFile filename ReadMode) hClose
            (\h -> do contents <- hGetContents h
                      putStrLn "File contents:"
                      putStrLn contents)

doWriteFile = do
  putStrLn "File to write: "
  filename <- getLine
  putStrLn "Enter text to go into the file (end with  '.'):"
  contents <- getLinesUntilDot
  bracket (openFile filename WriteMode) hClose
          (\h -> hPutStrLn h (unlines contents))

getLinesUntilDot = do 
  line <- getLine
  case line of 
    "." -> return []
    _ -> do nextLine <- getLinesUntilDot
            return (line:nextLine)

main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  doFileProgram
