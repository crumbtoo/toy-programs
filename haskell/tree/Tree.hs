module Tree
( singleton
, insert
, isElem
, fromList
, toList
, printTree
) where

import Text.Printf

data Tree a = EmptyNode | Node a (Tree a) (Tree a)
    deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyNode EmptyNode

insert :: (Ord a) => a -> Tree a -> Tree a
insert x EmptyNode = singleton x
insert x w@(Node a l r)
    | x == a    = w
    | x < a     = Node a (insert x l) r
    | x > a     = Node a l (insert x r)

isElem :: (Ord a) => a -> Tree a -> Bool
x `isElem` EmptyNode = False
x `isElem` (Node a l r)
    | x == a    = True
    | x > a     = x `isElem` r
    | x < a     = x `isElem` l

fromList :: (Ord a) => [a] -> Tree a
fromList [] = EmptyNode
fromList [x] = singleton x
fromList (x:xs) = insert x $ fromList xs

toList :: (Ord a) => Tree a -> [a]
toList EmptyNode = []
toList (Node a EmptyNode EmptyNode) = [a]
toList (Node a l r) = a : (toList l ++ toList r)

printDepth :: Int -> IO ()
printDepth 0 = return ()
printDepth 1 = putStr "├── "
printDepth n = putStr "│   " >> printDepth (n - 1)

printTree' :: (PrintfArg a) => Tree a -> Int -> String -> IO ()
printTree' EmptyNode _ _ = return ()
printTree' (Node a l r) depth name = do
    printDepth depth
    printf "%s: %v\n" name a
    printTree' l (depth + 1) "Left"
    printTree' r (depth + 1) "Right"

    

printTree :: (PrintfArg a) => Tree a -> IO ()
printTree t = printTree' t 0 "Root"

