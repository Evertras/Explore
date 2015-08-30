module AVLTree
(
    AVLTree(..)
  , findVal
  , makeTree
  , makeTreeFromList
  , traverse
  , insert
  , maxValue
)where

import Data.Maybe

data AVLTree a = Empty
             | AVLNode a (AVLTree a, AVLTree a)
             deriving Show

value :: AVLTree a -> a
value Empty = undefined
value (AVLNode val _) = val

findVal :: (Ord a) => AVLTree a -> a -> Maybe a
findVal Empty _ = Nothing
findVal (AVLNode val (l, r)) x
                | val == x = Just val
                | val < x = findVal r x
                | val > x = findVal l x

left :: AVLTree a -> AVLTree a
left Empty = Empty
left (AVLNode _ (l, _)) = l

right :: AVLTree a -> AVLTree a
right Empty = Empty
right (AVLNode _ (_, r)) = r

makeTree :: (Ord a) => a -> AVLTree a
makeTree a = insert Empty a

makeTreeFromList :: (Ord a) => [a] -> AVLTree a
makeTreeFromList xs = foldl insert Empty xs

traverse :: (Ord a) => AVLTree a -> [a]
traverse Empty = []
traverse (AVLNode x (l, r)) = x : ((traverse l) ++ (traverse r))

maxValue :: (Ord a) => AVLTree a -> a
maxValue (AVLNode x (_, Empty)) = x
maxValue (AVLNode x (_, r)) = maxValue r

mapTree :: (Ord a) => (a -> b) -> AVLTree a -> AVLTree b
mapTree _ Empty = Empty
mapTree func (AVLNode x (l, r)) = AVLNode (func x) (mapTree func l, mapTree func r)

insert :: (Ord a) => AVLTree a -> a -> AVLTree a
insert Empty x = AVLNode x (Empty, Empty)
insert (AVLNode y (left, right)) x
                | x == y = AVLNode x (left, right)
                | x < y = rebalance (AVLNode y (insert left x, right)) x
                | otherwise = rebalance (AVLNode y (left, insert right x)) x

height :: (Ord a) => AVLTree a -> Int
height Empty = -1
height (AVLNode _ (left, right)) = 1 + Prelude.max (height left) (height right)

balanceFactor :: (Ord a) => AVLTree a -> Int
balanceFactor Empty = 0
balanceFactor (AVLNode _ (left, right)) = (height left) - (height right)

rotateRight :: (Ord a) => AVLTree a -> AVLTree a
rotateRight Empty = error "Cannot rotate empty node"
rotateRight (AVLNode x (l, r)) = AVLNode leftVal (leftLeft, AVLNode x (leftRight, r))
                                        where leftVal = value l
                                              leftLeft = left l
                                              leftRight = right l
                                              rightRight = right r

rotateLeft :: (Ord a) => AVLTree a -> AVLTree a
rotateLeft Empty = error "Cannot rotate empty node"
rotateLeft (AVLNode x (l, r)) = AVLNode rightVal (AVLNode x (l, rightLeft), rightRight)
                                        where rightVal = value r
                                              leftLeft = left l
                                              rightLeft = left r
                                              rightRight = right r

rebalance :: (Ord a) => AVLTree a -> a -> AVLTree a
rebalance Empty _ = Empty
rebalance node@(AVLNode z (l, r)) i
            | abs (nodeBalance) <= 1 = node
            | abs (rightBalance) > 1 = AVLNode z (l, rebalance r z)
            | abs (leftBalance) > 1 = AVLNode z (rebalance l z, r)
            | i < z && i < y = rotateRight nz
            | i < z && i > y = rotateRight (AVLNode z (rotateLeft l, r))
            | i > z && i > y = rotateLeft nz
            | i > z && i < y = rotateLeft (AVLNode z (l, rotateRight r))
            | otherwise = error "No duplicates allowed"
            where nodeBalance = balanceFactor node
                  rightBalance = balanceFactor r
                  leftBalance = balanceFactor l
                  nz = node
                  ny = if i < z then l else r
                  nx = if i < (value ny) then (left ny) else (right ny)
                  y = value ny
