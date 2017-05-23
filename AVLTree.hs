module AVLTree where

import Debug.Trace (trace)
data AVLTree a = Empty | AVLNode { label :: a
                                 , height :: Int
                                 , left :: AVLTree a
                                 , right :: AVLTree a
                                 } deriving (Eq, Show)

getHeight :: AVLTree a -> Int
getHeight Empty = 0
getHeight (AVLNode _ h _ _) = h

balanceFactor :: AVLTree a -> Int
balanceFactor Empty = 0
balanceFactor (AVLNode _ _ lc rc) = getHeight lc - getHeight rc

insert :: (Ord a, Show a) => AVLTree a -> a -> AVLTree a
insert Empty x = AVLNode x 1 Empty Empty
insert (AVLNode l h lc rc) x = let
  (newLC, newRC) = if (x < l) then (insert lc x, rc)
                              else (lc, insert rc x)
  newHeight = 1 + (max (getHeight newLC) (getHeight newRC))
  in rebalance $ AVLNode l newHeight newLC newRC

rebalance :: (Show a) => AVLTree a -> AVLTree a
rebalance tree
  | trace ("rebalance: " ++ show tree) False = undefined
rebalance tree = case tree of
  Empty               -> Empty
  (AVLNode l h lc rc) ->
    let balanceFactor = getHeight lc - getHeight rc
    in case balanceFactor of
      -1 -> tree
      0  -> tree
      1  -> tree
      -2 -> rotateLeftMaybeViaRight tree
      2  -> rotateRightMaybeViaLeft tree
      _  -> error ("bad balance factor in tree: " ++ show tree)

rotateRight :: AVLTree a -> AVLTree a
rotateRight Empty = Empty
rotateRight (AVLNode l h (AVLNode ll lh llc lrc) rc) = let
  newRC = AVLNode l (1 + max (getHeight lrc) (getHeight rc)) lrc rc
  newHeight = 1 + max (getHeight llc) (getHeight newRC)
  in AVLNode ll newHeight llc newRC

rotateLeft :: AVLTree a -> AVLTree a
rotateLeft Empty = Empty
rotateLeft (AVLNode l h lc (AVLNode rl rh rlc rrc)) = let
  newLC = AVLNode l (1 + max (getHeight lc) (getHeight rlc)) lc rlc
  newHeight = 1 + max (getHeight newLC) (getHeight rrc)
  in AVLNode rl newHeight newLC rrc

rotateLeftMaybeViaRight :: AVLTree a -> AVLTree a
rotateLeftMaybeViaRight Empty = Empty
rotateLeftMaybeViaRight (AVLNode l h lc rc) = let
  needRightRotate = balanceFactor rc > 0
  newRC = if needRightRotate then rotateRight rc else rc
  newHeight = max (getHeight lc) (getHeight newRC)
  in rotateLeft $ AVLNode l newHeight lc newRC

rotateRightMaybeViaLeft :: AVLTree a -> AVLTree a
rotateRightMaybeViaLeft Empty = Empty
rotateRightMaybeViaLeft (AVLNode l h lc rc) = let
  needLeftRotate = balanceFactor lc < 0
  newLC = if needLeftRotate then rotateLeft lc else lc
  newHeight = max (getHeight newLC) (getHeight rc)
  in rotateRight $ AVLNode l newHeight newLC rc

makeTree :: Int -> AVLTree Int
makeTree n = foldl insert Empty ([1..n])

delete0 :: (Ord a, Show a) => AVLTree a -> a -> AVLTree a
delete0 Empty _ = error "I don't think that element is in this tree"
delete0 (AVLNode l h lc rc) x
  | x < l = let newLC = delete0 lc x
                newHeight = 1 + max (getHeight newLC) (getHeight rc)
            in rebalance $ AVLNode l newHeight newLC rc
  | x > l = let newRC = delete0 rc x
                newHeight = 1 + max (getHeight lc) (getHeight newRC)
            in rebalance $ AVLNode l newHeight lc newRC
  -- Now we can assume x == l
  -- if it has no right child, just put the left one in its place
  | rc == Empty = lc
  -- if it has no left child, just put the right one in its place
  | lc == Empty = rc
  -- okay it has two children. we pop out the min value from the right child
  -- and make that our value
  | otherwise = let
      (newValue, newRC) = findAndDeleteMin rc
      newHeight = 1 + max (getHeight lc) (getHeight newRC)
      in rebalance $ AVLNode newValue newHeight lc newRC

findAndDeleteMin :: (Show a) => AVLTree a -> (a, AVLTree a)
findAndDeleteMin Empty = error "you're doing it wrong"
-- If lc is Empty then we have found the minimum value. Return this value and
-- promote its right child.
findAndDeleteMin (AVLNode l h Empty Empty) = (l, Empty)
findAndDeleteMin (AVLNode l h Empty rc) = (l, rc {height = (height rc) - 1})
-- We need to recurse down our left child.
findAndDeleteMin (AVLNode l h lc rc) = let
  (minVal, newLC) = findAndDeleteMin lc
  newHeight = 1 + max (getHeight newLC) (getHeight rc)
  in (minVal, rebalance $ AVLNode l newHeight newLC rc)

prettyPrint :: (Show a) => AVLTree a -> IO ()
prettyPrint tree = putStrLn $ prettyPrint0 tree 0

prettyPrint0 :: (Show a) => AVLTree a -> Int -> String
prettyPrint0 tree indent = let
  indentation = take indent $ repeat ' '
  treeDesc = case tree of
    Empty -> "nil"
    AVLNode l h lc rc -> (show l) ++ "h" ++ (show h) ++ (maybeRecurse lc) ++ (maybeRecurse rc)
  maybeRecurse child = case child of
    Empty -> ""
    _       -> "\n" ++ prettyPrint0 child (indent + 2)
  in (indentation ++ treeDesc)

verifyHeight :: (Show a) => AVLTree a -> Int
verifyHeight Empty = 0
verifyHeight (AVLNode l h lc rc) = let
  guess = 1 + max (verifyHeight lc) (verifyHeight rc)
  output = if (guess == h) then h else error ("height at " ++ show l ++ " did not match")
  in output
