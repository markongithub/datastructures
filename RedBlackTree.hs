module RedBlackTree where

import Debug.Trace (trace)

data NodeColor = Red | Black | DBlack deriving (Eq, Show)

data RBTree a = NilNode | DBNilNode | RBNode { color :: NodeColor
                                             , label :: a
                                             , left :: RBTree a
                                             , right :: RBTree a
                                             } deriving (Eq, Show)

insert :: (Eq a, Ord a, Show a) => RBTree a -> a -> RBTree a
insert tree x = let
  tempTree = insert0 tree x
  in tempTree {color = Black}

insert0 :: (Eq a, Ord a, Show a) => RBTree a -> a -> RBTree a
insert0 NilNode x = RBNode Red x NilNode NilNode
insert0 (RBNode c l lc rc) x = let
  insertRight = RBNode c l lc (insert0 rc x)
  insertLeft = RBNode c l (insert0 lc x) rc
  newUnbalanced = if (x < l) then insertLeft else insertRight
  in rebalance newUnbalanced

-- black node with red left child and red left-left grandchild
isLeftLeft :: RBTree a -> Bool
isLeftLeft (RBNode Black _ (RBNode Red _ (RBNode Red _ _ _) _) _) = True
isLeftLeft _ = False

rebalanceLeftLeft :: RBTree a -> RBTree a
-- left child becomes root. root becomes right child. llc becomes lc.
rebalanceLeftLeft (RBNode Black l (RBNode Red ll (RBNode Red lll lllc llrc) lrc) rc) = RBNode Red ll (RBNode Black lll lllc llrc) (RBNode Black l lrc rc)
rebalanceLeftLeft _ = error "This is not a left-left case"

isRightRight :: RBTree a -> Bool
isRightRight (RBNode Black _ _ (RBNode Red _ _ (RBNode Red _ _ _))) = True
isRightRight _ = False

rebalanceRightRight (RBNode Black l lc (RBNode Red rl rlc (RBNode Red rrl rrlc rrrc))) = RBNode Red rl (RBNode Black l lc rlc) (RBNode Black rrl rrlc rrrc)
rebalanceRightRight _ = error "This is not a right-right case"

isLeftRight :: RBTree a -> Bool
isLeftRight (RBNode Black _ (RBNode Red _ _ (RBNode Red _ _ _)) _) = True
isLeftRight _ = False

-- this is in the style Okasaki used but I am trying to follow the logic from
-- https://en.wikipedia.org/wiki/Red%E2%80%93black_tree#/media/File:Red-black_tree_insert_case_4.svg

rebalanceLeftRight :: RBTree a -> RBTree a
rebalanceLeftRight (RBNode Black l (RBNode Red ll llc (RBNode Red lrl lrlc lrrc)) rc) = let
  interimTree = RBNode Black l (RBNode Red lrl (RBNode Red ll llc lrlc) lrrc) rc
  in rebalanceLeftLeft interimTree

isRightLeft :: RBTree a -> Bool
isRightLeft (RBNode DBlack _ _ (RBNode Red _ (RBNode Red _ _ _) _)) = True
isRightLeft (RBNode Black _ _ (RBNode Red _ (RBNode Red _ _ _) _)) = True
isRightLeft _ = False

rebalanceRightLeft :: RBTree a -> RBTree a
rebalanceRightLeft (RBNode Black l lc (RBNode Red rl (RBNode Red rll rllc rlrc) rrc)) = let
  newRightChild = RBNode Red rll rllc (RBNode Red rl rlrc rrc)
  interimTree = RBNode Black l lc newRightChild
  in rebalanceRightRight interimTree

rebalanceDB :: (Show a) => RBTree a -> RBTree a
-- I didn't divide this up into special cases with their own functions, like
-- I did in rebalance. Too many ins and outs and what-have-yous.
-- Red node, one DB child, one black child
rebalanceDB (RBNode Red l DBNilNode NilNode) = error "I did not see DBNilNode NilNode coming"
rebalanceDB (RBNode Red l DBNilNode (RBNode Black rl rlc rrc)) = rebalance $ RBNode Black rl (RBNode Red l NilNode rlc) rrc
rebalanceDB (RBNode Red l (RBNode DBlack ll llc lrc) NilNode) = error "I did not see DBlack opposite Nil coming"
rebalanceDB (RBNode Red l (RBNode DBlack ll llc lrc) (RBNode Black rl rlc rrc)) = RBNode Black rl (RBNode Red l (RBNode Black ll llc lrc) rlc) rrc
-- if DB is on the right
rebalanceDB (RBNode Red l NilNode DBNilNode) = error "I did not see NilNode DBNilNode coming"
rebalanceDB (RBNode Red l NilNode (RBNode DBlack rl rlc rrc)) = error "I did not see Nil opposite DBlack coming"
rebalanceDB (RBNode Red l (RBNode Black ll llc lrc) DBNilNode) = RBNode Black ll llc (RBNode Red l lrc NilNode)
rebalanceDB (RBNode Red l (RBNode Black ll llc lrc) (RBNode DBlack rl rlc rrc)) = RBNode Black ll llc (RBNode Red l lrc (RBNode Black rl rlc rrc))
-- black node, one DB child, one black child - these could introduce DB-red-red
-- violations so we immediately call this function again to fix those
-- if DB is on the left
rebalanceDB (RBNode Black l DBNilNode (RBNode Black rl rlc rrc)) = rebalanceDB $ RBNode DBlack rl (RBNode Red l NilNode rlc) rrc
rebalanceDB (RBNode Black l (RBNode DBlack ll llc lrc) (RBNode Black rl rlc rrc)) = rebalanceDB $ RBNode DBlack rl (RBNode Red l (RBNode Black ll llc lrc) rlc) rrc
-- if DB is on the right
rebalanceDB (RBNode Black l (RBNode Black ll llc lrc) DBNilNode) = rebalanceDB $ RBNode DBlack ll llc (RBNode Red l lrc NilNode)
rebalanceDB (RBNode Black l (RBNode Black ll llc lrc) (RBNode DBlack rl rlc rrc)) = rebalanceDB $ RBNode DBlack ll llc (RBNode Red l lrc (RBNode Black rl rlc rrc))
-- DB-red-red violations
rebalanceDB (RBNode DBlack l (RBNode Red ll llc (RBNode Red lrl lrlc lrrc)) rc)  = RBNode Black lrl (RBNode Black ll llc lrlc) (RBNode Black l lrrc rc)
rebalanceDB (RBNode DBlack l lc (RBNode Red rl (RBNode Red rll rllc rlrc) rrc)) = RBNode Black rll (RBNode Black l lc rllc) (RBNode Black rl rlrc rrc)
-- black node, one red child, one DB child
-- if DB is on the left...
rebalanceDB (RBNode Black l DBNilNode (RBNode Red rl (RBNode Black rll rllc rlrc) rrc)) = let
  newLeft = RBNode Black rll (RBNode Red l NilNode rllc) rlrc
  in RBNode Black rl (rebalance newLeft) rrc
rebalanceDB (RBNode Black l (RBNode DBlack ll llc lrc) (RBNode Red rl (RBNode Black rll rllc rlrc) rrc)) = let
  newLeft = RBNode Black rll (RBNode Red l (RBNode Black ll llc lrc) rllc) rlrc
  in RBNode Black rl (rebalance newLeft) rrc
-- if DB is on the right...
rebalanceDB (RBNode Black l (RBNode Red ll llc (RBNode Black lrl lrlc lrrc)) DBNilNode) = let
  newRight = RBNode Black lrl lrlc (RBNode Red l lrrc NilNode)
  in RBNode Black ll llc (rebalance newRight)
rebalanceDB (RBNode Black l (RBNode Red ll llc (RBNode Black lrl lrlc lrrc)) (RBNode DBlack rl rlc rrc)) = let
  newRight = RBNode Black lrl lrlc (RBNode Red l lrrc (RBNode Black rl rlc rrc))
  in RBNode Black ll llc (rebalance newRight)
rebalanceDB t = t

rebalance :: (Show a) => RBTree a -> RBTree a
rebalance tree0
  -- This function just handles black-red-red violations.
  -- rebalanceDB does everything involving Double Black nodes.
  -- | trace ("Rebalance: " ++ show tree) False = undefined
  | isLeftLeft tree = rebalanceLeftLeft tree
  | isRightRight tree = rebalanceRightRight tree
  | isLeftRight tree = rebalanceLeftRight tree
  | isRightLeft tree = rebalanceRightLeft tree
  | otherwise = tree
  where tree = rebalanceDB tree0

depth :: RBTree a -> Int
depth NilNode = 0
depth (RBNode _ _ lc rc) = 1 + (max (depth lc) (depth rc))

size :: RBTree a -> Int
size NilNode = 0
size DBNilNode = error "I think you messed up"
size (RBNode _ _ lc rc) = 1 + size lc + size rc

isBlack :: RBTree a -> Bool
isBlack NilNode = True
isBlack (RBNode Black _ _ _) = True
isBlack _ = False

validate :: RBTree a -> Int
validate NilNode = 0
validate DBNilNode = error "There is a DBNilNode here"
validate (RBNode DBlack _ _ _) = error "There is a tree with BB root here."
validate (RBNode c _ lc rc) = let
  addend = if c == Black then 1 else 0
  left = validate lc
  right = validate rc
  redOkay = (c == Black) || ((isBlack lc) && (isBlack rc))
  redCrash = if redOkay then True else error "Oh god we broke the red rule"
  in if (redCrash && (left == right)) then (left + addend) else error "oh god the black heights don't match"

-- just for testing, insert the integers 1 to n into a tree.
makeTree :: Int -> RBTree Int
makeTree n = foldl insert NilNode ([1..n])

contains :: (Eq a, Ord a) => RBTree a -> a -> Bool
contains NilNode _ = False
contains (RBNode _ val lc rc) x
  | x == val = True
  | x < val = contains lc x
  | x > val = contains rc x

treeToList :: RBTree a -> [a]
treeToList NilNode = []
treeToList (RBNode _ val lc rc) = (treeToList lc) ++ (val:(treeToList rc))

prettyPrint :: (Show a) => RBTree a -> IO ()
prettyPrint tree = putStrLn $ prettyPrint0 tree 0

prettyPrint0 :: (Show a) => RBTree a -> Int -> String
prettyPrint0 tree indent = let
  indentation = take indent $ repeat ' '
  treeDesc = case tree of
    NilNode -> "nil"
    RBNode c l lc rc -> (show c) ++ (show l) ++ (maybeRecurse lc) ++ (maybeRecurse rc)
  maybeRecurse child = case child of
    NilNode -> ""
    _       -> "\n" ++ prettyPrint0 child (indent + 2)
  in (indentation ++ treeDesc)

-- delete :: RBTree a -> Int -> RBTree a
-- delete NilNode _ = error "Why are we trying to delete from a nil node"
-- delete (RBNode c l lc rc) x
--   | x < l = delete lc x
--   | x > l = delete rc x
--   | lc == NilNode && rc == NilNode = NilNode
--   | 
--   | rc == NilNode = lc
--   |

-- deleteWithNoChildren :: RBTree a -> a -> RBTree a
-- deleteWithNoChildren NilNode _ = error "Why are we trying to delete from a nil node"
-- deleteWithNoChildren (RBNode c l lc rc) x
--   | x == l = error "uh not implemented"
--   | x == label rc = deleteRightChildNoGrandchildren
--   | x == label lc = deleteLeftChildNoGrandchildren
--   | 

delete :: (Eq a, Ord a, Show a) => RBTree a -> a -> RBTree a
delete tree x = let
  tempTree = delete0 tree x
  in case tempTree of
    DBNilNode      -> NilNode
    NilNode        -> NilNode
    RBNode _ _ _ _ -> tempTree {color = Black}

delete0 :: (Ord a, Show a) => RBTree a -> a -> RBTree a
delete0 NilNode _ = error "I don't think that element is in this tree."
delete0 (RBNode c l lc rc) x
  | x < l = rebalance $ RBNode c l (delete0 lc x) rc
  | x > l = rebalance $ RBNode c l lc (delete0 rc x)
-- okay, so this is the node we need to delete but we will need to promote
-- its in-order successor into its place
  | otherwise = case rc of
      -- right child is nil so we'll promote the leftt regardless
      NilNode -> rebalance $ promoteSingleChild (RBNode c l lc rc) lc
      -- right child has content, so find the minimum and put that in here
      _       -> rebalance $ RBNode c newValue lc newRC
        where (newValue, newRC) = findAndDeleteMin rc
      
findAndDeleteMin :: (Show a) => RBTree a -> (a, RBTree a)
findAndDeleteMin NilNode = error "you did it wrong"
findAndDeleteMin DBNilNode = error "you did it wrong"
-- This is the minimum value. Return this value and promote its right child.
findAndDeleteMin (RBNode c l NilNode rc) = (l, promoteSingleChild (RBNode c l NilNode rc) rc)
-- We need to recurse down our left child.
findAndDeleteMin (RBNode c l lc rc) = let
  (minVal, newLC) = findAndDeleteMin lc
  in (minVal, rebalance $ RBNode c l newLC rc)

promoteSingleChild :: RBTree a -> RBTree a -> RBTree a
promoteSingleChild NilNode _ = error "you did it wrong"
promoteSingleChild DBNilNode _ = error "you did it wrong"
-- If parent is red, just promote the child.
promoteSingleChild (RBNode Red _ _ _) child = child
-- If child is red, just promote the child but paint it black.
promoteSingleChild (RBNode Black _ _ _) (RBNode Red cl clc crc) = (RBNode Black cl clc crc)
-- If child is a NilNode, make it double black.
promoteSingleChild (RBNode Black _ _ _) NilNode = DBNilNode
-- If the child is a black tree, make it double black.
promoteSingleChild parent child = child {color = DBlack}
