{-# LANGUAGE MultiParamTypeClasses, ConstraintKinds #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}

{- |
    Module      :  SDP.Tree
    Copyright   :  (c) Andrey Mulik 2020
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    @SDP.Tree@ provides classes 'Tree' and 'ParentTree' for basic tree
    operations.
-}
module SDP.Tree
(
  -- * Exports
  module SDP.Node,
  
  -- * Basic tree
  Tree (..), Tree1,
  
  -- ** Tree with parent
  ParentTree (..), ParentTree1
)
where

import Prelude ()
import SDP.SafePrelude

import SDP.Indexed
import SDP.Node

default ()

--------------------------------------------------------------------------------

{- |
  Tree - a class of trees, recursive structures consisting of nodes. So, this
  class provides recursive operations, while non-recursive ones are defined in
  Node superclass.
-}
class (Node t e) => Tree t e
  where
    {-# MINIMAL insertTree, deleteTree, ((</|) | shiftCTL), ((|\>) | shiftCTR), minTree, maxTree #-}
    
    {- |
      Creates tree from lists of values and branches, should be defined
      everywhere.
      * If there is not enough data (elements or branches), return an empty tree
      * If there is too much data (elements or branches), ignore them
      * If the resulting tree (may be) incorrect, call 'normTree'
      
      Note that 'tree' works with values, not elements.
    -}
    tree :: [e] -> [t] -> t
    tree =  normTree ... node
    
    -- | Tree normalization function, may fail.
    normTree :: t -> t
    normTree =  id
    
    -- | Returns all tree 'descendant' (childs, childs of childs ...).
    descendant :: t -> [t]
    descendant es = let bs = childs es in concat (bs : map descendant bs)
    
    -- | Returns all tree childs and same tree.
    descendant' :: t -> [t]
    descendant' es = es : descendant es
    
    {- |
      Inserts element to tree. If the tree already contains such an element and
      duplicates are not allowed, it should return the given (or equivalent)
      tree.
    -}
    insertTree :: (Eq e) => e -> t -> t
    
    {- |
      Deletes element from tree. If the tree doesn't contain such an element, it
      should return the given (or equivalent) tree.
      
      deleteTree doesn't guarantee that the tree will not contain any such
      elements - if the data is incorrect, it may not be found. Also, some trees
      allow duplicates.
    -}
    deleteTree :: (Eq e) => e -> t -> t
    
    -- | Same as 'deleteTree', but remove all such elements.
    deleteTree' :: (Eq e) => e -> t -> t
    deleteTree' =  deleteTree
    
    {- |
      Applies function to n-th element. If n is out of range, return the given
      (or equivalent) tree.
    -}
    onElem :: (e -> e) -> Int -> t -> t
    onElem f n = \ ts@(es :/*\: bs) -> indexIn es n ? write es n (f (es !^ n)) :/*\: bs $ ts
    
    onElems :: (e -> e) -> t -> t
    onElems f = \ (es :/*\: bs) -> f <$> es :/*\: bs
    
    {- |
      Applies function to n-th branch. If n is out of range, return the given
      (or equivalent) tree.
    -}
    onBranch :: (t -> t) -> Int -> t -> t
    onBranch f n = \ ts@(es :/*\: bs) -> indexIn bs n ? es :/*\: (write bs n (f (bs !^ n))) $ ts
    
    -- | Applies function to all branches.
    onBranches :: (t -> t) -> t -> t
    onBranches f = \ (es :/*\: bs) -> es :/*\: f <$> bs
    
    -- | Recursively applies the function to all branches, and then to the root.
    onTree :: (t -> t) -> t -> t
    onTree f = f . onBranches (onTree f)
    
    -- | Find the element number in current node (from 0).
    elemPos :: (Eq e) => e -> t -> Maybe Int
    elemPos e = ((== e) .$) . nodeElems
    
    {- |
      The path (number of value or child from 0) that you need to move to reach
      the element (if any) in the ordered tree. If the element is a value of
      the current node, returns its number.
      
      This is a service non-recursive function.
      
      > es = SomeTree (SomeLeaf 1) 2 (SomeLeaf 4)
      > branchPos es (-600) = 0
      > branchPos es 1 == 0
      > branchPos es 2 == 0
      > branchPos es 3 == 1
      > branchPos Z  x == 0 -- forall x
      
      because
      
      > -600 <= treeElem es 0 -- => -600 may be in left branch or in element (0)
      > 1 <= treeElem es 0 -- => 1 may be in left branch or in element (0)
      > 2 <= treeElem es 0 -- => 2 may be in left branch or in element (0)
      > 3 > treeElem es 0 -- => 3 may be in right branch (1)
      > branch Z 0 == undefined -- => 0 is incorrect branch number.
    -}
    branchPos :: (Ord e) => e -> t -> Int
    branchPos e = foldr' (\ x n -> e <= x ? n $ n + 1) 0 . nodeElems
    
    -- | Minimal element of tree, may fail.
    minTree :: (Ord e) => t -> e
    
    -- | Maximal element of tree, may fail.
    maxTree :: (Ord e) => t -> e
    
    -- | Left n-position cyclic shift of branches and values.
    (</|) :: t -> Int -> t
    ts </| n = n < 1 ? ts $ shiftCTL ts </| (n - 1)
    
    -- | Right n-position cyclic shift of branches and values.
    (|\>) :: Int -> t -> t
    n |\> ts = n < 1 ? ts $ (n - 1) |\> shiftCTR ts
    
    -- | Left 1-position cyclic shift of branches and values.
    shiftCTL :: t -> t
    shiftCTL =  (</| 1)
    
    -- | Right 1-position cyclic shift of branches and values.
    shiftCTR :: t -> t
    shiftCTR =  (1 |\>)

--------------------------------------------------------------------------------

-- | A class of trees whose nodes can access the 'parent'.
class (Tree t e) => ParentTree t e
  where
    {-# MINIMAL (isRoot, parent | ancestors), (parentPos, siblings | precending, following) #-}
    
    -- | Checks if node is 'root'.
    isRoot :: t -> Bool
    isRoot =  null . ancestors
    
    -- | Returns 'root' of this 'tree'.
    root :: t -> t
    root =  head . ancestors'
    
    -- | Node depth - distance between node and 'root'.
    nodeDepth :: t -> Int
    nodeDepth =  length . ancestors
    
    -- | Node level (1 + depth).
    nodeLevel :: t -> Int
    nodeLevel =  (+ 1) . nodeDepth
    
    -- | Returns 'parent' node (direct ancestor), if any. May fail.
    parent :: t -> t
    parent =  last . ancestors
    
    -- | Selects ancestors, from 'root' to 'parent'.
    ancestors :: t -> [t]
    ancestors =  go []
      where
        go as Root = as
        go as  es  = go (p : as) p where p = parent es
    
    -- | Returns node 'ancestors' and (then) the node itself. Non-empty.
    ancestors' :: t -> [t]
    ancestors' es = ancestors' es :< es
    
    {- |
      Selects node siblings (other 'childs' with the same 'parent').
      
      > siblings   es = precending es ++ following es
      > precending es = take (parentPos es) es
      > following  es = drop (parentPos es) es
    -}
    siblings :: t -> [t]
    siblings es = precending es ++ following es
    
    {- |
      Selects 'siblings' before node.
      
      > precending root = []
    -}
    precending :: t -> [t]
    precending Root = []
    precending  es  = parentPos es `take` childs (parent es)
    
    {- |
      Selects all nodes that appear before the current node.
      
      > precending' root = []
    -}
    precending' :: t -> [t]
    precending' =  concatMap precending . ancestors'
    
    {- |
      Selects 'siblings' after node.
      
      > following root = []
    -}
    following :: t -> [t]
    following Root = []
    following  es  = (parentPos es + 1) `drop` childs (parent es)
    
    -- | Selects all nodes that appear after the current node.
    following' :: t -> [t]
    following' =  concatMap following . reverse . ancestors'
    
    -- | Selects 'parent' and (then) 'siblings' of node.
    neighbor :: t -> [t]
    neighbor es = parent es : siblings es
    
    {- |
      Node number in 'parent' node (from 0).
      
      > parentPos root = -1
      > child (parent es) (parentPos es) = es
    -}
    parentPos :: t -> Int
    parentPos Root = -1
    parentPos  es  = length (precending es)

--------------------------------------------------------------------------------

{- Constraint synonyms. -}

-- | Kind (* -> *) Tree.
type Tree1 t e = Tree (t e) e

-- | Kind (* -> *) ParentTree.
type ParentTree1 t e = ParentTree (t e) e

--------------------------------------------------------------------------------

-- | Unidirectional pattern synonym for 'isRoot'.
pattern Root :: (ParentTree t e) => t
pattern Root <- (isRoot -> True)


