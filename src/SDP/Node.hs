{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, ConstraintKinds #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}

{- |
    Module      :  SDP.Node
    Copyright   :  (c) Andrey Mulik 2020
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    @SDP.Node@ provides 'Node', 'DegreeNode', 'WideNode' and 'ShiftNode' classes
    for basic node manipulation.
-}
module SDP.Node
(
  -- * Basic node
  Node (..), Node1, pattern Leaf, pattern (:/*\:),
  
  nodeLeft, nodeRight, leftChild, rightChild,
  
  -- ** Node with variable degree
  DegreeNode (..), DegreeNode1, pattern (:/+), pattern (:+\),
  
  -- ** Node with variable width
  WideNode (..), WideNode1, pattern (:/*), pattern (:*\),
  
  -- * Non-cyclic shift
  ShiftNode (..), ShiftNode1
)
where

import Prelude ()
import SDP.SafePrelude

import SDP.Indexed

infix 3 :/*\:

default ()

--------------------------------------------------------------------------------

-- | Node provides basic operations with nodes.
class Node n v | n -> v
  where
    {-# MINIMAL node, (nodeWidth, nodeElem | nodeElems), (nodeDegree, child | childs) #-}
    
    {- |
      Create node from lists of values and childs, should be defined
      everywhere.
      * If there is not enough data, return an empty tree
      * If there is too much data, ignore them
    -}
    node :: [v] -> [n] -> n
    
    -- | Return leaf node.
    leaf :: v -> n
    leaf e = [e] `node` []
    
    -- | Checks if node is 'leaf'.
    isLeaf :: n -> Bool
    isLeaf =  (== 0) . nodeDegree
    
    fromNode :: n -> ([v], [n])
    fromNode es = (nodeElems es, childs es)
    
    -- | Actual count of node values.
    nodeWidth :: n -> Int
    nodeWidth =  length . nodeElems
    
    -- | Node value by number, may fail.
    nodeElem :: n -> Int -> v
    nodeElem =  (!) . nodeElems
    
    -- | List of node values.
    nodeElems :: n -> [v]
    nodeElems es = nodeElem es <$> [0 .. nodeWidth es - 1]
    
    -- | Actual count of node childs.
    nodeDegree :: n -> Int
    nodeDegree =  length . childs
    
    -- | Node child by number, may fail.
    child :: n -> Int -> n
    child =  (!) . childs
    
    -- | List of node childs.
    childs :: n -> [n]
    childs es = child es <$> [0 .. nodeDegree es - 1]

-- | 'Leaf' is generic pattern synonym for 'isLeaf' and 'leaf'.
pattern Leaf :: (Node n v) => v -> n
pattern Leaf e <- (isLeaf ?+ nodeElems -> Just [e]) where Leaf = leaf

-- | (:/*\:) is generic pattern synonym for 'node', 'fromNode' and 'childs'.
pattern (:/*\:) :: (Node n v) => [v] -> [n] -> n
pattern es :/*\: bs <- (fromNode -> (es, bs)) where (:/*\:) = node

--------------------------------------------------------------------------------

-- | 'WideNode' defines operations on nodes with a variable number of values (width).
class (Node n v) => WideNode n v
  where
    {- |
      'unconsNodeValues' is service function for (:+\) that extracts left value
      from node.
    -}
    unconsNodeValues :: n -> (v, n)
    
    {- |
      'unsnocNodeValues' is service function for (:+\) that extracts right value
      from node.
    -}
    unsnocNodeValues :: n -> (n, v)
    
    -- | @e /+ ns@ prepends value @e@ to node @ns@.
    (/+) :: v -> n -> n
    
    -- | @ns +\ e@ appends value @e@ to node @ns@.
    (+\) :: n -> v -> n

-- | Add/remove left value of the node.
pattern (:/+) :: (WideNode n v) => v -> n -> n
pattern node :/+ e <- (unconsNodeValues -> (node, e)) where (:/+) = (/+)

-- | Add/remove right value of the node.
pattern (:+\) :: (WideNode n v) => n -> v -> n
pattern e :+\ node <- (unsnocNodeValues -> (e, node)) where (:+\) = (+\)

--------------------------------------------------------------------------------

-- | 'DegreeNode' defines operations on nodes with a variable number of childs (degree).
class (Node n v) => DegreeNode n v
  where
    {- |
      'unconsNodeChilds' is service function for (:*\) that extracts left value
      from node. Returns pair of extracted branch and shrinked node.
    -}
    unconsNodeChilds :: n -> (n, n)
    
    {- |
      'unsnocNodeValues' is service function for (:+\) that extracts right value
      from node. Returns pair of shrinked node and extracted branch.
    -}
    unsnocNodeChilds :: n -> (n, n)
    
    -- | @br /* ns@ prepends branch @br@ to node @ns@.
    (/*) :: n -> n -> n
    
    -- | @ns *\ br@ appends branch @br@ to node @ns@.
    (*\) :: n -> n -> n

-- | Add/remove left branch of node.
pattern (:*\) :: (DegreeNode n v) => n -> n -> n
pattern br :*\ node <- (unconsNodeChilds -> (node, br)) where (:*\) = (*\)

-- | Add/remove right branch of node.
pattern (:/*) :: (DegreeNode n v) => n -> n -> n
pattern br :/* node <- (unsnocNodeChilds -> (br, node)) where (:/*) = (/*)

--------------------------------------------------------------------------------

{- |
  ShiftNode defines operations with nodes that support non-cyclic shift
  (simultaneous extraction of the same number of values and childs on the left
  or right side).
  
  Note that ShiftNode doesn't support the two-sided shift, so non-positive shift
  of tree is same tree.
-}
class (Node n v) => ShiftNode n v
  where
    {-# MINIMAL ((/<|)|shiftTL), ((|>\)|shiftTR) #-}
    
    -- | Left n-position non-cyclic shift of branches and values.
    (/<|) :: n -> Int -> n
    ns /<| i = i < 1 ? ns $ shiftTL ns /<| (i - 1)
    
    -- | Right n-position non-cyclic shift of branches and values.
    (|>\) :: Int -> n -> n
    i |>\ ns = i < 1 ? ns $ (i - 1) |>\ shiftTR ns
    
    -- | Prepend branch and value.
    (/-) :: (v, n) -> n -> n
    (e, b) /- ~(es :/*\: bs) = (e :> es) :/*\: (b :> bs)
    
    -- | Append branch and value.
    (-\) :: n -> (n, v) -> n
    ~(es :/*\: bs) -\ (b, e) = (es :< e) :/*\: (bs :< b)
    
    -- | Left 1-position non-cyclic shift of branches and values.
    shiftTL :: n -> n
    shiftTL =  (/<| 1)
    
    -- | Right 1-position non-cyclic shift of branches and values.
    shiftTR :: n -> n
    shiftTR =  (1 |>\)

--------------------------------------------------------------------------------

-- | Rank (* -> *) Node.
type Node1 t e = Node (t e) e

-- | Rank (* -> *) WideNode.
type WideNode1 t e = WideNode (t e) e

-- | Rank (* -> *) ShiftNode.
type ShiftNode1 t e = ShiftNode (t e) e

-- | Rank (* -> *) DegreeNode.
type DegreeNode1 t e = DegreeNode (t e) e

--------------------------------------------------------------------------------

-- | Left value in node.
nodeLeft :: (Node n v) => n -> v
nodeLeft =  (`nodeElem` 0)

-- | Left child in node.
leftChild :: (Node n v) => n -> n
leftChild =  (`child` 0)

-- | Right value in node.
nodeRight :: (Node n v) => n -> v
nodeRight ns = nodeElem ns (nodeWidth ns - 1)

-- | Right child in node.
rightChild :: (Node n v) => n -> n
rightChild ns = child ns (nodeDegree ns - 1)

