{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}

{- |
    Module      :  SDP.Templates.GRose
    Copyright   :  (c) Andrey Mulik 2020
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    @SDP.Templates.GRose@ provides generalized 'Rose' tree.
-}
module SDP.Templates.GRose
(
  -- * Exports
  module SDP.Indexed,
  module SDP.Tree,
  
  -- * Rose tree
  GRose (..), Rose
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.Indexed
import SDP.Tree

import Data.List ( transpose )

import Text.Read.SDP

import Control.Exception.SDP

default ()

--------------------------------------------------------------------------------

-- | Generalized Rose Tree.
data GRose l i e = e :<: l (GRose l i e)

-- | More specific Rose Tree.
type Rose = GRose [] Int

--------------------------------------------------------------------------------

{- Eq and Eq1 instances. -}

instance (Eq e, Eq (l (GRose l i e))) => Eq (GRose l i e)
  where
    (x :<: xs) == (y :<: ys) = x == y && xs == ys

instance (Eq1 l) => Eq1 (GRose l i)
  where
    liftEq eq (x :<: xs) (y :<: ys) = eq x y && liftEq (liftEq eq) xs ys

--------------------------------------------------------------------------------

{- Show and Read instances. -}

{- |
  Show by definition, but \"e :<: []\" may be abbreviated as \"e\":
  
  > show (1 :<: []) == "1"
  > show (1 :<: [2 :<: []]) == "1 :<: [2]"
-}
instance (Show e, Linear1 l (GRose l i e)) => Show (GRose l i e)
  where
    showsPrec p (e :<:  Z) = showParen (p > appPrec) $ shows e
    showsPrec p (e :<: es) = showParen (p > appPrec) $ shows e
                                                     . showString " :<: "
                                                     . shows (listL es)

-- | Just like 'Show': > read "1 :<: []" = read "1" :: Rose Int
instance (Read e, Linear1 l (GRose l i e)) => Read (GRose l i e)
  where
    readPrec = parens (ns <|> lef)
      where
        ns   = liftA3 (\ e _ es -> e :<: fromList es) readPrec cons readPrec
        cons = expectPrec (Symbol ":<:")
        lef  = (:<: Z) <$> readPrec

--------------------------------------------------------------------------------

{- Functor, Zip and Foldable instances. -}

instance (Functor l) => Functor (GRose l i)
  where
    fmap f (e :<: bs) = f e :<: (fmap f <$> bs)

instance (Zip l) => Zip (GRose l i)
  where
    any2 f (e1:<:bs1) (e2:<:bs2) =
      f e1 e2 && any2 (any2 f) bs1 bs2
    
    any3 f (e1:<:bs1) (e2:<:bs2) (e3:<:bs3) =
      f e1 e2 e3 && any3 (any3 f) bs1 bs2 bs3
    
    any4 f (e1:<:bs1) (e2:<:bs2) (e3:<:bs3) (e4:<:bs4) =
      f e1 e2 e3 e4 && any4 (any4 f) bs1 bs2 bs3 bs4
    
    any5 f (e1:<:bs1) (e2:<:bs2) (e3:<:bs3) (e4:<:bs4) (e5:<:bs5) =
      f e1 e2 e3 e4 e5 && any5 (any5 f) bs1 bs2 bs3 bs4 bs5
    
    any6 f (e1:<:bs1) (e2:<:bs2) (e3:<:bs3) (e4:<:bs4) (e5:<:bs5) (e6:<:bs6) =
      f e1 e2 e3 e4 e5 e6 && any6 (any6 f) bs1 bs2 bs3 bs4 bs5 bs6
    
    all2 f (e1:<:bs1) (e2:<:bs2) =
      f e1 e2 && all2 (all2 f) bs1 bs2
    
    all3 f (e1:<:bs1) (e2:<:bs2) (e3:<:bs3) =
      f e1 e2 e3 && all3 (all3 f) bs1 bs2 bs3
    
    all4 f (e1:<:bs1) (e2:<:bs2) (e3:<:bs3) (e4:<:bs4) =
      f e1 e2 e3 e4 && all4 (all4 f) bs1 bs2 bs3 bs4
    
    all5 f (e1:<:bs1) (e2:<:bs2) (e3:<:bs3) (e4:<:bs4) (e5:<:bs5) =
      f e1 e2 e3 e4 e5 && all5 (all5 f) bs1 bs2 bs3 bs4 bs5
    
    all6 f (e1:<:bs1) (e2:<:bs2) (e3:<:bs3) (e4:<:bs4) (e5:<:bs5) (e6:<:bs6) =
      f e1 e2 e3 e4 e5 e6 && all6 (all6 f) bs1 bs2 bs3 bs4 bs5 bs6
    
    zipWith  f (e1:<:bs1) (e2:<:bs2) =
      f e1 e2:<:zipWith (zipWith f) bs1 bs2
    
    zipWith3 f (e1:<:bs1) (e2:<:bs2) (e3:<:bs3) =
      f e1 e2 e3:<:zipWith3 (zipWith3 f) bs1 bs2 bs3
    
    zipWith4 f (e1:<:bs1) (e2:<:bs2) (e3:<:bs3) (e4:<:bs4) =
      f e1 e2 e3 e4:<:zipWith4 (zipWith4 f) bs1 bs2 bs3 bs4
    
    zipWith5 f (e1:<:bs1) (e2:<:bs2) (e3:<:bs3) (e4:<:bs4) (e5:<:bs5) =
      f e1 e2 e3 e4 e5:<:zipWith5 (zipWith5 f) bs1 bs2 bs3 bs4 bs5
    
    zipWith6 f (e1:<:bs1) (e2:<:bs2) (e3:<:bs3) (e4:<:bs4) (e5:<:bs5) (e6:<:bs6) =
      f e1 e2 e3 e4 e5 e6:<:zipWith6 (zipWith6 f) bs1 bs2 bs3 bs4 bs5 bs6

instance (Foldable l) => Foldable (GRose l i)
  where
    foldr f base (e :<: bs) = e `f` foldr (flip $ foldr f) base bs
    foldl f base (e :<: bs) = foldl (foldl f) (f base e) bs

--------------------------------------------------------------------------------

{- Estimate and Bordered instances. -}

instance (Bordered (GRose l i e) i) => Estimate (GRose l i e)
  where
    (<==>) = on (<=>) sizeOf
    (<.=>) = (<=>) . sizeOf

instance (Bordered1 l i e, Linear1 l (GRose l i e)) => Bordered (GRose l i e) i
  where
    sizeOf = \ (_ :<: xs) -> o_foldr' ((+) . sizeOf) 1 xs
    bounds = defaultBounds . sizeOf

--------------------------------------------------------------------------------

{- TFold instance. -}

instance (Bordered1 l i (GRose l i e), Linear1 l (GRose l i e)) => TFold (GRose l i e) e
  where
    wfold f base =
      let go (e :<: bs) = ([e] :) . concat . transpose $ o_foldr ((:) . go) [] bs
      in  foldr (flip $ foldr f) base . go
    
    dfold f base (e :<: bs) = e `f` o_foldr (flip $ dfold f) base bs

--------------------------------------------------------------------------------

{- Node, DegreeNode and Tree instances. -}

instance (Bordered1 l i (GRose l i e), Linear1 l (GRose l i e)) => Node (GRose l i e) e
  where
    node es bs = null es ? empEx "node" $ head es :<: fromList bs
    
    leaf = (:<: Z)
    isLeaf   (_ :<: bs) = isNull bs
    fromNode (e :<: bs) = ([e], listL bs)
    
    nodeWidth  = const 1
    nodeDegree = \ (_ :<: bs) -> sizeOf bs
    
    nodeElems (e :<:  _) = [e]
    
    childs (_ :<: bs) = listL bs
    child  (_ :<: bs) = (bs !^)

instance (Bordered1 l i (GRose l i e), Linear1 l (GRose l i e)) => DegreeNode (GRose l i e) e
  where
    unconsNodeChilds (e :<: (b :> bs)) = (b, e :<: bs)
    unconsNodeChilds         _         = patEx "unconsNodeChilds"
    
    unsnocNodeChilds (e :<: (bs :< b)) = (e :<: bs, b)
    unsnocNodeChilds         _         = patEx "unsnocNodeChilds"
    
    b /* (e :<: bs) = e :<: (b :> bs)
    (e :<: bs) *\ b = e :<: (bs :< b)

instance (Bordered1 l i (GRose l i e), Linear1 l (GRose l i e)) => Tree (GRose l i e) e
  where
    fixTree       = id
    optimizeTree  = id
    normalizeTree = id
    
    isOptimal = const True
    isNormal  = const True
    
    tree = node
    
    descendant  = concatMap descendant' . childs
    descendant' = \ ([e] :/*\: bs) -> leaf e : concatMap descendant' bs
    
    insertTree x (e :<: bs) = e :<: (leaf x :> bs)
    deleteTree x = onTree $ \ n@(e :<: bs) -> e == x ? deleteRoot bs $ n
      where
        deleteRoot ((e :<: b) :> bs) = e :<: (deleteRoot b :> bs)
        deleteRoot _ = empEx "deleteTree"
    
    onElem f 0 (e :<: es) = f e :<: es
    onElem _ _ es = es
    
    onBranch f i (e :<: es) = e :<: write es i (f (es !^ i))
    
    elemPos x (e :<: _) = x == e ? Just 0 $ Nothing
    
    shiftCTL (e :<: (b :> bs)) = e :<: (bs :< b)
    shiftCTL ns = ns
    
    shiftCTR (e :<: (bs :< b)) = e :<: (b :> bs)
    shiftCTR ns = ns
    
    minTree (e :<: es) = o_foldr (min . minTree) e es
    maxTree (e :<: es) = o_foldr (min . maxTree) e es

--------------------------------------------------------------------------------

empEx :: String -> a
empEx =  throw . EmptyRange . showString "in SDP.Tree.Rose."

patEx :: String -> a
patEx =  throw . PatternMatchFail . showString "in SDP.Tree.Rose."



