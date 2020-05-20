---
title: Segment Tree
date: 2014-08-26
tags: ["Haskell"]
---
# Segment Tree

  * モノイド $(M,\ast,e)$
  * $a_1,..,a_n \in M$

に対して,以下のことがそれぞれ $O(log(n))$ で行えるデータ構造
  * $a_i \ast a_{i+1} \ast \dots \ast a_k$ を求める
  * $a_i$ の書き換え

# Verification
  AOJを用いて実装の正しさを確認した.

  * [DSL-2-A Range Minimum Query](http://judge.u-aizu.ac.jp/onlinejudge/description.jsp?id=DSL_2_A)
  * [DSL-2-B Range Sum Query](http://judge.u-aizu.ac.jp/onlinejudge/description.jsp?id=DSL_2_B)
# 実装
  Segment Tree は配列を使った破壊的な実装がよく知られている。
  しかし今回はHaskellで実装をおこなうために、副作用を用いない永続データ
  構造として実装した。
## コード
  [Range Sum Query](http://judge.u-aizu.ac.jp/onlinejudge/description.jsp?id=DSL_2_B) でverifyしたときのものを載せる
``` haskell
{-# OPTIONS_GHC -O2 -funbox-strict-fields #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE UnboxedTuples #-}
module Main (main) where
import Data.Int (Int32)
import Control.Monad
import Control.Applicative
import Data.Monoid
import Data.Maybe (fromJust)
import qualified Data.ByteString.Char8 as B8
getInts :: IO [Int]
getInts =  map (fst . fromJust . B8.readInt) . B8.words <$> B8.getLine

main :: IO ()
main = do
  n:q:_ <- getInts
  let bst = fromList $ replicate n mempty :: STree (Sum Int)
  loop bst q
  return ()
  where
    loop _ 0 = return ()
    loop !tree !cnt = do
      q : x : y :_ <- getInts
      if q == 0
        then do
        let tree' = update (mappend $ fromIntegral y) x tree
        loop tree' (cnt-1)
        else do
        print . getSum $ query (x,y) tree
        loop tree (cnt-1)

---------------------------------------------
--- Monoid instance for range minimum/maximum query
newtype Min = Min {fromMin :: Int32 } deriving (Ord,Show,Eq,Read)
instance Monoid Min where
  mempty = Min maxBound
  {-# INLINE mempty #-}
  mappend = min
  {-# INLINE mappend #-}

newtype Max = Max Int deriving (Ord,Show,Eq)
instance Monoid Max where
  mempty = Max minBound
  {-# INLINE mempty #-}
  mappend = max
  {-# INLINE mappend #-}

---------------------
--- Segment Tree
data STree v = Leaf {-# UNPACK #-}!Int !v
             | Branch {-# UNPACK #-}!(Int,Int) !v !(STree v) !(STree v)
             deriving (Show)
-- | make segment tree from [a_1 .. a_n] (1-based indexing)
fromList :: Monoid v => [v] -> STree v
fromList !xs = makeTree (1,(length xs)) xs
{-# INLINE fromList #-}

-- | make segment tree from [a_k .. a_l]
makeTree :: Monoid v => (Int,Int) -> [v] -> STree v
makeTree _ ![] = error "empty list"
makeTree (k,l) !es = loop $ map (uncurry f) (zip [k..l] es)
  where
    loop ![x] = x
    loop !xs = loop $ buildTree xs
    f :: Int -> v -> STree v
    f !ix !v = Leaf ix v
    buildTree !(a : b : ys) = let v = val a `mappend` val b
                          in Branch (left a,right b) v a b : buildTree ys
    buildTree !x = x

range :: STree v-> (Int,Int)
range !(Leaf r _) = (r,r)
range !(Branch r _ _ _) = r
{-# INLINE range #-}

val :: STree v -> v
val !(Leaf _ v) = v
val !(Branch _ v _ _) = v
{-# INLINE val #-}

left,right :: STree v -> Int
left (Main.range -> (l,_))  = l
right (Main.range -> (_,r)) = r
{-#INLINE left  #-}
{-#INLINE right #-}

-- | @query (l,r) t@ calculates @a_i <> .. <> a_r@. (@(<>)@ is equal to @mappend@)
-- O(log(n))
query :: Monoid v =>  (Int, Int) -> STree v -> v
query (ls,rs) t = loop t
  where
    loop (Leaf ix v)
      = if ls <= ix && ix <= rs then v else mempty
    loop (Branch (leftist,rightist) v lt rt)
      | rightist < ls || rs < leftist = mempty
      | ls <= leftist && rightist <= rs = v
       | otherwise = loop lt `mappend` loop rt
{-# SPECIALIZE query :: (Int,Int) -> STree (Sum Int) -> Sum Int #-}

-- | @update f ix tree@ makes tree whose @ix@-th element is updated by @f@.
-- O(log(n))
update :: Monoid v => (v -> v) -> Int -> STree v -> STree v
update f !ix !tree = loop tree
  where loop t = case t of
          Leaf i v -> if ix == i
                      then Leaf i (f v)
                      else tree
          Branch rng _ l@(Main.range -> (ll,lr)) r
            | ll <= ix && ix <= lr
              -> let l' = loop l
                 in Branch rng (val l' `mappend` val r) l' r
            | otherwise
              -> let r' = loop r
                 in Branch rng (val l `mappend` val r') l r'
-- | @add v ix tree@ is equal to @update (mappend v) ix tree.@
add :: Monoid v => v -> Int -> STree v -> STree v
add !v !ix !tree = loop tree
  where loop t = case t of
          Leaf i v0 -> if ix == i
                      then Leaf i (v `mappend` v0)
                      else tree
          Branch rng _ l@(Main.range -> (ll,lr)) r
            | ll <= ix && ix <= lr
              -> let l' = loop l
                 in Branch rng (val l' `mappend` val r) l' r
            | otherwise
              -> let r' = loop r
                 in Branch rng (val l `mappend` val r') l r'

-- | @insert ix v tree@ makes tree whose @ix@-th element is replaced by @v@.
-- O(log(n))
insert :: Monoid v => Int -> v -> STree v -> STree v
insert !ix !v !tree = loop tree
  where loop t = case t of
          Leaf i _ -> if ix == i
                      then Leaf i v
                      else tree
          Branch rng _ l@(Main.range -> (ll,lr)) r
            | ll <= ix && ix <= lr
              -> let l' = loop l
                 in Branch rng (val l' `mappend` val r) l' r
            | otherwise
              -> let r' = loop r
                 in Branch rng (val l `mappend` val r') l r'
```

# 補足

  仮定をモノイドから群に強めることで, 実装がより単純なデータ構造である
  Binary indexed tree を作ることができる. 
