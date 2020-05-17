---
title: "Haskellで最短経路"
date: 2014-07-02
---
#Haskellでダイクストラ法を書く

最近Dijkstra法を教える機会があったので、せっかくなのでHaskellでも書きました。
containersにPriority-queueが見つからなかったのでSetで代用してます。
ガリガリ副作用を使ってるので関数型っぽくはないです。

標準ライブラリだけを使ってるからcodeforcesでも使えるはず。
[AOJ:GRL_1_A](http://judge.u-aizu.ac.jp/onlinejudge/description.jsp?id=GRL_1_A)にあわせて書いてあります。

```haskell
import Control.Monad
import Control.Monad.State
import Control.Applicative
import Data.Array
import Data.Array.ST
import Data.Array.IO
import qualified Data.Set as S
import Data.Traversable
type Dist = Int
type Node = Int
type Graph = Array Node [(Node,Dist)]

getInput :: IO (Graph,Node)
getInput = do
  v:e:r:_ <- map read . words <$> getLine :: IO [Int]
  a <- newArray (0,v-1) [] :: IO (IOArray Node [(Node,Dist)])
  replicateM_ e $ do
    s:t:d:_ <- map read . words <$> getLine :: IO [Int]
    es <- readArray a s
    writeArray a s ((t,d):es)
  a' <- freeze a
  return (a',r)

dijkstra :: Graph -> Node -> Array Node Dist
dijkstra g s = runSTArray $ do
  a <- newArray bound maxBound
  evalStateT (loop a) (S.singleton (0,s))
  return a
  where
    bound = bounds g
    loop a = do
      isEmpty <- S.null <$> get
      unless isEmpty $ do
      (d,n) <- state S.deleteFindMin
      d' <- lift $ readArray a n
      when (d < d') $ do
        lift $ writeArray a n d
        let nexts = g ! n
        void $ for nexts $ \(ix,w) -> do
          d'' <- lift $ readArray a ix
          when (d + w < d'')
            $ modify $ S.insert (d+w,ix)
      loop a

    
main :: IO ()
main = do
  (g,r) <- getInput
  print g
  print $ dijkstra g r
```
