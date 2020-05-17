---
title: "Data.Sequence と View Pattern"
date: 2014-01-05
draft: false
---

C++のdequeに対応するモジュールとして、haskellでは純粋な関数の中で使えるData.Sequenceというモジュールがあります。
内部でFinger Treeというデータ構造を使うことで、左右への要素の追加を償却定数時間で行うことができるようです。
要素の追加には (|>) や (<|) といった演算子を使うことになるのですが、この演算子はコンストラクタではないため、これを用いてパターンマッチをすることができません。
しかしghcにはview patternをいう文法拡張があり、これをつかってパターンマッチをさせることができることを知りました。
[http://www.kotha.net/ghcguide_ja/7.6.2/syntax-extns.html](http://www.kotha.net/ghcguide_ja/7.6.2/syntax-extns.html)

data.sequenceでは予めviewに使える関数が定義されているため自分で定義する必要はありません。

以下のコードでは left と right の定義に使っています。

```haskell
{-# LANGUAGE ViewPatterns #-}
module Main where
--seq.hs
import qualified Data.Sequence as Sq
import Data.Sequence (ViewL(..),ViewR(..),(><),(<|),Seq)

main :: IO ()
main = do
  let x = Sq.empty :: Sq.Seq Int
  print x
  let x2 = 1 <| x
  print x2
  let x3 = x2 >< Sq.fromList [1..10]
  print x3
  print (right x3)
  print (left x3)

left :: Seq a -> a
left (Sq.viewl -> (l :< _)) = l

right :: Seq a -> a
right (Sq.viewr -> (_ :> r)) = r
```

実行結果
-------

```shell
% runhaskell seq.hs
fromList []
fromList [1]
fromList [1,1,2,3,4,5,6,7,8,9,10]
10
1
```

今年はせめて人並みにhaskellが使えるようになりたいです。精進します。


追記
----

パターンガードを使えばガード分の中でパターンマッチが行えます。例えば、

```haskell
left :: Seq a -> a
left (Sq.viewl -> (l :< _)) = l
```
は

```haskell
left :: Seq a -> a
left v | (l :< _) <- Sq.viewl v = l
```
と書けます。
