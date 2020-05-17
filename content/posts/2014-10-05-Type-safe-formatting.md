---
title: "Type safe Formatting"
date: 2014-10-05
tags: ["Haskell"]
---

めも
------
[Type-safe functional formatted IO](http://okmij.org/ftp/typed-formatting/)
を読んだ際のメモ.型でラムダ式っぽい事ができるのは便利。gadtの証人の型を作るのに役立ちそう


要約
-------

printfやscanfのフォーマット文字列を実装する。OCamlのprintfのような特殊な型付をせず type safe なインターフェースを実装したい。
フォーマット文字列から型付けを行うのは無理なので(Template Haskellで頑張れば話は別)、DSLとして実装して適切な型がつくようにする。

最終的に次のように書けるようになる.

```haskell
let hoge = lit "PiyoPiyo " ^ char ^ lit " fuga" ^ int
result  = sprintf hoge 'a' 100
-- result == "PiyoPiyo a 100"
{-
bad_result = sprintf hoge 300 'a'
-- これは型エラー
-}
```

the initial view
----------------

データ型`F a b`を考える。ここで、`F`は`F :: * -> * -> *`、つまり2引数の型コンストラクタ。
ここで型変数`b`は型変数`a`の1回以上の出現を含む型だとすると、Fは型レベルのラムダ式とみなせる。
この関数に引数を適用するにはFの第1引数`a`を適用したい型`x`でunifyしてやればよい。
すると`b`の中の`a`の出現が`x`で置き換えられ,関数適用の結果になる。
これはPrologでのラムダ式の挙動をエミュレートしているらしい。

```haskell
-- F a b を型での関数抽象,つまり(λa. b)とみなす
-- 型レベルid
x = lit "hoge" :: F a a

-- a の先頭に(Int ->)を付け加える演算
y = int 10     :: F a (Int -> a)

-- a の先頭に(Char ->)を付け加える演算
z = char 'c'   :: F a (Char -> a)

-- "p ^ q" はpの型関数にqの型関数を適用することを表す。ラムダ式でいう簡約
(^) :: F b c -> F a b -> F a c

-- このとき、x^yとy^zはそれぞれ次のような型になる。
x ^ y :: F a (Int -> a)
y ^ z :: F a (Int -> (Char -> a))
```

型が決まるまでの流れ(記法は適当)
---------

```
x ^ y ::
(F b c -> F a b -> F a c) (F a a) (F a (Int -> a))
= (\@b1 @c1 => (F a1 b1) -> F a1 c1) @a @a (F a (Int -> a))
= ((F a1 a) -> (F a1 a)) (F a (Int -> a))
= \@k1 @k2 -> (F k1 k2) $ @a @(Int->a)
= F a (Int -> a)
```

```
y^z :: (F b c -> F a b -> F a c) (F a (Char -> a)) (F a (Int -> a))
= \@t1 @t2 => (F a1 t1 -> F a1 t2) @a @(Char -> a) (F a (Int -> a))
= (F a1 a -> F a1 (Char -> a)) (F a (Int -> a))
= \@t1 @t2 => (F t1 (Char -> t2)) @a @(Int -> a)
= F a (Char -> Int -> a)
```

final view
----------
initial viewの双対(数学的な意味での双対じゃないっぽい？)。
initial viewではデータ型として宣言していたEDSLだったが、ここでは関数として書いている。
ユーザーからは与えられたコンビネータを使っている限り違いを意識できない。
こっちはデータ型を作らないでその場で結果を構築する。

Initial viewからFinal viewは自動変換が可能らしい。

その他
--------
Template haskellであれこれしたり、といった話が続く。

あとで読みたい
------------

* [Functional unparsing](http://www.brics.dk/RS/98/12/BRICS-RS-98-12.pdf)
* [On Typing Delimited Continuations:Three New Solutions to the Printf Problem](http://pllab.is.ocha.ac.jp/~asai/papers/tr08-2.pdf)
* [Formatting: a class act](http://www.cs.ox.ac.uk/ralf.hinze/publications/Format.ps.gz)

参考にしたもの
------------

* [More Logic More Types](http://www.math.nagoya-u.ac.jp/~garrigue/papers/mlmt.pdf)

メモ
------
* sprintf は polyvariadic .. 任意の個数の任意の型を持った引数をとれる。
* sscanf は partial function .. 引数によっては結果が無い関数になる
