---
title: "Data types a la carte のメモ"
date: 2014-06-07
---
以下の論文を読んだ内容を自分なりにまとめたメモ.
文章は適当なので後で書き直すかもしれない.

* [Data Types a la carte](http://www.cs.ru.nl/~W.Swierstra/Publications/DataTypesALaCarte.pdf)

目標：数式の木をつくる。
---------------
数式の木をつくる。この時に木を直接書くのではなく、中置記法のコンストラクタをサポートしたり、さらに以下で述べるような３つの目標を満たすようなコンストラクタを作る。

```haskell
-- <*>　はapplicative functor の ap ではない
--program1.hs--------
arithTree1 = val 100 <*> val 3 <+> val 2 --変更前
evalResult1 = eval arithTree -- evalResult == 302
---------------------
```

```haskell
--program2.hs---------
arithTree2 = val 100 <*> val 3 <+> sqrt (val 4) -- 変更後
evalResult2 = eval arithTree -- evalResult == 302
----------------------
```

設計の目標３つ

1. 計算があとから追加できるようにする
   あとから log,rootなどの四則演算以外の項を追加できるようにする
2. あとから計算を追加したとしても、evalの(Add,Mod,Mul)の評価部分は変更する必要のないようにする
3. program1は掛け算、足し算だけだったものに、sqrtを使えるようにしたprogram2をつくった。
   このときprogram1が動かなくなることのないようにする


計算をあとから追加できるようにするには
---------------------

組み立て可能なデータコンストラクタを定義する

```haskell
type BasicExpr = (Add :+: Val)
type NewExpr = (Add :+: Mul :+: Sqrt :+: Val)
```

* データコンストラクタを型のパラメータに取らせる
* データコンストラクタを組み合わせられるようにする
  (`a :+: b`みたいに書けるようにする)

DSLに新たにsqrtを追加すると、sqrtを表すデータコンストラクタが必要になる。
計算木に含まれるデータコンストラクタを組み込めるようにする。


型コンストラクタの組み合わせ
---------------------

型コンストラクタの直和を作る演算子`:+:`を用意する。
型パラメータeは後で説明するから今は気にしないでほしい

```haskell
data Add e = A e e
data Mul e = M e e
data Val e = V Int

infixl :<: 7
data (f :+: g) e = Inl (f e) | Inr (g e)
```


再帰型をつくるには？
---------------------

`MyExpr = Add MyExpr MyExpr | Val Int`
と同様の構造を作るとき、Addの子供にはMyExprが来てほしい。

再帰型を作るために型上の不動点演算子`Fix`を導入する。

```haskell
newtype Fix f = Fix (f (Fix f))
```

これを使うと4 * 2を表す構文木は以下のように書ける

```haskell
eight :: Fix (Mul :+: Val)
eight = Fix (Inl (Mul (Fix (Inr (Val 4))) (Fix (Inr (Val 2)))))
```

Fix,Inl,Inrを(心の中で)消して構造を見てみると、型の上で再帰構造になっているのがわかる。


```haskell
Mul (Val 4) (Val 2)
```

これで木構造が作れるようになった。

スマートコンストラクタ(1)
-------------------------

Inr とか Inl とか長ったらしくて書いてられない(あとFixも)
型から勝手に構造が決定されるコンストラクタをつくる。
こんな型をもったコンストラクタがあると嬉しい。

```haskell
val :: (Val :<: f, Functor f) => Int -> Expr f
(<+>) :: (Add :<: f, Functor f) => Expr f -> Expr f -> Expr f
```
(ここで`Val :<: f`は、[fにはValが含まれる]と読む)

つまり、完成した式の型に応じて自動的に適切な変換をするようなコンストラクタを作りたい。

型クラスをうまくつかってこれを実現する。


スマートコンストラクタ(2)
-------------------------

制約`sub :<: sup`が与えられた時,`sub a`を`sup a`にキャストすることが可能になる。
キャスト可能なことを型クラスとインスタンスで表現すると以下のようになる

```haskell
class (sub :<: sup) where
  inj :: sub a -> sup a

instance (Functor f) => f :<: f where
  inj x = x -- 反射律

instance (Functor f,Functor g) 
  => f :<: (f :+: g) where
  inj x = Inl x -- 左への埋め込み

instance (Functor f,Functor g,Functor h,f :<: h ) 
  => f :<: (g :+: h) where
  inj x = Inr (inj x) -- 右への埋め込み
```

不動点をとった型に埋め込むには以下の関数`inject`を定義すればいい

```haskell
inject :: (Functor f,Functor g,g :<: f) => g (Fix f) -> Fix f
inject = Fix . inj
```

上記の`inject`を使ってコンストラクタを定義すると以下のようになる。

```haskell
val :: (Val :<: f, Functor f) => Int -> Expr f
val x = inject (Val x)

(<+>) :: (Add :<: f, Functor f) => Expr f -> Expr f -> Expr f
x <+> y = inject (Add x y)
```

スマートコンストラクタはこれで完成。この式を評価する方法はまた次回
