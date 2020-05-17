---
title: "Data types a la carteのメモ2"
date: 2014-06-24
---

データ型の定義はできるようになったので、このデータ型を*評価*していく。
数式の評価とはなにか。各項についての*畳込み*のことである。

畳み込みというとリストに限定して考えてしまいがちだけど、ここで云う畳み込みは一般のデータ構造に対して言うことのできる畳み込みである。

どのように畳み込みを定義するか。コンストラクタごとにどのようにして値に落としこむかを記述すればいい。
以下のような2つのコンストラクタがあったとする。

```haskell
data Add e = Add e e
           deriving Functor
data Val e = Val Int
           deriving Functor
```

このAddとValからなる木を評価する場合、
以下のような関数があれば各コンストラクタについての評価はできる。

```haskell
class Functor f => Eval f where
  evalAlgebra :: (Functor f) => f Int -> Int
```
`Add` と `Val`　をこのクラスのインスタンスにすると、木の子供が評価されてた時に自身をどう評価すればいいかがわかる。
例えば, `evalAlgebra (Add 100 200) === 300` , `Val 12 === 12` と評価することができる。

`Fix (Add :+: Val)`のo評価の時はこれを再帰的に行わなければならない。
以下の関数を使って再帰的に評価をする。データ型の定義はできるようになったので、このデータ型を*評価*していく。
数式の評価とはなにか。各項についての*畳込み*のことである。

畳み込みというとリストに限定して考えてしまいがちだけど、ここで云う畳み込みは一般のデータ構造に対して言うことのできる畳み込みである。

どのように畳み込みを定義するか。コンストラクタごとにどのようにして値に落としこむかを記述すればいい。
以下のような2つのコンストラクタがあったとする。

```haskell
data Add e = Add e e   deriving Functor
data Val e = Val Int   deriving Functor
```

このAddとValからなる木を評価する場合、
以下のような関数があれば各コンストラクタについての評価はできる。

```haskell
class Functor f => Eval f where
  evalAlgebra :: (Functor f) => f Int -> Int
```
`Add` と `Val`　をこのクラスのインスタンスにすると、木の子供が評価されてた時に自身をどう評価すればいいかがわかる。
例えば, `evalAlgebra (Add 100 200) === 300` , `Val 12 === 12` と評価することができる。

`Fix (Add :+: Val)`のo評価の時はこれを再帰的に行わなければならない。
以下の関数を使って再帰的に評価をする。

```haskell
foldExpr :: Functor f => (f a -> a) -> Fix f -> a
foldExpr g (Fix t) = g (fmap (foldExpr g) t)
```
ここでの`g::(f a -> a)`は*Algebra*と呼ばれる。

定義を見ればわかる通り、数式の木`t`の子孫について畳み込みをして、その結果についてgを適用している。
これで`Fix f`の`f`さえfunctorになっていれば再帰的に畳み込みができる。

`Fix (Add :+: Val)`について畳み込みをするには,`Add :+: Val`がFunctorでなければならない。
なので `f :+: g` をFunctorにする。

```haskell
instance (Functor f,Functor g) => Functor (f :+: g) where
  fmap f (Inl l) = Inl $ fmap f l
  fmap f (Inr r) = Inr $ fmap f r
```

これで準備は整った。実際に評価部分を定義する。

```haskell
instance Eval Add where
  evalAlgebra (Add v1 v2) = v1 + v2
instance Eval Val where
  evalAlgebra (Val v) = v

eval :: Eval f => Fix f -> Int
eval t = foldExpr evalAlgebra t
```

これで畳込みが記述できた！


```haskell
foldExpr :: Functor f => (f a -> a) -> Fix f -> a
foldExpr g (Fix t) = g (fmap (foldExpr g) t)
```
ここでの`g::(f a -> a)`は*Algebra*と呼ばれる。

定義を見ればわかる通り、数式の木`t`の子孫について畳み込みをして、その結果についてgを適用している。
これで`Fix f`の`f`さえfunctorになっていれば再帰的に畳み込みができる。

`Fix (Add :+: Val)`について畳み込みをするには,`Add :+: Val`がFunctorでなければならない。
なので `f :+: g` をFunctorにする。

```haskell
instance (Functor f,Functor g) => Functor (f :+: g) where
  fmap f (Inl l) = Inl $ fmap f l
  fmap f (Inr r) = Inr $ fmap f r
```

これで準備は整った。実際に評価部分を定義する。

```haskell
instance Eval Add where
  evalAlgebra (Add v1 v2) = v1 + v2
instance Eval Val where
  evalAlgebra (Val v) = v

eval :: Eval f => Fix f -> Int
eval t = foldExpr evalAlgebra t
```

これで畳込みが記述できた！

Free Monadとの関連はまた次回
