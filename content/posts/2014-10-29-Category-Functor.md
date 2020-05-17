---
title: 圏論のノート2:Functor
date: 2014-10-29
---

<!--

> {-# LANGUAGE MultiParamTypeClasses ,TypeFamilies,FlexibleInstances#-}
> module CategoryFunctor where
> import Prelude()

-->

Haskellではもっぱらコンテナを表すのに使われるFunctorについて。

定義:Functor
===========

$C$と$D$を圏とする。
$F:C \rightarrow D$について,以下を満たす/表す時,Fを**関手(functor)**と呼ぶ.

1. 圏Cのobject $A$ から圏Dのobject $B$ への対応 $F:A \rightarrow F(A)$
2. 圏Cの射 $f:A \rightarrow B$ から圏Dの射 $F(f):F(A) \rightarrow F(B)$への対応
3. 任意のCのobject $A$とCのarrow $f$,$g$について
    * $F(id_A) = id_{F(A)}$
    * $F(g \circ f) = F(g) \circ F(f)$

例:HaskellのFunctor
--------

haskellの圏Haskでは、型がobject,関数がarrowである。
Functor型クラスを見てみる。

> class Functor f where

型`a`から型`f a`を構成するので,`f`がobjectの対応をとるものである。また、

>   fmap :: (a -> b) -> f a -> f b

は、型`(a -> b)`から型`(f a -> f b)`を構築する。これがarrowの対応になっている。
結合則は表現できないので、プログラマが保証しなければならない。


例:忘却関手(forgetful functor)
----------------------------

モノイド$(M,\bullet,e)$は、Mの要素をarrowとする唯一のobjectを持つ圏になる。
このモノイド圏から集合の圏への対応を考える。\
モノイド$(M,\bullet ,e)$に集合$M$を対応させ、
モノイド準同型 $h:(M, \bullet ,e) \rightarrow (M', \bullet' ,e')$
を、写像 $h:M \rightarrow M'$
に対応させると,$F:Mon \rightarrow Set$は関手になる。

__疑問__\
Functor型クラスだけではHask上のすべてのFunctorを表現できていないように見えた\
たとえば,関手Fについて、型の対応を`F(a) = a`,関数の対応を`F(f) -> f`とすればこれも関手になる。\
Functor型クラスは特定の種類の関手についてだけについて言及しているのかもしれない。
誰か詳しい人に教えてもらいたいです。

11/29追記
isomorphismを介して一意に定まるから、`F = Identity`としていい(?)

参考文献
-------

* [圏と関手入門](http://www.math.nagoya-u.ac.jp/~hasimoto/paper/class/cat10.pdf)
* [Basic Category Theory for Computer Scientists](http://mitpress.mit.edu/books/basic-category-theory-computer-scientists)
* [Wikibooks:Haskell/圏論](http://ja.wikibooks.org/wiki/Haskell/%E5%9C%8F%E8%AB%96)
