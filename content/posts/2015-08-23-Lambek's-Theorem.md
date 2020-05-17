---
title: Lambek's theorem
date: 2015-08-23
---

* $C :$ 圏
* $F : C \rightarrow C$ (自己函手)

##定理 Lambek's theorem

$F$ が initial algebra $$\alpha : F(X)
\rightarrow X$$を持つとき, $\alpha$ は $X$ と $F(X)$ の isomorphism に
なる

## 証明
$F(\alpha): F(F(X)) \rightarrow F(X)$ は $F(X)$ を carrier とすると algebra になる.
$\alpha$ が initial であることより、以下の図を可換にする $i : X \rightarrow F(X)$ が唯一存在する. ($\alpha$ から $F(\alpha)$ へ algebra を移す射)

$$
\require{AMScd}
\begin{CD}
F(X)         @>{F(i)}>>     F(F(X))\\
@V{\alpha}VV             @VV{F(\alpha)}V\\
X            @>{i}>>     F(X)
\end{CD}
$$

この$i$が$\alpha^{-1}$であることを示す.

まず $\alpha \circ i : X \rightarrow X$ を考える.

$$
\require{AMScd}
\begin{CD}
F(X)         @>{F(i)}>>     F(F(X))       @>{F(\alpha)}>> F(X)\\
@VV{\alpha}V             @VV{F(\alpha)}V       @VVV     \\
X            @>{i}>>        F(X)          @>{\alpha}>> X
\end{CD}
$$


これは algebra $(\alpha,X)$ から $(\alpha,X)$ への morphism になる.
そのため $(\alpha,X)$ が始代数であることから, $\alpha \circ i = id_X$となる.


次に $i \circ \alpha$ について考えるため,最初の可換図式にもどる.
$$
\require{AMScd}
\begin{CD}
F(X)         @>{F(i)}>>     F(F(X))\\
@V{\alpha}VV             @VV{F(\alpha)}V\\
X            @>{i}>>     F(X)
\end{CD}
$$
上の図が可換なため、 $i \circ \alpha = F(\alpha) \circ F(i)$ である.
そのため,
$$
\begin{eqnarray}
i \circ \alpha
& = & F(\alpha) \circ F(i) \\
& = & F(\alpha \circ i) \\
& = & F(id_X) \\
& = & id_{F(X)}
\end{eqnarray}
$$

以上より, $i \circ \alpha = id$, $\alpha \circ i = id$ なので $i = \alpha^{-1}$ が導けた.

## 自分なりの解釈
- $F(X) = X$ の解とみなすと, $X$ は $F$ の最小不動点だと思える
- 始代数が存在するとしたらそのキャリアは最小不動点と一致する（?）