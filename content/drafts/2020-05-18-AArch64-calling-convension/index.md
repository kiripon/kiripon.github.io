---
title: "AArch64 の呼び出し規約についてのメモ(WIP)"
date: 2020-05-18T01:12:17+09:00
draft: true
---

- [関数呼び出し規約](#%e9%96%a2%e6%95%b0%e5%91%bc%e3%81%b3%e5%87%ba%e3%81%97%e8%a6%8f%e7%b4%84)
  - [スタックポインタ](#%e3%82%b9%e3%82%bf%e3%83%83%e3%82%af%e3%83%9d%e3%82%a4%e3%83%b3%e3%82%bf)
  - [リンクレジスタ](#%e3%83%aa%e3%83%b3%e3%82%af%e3%83%ac%e3%82%b8%e3%82%b9%e3%82%bf)
  - [フレームポインタ](#%e3%83%95%e3%83%ac%e3%83%bc%e3%83%a0%e3%83%9d%e3%82%a4%e3%83%b3%e3%82%bf)
  - [プラットフォームレジスタ](#%e3%83%97%e3%83%a9%e3%83%83%e3%83%88%e3%83%95%e3%82%a9%e3%83%bc%e3%83%a0%e3%83%ac%e3%82%b8%e3%82%b9%e3%82%bf)
- [命令](#%e5%91%bd%e4%bb%a4)
  - [STP](#stp)
- [ARM Stack](#arm-stack)
- [コマンドのメモ](#%e3%82%b3%e3%83%9e%e3%83%b3%e3%83%89%e3%81%ae%e3%83%a1%e3%83%a2)


https://developer.arm.com/docs/ihi0055/d/procedure-call-standard-for-the-arm-64-bit-architecture

# 関数呼び出し規約

| レジスタ | caller save/callee save |       用途        |
| -------- | ----------------------- | ----------------- |
| sp       | n/a                     | Stack Pointer     |
| r30      | callee save             | Link Register     |
| r29      | callee save             | Frame Pointer     |
| r19..r28 | callee save             |                   |
| r18      |                         | Platform Register |


## スタックポインタ
スタックのアドレスを格納する

## リンクレジスタ
関数のリターンアドレスを保存する. 

## フレームポインタ
関数内のローカル変数などの格納領域アドレスを格納する

## プラットフォームレジスタ
プラットフォーム依存の特殊用途レジスタ            

# 命令

## STP 
レジスタペアストア命令.
ベースレジスタの値と即値のオフセットからアドレスを計算し, 2つの 32bit ワード, または 64bit ダブルワードを計算したアドレスに保存する.

- スタックポインタはどうなる?

# ARM Stack
ヒープはアドレスの大きい方向に伸びていき, 
スタックはアドレスの小さい方向に向けて伸びていく.


# コマンドのメモ

* ldd
依存している動的ライブラリを表示する.

* adrp
PC からの相対ページアドレスの計算を行う. (4kb アライン)

ラベルで指定したアドレスをレジスタに書き込む. 
プログラムカウンタ(PC) に 4GB の範囲のオフセットを加えたアドレスを (4kbを単位とした)ページアドレスに変換して, 指定したレジスタに書き込みます.

`adrp xd, label` と書くので, label のアドレスを直接 xd に転送しているように見えるけど見た目だけで、コンパイル時にアセンブラが PC に対してオフセットを加えるように書き換える.
