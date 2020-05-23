---
title: "Thread Local Storage について"
date: 2020-05-17
draft: true
tags: ["C++", "libc", "Thread Local Storage"]
---

# Thread Local Storage

[ELF Handling For Thread-Local Storage](https://www.uclibc.org/docs/tls.pdf) を読んでいます.
内容の理解のために, [shinh さんの記事](http://shinh.hatenablog.com/entry/20130527/1369581821)を参考に実験をしました

Thread Local Storage (TLS) はスレッドごとに異なる状態を持つ記憶域. C++11/C11 から標準言語仕様として導入された.
https://cpprefjp.github.io/lang/cpp11/thread_local_storage.html

TLS 管理領域を指しているレジスタを１つ作り, スレッドごとに異なる領域へのポインタをもたせることで実現している, というのはおおまかにきいたことがあるのですが,
具体的にどうなっているのかを調べます.
(特に厄介そうな終了処理周りを重点的に読んでいく)

## 生成されるコードの確認 (Aarch64)

具体的なコードをコンパイルして, どのようなコードが生成されるか見てみる.
```c++
// tls.cpp
thread_local int tls_value;
int tls_func() {
    return tls_value;
}
```

### 普通にコンパイル

```zsh
% clang++ --target=aarch64-linux-none-gnu -c tls.cpp
% llvm-objdump -d tls.o --demangle -r # -r はリロケーションエントリの表示
```

```txt
Disassembly of section .text:

0000000000000000 _Z8functionv:
       0: 48 d0 3b d5                   mrs     x8, TPIDR_EL0
       4: 08 01 40 91                   add     x8, x8, #0, lsl #12
                0000000000000004:  R_AARCH64_TLSLE_ADD_TPREL_HI12       global
       8: 08 01 00 91                   add     x8, x8, #0
                0000000000000008:  R_AARCH64_TLSLE_ADD_TPREL_LO12_NC    global
       c: 09 01 40 b9                   ldr     w9, [x8]
      10: 29 05 00 11                   add     w9, w9, #1
      14: 09 01 00 b9                   str     w9, [x8]
      18: 00 01 40 b9                   ldr     w0, [x8]
      1c: c0 03 5f d6                   ret

Disassembly of section .text._ZTW6global:

0000000000000000 _ZTW6global:
       0: 48 d0 3b d5                   mrs     x8, TPIDR_EL0
       4: 08 01 40 91                   add     x8, x8, #0, lsl #12
                0000000000000004:  R_AARCH64_TLSLE_ADD_TPREL_HI12       global
       8: 00 01 00 91                   add     x0, x8, #0
                0000000000000008:  R_AARCH64_TLSLE_ADD_TPREL_LO12_NC    global
       c: c0 03 5f d6                   ret
```

ぶっちゃけよくわからない.
```
4: 08 01 40 91                   add     x8, x8, #0, lsl #12
```
これは `x8 = x8 + #0 << 12` という意味だけど, 無意味なコードになっている. 

### PIC でコンパイル

```sh
% clang++ --target=aarch64-linux-none-gnu -c tls.cpp -fPIC
% llvm-objdump -d tls.o --demangle
```

```
Disassembly of section .text:

0000000000000000 <tls_func()>:
   0:   a9bf7bfd        stp     x29, x30, [sp, #-16]!　// スタックポインタ, フレームポインタの退避
   4:   910003fd        mov     x29, sp                // x29 <- sp
   8:   90000000        adrp    x0, 0 <tls_func()>     // ページ先頭のアドレス？
   c:   f9400001        ldr     x1, [x0]               // x1 <- [x0]
  10:   91000000        add     x0, x0, #0x0           // x0 <- x0 + 0
  14:   d63f0020        blr     x1
  18:   d53bd048        mrs     x8, tpidr_el0
  1c:   b8606900        ldr     w0, [x8, x0]
  20:   a8c17bfd        ldp     x29, x30, [sp], #16　// スタックポインタ, フレームポインタの復元
  24:   d65f03c0        ret

Disassembly of section .text._ZTW3tls:

0000000000000000 <TLS wrapper function for tls>:
   0:   a9bf7bfd        stp     x29, x30, [sp, #-16]!                // スタックポインタ, フレームポインタの退避
   4:   910003fd        mov     x29, sp                              // sp を x29 にコピー
   8:   90000000        adrp    x0, 0 <TLS wrapper function for tls> 
   c:   f9400001        ldr     x1, [x0]
  10:   91000000        add     x0, x0, #0x0
  14:   d63f0020        blr     x1
  18:   d53bd048        mrs     x8, tpidr_el0
  1c:   8b000100        add     x0, x8, x0
  20:   a8c17bfd        ldp     x29, x30, [sp], #16 // スタックポインタ, フレームポインタの復元
  24:   d65f03c0        ret
```

* `tpidr_el0` : スレッド固有の情報を保持するレジスタ
