---
title: "Thread Local Storage について"
date: 2020-05-17
draft: false
tags: ["C++", "libc", "Thread Local Storage"]
---
# Thread Local Storage

## Contents
- [Thread Local Storage](#thread-local-storage)
  - [Contents](#contents)
  - [Reference](#reference)
- [Thread Local Storage](#thread-local-storage-1)
  - [生成されるコードの確認 (Aarch64)](#生成されるコードの確認-aarch64)
    - [普通にコンパイル](#普通にコンパイル)
    - [PIC でコンパイル](#pic-でコンパイル)

## Reference
* https://android.googlesource.com/platform/bionic/+/HEAD/docs/elf-tls.md
* [ELF Handling For Thread-Local Storage](https://www.uclibc.org/docs/tls.pdf)
* http://shinh.hatenablog.com/entry/20130527/1369581821

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

0000000000000000 tls_func():
       0: 48 d0 3b d5                  	mrs	x8, TPIDR_EL0
       4: 08 01 40 91                  	add	x8, x8, #0, lsl #12q
		0000000000000004:  R_AARCH64_TLSLE_ADD_TPREL_HI12	tls_value
       8: 08 01 00 91                  	add	x8, x8, #0
		0000000000000008:  R_AARCH64_TLSLE_ADD_TPREL_LO12_NC	tls_value
       c: 00 01 40 b9                  	ldr	w0, [x8]
      10: c0 03 5f d6                  	ret

Disassembly of section .text._ZTW9tls_value:

0000000000000000 thread-local wrapper routine for tls_value:
       0: 48 d0 3b d5                  	mrs	x8, TPIDR_EL0
       4: 08 01 40 91                  	add	x8, x8, #0, lsl #12
		0000000000000004:  R_AARCH64_TLSLE_ADD_TPREL_HI12	tls_value
       8: 00 01 00 91                  	add	x0, x8, #0
		0000000000000008:  R_AARCH64_TLSLE_ADD_TPREL_LO12_NC	tls_value
       c: c0 03 5f d6                  	ret
```

### PIC でコンパイル

```sh
% clang++ --target=aarch64-linux-none-gnu -c tls.cpp -fPIC
% llvm-objdump -d tls.o --demangle
```

```
Disassembly of section .text:

0000000000000000 tls_func():
       0: fd 7b bf a9                  	stp	x29, x30, [sp, #-16]! // スタックポインタ, フレームポインタの退避
       4: fd 03 00 91                  	mov	x29, sp // x29 <- sp
       8: 00 00 00 90                  	adrp	x0, #0  // ページ先頭のアドレス？
		0000000000000008:  R_AARCH64_TLSDESC_ADR_PAGE21	tls_value
       c: 01 00 40 f9                  	ldr	x1, [x0] // x1 <- [x0]
		000000000000000c:  R_AARCH64_TLSDESC_LD64_LO12	tls_value
      10: 00 00 00 91                  	add	x0, x0, #0 // x0 <- x0 + 0
		0000000000000010:  R_AARCH64_TLSDESC_ADD_LO12	tls_value
      14: 20 00 3f d6                  	blr	x1
		0000000000000014:  R_AARCH64_TLSDESC_CALL	tls_value
      18: 48 d0 3b d5                  	mrs	x8, TPIDR_EL0
      1c: 00 69 60 f8                  	ldr	x0, [x8, x0]
      20: fd 7b c1 a8                  	ldp	x29, x30, [sp], #16 // スタックポインタ, フレームポインタの復元
      24: c0 03 5f d6                  	ret

Disassembly of section .text._ZTW9tls_value:

0000000000000000 thread-local wrapper routine for tls_value:
       0: fd 7b bf a9                  	stp	x29, x30, [sp, #-16]! // 16-byte Folded Spill
       4: fd 03 00 91                  	mov	x29, sp               // フレームポインタを更新
       8: 00 00 00 90                  	adrp	x0, #0
		0000000000000008:  R_AARCH64_TLSDESC_ADR_PAGE21	tls_value
       c: 01 00 40 f9                  	ldr	x1, [x0]
		000000000000000c:  R_AARCH64_TLSDESC_LD64_LO12	tls_value
      10: 00 00 00 91                  	add	x0, x0, #0
		0000000000000010:  R_AARCH64_TLSDESC_ADD_LO12	tls_value
      14: 20 00 3f d6                  	blr	x1
		0000000000000014:  R_AARCH64_TLSDESC_CALL	tls_value // リンカーへのヒント？
      18: 48 d0 3b d5                  	mrs	x8, TPIDR_EL0
      1c: 00 01 00 8b                  	add	x0, x8, x0
      20: fd 7b c1 a8                  	ldp	x29, x30, [sp], #16 // スタックポインタ, フレームポインタの復元
      24: c0 03 5f d6                  	ret
```

* `tpidr_el0` : スレッド固有の情報を保持するレジスタ


LLD のコードを見ると,  AArch64::relaxTlsGdToLe で最適化をしている.
```cpp
void AArch64::relaxTlsGdToLe(uint8_t *loc, const Relocation &rel,
                             uint64_t val) const {
  // TLSDESC Global-Dynamic relocation are in the form:
  //   adrp    x0, :tlsdesc:v             [R_AARCH64_TLSDESC_ADR_PAGE21]
  //   ldr     x1, [x0, #:tlsdesc_lo12:v  [R_AARCH64_TLSDESC_LD64_LO12]
  //   add     x0, x0, :tlsdesc_los:v     [R_AARCH64_TLSDESC_ADD_LO12]
  //   .tlsdesccall                       [R_AARCH64_TLSDESC_CALL]
  //   blr     x1
  // And it can optimized to:
  //   movz    x0, #0x0, lsl #16
  //   movk    x0, #0x10
  //   nop
  //   nop
...
```
