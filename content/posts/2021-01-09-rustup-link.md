---
title: 自前でビルドした rustc を rustup に関連付ける
date: 2021-01-09
draft: true
---

# 自前でビルドした rustc を rustup に関連付ける

新規に rust のターゲットを追加するときのビルド手順とターゲットの関連づけ方法についてのメモ。

## Rust ツールチェインのビルドまで

ソースコードからビルドする。ビルド手順は公式の方法に従う。ほげ

* https://github.com/rust-lang/rust#building-on-a-unix-like-system

```bash
$ git clone  https://github.com/rust-lang/rust.git
$ cd rust
$ cp config.toml.example config.toml
```

今回は aarch64 ターゲットが必要なので、 `config.toml` を編集する。

```bash
$ emacs config.toml
$ diff config.toml config.toml.example
186c186
< target = ["x86_64-pc-windows-msvc", "aarch64-unknown-none"]
---
> #target = ["x86_64-unknown-linux-gnu"]
```



ビルド設定を変更後、ビルドを行う。

```bash
$ ./x.py build
```



## rustup から利用可能にする

`rustup toolchain link` コマンドを使用する。

```bash
$ rustup toolchain link local ./build/x86_64-pc-windows-msvc/stage1
```



## 参考

1. [Rustコンパイラの自前ビルド](https://qnighy.hatenablog.com/entry/2017/06/16/220000)
2. [Rustコンパイラのデバッグログを取得する](https://qiita.com/tatsuya6502/items/2781132848229afb958e)
3. https://github.com/termoshtt/accel
4. https://users.rust-lang.org/t/solved-cross-compile-to-aarch64-from-windows10/22731