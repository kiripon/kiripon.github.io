---
title: 自前でビルドした rustc を rustup に関連付ける
date: 2021-01-06
draft: true
---

# 自前でビルドした rustc を rustup に関連付ける

## Rust ツールチェインのビルド

ソースコードからビルドする。ビルド手順は公式の方法に従う。

* https://github.com/rust-lang/rust#building-on-a-unix-like-system

```
$ git clone  https://github.com/rust-lang/rust.git
$ cd rust
$ cp config.toml.example config.toml
$ ./x.py build
```



## 参考

1. [Rustコンパイラの自前ビルド](https://qnighy.hatenablog.com/entry/2017/06/16/220000)
2. [Rustコンパイラのデバッグログを取得する](https://qiita.com/tatsuya6502/items/2781132848229afb958e)