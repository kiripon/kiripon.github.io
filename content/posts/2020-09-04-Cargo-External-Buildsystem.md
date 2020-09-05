---
title: "外部ビルドシステムに Cargo プロジェクトを組み込む"
date: 2020-09-04T21:52:08+09:00
draft: false
tags: ["Rust", "Makefile", "Cargo"]
---

Make から Cargo を叩いてビルドする、というのは結構よくやられているけれど、ちゃんとしたMakefile を書くのは結構難しいです。
外部ビルドシステムから cargo プロジェクトが管理するファイルの依存関係を追跡する方法

## 参考にした資料

## Cargo の解析したソースコードの依存関係を見る

`cargo build` コマンドで実行ファイルやライブラリのビルドを行うと、 cargo はそのビルドの依存関係をファイルに出力してくれます。

例えば `foo` という cargo プロジェクトを作ってビルドすると、成果物と同じディレクトリに `foo.d` というファイルが作られます.
```shell
% cargo new foo
     Created binary (application) `foo` package
% cd foo
% cargo build
   Compiling foo v0.1.0 (/path/to/foo)
    Finished dev [unoptimized + debuginfo] target(s) in 3.28s
% ls target/debug/
build       deps        examples    foo         foo.d       foo.dSYM    incremental
```

このファイルは [dep-info](https://doc.rust-lang.org/cargo/guide/build-cache.html#dep-info-files) 
ファイルと呼ばれ、外部ビルドシステムからファイルの依存関係を追跡するために生成されるファイルです。


ファイルの中には生成したファイルとその依存関係の対応が書かれています。

```makefile
/path/to/foo/target/debug/foo: /path/to/foo/src/main.rs
```

今回は GNU Make を外部ビルドシステムとして使用し、このファイルを Makefile に `include` することで cargo でビルドしたファイルの依存関係の追跡を行います。


## Makefile の書き方

### dep-info を相対パスで出力する

Cargo の生成する dep-info は, デフォルトでは絶対パスで出力されます.
Makefile のルール間依存はファイルパスベースではなく文字列ベースで判定されます.
そのため、ビルドルール内で相対パスと絶対パスが混在しているとルール間で依存があると判定されません.

これを避けるために、dep-info のファイルパスが相対パスになるように設定を変更します.
以下の設定を `foo/.cargo/config.toml` に追記します

```yaml
[build]
dep-info-basedir = "."
```

`target/debug/foo.d` 中のファイルパスが全て相対パスになります.

```makefile
target/debug/foo: src/main.rs
```

### Makefile から dep-info を読み込む

Makefile からファイルを読み込むには `-include` ディレクティブを使用します. (参照: https://www.gnu.org/software/make/manual/html_node/Include.html)

初回ビルド時には dep-info ファイルは存在しないため、`include` ではなく `-include` を使用します.
`include` とは異なり, `-include` は対象のファイルが存在しなかった場合は無視をします.

```makefile
-include target/debug/foo.d
```

### Makefile 全体

以上のことに気をつけて Makefile を書くと, 以下のようになります.

```makefile
.PHONY: all clean 

all: target/release/foo

target/debug/foo:
	cargo build

target/release/foo:
	cargo build --release

-include target/debug/foo.d
-include target/release/foo.d

clean:
	cargo clean
```

