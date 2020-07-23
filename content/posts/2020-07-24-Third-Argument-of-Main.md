---
title: "main 関数の第三引数"
date: 2020-07-24
tags: ["libc", "C"]
---

# 要約

 [C11の規格](http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1548.pdf) 上では、 `main()` 関数に使用できるシグネチャとして言及されているものは、以下の２通りがある。

```c++
int main();
int main(int argc, char** argv);
```

規格ではこの２つに加えて、環境依存のエントリーポイントの用意を許容している。
実際に Microsoft Windows では, 追加で環境変数の配列へのポインタを引数に持つ `main()` 関数の定義が許されている。

```c++
main( int argc, char *argv[ ], char *envp[ ] )
```
詳細は https://docs.microsoft.com/ja-jp/cpp/c-language/main-function-and-program-execution?view=vs-2019 を参照.

## 調査の動機
LLVM のlibc を読んでいたら、main 関数が見慣れないシグネチャで宣言されていた。

https://github.com/llvm/llvm-project/blob/master/libc/loader/linux/x86_64/start.cpp#L15
```c++
extern "C" int main(int, char **, char **);
```

`main()` 関数は引数なし版と `argc` と `argv` の2引数をとる版の２通りしか知らなかったので、この宣言が何を意味しているのかを調査した。

## 調査の方法

"c main third argument" でググったら↓が見つかった。
https://stackoverflow.com/questions/10321435/is-char-envp-as-a-third-argument-to-main-portable
C の規格で実装依存のエントリーポイントを用意することが許されているらしい。

### 規格書での規定
C11 のドラフトを参照したところ、 5.1.2.2.1 に書いてあった。
```
5.1.2.2.1 Program startup
1 The function called at program startup is named main. The implementation declares no prototype for this function. It shall be defined with a return type of int and with no parameters:
...
or with two parameters 
... or in some other implementation-defined manner.
```
http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1548.pdf

### 実装されている環境

* glibc, llvm-libc, musl のどの libc もこの実装をサポートしていた。
* Windows でもサポートされている
  * https://docs.microsoft.com/ja-jp/cpp/c-language/main-function-and-program-execution?view=vs-2019 を参照.

Windows を参考に `argp` には環境変数の文字列配列へのポインタが格納されているらしい。
以下のコードでの動作を確認したところ、確かに環境変数が出力された。

```c++
int main(int argc, char** argv, char** envp)
{
    for(int i = 0; envp[i] != nullptr; ++i)
    {
        printf("%s¥n", envp[i]);
    }
}
```

