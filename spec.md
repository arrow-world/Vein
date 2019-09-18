# 作るもの
- Core Library
- AFRP Compiler
- FRP Arrowizer

---

# Core Library
Veinの核となるライブラリ.
信号型と原始的な信号関数を定義する方法を提供する.

`compile : SF -> Port -> LLVM`関数は

## 信号型の定義手法
信号型の定義は, 型クラス`SignalType`を実装するデータ型を定義することで成される.
- `SignalType`の実装によって信号型の挙動を定義する
- データ型の値は, 単に信号型の型パラメタの情報のみをもつ

## 原始的な信号関数(Primitive Signal Function, PSF)の定義手法
PSFの定義は, 型クラス`PrimitiveSignalFunction`を実装するデータ型を定義することで成される
- `PrimitiveSignalFunction`の実装によってPSFの挙動を定義する
- データ型の値は, 単に信号関数のパラメタの情報のみをもち, 関数の状態などはもたない

--- 

# AFRP Compiler
AFRPスタイルの信号関数とSource/Sinkとの接続情報を与えられて, LLVMにコンパイルする.
llvm-hsを用いる予定.

--- 

# FRP Arrowizer
CFRPスタイルのEDSLソースコードをホスト言語上の~~AFRPスタイルの信号関数~~Monoidal圏の射へ翻訳するライブラリ.
ホスト言語はHaskell. Template HaskellによってHaskellのソースコードにEDSLを埋め込む.

## 基本機能
隠れ入力/隠れ出力のない(カリー化されていない, 入出力がタプル状の)ラムダ抽象を, ~~AFRPスタイルの信号関数~~Monoidal型クラスによるMonoidal圏の射に翻訳する.

## 拡張機能
- カリー化されたラムダ抽象を自動で非カリー化して翻訳する (AutoUncurrying)
- `s: Behavior (Behavior a)`のような高階な信号を扱えるようにする (HigherOrder)
- 非カリー化不可能な*隠れ入力*, *隠れ出力*を必要とする信号関数を扱えるようにする (HiddenIO)

---

FRP Arrowizerは簡潔なCFRPのための非常に重要なモジュールだが, あくまで要となるのはAFRP Compilerの側.
