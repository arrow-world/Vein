Q. 何をどの言語で実装する?

# 目的
0. 回路プログラミング/FRPをするための枠組みの作成
1. CFRPで書き, AFRPに翻訳して機械語に落とす.
2. 翻訳器と(機械語への)コンパイラを作る必要がある.

# CFRPからAFRPへの翻訳器: FRP Arrowizer
Q. EDSLとして実現するか, 新たな言語として実現するか?

- SourceとSinkの概念
  - Sourceは外部から情報を受けとり回路に入力する
  - Sinkは回路の出力を外部へ転送する

EDSLによる実現の場合 (このEDSLをVeinと呼ぶ)
Q. どの言語にホストするか?
0. 関数型
1. IdrisかHaskellか, または新しい言語か?

非正格評価の必要性 (Haskell)
- ループのある回路を翻訳するとき, non-strictでないと面倒では
- 遷移関数書くとき, non-strictで書きたい

依存型の必要性 (Idris)
- Monoidal Categoryを型クラスあるいはデータ型で表現するために, 項をパラメタにする必要があるのでは
  - Haskellでもできてるっぽい

Effectの必要性 (Idris)
- 作用の規定のため
- SourceとSink, あるいは信号型を定義するときEffectで書きたい
- 結局機械語に落ちるのなら, どうでもいい?
- 別にホスト言語になくとも同じくEDSLで新しく作ればよい?
  - また作るものが増える
  - とりあえずMonadで我慢して, 後でEffect実装して書き直す?

---

0. できるだけ新しく言語は作りたくない
1. ArrowizerはEDSLで
2. ホスト言語
- Monoidal CategoryがHaskellには既にある
- EffectはCFRPと同様EDSLとして実現してしまうこともできる
  - まずはMonadで我慢
- Haskellは
  - コミュニティが大きい
  - テンプレートによってEDSLが容易に実現できる
ことから, Haskellで十分と考えられるため, Haskellとする. non-strictだよ, やったね.

# AFRPのコンパイル: AFRP Compiler
- 副作用の定義に従ってLLVMに投げるだけ
- 遷移関数等に埋め込まれたラムダ項まで翻訳する?
  - しないなら, どうやってホスト言語のコンパイラと連携するか
    - もしかして連携なんて必要ない?
      - LLVMコードからそのままホスト言語の関数を呼べる可能性

Haskellを使うなら, llvm-hsが使えそう
