CL-HAML
=======

CL-HAMLは RubyのHamlっぽい感じで HTMLを書くためのライブラリです。
結構適当な状態なので、問題に遭遇しても自力で解決できる人以外は 触らぬが吉です。

Hamlってなんぞ？
----------------
[Hamlの公式サイト](http://haml-lang.com/ "Haml")


CL-HAMLは、どのくらい Haml？
----------------------------
Hamlとの違いは以下

- Ruby ではなく Common Lispで動作
- 埋め込みコードも Ruby ではなく Common Lisp
- ファイル先頭の !!! を無視
- 出力HTMLのインデントが異なる (CL-WHO依存)
- 属性 {} の書き方が plistな感じ
- 改行制御出来ません
- 閉じタグ無し(要素名の末尾に/)が正しく処理されない
- HTMLコメントを認識しない(そのうち対応予定)
- 複数行Hamlコメントを認識しない(そのうち対応予定)
- 複数行（行末に｜を置く奴）
- filterは javascriptと cssくらいしか認識しない
- = は escapeされる(escapeしたくないときは != を使ってね)
- = が常に escapeされるので、&= がない

他にもいっぱいあるはずだけど、作ってる人自身認識出来ていない。


CL-HAMLを試す
-------------
当然、quicklispに入っているはずはないので(個人的には入れるつもりもない)、githubから tarを取ってくるとか git cloneするとかしてください。
また cl-who, split-sequence, cl-ppcre, metatilities に依存してるので、quicklispや clbuildで用意してください。もちろん tarを直接取ってきても構いません。
全部 asdf が読んでくれる場所に置いて `(asdf:oos 'asdf:load-op :cl-haml)` とすれば load出来ると思います。

cl-hamlには サンプルな hamlファイルが入ってます。

    CL-USER> (cl-haml:haml (merge-pathnames "example.haml" (asdf:system-source-directory :cl-haml)))


作った人
--------
Hiroyuki Tokunaga <inuzini.jiro@gmail.com>


ライセンス
----------
MIT License
