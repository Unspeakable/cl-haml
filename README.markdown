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
- 出力HTMLのインデントが異なる (CL-WHO依存をちょっと改造)
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

- ファイル先頭の !!! を無視 - 2011-08-31対応
- インデントがちょっと 本家HAMLっぽくなったよ - 2011-09-22


CL-HAMLを試す
-------------
当然、quicklispに入っているはずはないので(個人的には入れるつもりもない)、githubから tarを取ってくるとか git cloneするとかしてください。
また cl-who, split-sequence, cl-ppcre, metatilities に依存してるので、quicklispや clbuildで用意してください。もちろん tarを直接取ってきても構いません。
全部 asdf が読んでくれる場所に置いて `(asdf:oos 'asdf:load-op :cl-haml)` とすれば load出来ると思います。

render関数が hamlファイルを読み込み、評価した結果のHTMLを文字列として返します。
render関数の第一引数は cl-haml-builderクラスのインスタンスで、最低限必須の設定として、load-rootに hamlファイルの置かれたディレクトリを指定します。
第二引数に hamlファイルの名前を拡張子無しで指定。"hoge/example"のような指定で、ディレクトリ内のファイルも可能。ちなみに拡張子は `".haml"`という小文字固定です。
第三引数には hamlファイル内で使う値の list。

cl-hamlには サンプルな hamlファイルが入ってます。

    CL-USER> (asdf:oos 'asdf:load-op :cl-haml)
    CL-USER> (use-package :cl-haml)
    CL-USER> (let ((builder (make-instance 'cl-haml:cl-haml-builder
                                           :load-root (asdf:system-source-directory :cl-haml))))
               (cl-haml:render builder "example2" nil))


作った人
--------
Hiroyuki Tokunaga <inuzini.jiro@gmail.com>


ライセンス
----------
MIT License
