# CL-HAML

## What is CL-HAML?

[Haml](http://haml-lang.com/ "Haml")風の記法を持つ Common Lisp用HTMLジェネレータです。インデントや簡略構文によって簡潔な記述が行えます。

Common Lispには、S式をHTMLに変換するライブラリが豊富に存在しますが、その他の方式のライブラリはあまり活発ではないのでは？と感じました。S式は人類の発明した最も優れた記述方法ですが、残念ながらデザイナには少々敷居が高いようです(デザイナは Emacsを使用しないので)。なので、CL-HAMLを作りました! CL-HAMLは、デザイナにも Lisperにも 書きにくい気はします。でも、本家Hamlが書けるデザイナがいるので大丈夫です、きっと。

注意：大変申し訳ありませんが、まだまだ完成には程遠く、エラーメッセージが不親切だったり、酷いバグが残っていたりします。

それでも試していただける方、バグの連絡や 改善の提案をいただけると嬉しいです。

## Install

CL-HAML is available on [QuickLisp](http://www.quicklisp.org/beta/).

    (ql:quickload :cl-haml)

or git clone from Github.

    git clone https://github.com/Unspeakable/cl-haml.git

## Execute

    CL-USER> (ASDF:LOAD-SYSTEM :CL-HAML)
    CL-USER> (CL-HAML:EXECUTE-HAML (MERGE-PATHNAMES
                                      "examples/example2.haml"
                                      (ASDF:SYSTEM-SOURCE-DIRECTORY :CL-HAML))
                                   :ENV '(:ARGUMENT "Hello!"))
