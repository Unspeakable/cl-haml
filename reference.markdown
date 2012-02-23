# CL-HAML Reference

## Options
CL-HAMLにはいくつかのオプションがあり、スペシャル変数の値を変更することで挙動を変更することが出来ます。

### \*escape-html\*
`=` で lispコードの評価結果を出力HTMLに埋め込む際、文字列をエスケープするか否かのフラグです。デフォルトでは `nil`(エスケープしない)になっています。`t`を設定すると、`!=` が出力内容をエスケープしなくなります。

### \*html-mode\*
DOCTYPEの内容を決定します。デフォルトでは `:xhtml`が指定されています。他に `:html4`, `:html5`の2種類が指定可能です。

### \*function-package\*
.hamlファイルの内容を関数化する際に、パッケージ名を指定していないシンボルをどこのパッケージに インターンするかを制御します。デフォルトでは `:cl-user` が指定されています。

## Plain Text
普通のテキストです。インデントのルールを守った単純なテキストは Plain Textとして処理します。例えば:

    %gee
      %whiz
        Wow this is cool!

上記の内容は以下のように 出力されます。

    <gee><whiz>Wow this is cool!</whiz></gee>

TODO: ちなみに、本家の Hamlでは以下のようになります。

    <gee>
      <whiz>
        Wow this is cool!
      </whiz>
    </gee>

将来的には、本家Hamlと同じ出力となるよう修正する予定です。

また、以下のように Plain Textに HTMLをそのまま記述することも可能です。

    %p
      <div id="blah">Blah!</div>

上記の内容は以下のように 出力されます。

    <p><div id="blah">Blah!</div></p>

### Escaping: \\
空白文字以外で、行の先頭が `\`の場合、その後にどのような内容が来ても Plain Textとして処理します。その際、先頭の `\`は除去されます。CL-HAMLの構文として処理されるような内容を Plain Textとして表示したい場合に使用してください。

    %p
      \= (format nil "Hoge")

上記の内容は以下のように 出力されます。

    <p>= (format nil "Hoge")</p>

## HTML Elements
### Element Name: %
HTML/XMLのタグ指定です。行の先頭が `%`で開始している場合、連続するアルファベットや数字、-, _は タグ名だと判断されます。

    %one
      %two
        %three Hey there

上記の内容は以下のように 出力されます。

    <one><two><three>Hey there</three></two></one>

### Attributes: {} or ()
HTML/XMLの属性指定です。LISPの plist形式で指定しますが、括弧の種類は `{}` でも `()`でも同じように処理します。属性名は `:`から始まる キーワードシンボルを使用します。

    %html{:xmlns "http://www.w3.org/1999/xhtml" :xml:lang "en" :lang "en"}

上記の内容は以下のように 出力されます。

    <html xmlns='http://www.w3.org/1999/xhtml' xml:lang='en' lang='en'></html>

注意点：本家Hamlでは `{}` と `()`では別の属性指定方法ですが、CL-HAMLでは後者の指定方法であった HTML-style attributesには対応していません。また、`=>`とか 要素間の `,`は不要です(書くとエラーになります)。属性名に特殊な記号が入っていても 文字列にする必要はありません(文字列にするとエラーに...)。

#### :class and :id Attributes
`.class` と `.id` という関数を用意しました。`.class`関数は 引数全てを 半角スペースで結合した文字列、`.id`関数は 引数全てを _ で結合した文字列を返します。

    - dotimes (i 10)
      - let ((type "numeric"))
        %div{:id (.id "Hello" i) :class (.class "box" type)}= i

上記の内容は以下のように 出力されます。

    <div id='Hello_0' class='box numeric'>0</div>
    <div id='Hello_1' class='box numeric'>1</div>
    <div id='Hello_2' class='box numeric'>2</div>
    <div id='Hello_3' class='box numeric'>3</div>
    <div id='Hello_4' class='box numeric'>4</div>
    <div id='Hello_5' class='box numeric'>5</div>
    <div id='Hello_6' class='box numeric'>6</div>
    <div id='Hello_7' class='box numeric'>7</div>
    <div id='Hello_8' class='box numeric'>8</div>
    <div id='Hello_9' class='box numeric'>9</div>

#### Boolean Attributes
inputタグ(チェックボックス)の "checked" や optionタグの "selected"といった属性は、属性があるか否かに意味があります。そんな"属性そのものを消したい"場合に、この Boolean Attributesが役に立ちます。値として `nil`を指定した場合、出力結果から属性そのものが消えます。

    %input{:selected t}

上記の内容は以下のように 出力されます。

    <input selected='selected' />

そして `nil`を指定すると

    %input{:selected t}

以下のように 出力されます。

    <input />

### Class and ID: . and #
よく利用する属性である classと idは特別な記述方法により、簡単に指定することが出来ます。idは `#xxx` という # で始まる文字列、`.xxx` という . で始まる文字列を Element Nameと Attributesの間に記述します。両方指定する場合は IDを先に記述、Classは複数記述可能です。

    %div#things
      %span#rice Chicken Fried
      %p.beans{:food "true"} The magical fruit
      %h1#id.class.otherclass La La La

以下のように 出力されます。

    <div id='things'>
      <span id='rice'>Chicken Fried</span>
      <p class='beans' food='true'>The magical Fruit</p>
      <h1 id='id' class='class otherclass'>La La La</h1>
    </div>

これは

    %div#content
      %div.articles
        %div.article.title Doogie Howser Comes Out
        %div.article.date 2006-11-05
        %div.article.entry
          Neil Patrick Harris would like to dispel any rumors that he is straight

こうなる

    <div id='content'>
      <div class='articles'>
        <div class='article title'>Doogie Howser Comes Out</div>
        <div class='article date'>2006-11-05</div>
        <div class='article entry'>
          Neil Patrick Harris would like to dispel any rumors that he is straight
        </div>
      </div>
    </div>

#### Implicit Div Elements

ID か Classを 前述の #id, .classの記法で指定し、なおかつ Elementが divである場合、Elementの指定(つまり `%div`)を省略可能です。

    #collection
      .item
        .description What a cool item!

上記は以下と同じ意味となり:

    %div#collection
      %div.item
        %div.description What a cool item!

どちらも 出力結果は同じ:

    <div id='collection'>
      <div class='item'>
        <div class='description'>What a cool item!</div>
      </div>
    </div>

#### Self-Closing Tags: /

付加することは出来ますが、現状無視します(デフォルトで self-closeされます)。

#### Whitespace Removal: > and <

TODO

#### Object Reference: []

TODO

### Doctype: !!!

Doctype宣言を `!!!`を使用することで簡単に書くことが出来る。
後ろに書くオプション、および全体オプションの値によって出力する Doctype宣言を変更することが出来る。
全体オプション `*haml-mode*`が `:xhtml`(デフォルト値)の場合、以下の Doctype宣言が利用可能。

* !!!  
  XHTML 1.0 Transitional  
      `<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">`
* !!! Strict  
  XHTML 1.0 Strict  
      `<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">`
* !!! Frameset  
  XHTML 1.0 Frameset  
      `<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Frameset//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd">`
* !!! 5  
  XHTML 5  
      `<!DOCTYPE html>`
* !!! 1.1  
  XHTML 1.1  
      `<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.1//EN" "http://www.w3.org/TR/xhtml11/DTD/xhtml1.dtd">`
* !!! Basic  
  XHTML Basic 1.1  
      `<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML Basic 1.1//EN" "http://www.w3.org/TR/xhtml11/DTD/xhtml1.dtd">`
* !!! Mobile  
  XHTML Mobile 1.2  
      `<!DOCTYPE html PUBLIC "-//WAPFORUM//DTD XHTML Mobile 1.2//EN" "http://www.openmobilealliance.org/tech/DTD/xhtml-mobile12.dtd">`
* !!! RDFa  
  XHTML+RDFa 1.0  
      `<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML+RDFa 1.0//EN" "http://www.w3.org/MarkUp/DTD/xhtml-rdfa-1.dtd">`

`*haml-format*` に `:html4` を設定すると以下のようになる:

* !!!  
  HTML 4.01 Transitional  
      `<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">`
* !!! Strict  
  HTML 4.01 Strict  
      `<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01//EN" "http://www.w3.org/TR/html4/strict.dtd">`
* !!! Frameset  
  HTML 4.01 Frameset  
      `<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Frameset//EN" "http://www.w3.org/TR/html4/frameset.dtd">`

`*haml-format*` に `:html5` を設定すると `!!!` だけが使用可能になり、出力は `<!DOCTYPE html>`になる。

※ 本家Hamlに存在する 文字コード指定は 実装しない(UTF-8しか対応しない)

### Comments

#### Haml Comments: -#

HTMLには出力されない、CL-HAML上だけのコメントも存在する:

    %p foo
    -# This is a comment
    %p bar

コンパイルすると:

    <p>foo</p>
    <p>bar</p>

これも複数行に対応している:

    %p foo
    -#
      This won't be displayed
        Nor will this
    %p bar

コンパイルすると:

    <p>foo</p>
    <p>bar</p>

### Common Lisp Evaluation

#### Inserting Common Lisp: =

`=` で始まる行は、後続の S式を評価した結果を、出力結果に埋め込む:

    %p
      = (format nil "~{~A~^ ~}" '("hi" "there" "reader!"))
      = "yo"

コンパイルすると:

    <p>
      hi there reader!
      yo
    </p>

全体オプション `*escape-html*` に `t`を指定すると(デフォルトでは `nil`)、`=`による埋め込む文字列が サニタイズされる:

    = "<script>alert(\"I'm evil!\");</script>"

    &lt;script&gt;alert("I&quot;m evil");&lt;/script&gt;

タグの指定を同時に行うことも可能。この場合はタグや属性の指定後に `=` :

    %p= "Common Lisp"

コンパイルすると:

    <p>Common Lisp</p>


#### Running Common Lisp: -

`-` で始まる行は Common Lispの S式として評価される。`=` との違いは、その戻り値は無視されること:

    - let ((foo "Hello"))
      - setf foo (concatenate 'string foo " there")
      - setf foo (concatenate 'string foo " you!")
      %p= foo

コンパイルすると:

    <p>Hello there you!</p>

このように、一番外側の括弧は省略して記述する。同レベル以下のインデントが来たら括弧が閉じられた、と判断する。

行末に \ を記述すると \を削除したうえで 次の行を連結する。

    - let ((list (loop repeat 10 \
                       for x = (random 100) \
                       collect x)))

以下のように、処理の中で CL-HAMLの記述も出来る

    %ul
      - loop for i from 42 to 47 do
        %li= i
    %p See, I can count!

コンパイルすると:

    <ul>
      <li>42</li>
      <li>43</li>
      <li>44</li>
      <li>45</li>
      <li>46</li>
      <li>47</li>
    </ul>
    <p>See, I can count!</p>

条件分岐も書けるが、いろいろ注意したほうがいい:

    %p
      - case 2
        - 1
          = "1!"
        - 2
          = "2?"
        - 3
          = "3."

コンパイルすると:

    <p>
      2?
    </p>

BUG: 以下のコードが正しく動作しない気がする

    - case 2
      - 1
        1!
      - 2
        2?
      - 3
        3.

#### Whitespace Preservation: ~

未実装

#### Common Lisp Interpolation: ${}, @{}？

未実装

#### Escaping HTML: &=

オプション `*escape-html*` の値とは関係なく、常に埋め込む文字列が サニタイズされる:

    &= "I like cheese & crackers"

コンパイルすると:

    I like cheese &amp; crackers

#### Unescaping HTML: !=

全体オプション `*escape-html*` に対する振る舞いが、`=`と正反対の動作をする。
つまり、`t` の場合はサニタイズせず、`nil`の場合に サニタイズする。
`*escape-html*` が `t` の場合:

    = "I feel <strong>!"
    != "I feel <strong>!"

この評価結果は以下のようになる:

    I feel &lt;strong&gt;!
    I feel <strong>!

### Filters

キーワードシンボルで始まる行から続くブロックに対して、通常とは異となる処理を行う。

    %p
      :javascript
        function hello(arg) {
          alert("Hello, " + arg);
        }

コンパイルすると:

    <p>
      <script type='text/javascript'>
        function hello(arg) {
        alert("Hello, " + arg);
        }
      </script>

    </p>

* :javascript
* :css

カスタムフィルターは、いまのところ非対応。

### Multiline: |

TODO
