(in-package :markdown.cl-test)
(in-suite markdown.cl-extra-tests)

(test img
  "test image link"
  (is (null (tree-diff (tree (parse
                              "![HTML5](http://www.w3.org/html/logo/img/mark-word-icon.png)"))
                       (xmls:parse (concatenate 'string "<html>" "<p><img src=\"http://www.w3.org/html/logo/img/mark-word-icon.png\" alt=\"HTML5\" /></p>" "</html>"))))))

(test header-level1-hash-sign-trailing-2-spaces
 (is (null (tree-diff (tree (parse
 "# this is an h1 with two trailing spaces  
A new paragraph."))
 (xmls:parse (concatenate 'string "<html>" "<h1>this is an h1 with two trailing spaces</h1>

<p>A new paragraph.</p>" "</html>"))))))

 
(test strong-star
 (is (null (tree-diff (tree (parse
 "**double asterisks**"))
 (xmls:parse (concatenate 'string "<html>" "<p><strong>double asterisks</strong></p>" "</html>"))))))

 
(test blockquote-added-markup
 (is (null (tree-diff (tree (parse
 "> # heading level 1
> 
> paragraph"))
 (xmls:parse (concatenate 'string "<html>" "<blockquote>
<h1>heading level 1</h1>

<p>paragraph</p>
</blockquote>" "</html>"))))))

(test ampersand-text-flow
 (is (null (tree-diff (tree (parse
 "An ampersand & in the text flow is escaped as an html entity."))
 (xmls:parse (concatenate 'string "<html>" "<p>An ampersand &amp; in the text flow is escaped as an html entity.</p>" "</html>"))))))

 
(test EOL-CR
 (is (null (tree-diff (tree (parse
 "These lines all end with end of line (EOL) sequences.Seriously, they really do.If you don't believe me: HEX EDIT!"))
 (xmls:parse (concatenate 'string "<html>" "<p>These lines all end with end of line (EOL) sequences.</p><p>Seriously, they really do.</p><p>If you don't believe me: HEX EDIT!</p>" "</html>"))))))

 
(test paragraph-trailing-tab
 (is (null (tree-diff (tree (parse
 "This is a paragraph with 1 trailing tab.	"))
 (xmls:parse (concatenate 'string "<html>" "<p>This is a paragraph with 1 trailing tab.    </p>" "</html>"))))))

 
(test img-idref
 (is (null (tree-diff (tree (parse
 "![HTML5][h5]

[h5]: http://www.w3.org/html/logo/img/mark-word-icon.png"))
                      (xmls:parse (concatenate 'string "<html>" "<p><img src=\"http://www.w3.org/html/logo/img/mark-word-icon.png\" alt=\"HTML5\" /></p>" "</html>"))))))


(test horizontal-rule-3-stars
 (is (null (tree-diff (tree (parse
 "***"))
 (xmls:parse (concatenate 'string "<html>" "<hr />" "</html>"))))))

 
(test horizontal-rule-3-underscores
 (is (null (tree-diff (tree (parse
 "___"))
 (xmls:parse (concatenate 'string "<html>" "<hr />" "</html>"))))))
 
 
(test img-idref-title
 (is (null (tree-diff (tree (parse
 "![HTML5][h5]

[h5]: http://www.w3.org/html/logo/img/mark-word-icon.png \"HTML5 for everyone\""))
 (xmls:parse (concatenate 'string "<html>" "<p><img src=\"http://www.w3.org/html/logo/img/mark-word-icon.png\" alt=\"HTML5\" title=\"HTML5 for everyone\" /></p>" "</html>"))))))

 
(test list-blockquote
 (is (null (tree-diff (tree (parse
 "*   a list containing a blockquote

    > this the blockquote in the list"))
 (xmls:parse (concatenate 'string "<html>" "<ul>
<li><p>a list containing a blockquote</p>

<blockquote>
<p>this the blockquote in the list</p>
</blockquote></li>
</ul>
" "</html>"))))))

(test asterisk
 (is (null (tree-diff (tree (parse
 "This is * an asterisk which should stay as is."))
 (xmls:parse (concatenate 'string "<html>" "<p>This is * an asterisk which should stay as is.</p>" "</html>"))))))

 
(test link-idref-title
 (is (null (tree-diff (tree (parse
 "[World Wide Web Consortium][w3c]

[w3c]: http://www.w3.org/ \"Discover w3c\""))
 (xmls:parse (concatenate 'string "<html>" "<p><a href=\"http://www.w3.org/\" title=\"Discover w3c\">World Wide Web Consortium</a></p>" "</html>"))))))

 
 
(test paragraphs-trailing-spaces
 (is (null (tree-diff (tree (parse
 "This is a paragraph with a trailing space. "))
 (xmls:parse (concatenate 'string "<html>" "<p>This is a paragraph with a trailing space. </p>" "</html>"))))))


(test em-star
 (is (null (tree-diff (tree (parse
 "*single asterisks*"))
 (xmls:parse (concatenate 'string "<html>" "<p><em>single asterisks</em></p>" "</html>"))))))

 
(test 2-paragraphs-line-returns
 (is (null (tree-diff (tree (parse
 "A first paragraph.



A second paragraph after 3 CR (carriage return)."))
 (xmls:parse (concatenate 'string "<html>" "<p>A first paragraph.</p>

<p>A second paragraph after 3 CR (carriage return).</p>" "</html>"))))))

 
(test link-idref-title-single-quote
 (is (null (tree-diff (tree (parse
 "[World Wide Web Consortium][w3c]

[w3c]: http://www.w3.org/ 'Discover w3c'"))
                      (xmls:parse (concatenate 'string "<html>" "<p><a href=\"http://www.w3.org/\" title=\"Discover w3c\">World Wide Web Consortium</a></p>" "</html>"))))))

 
(test blockquote-nested-multiplereturn
 (is (null (tree-diff (tree (parse
 "> This is the first level of quoting.
>
> > This is nested blockquote."))
 (xmls:parse (concatenate 'string "<html>" "<blockquote>
<p>This is the first level of quoting.</p>

<blockquote>
<p>This is nested blockquote.</p>
</blockquote>
</blockquote>" "</html>"))))))

 
(test link-automatic
 (is (null (tree-diff (tree (parse
 "This is an automatic link <http://www.w3.org/>"))
 (xmls:parse (concatenate 'string "<html>" "<p>This is an automatic link <a href=\"http://www.w3.org/\">http://www.w3.org/</a></p>" "</html>"))))))

 
(test em-middle-word
 (is (null (tree-diff (tree (parse
 "as*te*risks"))
 (xmls:parse (concatenate 'string "<html>" "<p>as<em>te</em>risks</p>" "</html>"))))))

 
(test header-level3-hash-sign-closed
 (is (null (tree-diff (tree (parse
 "### This is an H3 ###"))
 (xmls:parse (concatenate 'string "<html>" "<h3>This is an H3</h3>" "</html>"))))))

 
(test blockquote-multiline-1-space-begin
 (is (null (tree-diff (tree (parse
 "> A blockquote
> on multiple lines
> like this."))
 (xmls:parse (concatenate 'string "<html>" "<blockquote>
<p>A blockquote
on multiple lines
like this.</p>
</blockquote>" "</html>"))))))

 
(test blockquote-multiline-2-paragraphs
 (is (null (tree-diff (tree (parse
 ">A blockquote
>on multiple lines
>like this.
>
>But it has
>two paragraphs."))
 (xmls:parse (concatenate 'string "<html>" "<blockquote>
<p>A blockquote
on multiple lines
like this.</p>

<p>But it has
two paragraphs.</p>
</blockquote>" "</html>"))))))

 
(test strong-underscore
 (is (null (tree-diff (tree (parse
 "__double underscores__"))
 (xmls:parse (concatenate 'string "<html>" "<p><strong>double underscores</strong></p>" "</html>"))))))

(test header-level2-hash-sign
 (is (null (tree-diff (tree (parse
 "## This is an H2"))
 (xmls:parse (concatenate 'string "<html>" "<h2>This is an H2</h2>" "</html>"))))))

 
(test link-idref-implicit
 (is (null (tree-diff (tree (parse
 "[w3c][]

[w3c]: http://www.w3.org/"))
 (xmls:parse (concatenate 'string "<html>" "<p><a href=\"http://www.w3.org/\">w3c</a></p>" "</html>"))))))


 
(test link-idref-angle-bracket
 (is (null (tree-diff (tree (parse
 "[World Wide Web Consortium][w3c]

[w3c]: <http://www.w3.org/>"))
 (xmls:parse (concatenate 'string "<html>" "<p><a href=\"http://www.w3.org/\">World Wide Web Consortium</a></p>" "</html>"))))))

 
(test link-idref-title-paranthesis
 (is (null (tree-diff (tree (parse
 "[World Wide Web Consortium][w3c]

[w3c]: http://www.w3.org/ (Discover w3c)"))
 (xmls:parse (concatenate 'string "<html>" "<p><a href=\"http://www.w3.org/\" title=\"Discover w3c\">World Wide Web Consortium</a></p>" "</html>"))))))

 
(test link-idref-title-next-line
 (is (null (tree-diff (tree (parse
 "[World Wide Web Consortium][w3c]

[w3c]: http://www.w3.org/
   \"Discover W3C\""))
 (xmls:parse (concatenate 'string "<html>" "<p><a href=\"http://www.w3.org/\" title=\"Discover W3C\">World Wide Web Consortium</a></p>" "</html>"))))))


 
(test header-level1-hash-sign-closed
 (is (null (tree-diff (tree (parse
 "# This is an H1 #"))
 (xmls:parse (concatenate 'string "<html>" "<h1>This is an H1</h1>" "</html>"))))))

 
(test blockquote-nested-multiplereturn-level1
 (is (null (tree-diff (tree (parse
 "> This is the first level of quoting.
>
> > This is nested blockquote.
>
> Back to the first level.
"))
 (xmls:parse (concatenate 'string "<html>" "<blockquote>
<p>This is the first level of quoting.</p>

<blockquote>
<p>This is nested blockquote.</p>
</blockquote>

<p>Back to the first level.</p>
</blockquote>" "</html>"))))))

 
(test link-idref-implicit-spaces
 (is (null (tree-diff (tree (parse
 "[World Wide Web Consortium][]

[World Wide Web Consortium]: http://www.w3.org/"))
 (xmls:parse (concatenate 'string "<html>" "<p><a href=\"http://www.w3.org/\">World Wide Web Consortium</a></p>" "</html>"))))))

 
(test ordered-list-items
 (is (null (tree-diff (tree (parse
 "1. list item 1
2. list item 2
3. list item 3"))
 (xmls:parse (concatenate 'string "<html>" "<ol>
<li>list item 1</li>
<li>list item 2</li>
<li>list item 3</li>
</ol>" "</html>"))))))

 
(test line-break-2-spaces
 (is (null (tree-diff (tree (parse
 "A first sentence  
and a line break."))
 (xmls:parse (concatenate 'string "<html>" "<p>A first sentence<br />
and a line break.</p>" "</html>"))))))

 
(test blockquote-line
 (is (null (tree-diff (tree (parse
 ">This a very long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long paragraph in a blockquote."))
 (xmls:parse (concatenate 'string "<html>" "<blockquote>
<p>This a very long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long paragraph in a blockquote.</p>
</blockquote>" "</html>"))))))

 
(test blockquote-nested
 (is (null (tree-diff (tree (parse
 "> This is the first level of quoting.
> > This is nested blockquote.
"))
 (xmls:parse (concatenate 'string "<html>" "<blockquote>
<p>This is the first level of quoting.</p>

<blockquote>
<p>This is nested blockquote.</p>
</blockquote>
</blockquote>" "</html>"))))))

 
(test 2-paragraphs-hard-return
 (is (null (tree-diff (tree (parse
 "This is a first paragraph,
on multiple lines.

This is a second paragraph
which has multiple lines too."))
 (xmls:parse (concatenate 'string "<html>" "<p>This is a first paragraph,
on multiple lines.</p>

<p>This is a second paragraph
which has multiple lines too.</p>" "</html>"))))))

 
(test blockquote-line-2-paragraphs
 (is (null (tree-diff (tree (parse
 ">A blockquote with a very long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long line.

>and a second very long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long line."))
 (xmls:parse (concatenate 'string "<html>" "<blockquote>
<p>A blockquote with a very long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long line.</p>

<p>and a second very long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long line.</p>
</blockquote>" "</html>"))))))

 
(test ampersand-uri
 (is (null (tree-diff (tree (parse
 "There is an [ampersand](http://validator.w3.org/check?uri=http://www.w3.org/&verbose=1) in the URI."))
 (xmls:parse (concatenate 'string "<html>" "<p>There is an <a href=\"http://validator.w3.org/check?uri=http://www.w3.org/&amp;verbose=1\">ampersand</a> in the URI.</p>" "</html>"))))))

 
(test blockquote-multiline
 (is (null (tree-diff (tree (parse
 ">A blockquote
>on multiple lines
>like this"))
 (xmls:parse (concatenate 'string "<html>" "<blockquote>
<p>A blockquote
on multiple lines
like this</p>
</blockquote>" "</html>"))))))

(test paragraphs-3-leading-spaces
 (is (null (tree-diff (tree (parse
 "   This is a paragraph with 3 leading spaces."))
 (xmls:parse (concatenate 'string "<html>" "<p>This is a paragraph with 3 leading spaces.</p>" "</html>"))))))

 
(test link-bracket-paranthesis
 (is (null (tree-diff (tree (parse
 "[W3C](http://www.w3.org/)"))
 (xmls:parse (concatenate 'string "<html>" "<p><a href=\"http://www.w3.org/\">W3C</a></p>" "</html>"))))))

 
(test header-level1-hash-sign-trailing-1-space
 (is (null (tree-diff (tree (parse
 " # This is an H1"))
 (xmls:parse (concatenate 'string "<html>" "<p># This is an H1</p>" "</html>"))))))

 
(test inline-code-with-visible-backtick
 (is (null (tree-diff (tree (parse
 "``We love `code` for everything``"))
 (xmls:parse (concatenate 'string "<html>" "<p><code>We love `code` for everything</code></p>" "</html>"))))))

 
(test paragraphs-2-leading-spaces
 (is (null (tree-diff (tree (parse
 "  This is a paragraph with 2 leading spaces."))
 (xmls:parse (concatenate 'string "<html>" "<p>This is a paragraph with 2 leading spaces.</p>" "</html>"))))))

 
(test inline-code
 (is (null (tree-diff (tree (parse
 "``We love `code` for everything``"))
 (xmls:parse (concatenate 'string "<html>" "<p><code>We love `code` for everything</code></p>" "</html>"))))))

 
(test header-level5-hash-sign-closed
 (is (null (tree-diff (tree (parse
 "##### This is an H5 #####"))
 (xmls:parse (concatenate 'string "<html>" "<h5>This is an H5</h5>" "</html>"))))))


(test unordered-list-unindented-content
 (is (null (tree-diff (tree (parse
 "*   This a very long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long paragraph in a list.
*   and yet another long long long long long long long long long long long long long long long long long long long long long long line."))
 (xmls:parse (concatenate 'string "<html>" "<ul>
<li>This a very long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long paragraph in a list.</li>
<li>and yet another long long long long long long long long long long long long long long long long long long long long long long line.</li>
</ul>" "</html>"))))))

 
(test horizontal-rule-3-dashes-spaces
 (is (null (tree-diff (tree (parse
 "- - -"))
 (xmls:parse (concatenate 'string "<html>" "<hr />" "</html>"))))))
 
(test horizontal-rule-7-dashes
 (is (null (tree-diff (tree (parse
 "-------"))
 (xmls:parse (concatenate 'string "<html>" "<hr />" "</html>"))))))

 
(test unordered-list-items-leading-2spaces
 (is (null (tree-diff (tree (parse
 "  * list item 1
  * list item 2
  * list item 3"))
 (xmls:parse (concatenate 'string "<html>" "<ul>
<li>list item 1</li>
<li>list item 2</li>
<li>list item 3</li>
</ul>" "</html>"))))))

 
(test ordered-list-items-random-number
 (is (null (tree-diff (tree (parse
 "1. list item 1
8. list item 2
1. list item 3"))
 (xmls:parse (concatenate 'string "<html>" "<ol>
<li>list item 1</li>
<li>list item 2</li>
<li>list item 3</li>
</ol>" "</html>"))))))

 
(test header-level2-dash-underlined
 (is (null (tree-diff (tree (parse
 "This is an H2
-------------"))
 (xmls:parse (concatenate 'string "<html>" "<h2>This is an H2</h2>" "</html>"))))))

 
(test paragraphs-leading-space
 (is (null (tree-diff (tree (parse
 " This is a paragraph with 1 leading space."))
 (xmls:parse (concatenate 'string "<html>" "<p>This is a paragraph with 1 leading space.</p>" "</html>"))))))

 
(test header-level1-equal-underlined
 (is (null (tree-diff (tree (parse
 "This is an H1
============="))
 (xmls:parse (concatenate 'string "<html>" "<h1>This is an H1</h1>" "</html>"))))))

 
(test paragraph-line
 (is (null (tree-diff (tree (parse
 "This a very long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long paragraph on 1 line."))
 (xmls:parse (concatenate 'string "<html>" "<p>This a very long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long paragraph on 1 line.</p>" "</html>"))))))

 
(test link-bracket-paranthesis-title
 (is (null (tree-diff (tree (parse
 "[W3C](http://www.w3.org/ \"Discover w3c\")"))
 (xmls:parse (concatenate 'string "<html>" "<p><a href=\"http://www.w3.org/\" title=\"Discover w3c\">W3C</a></p>" "</html>"))))))

 
(test header-level2-hash-sign-closed
 (is (null (tree-diff (tree (parse
 "## This is an H2 ##"))
 (xmls:parse (concatenate 'string "<html>" "<h2>This is an H2</h2>" "</html>"))))))

 
(test unordered-list-items-dashsign
 (is (null (tree-diff (tree (parse
 "- list item 1
- list item 2
- list item 3"))
 (xmls:parse (concatenate 'string "<html>" "<ul>
<li>list item 1</li>
<li>list item 2</li>
<li>list item 3</li>
</ul>" "</html>"))))))

(test blockquote-multiline-1-space-end
 (is (null (tree-diff (tree (parse
 ">A blockquote 
>on multiple lines 
>like this. "))
 (xmls:parse (concatenate 'string "<html>" "<blockquote>
<p>A blockquote 
on multiple lines 
like this. </p>
</blockquote>" "</html>"))))))

 
(test horizontal-rule-3-dashes
 (is (null (tree-diff (tree (parse
 "---"))
 (xmls:parse (concatenate 'string "<html>" "<hr />" "</html>"))))))

 
(test img-title
 (is (null (tree-diff (tree (parse
 "![HTML5](http://www.w3.org/html/logo/img/mark-word-icon.png \"HTML5 logo for everyone\")"))
 (xmls:parse (concatenate 'string "<html>" "<p><img src=\"http://www.w3.org/html/logo/img/mark-word-icon.png\" alt=\"HTML5\" title=\"HTML5 logo for everyone\" /></p>" "</html>"))))))

 
(test em-underscore
 (is (null (tree-diff (tree (parse
 "_single underscores_"))
 (xmls:parse (concatenate 'string "<html>" "<p><em>single underscores</em></p>" "</html>"))))))

 
(test header-level4-hash-sign
 (is (null (tree-diff (tree (parse
 "#### This is an H4"))
 (xmls:parse (concatenate 'string "<html>" "<h4>This is an H4</h4>" "</html>"))))))

 
(test paragraph-trailing-leading-spaces
 (is (null (tree-diff (tree (parse
 " This is a paragraph with a trailing and leading space. "))
 (xmls:parse (concatenate 'string "<html>" "<p>This is a paragraph with a trailing and leading space. </p>" "</html>"))))))

 
(test header-level3-hash-sign
 (is (null (tree-diff (tree (parse
 "### This is an H3"))
 (xmls:parse (concatenate 'string "<html>" "<h3>This is an H3</h3>" "</html>"))))))

 
(test header-level6-hash-sign-closed
 (is (null (tree-diff (tree (parse
 "###### This is an H6  ######"))
 (xmls:parse (concatenate 'string "<html>" "<h6>This is an H6</h6>" "</html>"))))))

(test EOL-CR+LF
 (is (null (tree-diff (tree (parse
 "These lines all end with end of line (EOL) sequences.

Seriously, they really do.

If you don't believe me: HEX EDIT!

"))
 (xmls:parse (concatenate 'string "<html>" "<p>These lines all end with end of line (EOL) sequences.</p>

<p>Seriously, they really do.</p>

<p>If you don't believe me: HEX EDIT!</p>" "</html>"))))))

 
(test link-idref-space
 (is (null (tree-diff (tree (parse
 "[World Wide Web Consortium] [w3c]

[w3c]: http://www.w3.org/"))
 (xmls:parse (concatenate 'string "<html>" "<p><a href=\"http://www.w3.org/\">World Wide Web Consortium</a></p>" "</html>"))))))

 
(test unordered-list-items-leading-1space
 (is (null (tree-diff (tree (parse
 " * list item 1
 * list item 2
 * list item 3"))
 (xmls:parse (concatenate 'string "<html>" "<ul>
<li>list item 1</li>
<li>list item 2</li>
<li>list item 3</li>
</ul>
" "</html>"))))))

(test header-level1-hash-sign
 (is (null (tree-diff (tree (parse
 "# This is an H1"))
 (xmls:parse (concatenate 'string "<html>" "<h1>This is an H1</h1>" "</html>"))))))

 
(test header-level4-hash-sign-closed
 (is (null (tree-diff (tree (parse
 "#### This is an H4 ####"))
 (xmls:parse (concatenate 'string "<html>" "<h4>This is an H4</h4>" "</html>"))))))

(test unordered-list-items-plussign
 (is (null (tree-diff (tree (parse
 "+ list item 1
+ list item 2
+ list item 3"))
 (xmls:parse (concatenate 'string "<html>" "<ul>
<li>list item 1</li>
<li>list item 2</li>
<li>list item 3</li>
</ul>" "</html>"))))))

 
(test 2-paragraphs-line
 (is (null (tree-diff (tree (parse
 "This a very long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long paragraph on 1 line.

A new long long long long long long long long long long long long long long long long paragraph on 1 line."))
 (xmls:parse (concatenate 'string "<html>" "<p>This a very long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long long paragraph on 1 line.</p>

<p>A new long long long long long long long long long long long long long long long long paragraph on 1 line.</p>" "</html>"))))))

 
(test header-level5-hash-sign
 (is (null (tree-diff (tree (parse
 "##### This is an H5"))
 (xmls:parse (concatenate 'string "<html>" "<h5>This is an H5</h5>" "</html>"))))))

 
(test paragraph-hard-return
 (is (null (tree-diff (tree (parse
 "This is a paragraph
on multiple lines
with hard return."))
 (xmls:parse (concatenate 'string "<html>" "<p>This is a paragraph
on multiple lines
with hard return.</p>" "</html>"))))))

(test code-4-spaces
 (is (null (tree-diff (tree (parse
 "    10 PRINT HELLO INFINITE
    20 GOTO 10"))
 (xmls:parse (concatenate 'string "<html>" "<pre><code>10 PRINT HELLO INFINITE
20 GOTO 10
</code></pre>" "</html>"))))))

 
(test unordered-list-items-asterisk
 (is (null (tree-diff (tree (parse
 "* list item 1
* list item 2
* list item 3
"))
 (xmls:parse (concatenate 'string "<html>" "<ul>
<li>list item 1</li>
<li>list item 2</li>
<li>list item 3</li>
</ul>" "</html>"))))))

 
(test asterisk-near-text
 (is (null (tree-diff (tree (parse
 "This is \*an asterisk which should stay as is."))
 (xmls:parse (concatenate 'string "<html>" "<p>This is *an asterisk which should stay as is.</p>" "</html>"))))))


 
(test code-1-tab
 (is (null (tree-diff (tree (parse
 "	10 PRINT HELLO INFINITE
	20 GOTO 10"))
 (xmls:parse (concatenate 'string "<html>" "<pre><code>10 PRINT HELLO INFINITE
20 GOTO 10
</code></pre>" "</html>"))))))

 
(test header-level6-hash-sign
 (is (null (tree-diff (tree (parse
 "###### This is an H6"))
 (xmls:parse (concatenate 'string "<html>" "<h6>This is an H6</h6>" "</html>"))))))

 
(test EOL-LF
 (is (null (tree-diff (tree (parse
 "These lines all end with end of line (EOL) sequences.

Seriously, they really do.

If you don't believe me: HEX EDIT!

"))
 (xmls:parse (concatenate 'string "<html>" "<p>These lines all end with end of line (EOL) sequences.</p>

<p>Seriously, they really do.</p>

<p>If you don't believe me: HEX EDIT!</p>" "</html>"))))))

 
(test unordered-list-paragraphs
 (is (null (tree-diff (tree (parse
 "* list item in paragraph

* another list item in paragraph"))
 (xmls:parse (concatenate 'string "<html>" "<ul>
<li><p>list item in paragraph</p></li>
<li><p>another list item in paragraph</p></li>
</ul>" "</html>"))))))

 
(test unordered-list-items-leading-3spaces
 (is (null (tree-diff (tree (parse
 "   * list item 1
   * list item 2
   * list item 3"))
 (xmls:parse (concatenate 'string "<html>" "<ul>
<li>list item 1</li>
<li>list item 2</li>
<li>list item 3</li>
</ul>" "</html>"))))))

 
(test strong-middle-word
 (is (null (tree-diff (tree (parse
 "as**te**risks"))
 (xmls:parse (concatenate 'string "<html>" "<p>as<strong>te</strong>risks</p>" "</html>"))))))



 
(test ordered-list-inner-par-list
 (is (null (tree-diff (tree (parse
 "1. 1

    - inner par list

2. 2
"))
 (xmls:parse (concatenate 'string "<html>" "<ol>
  <li>
    <p>1</p>
    <ul>
      <li>inner par list</li>
    </ul>
  </li>
  <li><p>2</p></li>
</ol>
" "</html>"))))))

 
(test link-idref
 (is (null (tree-diff (tree (parse
 "[World Wide Web Consortium][w3c]

[w3c]: http://www.w3.org/"))
 (xmls:parse (concatenate 'string "<html>" "<p><a href=\"http://www.w3.org/\">World Wide Web Consortium</a></p>" "</html>"))))))


  

