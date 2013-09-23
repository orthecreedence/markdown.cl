(in-package :markdown.cl-test)
(in-suite markdown.cl-test)

;; TODO: test that <em> and <strong> tags can span across <code> blocks:
;;   *this should wrap `this code block`*
;; TODO: test <strong> tags (**, __)
;; TODO: test character escapign (ie \* \- etc)

(test links
  "Test a number of link/image styles."
  (is (null (tree-diff (tree (parse "
This is a paragraph with a ![panda][] picture of a [panda](http://wikipedia.com/pandas)
in it. Pandas are a [strange][1] kind of animal that are bears but are nice. I often wonder
why pandas are so [nice][2].

[1]: http://people.are.strange.com/when-youre-a-stranger/faces-look-ugly/when-youre-alone
[2]: http://nicethings.com

They could be [ripping us limb from limb](/my-page/on-ripping \"Rip!\") as seen in this
![ripping](/images/rip.jpg \"Photo of a shirt being ripped to shreds by a bear\") photo but
they *choose* not to.

[panda]: http://www.pandas.com/panda1.jpg
"))
'("html" NIL
 ("p" NIL "This is a paragraph with a"
  ("img" (("alt" "panda") ("src" "http://www.pandas.com/panda1.jpg")))
  "picture of a" ("a" (("href" "http://wikipedia.com/pandas")) "panda")
  "in it. Pandas are a"
  ("a"
   (("href"
     "http://people.are.strange.com/when-youre-a-stranger/faces-look-ugly/when-youre-alone"))
   "strange")
  "kind of animal that are bears but are nice. I often wonder
why pandas are so"
  ("a" (("href" "http://nicethings.com")) "nice") ".")
 ("p" NIL "They could be"
  ("a" (("title" "Rip!") ("href" "/my-page/on-ripping"))
   "ripping us limb from limb")
  "as seen in this"
  ("img"
   (("title" "Photo of a shirt being ripped to shreds by a bear")
    ("alt" "ripping") ("src" "/images/rip.jpg")))
  "photo but
they"
  ("em" NIL "choose") "not to."))))))

(test blockquotes
  "Test a variety of blockquote styles."
  (is (null (tree-diff (tree (parse "
> this is a simple
> blockquote"))
'("html" NIL ("blockquote" NIL ("p" NIL "this is a simple
blockquote"))))))

  (is (null (tree-diff (tree (parse "
> this is a lazy
blockquote"))
'("html" NIL ("blockquote" NIL ("p" NIL "this is a lazy
blockquote"))))))

  (is (null (tree-diff (tree (parse "
> this is an
> > embedded
> blockquote, and also has
>
> - a list
> - in it"))
'("html" NIL ("blockquote" NIL ("p" NIL "this is an") ("blockquote" NIL ("p" NIL "embedded")) ("p" NIL "blockquote, and also has")
              ("ul" NIL ("li" NIL "a list") ("li" NIL "in it")))))))

  (is (null (tree-diff (tree (parse "
-   this is a paragraph list

    with two embedded

    > blockquotes
    > > subquote
    > inside of it

    which 

    > should work fine
    >
    > 1. list
    > 2. item
- another

    > embedded quote, just for fun"))
'("html" NIL
 ("ul" NIL
  ("li" NIL ("p" NIL "this is a paragraph list")
   ("p" NIL "with two embedded")
   ("blockquote" NIL ("p" NIL "blockquotes")
    ("blockquote" NIL ("p" NIL "subquote")) ("p" NIL "inside of it"))
   ("p" NIL "which")
   ("blockquote" NIL ("p" NIL "should work fine")
    ("ol" NIL ("li" NIL "list") ("li" NIL "item"))))
  ("li" NIL ("p" NIL "another")
   ("blockquote" NIL ("p" NIL "embedded quote, just for fun")))))))))

(test html-blocks
  "Test that HTML blocks embedded in markdown are properly formatted (ie ignored)."
  (is (null (tree-diff (tree (parse "
My main header
==============
Gosh, gee golly, it sure is a nice day out, boy howdy!!

You said it, Fred!! Let's go down to the creek and shove nighcrawlers
in each others butts! I bet I can get 20 of 'em in yours this time!!!

<blockquote>
Little did the boys know that it wasn't just nightcrawlers going in
their butts, for Big Ed, a convicted pedophile, had just taken up
residence in a lonely tin hut at the edge of the creek.
</blockquote>

Gee whillickers, this is going to be fun!!

<div>
Yes, it is, thought Ed to *himself* as he spotted the boys approaching
using his high-powered telescope. Yes, it is.
</div>

Suddenly, Fred tripped over what seemed to be a tree root.

<pre><code>$object = 'root';
$boys = 'fine';
if(net_falls_over_boys())
{
    $object = 'tripwire';
    $boys = 'fucked (quite literally)';
}</code></pre>

Uh oh.

<span>Well *this* is a fine mess you've gotten us into, Randy.</span>
"))
'("html" NIL ("h1" NIL "My main header")
 ("p" NIL "Gosh, gee golly, it sure is a nice day out, boy howdy!!")
 ("p" NIL
  "You said it, Fred!! Let's go down to the creek and shove nighcrawlers
in each others butts! I bet I can get 20 of 'em in yours this time!!!")
 ("blockquote" NIL
  "Little did the boys know that it wasn't just nightcrawlers going in
their butts, for Big Ed, a convicted pedophile, had just taken up
residence in a lonely tin hut at the edge of the creek.")
 ("p" NIL "Gee whillickers, this is going to be fun!!")
 ("div" NIL
  "Yes, it is, thought Ed to *himself* as he spotted the boys approaching
using his high-powered telescope. Yes, it is.")
 ("p" NIL "Suddenly, Fred tripped over what seemed to be a tree root.")
 ("pre" NIL
  ("code" NIL "$object = 'root';
$boys = 'fine';
if(net_falls_over_boys())
{
    $object = 'tripwire';
    $boys = 'fucked (quite literally)';
}"))
 ("p" NIL "Uh oh.")
 ("p" NIL
  ("span" NIL "Well" ("em" NIL "this")
   "is a fine mess you've gotten us into, Randy.")))))))

(test lists
  "Test a bunch of lists all crammed together in a pile."
  (is (null (tree-diff (tree (parse "
-  bullet1 is a great bullet
  - sub bullet one has
  multiple lines!
    - sub sub bullet! oh no you didnt!!
    - oh yes i did!!
  1. this is great!!!
  2. ohhhh *heyyyyy yeah!!*
- bullet 2 is grand
-   this is a list item with multiple paragraphs

    here it is again

    another paragraph
- bullet 3
is a lazy piece of crap!!
omg poop

1. hai
2. number list!!


* blockquote in list item

    > line1
    > line2


- code in li, LOL

        (defun my-function (value)
          ;; lol
          (1+ value))

-and another
here's a test

    multi-paragraph list

    item


- paragraph

- list

- items
"
))
'("html" NIL
 ("ul" NIL
  ("li" NIL "bullet1 is a great bullet"
   ("ul" NIL
    ("li" NIL "sub bullet one has multiple lines!"
     ("ul" NIL ("li" NIL "sub sub bullet! oh no you didnt!!")
      ("li" NIL "oh yes i did!!"))))
   ("ol" NIL ("li" NIL "this is great!!!")
    ("li" NIL "ohhhh" ("em" NIL "heyyyyy yeah!!"))))
  ("li" NIL "bullet 2 is grand")
  ("li" NIL ("p" NIL "this is a list item with multiple paragraphs")
   ("p" NIL "here it is again") ("p" NIL "another paragraph"))
  ("li" NIL "bullet 3 is a lazy piece of crap!! omg poop"))
 ("ol" NIL ("li" NIL "hai") ("li" NIL "number list!!"))
 ("ul" NIL
  ("li" NIL ("p" NIL "blockquote in list item")
   ("blockquote" NIL
    ("p" NIL "line1
line2"))))
 ("ul" NIL
  ("li" NIL ("p" NIL "code in li, LOL")
   ("pre" NIL
    ("code" NIL "(defun my-function (value)
  ;; lol
  (1+ value))")))
  ("li" NIL ("p" NIL "and another here's a test")
   ("p" NIL "multi-paragraph list") ("p" NIL "item")))
 ("ul" NIL ("li" NIL ("p" NIL "paragraph")) ("li" NIL ("p" NIL "list"))
  ("li" NIL ("p" NIL "items"))))))))

(test everything
  "Test many markdown parsers, when shoved together in a small space."
(is (null (tree-diff (tree (parse "
header1
====
this is a paragraph
html iz kewl `&mdash;` as shown in that code block
it has ``some code (which <strong>use</strong> `)``

this is a paragraph [with some][link1] links in it.
it is [meant](http://wikipedia.com/meaning \"meaning\")
to [test][test-link] and shit. [test-link][].

[link1]: http://mylinktest.com/why-links-r-kewl
[test-link]: http://test.com/markdown (a title)

* * *

1946\\. was a good year

* some bullets
+ are *good*
  - and dont confuse parsers
  - lol sub bullets
- for formatting
  because they wurklol

i made code LOL
---------------

    <div>
        &copy;
    </div>

1. numbers
2. can be useful
  1. but only
  2. sometimes
3. for formatting LOL

* wrap these list

* items in paragraph tags

* plz!!

### quotes
sometimes i like to quote idiots

-   i want to have a bullet
    with two `paragraphs` lol

    > here's a paragraphed
    > blockquote with
    > > a subquote!!

    so here's the scoop

        and here's a `code` block!

    > another blockquote
    > in a list

    end of bs

> ## header quote
quote paragraph
lol paragraph
also 5 > 4 normally

> poop
> > shit 
> fuck

end of markdown...
"))
'("html" NIL ("h1" NIL "header1")
 ("p" NIL "this is a paragraph
html iz kewl" ("code" NIL "&mdash;") "as shown in that code block
it has" ("code" NIL "some code (which &lt;strong&gt;use&lt;/strong&gt; `)"))
 ("p" NIL "this is a paragraph"
  ("a" (("href" "http://mylinktest.com/why-links-r-kewl")) "with some")
  "links in it.
it is"
  ("a" (("title" "meaning") ("href" "http://wikipedia.com/meaning"))
   "meant")
  "to"
  ("a" (("title" "a title") ("href" "http://test.com/markdown")) "test")
  "and shit."
  ("a" (("title" "a title") ("href" "http://test.com/markdown"))
   "test-link")
  ".")
 ("hr" NIL) ("p" NIL "1946. was a good year")
 ("ul" NIL ("li" NIL "some bullets")
  ("li" NIL "are" ("em" NIL "good")
   ("ul" NIL ("li" NIL "and dont confuse parsers")
    ("li" NIL "lol sub bullets")))
  ("li" NIL "for formatting because they wurklol"))
 ("h2" NIL "i made code LOL")
 ("pre" NIL
  ("code" NIL "&lt;div&gt;
    &copy;
&lt;/div&gt;"))
 ("ol" NIL ("li" NIL "numbers")
  ("li" NIL "can be useful"
   ("ol" NIL ("li" NIL "but only") ("li" NIL "sometimes")))
  ("li" NIL "for formatting LOL"))
 ("ul" NIL ("li" NIL ("p" NIL "wrap these list"))
  ("li" NIL ("p" NIL "items in paragraph tags"))
  ("li" NIL ("p" NIL "plz!!")))
 ("h3" NIL "quotes") ("p" NIL "sometimes i like to quote idiots")
 ("ul" NIL
  ("li" NIL
   ("p" NIL "i want to have a bullet with two" ("code" NIL "paragraphs")
    "lol")
   ("blockquote" NIL
    ("p" NIL "here's a paragraphed
blockquote with")
    ("blockquote" NIL ("p" NIL "a subquote!!")))
   ("p" NIL "so here's the scoop")
   ("pre" NIL ("code" NIL "and here's a `code` block!"))
   ("blockquote" NIL
    ("p" NIL "another blockquote
in a list"))
   ("p" NIL "end of bs")))
 ("blockquote" NIL ("h2" NIL "header quote")
  ("p" NIL "quote paragraph
lol paragraph
also 5 &gt; 4 normally"))
 ("blockquote" NIL ("p" NIL "poop") ("blockquote" NIL ("p" NIL "shit"))
  ("p" NIL "fuck"))
 ("p" NIL "end of markdown..."))))))

;; -----------------------------------------------------------------------------
;; temporary tests
;; -----------------------------------------------------------------------------
#|
(defun test-all (&optional show)
  (format t "--------------~%")
  (let* ((str "
header1
====
this is a paragraph
html iz kewl `&mdash;` as shown in that code block
it has ``some code (which <strong>use</strong> `)``

this is a paragraph [with some][link1] links in it.
it is [meant](http://wikipedia.com/meaning \"meaning\")
to [test][test-link] and shit. [test-link][].

[link1]: http://mylinktest.com/why-links-r-kewl
[test-link]: http://test.com/markdown (a title)

* * *

1946\\. was a good year

* some bullets
+ are *good*
  - and dont confuse parsers
  - lol sub bullets
- for formatting
  because they wurklol

i made code LOL
---------------

    <div>
        &copy;
    </div>

1. numbers
2. can be useful
  1. but only
  2. sometimes
3. for formatting LOL

* wrap these list

* items in paragraph tags

* plz!!

### quotes
sometimes i like to quote idiots

-   i want to have a bullet
    with two `paragraphs` lol

    > here's a paragraphed
    > blockquote with
    > > a subquote!!

    so here's the scoop

        and here's a `code` block!

    > another blockquote
    > in a list

    end of bs

> ## header quote
quote paragraph
lol paragraph
also 5 > 4 normally

> poop
> > shit 
> fuck

end of markdown...
")
         (str (parse str)))
    (when show str)
    (tree str)
    ))

(defun test-list (&optional return)
  (let* ((str "
-  bullet1 is a great bullet
  - sub bullet one has
  multiple lines!
    - sub sub bullet! oh no you didnt!!
    - oh yes i did!!
  1. this is great!!!
  2. ohhhh *heyyyyy yeah!!*
- bullet 2 is grand
-   this is a list item with multiple paragraphs

    here it is again

    another paragraph
- bullet 3
is a lazy piece of crap!!
omg poop

1. hai
2. number list!!


* blockquote in list item

    > line1
    > line2


- code in li, LOL

        (defun my-function (value)
          ;; lol
          (1+ value))

-and another
here's a test

    multi-paragraph list

    item


- paragraph

- list

- items
"
)
         (str (parse str)))
    (when return str)
    (tree str)
    ))

(defun test-links (&optional show)
  (format t "--------------~%")
  (let* ((str "
This is a paragraph with a ![panda][] picture of a [panda](http://wikipedia.com/pandas)
in it. Pandas are a [strange][1] kind of animal that are bears but are nice. I often wonder
why pandas are so [nice][2].

[1]: http://people.are.strange.com/when-youre-a-stranger/faces-look-ugly/when-youre-alone
[2]: http://nicethings.com

They could be [ripping us limb from limb](/my-page/on-ripping \"Rip!\") as seen in this
![ripping](/images/rip.jpg \"Photo of a shirt being ripped to shreds by a bear\") photo but
they *choose* not to.

[panda]: http://www.pandas.com/panda1.jpg
")
         (str (parse str)))
    (when show str)
    (tree str)
    ))
|#

