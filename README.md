markdown.cl
===========
This is a [markdown](http://daringfireball.net/projects/markdown/syntax) parsing
library for Common Lisp. It also parses table markdown using the github syntax. 

The goal is to provide a very simple interface for
turning markdown into valid HTML.

It currently doesn't support plugins or hooks or anything of that kind. I'd like
to add these in the near future, so stay tuned.

*markdown.cl is in beta!!!* Expect bugs/parsing errors.

Documentation
-------------
The interface exposes two functions:

### parse
```common-lisp
(defun parse (markdown-string &key disable-parsers))
  => HTML string
```
This takes a markdown string and converts it into HTML.

The `:disable-parsers` keyword allows disabling of any sub-parser during the
processing. None of these parsers are currently public, however I'd like to
eventually make interfaces for them to be customizable (which would more or less
act as a plugin system).

### parse-file
```common-lisp
(defun parse-file (path &key disable-parsers))
  => HTML string
```
This parses a markdown file by loading the contents and sending them to [parse](#parse).
It's just a simple wrapper.

Tests
-----
The tests for markdown.cl are, right now, just large blobs of markdown that are
parsed and tested using [xmls](http://common-lisp.net/project/xmls/) (my
favorite XML parser).

```common-lisp
(ql:quickload :markdown.cl-test)
(markdown.cl-test:run-tests)
```
