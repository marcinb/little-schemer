little-schemer
==============

Playing with lisp &amp; recursion while reading through [The Little Schemer](https://www.goodreads.com/book/show/548914.The_Little_Schemer)

## Install Scheme

If you're on a Mac, use [Homebrew](https://brew.sh/):

`brew install mit-scheme`

Otherwise, consult https://groups.csail.mit.edu/mac/users/gjs/6.945/dont-panic/

## Trying out the examples

Run scheme: `scheme`

To load a file with functions definitions (eg. *lists.scm*) do:

```
1 ]=> (load "lists.scm")

1 ]=> (rember `c `(a b c d))

;Value: (a b d)
```
