Sild
=====

Sild Is a Lisp Dialect! _Sild_ also means "herring" in Danish. It's also an
anagram of "lisp" if you flip the 'p' around.

```
.        `\\\,
|\ _.--''""````"''--._
  }sild             o `\
|/ '--.._____'`'_._..~~`
`
```

Why did you do this?
------------------

For the learns. I wrote three iterations of this language. The first one was
a [disaster](https://github.com/urthbound/risp)! The [second
](https://github.com/urthbound/sildold) I got working pretty well! But then I
tried to explain it to someone and completely couldn't, so I threw it all out
and started over for a 3rd time, making really atomic commits and
writing blog posts about the whole thing as I was developing it. Those posts
currently live here:

[blog.jfo.click](http://blog.jfo.click)

Sild could best be described as a busted scheme-like lisp. I did not attempt to
achieve compliance with any [existing standard](http://www.r6rs.org/), but in
the absence of a compelling reason to the contrary I would default to a
scheme-like design. I used [Petite Chez
Scheme](http://www.scheme.com/petitechezscheme.html) as a model in these
instances.

I learned an enormous amount from this project, about Lisp history and
practice, about C, and about language design in general. Sild still has a long
way to go before it could be considered useful, but it is nonetheless, in a very
real sense, already a _functioning programming language_.

Installation
------------

```
git clone http://github.com/urthbound/sild
cd sild
```

and then...

```
make
```

That should work on OSX! It will probably work on other systems, too, but I
don't know that for sure.

That will give you a `sild` executable that you can execute directly, or put on
your path.

Usage:
------

```
$ ./sild filename.sld
```

... in the directory where `sild` lives Will run the file specified by
`filename.sld`! I've chosen `.sld` as the Sild extension. Because it is 3/4 of
the same letters. Very clever.

Syntax
------

Sild is a lisp dialect. Right now, it supports the following:

```
quote
car
cdr
cons
eq
atom
cond
```

(And also the `'` macro!)

It has two top level only void return value functions!

```
define
display
```

And it has lambdas!

```
lambda
```

For more detailed explanations and examples, see
[examples/basics.sld](http://github.com/urthbound/sild/blob/master/examples/basics.sld)

You can top level bind whatever to these canonical names, I enjoy this
one:

```scheme
(define λ lambda)
```

You could in theory do this for all the keywords! Here is a fully functional
[Japanese example](http://github.com/urthbound/sild/blob/master/examples/japanese.sld).

Todo
----

Oh so, so much! For starters, there's an enormous memory leak with regard to
persistent procedure environments. So I need proper garbage collection of some kind!

It would be nice to have more effective error messages in general, and a stack
trace!

And a proper cross platform makefile!

Also there are no numbers or mathematical operations, or strings or string
operations, or types of any kind really, or system IO.

And it's not properly tail call optimized! This is a big one.

And there is no repl, either! A lisp without a repl, for shame, for shame.

I consider these features to be beyond the working v0.1 prototype presented
here, and look forward to studying how to implement them in the future! I
wanted very much to do all these things and then release the whole language at
once, but I would never have finished, frankly, and the original intent was to
implement a minimal lisp in which I would be able to write non-trivial lambda
calculus in. It can do that now, so hooray!
