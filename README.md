[![Build Status](https://travis-ci.org/rexim/Morganey.svg?branch=master)](https://travis-ci.org/rexim/Morganey)

# Morganey [![Status Umbra](https://img.shields.io/badge/status-umbra-red.svg)](https://github.com/ForNeVeR/andivionian-status-classifier)

It's the [Lambda Calculus][wiki-lambda-calculus] Interpreter and it
interprets lambda terms! \o/

## Usage ##

Before doing anything useful with Morganey you need to install
[sbt][scala-sbt] first.

### REPL ###

To run the REPL just enter

    $ sbt run

in the source code directory and start typing lambda terms there. The
syntax of the lambda terms is

    <term> ::= <variable> | <function> | <application>
    <variable> ::= [a-z]+
    <function> ::= ( <lambda-symbol> . <variable> <term> )
    <application> ::= ( <term> <term> )
    <lambda-symbol> ::= λ | \

The REPL will take the entered lambda term, beta-reduce it with the
normal order reduction strategy and output the normal form of the
entered lambda term.

**WARNING!** If the term is not reduciable to a beta normal form (for
example `(\x . (x x)) (\x . (x x))`) the REPL will crash with the
stack overflow exception.

To quit the REPL just `^C` it.

### Unit Tests ###

To run the Unit Tests enter

    $ sbt clean coverage test

in the source code directory.

And after that you can take a look at the Unit Test coverage
results. Just open `target/scala-2.11/scoverage-report/index.html`
with your favorite browser.

Enjoy!

## License ##

Copyright (C) 2015 Codingteam

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

[wiki-lambda-calculus]: https://en.wikipedia.org/wiki/Lambda_calculus
[scala-sbt]: http://www.scala-sbt.org/
