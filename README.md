[![Build Status](https://travis-ci.org/rexim/Morganey.svg?branch=master)](https://travis-ci.org/rexim/Morganey)

# Morganey [![Status Umbra](https://img.shields.io/badge/status-umbra-red.svg)](https://github.com/ForNeVeR/andivionian-status-classifier)

It's the [Lambda Calculus][wiki-lambda-calculus] Interpreter and it
interprets lambda terms! \o/

## Usage ##

It's unusable yet. But we are working on this. Right now you can run
some unit tests. To do that you need to install [sbt][scala-sbt]
first. And after that you can simply

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
