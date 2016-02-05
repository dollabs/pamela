PAMELA grammar
==============

This repository comprises the PAMELA grammar in EBNF format
as well as some tooling based on [instaparse](https://github.com/Engelberg/instaparse)

NOTE: PAMELA does not actually use this EBNF directly as
part of its parsing engine.

## pgrammar

The `pgrammar` script is located in `../bin` and can be used
to quickly check the grammar of a PAMELA input file.

With one argument **pgrammar** checks that one file:
````
tmarble@ficelle 347 :) pgrammar ../src/test/pamela/waypoint.pamela
Checking grammar for: waypoint.pamela
Success! parse tree in:  ../src/test/pamela/waypoint.txt
parse diagram in:  ../src/test/pamela/waypoint.png
tmarble@ficelle 348 :) pgrammar

````

On errors **pgrammar** will return exit code 1 and point to the problem:
````
tmarble@ficelle 345 :) pgrammar bad-args.pamela
Checking grammar for: bad-args.pamela
{:index 20,
 :reason [{:tag :string, :expecting "["}],
 :line 1,
 :column 21,
 :text "(defpclass bad-args :not-a-vector)"}
Failure!
tmarble@ficelle 346 :(
````

Without any arguments **pgrammar** will test all the files located
in `../src/test/pamela/regression`.

## analyze

You may run the analyzer on the PAMELA regression tests as follows:

````
$ ./bin/analyze
Analyzing PAMELA regression files...
Checking grammar for: try-bounds.pamela
Success! parse tree in:  target/analysis/try-bounds.txt
...

view the regression files with...
open target/analysis/index.html
````

## insta2w3c

It may be useful to convert from the flavor of EBNF used by
Instaparse to the flavor used by the [W3C](https://www.w3.org/TR/xquery/#EBNFNotation). The script `insta2w3c` will convert the PAMELA EBNF
(or another Instaparse EBNF file given as an argument)
to the w3c flavor of EBNF.

For example this w3c EBNF flavor is useful in generating
a "railroad diagram" using the
[Railroad Diagram Generator](http://www.bottlecaps.de/rr/ui).

In the case of PAMELA this conversion has already been done:

* [pamela.ebnf](resources/data/pamela.ebnf) PAMELA Instaparse EBNF
* [pamela.ebnf.w3c](resources/data/pamela.ebnf.w3c) PAMELA w3c EBNF
* [pamela.railroad.svg](resources/data/pamela.railroad.svg) PAMELA Railroad Diagram

![PAMELA Railroad Diagram](http://dollabs.github.io/pamela/grammar/resources/data/pamela.railroad.svg")

### Acknowledgement and Disclaimer:
This material is based upon work supported by the Army Contracting
and DARPA under contract No. W911NF-15-C-0005.
Any opinions, findings and conclusions or recommendations expressed
in this material are those of the author(s) and do necessarily reflect the
views of the Army Contracting Command and DARPA.
