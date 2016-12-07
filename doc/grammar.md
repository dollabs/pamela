PAMELA grammar
==============

This repository comprises the PAMELA grammar in EBNF format
as well as tooling based on [instaparse](https://github.com/Engelberg/instaparse)

Please see
* The [PAMELA Language Manual](doc/PAMELA.md)

## Syntactic analysis

The `pamela` command line can run a simple syntactic check on your pamela source file(s). For example:

```
pamela -i test/pamela/circuit.pamela -o circuit.txt
```

If the PAMELA source file parses correctly the parse tree will
be saved in the output file, else the source of the error
will be shown.

## insta2w3c

It may be useful to convert from the flavor of EBNF used by
Instaparse to the flavor used by the [W3C](https://www.w3.org/TR/xquery/#EBNFNotation). The script `insta2w3c` will convert the PAMELA EBNF
(or another Instaparse EBNF file given as an argument)
to the w3c flavor of EBNF.

For example this w3c EBNF flavor is useful in generating
a "railroad diagram" using the
[Railroad Diagram Generator](http://www.bottlecaps.de/rr/ui).

In the case of PAMELA this conversion has already been done:

* [pamela.ebnf](../resources/public/pamela.ebnf) PAMELA Instaparse EBNF
* [pamela.ebnf.w3c](../resources/public/pamela.ebnf.w3c) PAMELA w3c EBNF
* [pamela.railroad.xhtml](http://dollabs.github.io/pamela/doc/pamela.railroad.xhtml) PAMELA Railroad Diagram

### Acknowledgement and Disclaimer:
This material is based upon work supported by the Army Contracting
and DARPA under contract No. W911NF-15-C-0005.
Any opinions, findings and conclusions or recommendations expressed
in this material are those of the author(s) and do necessarily reflect the
views of the Army Contracting Command and DARPA.
