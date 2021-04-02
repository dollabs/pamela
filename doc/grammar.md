Pamela grammar
==============

This repository comprises the Pamela grammar in EBNF format
as well as tooling based on [instaparse](https://github.com/Engelberg/instaparse)

Please see
* The [Pamela Language Manual](PAMELA.md)

## Syntactic analysis

The `pamela` command line can run a simple syntactic check on your Pamela source file(s). For example:

```
pamela -i test/pamela/circuit.pamela -o circuit.txt check
```

If the Pamela source file parses correctly the parse tree will
be saved in the output file, else the source of the error
will be shown.

## insta2w3c

There are multiple variants of EBNF in use [[See XKCD on standards]](https://xkcd.com/927/).  It may be useful to convert from the flavor of EBNF used by
Instaparse to the flavor used by the [W3C](https://www.w3.org/TR/xquery/#EBNFNotation). The script `insta2w3c` will convert the Pamela Instaparse EBNF
(or another Instaparse EBNF file given as an argument)
to the W3C flavor of EBNF.

For example this W3C EBNF flavor is useful in generating
a "railroad diagram" using the
[Railroad Diagram Generator](http://www.bottlecaps.de/rr/ui).

In the case of Pamela this conversion has already been done:

* [pamela.ebnf](../resources/public/pamela.ebnf) Pamela Instaparse EBNF
* [pamela.ebnf.w3c](../resources/public/pamela.ebnf.w3c) Pamela W3C EBNF
* [pamela.railroad.xhtml](../resources/public/pamela.railroad.xhtml) Pamela Railroad Diagram

### Acknowledgement and Disclaimer:
This material is based upon work supported by the Army Contracting
and DARPA under contract No. W911NF-15-C-0005.
Any opinions, findings and conclusions or recommendations expressed
in this material are those of the author(s) and do necessarily reflect the
views of the Army Contracting Command and DARPA.

