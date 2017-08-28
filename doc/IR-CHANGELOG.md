IR CHANGELOG
============

The purpose of this document is to cover changes in the
PAMELA Intermediate Representation (IR).

## IR Thumbnail

*NOTE:* there currently is not an overall grammar for the IR

The IR is represented as a Clojure map. The keys
of the top level map are symbols PAMELA classes (with the exception
of pamela/lvars which lists Logical Variable default values).

Generally speaking each subcomponent of the IR is a Clojure
map which includes a `:type` key.

Any element of the IR can be referred to by an **irks** vector
(think IR keys). The idea is that the Clojure form
`(get-in ir irks)` should return the element designated
by **irks** (see [get-in](https://clojuredocs.org/clojure.core/get-in)).

## IR :types

_TBD_

## IR Changes

1. Handling multiple function arities (#86)
  * with only one method definition ("mdef") per name the **irks** to the **mdef** was `[pclass :methods method]`
  * now `[pclass :methods method]` is a vector or **mdefs** (which differ in signature, including arity)
2. New syntatic organization of rubrics with PR#137
  * In the process of fixing #121 running `boot test --namespaces testing.pamela.parser` passed 100% indicating proper semantic comparison of the maps. New rubrics were checked in to ensure that they are also syntactically identical (benefiting from the previous changes in sorting maps).
3. Changes from fixing Issue #136 Field references as symbols
  * All IR references to fields are now symbols instead of keywords.
  * In order to carefully verify the new parser implementation new tool `bin/diffir` was created to analyze IR changes from running `boot test --namespaces testing.pamela.parser`
  * The output of this tool has been captured here in [issue-136.diffir.txt](issue-136.diffir.txt) and [issue-136.diffir.diff](issue-136.diffir.diff) such that one can verify that there are no structural changes to the IR.
