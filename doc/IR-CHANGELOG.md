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
