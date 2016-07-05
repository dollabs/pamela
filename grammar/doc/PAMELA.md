# PAMELA language manual

This document (will become) the reference manual for the PAMELA language.

* pclass
 * A pclass may, optionally, be given an :id to identify this instance
   (the :id is unset if not explicitly given)

* modes
 * Modes may be a simple enum
 * Modes may be based on a simply boolean expression involving fields
 * In the future modes may be based on differential equations or
   linear equations.

* choice
  * guard conditions may be specified to indicate a choice is to be considered
  * probability may be given to indicate the non-deterministic preference
    for a choice. If some choice conditions have guards that are not satisified
    the probabilities are renormalized among the enabled choices prior to making
    a non-deterministic decision.

* observable
  An observable field means that it's value may be set by the
  outside world interface. (Only pclass fields may be mutated as observables,
  not lvar's).

* fields
  * access is private by default
