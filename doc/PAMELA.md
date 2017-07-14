# Pamela language manual

This document is the reference manual for the Pamela language (Probabalistic Advanced Modeling and Execution Learning Architecture).

For background information on Pamela please see the primary
[README](../README.md)

For the Pamela EBNF grammar please see the [Pamela grammar](grammar.md)


## Pamela Introduction

The Pamela language is intended as a declarative language for expressing
domain models which can be "executed" with a planner and potentially
augmented through machine learning.

## Pamela source

Pamela source code is saved in files with the extension `.pamela`
(by convention). The "flavor" of the syntax corresponds to that
of Clojure - a modern LISP.

Pamela classes may be read from a file, from standard input, from
a string, retrieved from the model database or entered in
the Clojure REPL.

The `pamela` executable supports 2 primary operations: `build` and `htn`.  The `build` operation causes the Pamela to be read and translated into a rich Intermediate Representation (IR) that can easily be manipulated by software components.  The `htn` operation takes Pamela along with a specified root task, and then generates both a *hierarchical task network* (HTN) and a *temporal plan network* (TPN) that can be executed by a Pamela planner and execution environment.

## Pamela language

### defpclass

The `defpclass` command is the top level Pamela construct which
defines a Pamela class and defines a model symbol which is
bound to a the class constructor function.

```
(defpclass psw [gnd pwr]
  :meta {:version "0.2.1"
         :depends [[pwrvals "0.2.0"]]
         :doc "Power Switch"}

  :fields {:TP1 gnd
           :TP2 pwr
           :pwr (mode-of pwrvals :none)}
  :modes {:on (= :pwr :high)
          :off (= :pwr :none)
          :fail true}
  :methods [(defpmethod turn-on
              {:pre :off :post :on :bounds [1 3]
               :doc "turns on the power supply"}
              [])
            (defpmethod turn-off
              {:pre :on :post :off :bounds [1 3]
               :doc "turns off the power supply"}
              [])
            (defpmethod reset
              {:post :off
               :doc "resets the power supply"}
              [])]
  :transitions {:off:on {:pre :off :post :on
                         :doc "turning on"}
                :on:off {:pre :on :post :off
                         :doc "turning off"}
                :*:fail {:probability 0.0000001
                         :doc "spontaneous switch failure"}})
```

The first argument to `defpclass` is the Pamela class symbol (*pclass*)
being defined.

Then, there is a vector formal arguments to the *pclass* (which may be empty).

The *pclass* is then defined by a series of options: each introduced
with a keyword and elaborated with a map.

#### :meta

The **:meta** map contains optional metadata for the Pamela class,
with the following entries:

* **:doc** documentation string
* **:version** version string. Typically Pamela models are versioned using the [semantic versioning](http://semver.org/) scheme.
* **:icon** pathname to file for an icon representing this *pclass*
* **:depends** The *pclasses* that this *pclass* depends upon. This is a vector of vectors expressing each dependency.
** **[dependent-pclass "dependent-version"]** a dependency is expressed as a vector with the symbol for the dependent-pclass and the string corresponding the the dependent-version of that *pclass*. If a dependent *pclass* has not been declared prior to evaluating this **defpclass** an exception will be thrown.


#### :inherit

The **:inherit** option specifies a vector of Pamela super-classes.
The idea is that Pamela classes may subclass a parent class for
further specialization. Pamela superclasses are *not* yet implemented.

*NOTE*: **:inherit** may be folded in with **:meta** in the future.

#### :fields

Each field is like member data for the *pclass* and may refer
to another *pclass* instance, an *LVar* or a value. Simple
field values may be a boolean, string, keyword or a number.

A field is defined with a map of

* **:initial** The initial field value. This may be a simple value, or a symbol representing an argument passed into this *pclass* constructor, or a constructor call for another *pclass* or *LVar*.
* **:access** The access for this field :public or :private (the default).
* **:observable** True if this field is an observable input from the plant (false by default).  An observable field means that it's value may be set by the
  outside world interface. (Only pclass fields may be mutated as observables,
  not lvar's).

If the default values for **:access** and **:observable** are acceptable then
the initial field value may be used by itself (instead of in a subordinate map containing a single **:initial** key and value).

Arguments to a *pclass* constructor may be simple values, including
keywords (except for **:id** and **:interface**), or symbols. If a symbol is
used in a *pclass* constructor it may refer to a previously defined
field (but as a symbol, not a keyword) or an argument passed into this
*pclass*.

Options to a *pclass* constructor may include:

* **:id** The identifier (string) for this *pclass* instance.  Assumed to be "plant" if not explicitly specified.
* **:plant-part** The secondary identifier (string) for this *pclass* instance
* **:interface** The identifier (string) for interface used in plant interactions with *pclass* instance

#### :modes

Each *pclass* instance is always in one of a set of enumerated modes.

The modes for a *pclass* can be specifed in one of two ways:

* A vector of keywords
* A map with a keyword entry for each mode and a value representing the *mode condition*.

A *mode-condition* is a conditional expression, that when evaluated
to true, means that the given mode is *possible* for this *pclass*
instance. Certain modes (e.g. failure modes or enumerations)
may have the trivial condition expression `true` which means that
the given mode is always possible.

##### <a name="conditional-expressions"></a>conditional expressions

A conditional expression may be comprised, recursively, of the following:

* `(and a b c...)`
* `(or a b c...)`
* `(implies a b c...)`
* `(not a)`
* `(= d e)`

Each operand is disambiguated based on type as follows:

  * *Operand is a keyword*: First, it is checked to see if the keyword is a field name. If so the field reference is made explicit with `(:fieldname this)`.
  * *Operand is a keyword (but not a field name)*: If it is a mode name, then the mode reference is made explicit with `(mode-of this :mode-name)`.
  * The operand may be an explicit field reference `(:fieldname pclass)` where **pclass** may be `this` or one of the formal arguments to *defpclass*. When a field is a *pclass* the field reference is understood to return the mode of that *pclass instance*.
	  * **TODO**: *Can `pclass` refer to a literal pclass name (in addition to a pclass formal argument)?*
  * The operand may be an explicit mode reference `(mode-of pclass :mode-name)` where **pclass** may be `this` or the symbol for a previously defined *pclass* and **:mode-name** is one of the modes of that *pclass*.
     * **TODO**: *The use of `pclass` here is different than for `:fieldname` just above.  Can `pclass` refer to a pclass formal argument (in addition to a literal pclass name)?*
  * In the future, modes may be based on differential equations or linear equations.

#### :transitions

This (optional) section, provides the specifications of the mode transitions for the modes defined within this `defpclass`.


#### :methods and defpmethod

A *pclass* may have methods specified in the vector value of **:methods**.
Each element of this vector should be a Pamela method as constructed
by the *defpmethod* command.

````
(defpmethod turn-on
  {:pre :off :post :on :bounds [1 3]
   :doc "turns on the power supply"}
  [])
````

The first argument to `defpmethod` is the symbol for the method
to be defined. Note that this symbol cannot override the name any of the [Pamela built-in methods](#builtins).

Then there is an optional conditions map which may contain any
of the following keys:

* **:doc** The documentation string for this method
* **:pre** The precondition for running this method (see [conditional expressions](#conditional-expressions))
* **:post** The postcondition for running this method (see [conditional expressions](#conditional-expressions))
* **:bounds** The [bounds](#bounds) for this function
* **:cost** The cost of this function (number)
* **:reward** The reward of this function (number)
* **:controllable** By default, user-defined methods are not controllable (while the built-ins like **delay** and **ask** are controllable). This field may be set to true to indicate the method is controllable by the planner (boolean), meaning that the planner has the ability to cancel execution of that method before it finishes.
* **:primitive** By default, methods without a body are assumed to be primitive (where the implementation is defined by the plant), and methods with a body are non-primitive.  This provides an override: when `true`, the body is assumed to be documentation for the method behavior, that may optionally be used for higher-level reasoning (e.g., by the planner).
* **:display-name** This specifies the name of this method that is displayed to the user (e.g., in PlanViz).  If not specified, the `:display-name` is assumed to be a Title Case version of the method name symbol, with the hyphens converted to spaces (e.g., `"Turn On"`).

The third argument to `defpmethod` is a vector of zero or more formal arguments to the method.

The next form is the function which comprises the body of the method.
If the body is empty then this is considered a *primitive method* which
is implemented by the plant (unless overridden by the **:primitive** key as described above).

Lastly, following the function form of the method, there may be zero or more `between` statements.

<!--* *TBD*
* all not controllable by default, except delay

-->


### <a name="bounds"></a>bounds

Bounds values are a vector which contain a lower bound and an upper bound.
Each of the bounds are a number (integer or floating point) which represent
arbitrary time units for the execution engine. In addition the upper bound
may be the keyword **:infinity** to indicate there is no upper bound.

The special case of bounds `[0 :infinity]` is considered to be the
*default bounds* as there is no lower bound nor upper bound.

<!--
### conditions

mode conditions
transition pre, post conditions
defpmethod pre, post conditions
conditional expressions inside defpmethod's

### functions
-->

# <a name="builtins"></a>Pamela Built-In Methods
The following is the set of the built-in methods defined in Pamela. All of these are supported by the Pamela parser (i.e., via the `build` operation).  However methods marked with *"[Not yet supported]"* are not supported for the Pamela `htn` operation in the current Pamela release.

## **ask**
 * *Description*: Wait until the condition is true, and then proceed. Often `ask` is used in conjuction with a `tell` that is running in a parallel execution thread.  If the invocation takes longer than the (optionally) specifed upper temporal bound, the statement will be considered to have failed.
 * *Syntax*: `'(' 'ask' cond-expr opt-bounds? ')'`
 * *Example*: `(ask (= door "open") :bounds [0 10])`

## **assert**
 * *Description*: Instruct the execution environment to make the condition true (if it isn't already true), and then proceed. Usually, a call to `assert` will cause the planner to dynamically generate a new subplan (i.e., HTN and TPN) whose post condition will satisfy the asserted condition.  If the invocation takes longer than the (optionally) specifed upper temporal bound, the statement will be considered to have failed.
 * *Syntax*: `'(' 'assert' cond-expr opt-bounds? ')'`
 * *Example*: `(assert (= door "open") :bounds [0 10])`


## **choose**
 * *Description*: Select and execute one or more of the specified choices.  Syntactically, this is the same form as `choose-whenever`; the one exception is that when a choice is selected in a `choose`, that choice will never be cancelled and an alternative choice selected.  By default, one of the choices is selected, but the use of `:exactly`, `:min`, and `:max` will require more than one choice to be selected.  If N choices are chosen, and any of those choices should fail, the execution environment may select an alternative choice.
 * Guard conditions may be specified to indicate the condition for which a choice is to be considered.
 * Probability may be given to indicate the non-deterministic preference
    for a choice. If some choice conditions have guards that are not satisfied
    the probabilities are renormalized among the enabled choices prior to making
    a non-deterministic decision.
 * *Syntax*:
 * `choose   ::= '(' 'choose' choose-opt* choice+ ')'`
 * `choice   ::= '(' 'choice' choice-opt* fn ')'`
 * `choose-opt
         ::= fn-opt
           | exactly
           | min
           | max`
 * `choice-opt
         ::= label
           | opt-bounds
           | cost
           | reward
           | probability
           | guard
           | enter
           | leave`
 * *Example*:

```
 (choose :bounds [50 100]
        (choice
         (option1 :bounds [55 65]))
        (choice
         (option2 :bounds [65 75]))
        (choice
         (option3 :bounds [75 85])))
```

## **choose-whenever**
 * *Description*: Select and execute one or more of the specified choices.  Syntactically, this is the same form as `choose`; the one exception is that when a choice is selected in a `choose`, that choice will never be cancelled and an alternative choice selected.  For `choose-whenever`, the execution environment may cancel execution of a choice at any time, and begin execution of an alternative choice. By default, one of the choices is selected, but the use of `:exactly`, `:min`, and `:max` will require more than one choice to be selected.  If N choices are chosen, and any of those choices should fail, the execution environment may select an alternative choice.
 * Note: The execution environment will often rely on information contained in the `choice-opt` declarations to guide when and which choices should be cancelled and selected.
 * *Syntax*:
 * `choose-whenever   ::= '(' 'choose-whenever' choose-opt* choice+ ')'`
 * `choice   ::= '(' 'choice' choice-opt* fn ')'`
 * `choose-opt
         ::= fn-opt
           | exactly
           | min
           | max`
 * `choice-opt
         ::= label
           | opt-bounds
           | cost
           | reward
           | probability
           | guard
           | enter
           | leave`
 * *Example*:

```
 (choose-whenever :bounds [50 100]
        (choice
         (option1 :bounds [55 65]))
        (choice
         (option2 :bounds [65 75]))
        (choice
         (option3 :bounds [75 85])))
```


## **optional**
 * *Description*: Optionally, execute the `fn` statement.
 * Note: In order for the execution environment to determine whether the optional `fn` statement should be executed, it would be useful to specify one or more `fn-opt`  parameters (e.g., `:cost<= 10`).
 * This statement is implemented as a macro in terms of `choose` and `delay`.  Since it is a macro, an `optional` statement will never appear in the Pamela Intermediate Representation (IR).
 * *Syntax*: `'(' 'optional' fn-opt* fn ')'`
 * *Example*: `(optional (clean-window))`




## **delay**
 * *Description*: Wait for a time duration as specified by the `:bounds`.   time units (typically seconds), as specified by the lower bound of the `:bounds` specification.  Note that the upper bound is ignored.
 * *Syntax*: `'(' 'delay' delay-opt* ')'`
 * *Example*: `(delay :bounds [10 10])`

## **parallel**
 * *Description*: Execute each of the subordinate statements (logically) in parallel.  There is no requirement that the all of the statements are actually executed concurrently, nor is there any implied order of execution.  All that matters is that the `parallel` statement will complete when all of the subordinate statements complete.
 * *Syntax*:
 * `'(' 'parallel' fn-opt* fn+ ')'`
 * *Example*:
```
 (parallel :bounds [50 100]
          (do-this)
          (do-that))
```

## **slack-parallel**
 * *Description*: Execute each of the subordinate statements (logically) in parallel.  There is no requirement that the all of the statements are actually executed concurrently, nor is there any implied order of execution.  All that matters is that the `slack-parallel` statement will complete when all of the subordinate statements complete.
 * The *==slack==* refers to the fact that each subordinate statement is surrounded (before and after) by a `delay :bounds [0 :infinity]`
 * This statement is implemented as a macro in terms of `parallel`, `sequence` and `delay`.  Since it is a macro, a `slack-parallel` statement will never appear in the Pamela Intermediate Representation (IR).
 * *Syntax*:
 * `'(' 'slack-parallel' fn-opt* fn+ ')'`
 * *Example*:
```
 (slack-parallel :bounds [50 100]
          (do-this)
          (do-that))
```


## **soft-parallel**
 * *Description*: Execute each of the subordinate statements (logically) in parallel.  There is no requirement that the all of the statements are actually executed concurrently, nor is there any implied order of execution.  All that matters is that the `soft-parallel` statement will complete when all of the subordinate statements complete.
 * The *==soft==* refers to the fact that each subordinate statement is treated as a choice between that statement and a 0-duration `delay`, i.e., that each subordinate statement is considered optional.
 * This statement is implemented as a macro in terms of `parallel`, `choose` and `delay`.  Since it is a macro, a `soft-parallel` statement will never appear in the Pamela Intermediate Representation (IR).
 * *Syntax*:
 * `'(' 'slack-parallel' fn-opt* fn+ ')'`
 * *Example*:
```
 (soft-parallel :bounds [50 100]
          (do-this)
          (do-that))
```


## **sequence**
 * *Description*: Execute each of the subordinate statements in sequential order.  The `sequence` statement will complete when all of the subordinate statements complete.
 * *Syntax*:
 * `'(' 'sequence' fn-opt* fn+ ')'`
 * *Example*:
```
 (sequence :bounds [50 100]
          (do-this-first)
          (do-this-second))
```

## **slack-sequence**
 * *Description*: Execute each of the subordinate statements in sequential order.  The `slack-sequence` statement will complete when all of the subordinate statements complete.
 * The *==slack==* refers to the fact that each subordinate statement is surrounded (before and after) by a `delay :bounds [0 :infinity]`
 * This statement is implemented as a macro in terms of `sequence` and `delay`.  Since it is a macro, a `slack-sequence` statement will never appear in the Pamela Intermediate Representation (IR).
 * *Syntax*:
 * `'(' 'slack-sequence' fn-opt* fn+ ')'`
 * *Example*:
```
 (slack-sequence :bounds [50 100]
          (do-this-first)
          (do-this-second))
```


## **soft-sequence**
 * *Description*: Execute each of the subordinate statements in sequential order.  The `soft-sequence` statement will complete when all of the subordinate statements complete.
 * The *==soft==* refers to the fact that each subordinate statement is treated as a choice between that statement and a 0-duration `delay`, i.e., that each subordinate statement is considered optional.
 * This statement is implemented as a macro in terms of `sequence`, `choose` and `delay`.  Since it is a macro, a `soft-sequence` statement will never appear in the Pamela Intermediate Representation (IR).
 * *Syntax*:
 * `'(' 'soft-sequence' fn-opt* fn+ ')'`
 * *Example*:
```
 (soft-sequence :bounds [50 100]
          (do-this-first)
          (do-this-second))
```


## **tell**
 * *Description*: Instruct the execution environment that the specified condition is true, and then proceed. Often `ask` is used in conjuction with a `tell` that is running in a parallel execution thread.
 * *Syntax*: `'(' 'tell' cond-expr ')'`
 * *Example*: `(tell (= door "open"))`

## **try** [Not yet supported]
 * *Description*: Attempt to execute the initial `fn` within the specified temporal bounds.  If that `fn` invocation fails (because of an explicit failure of the `fn`, or because of a violation of the temporal bounds), then the `catch` `fn` is executed.
 * *Syntax*: `'(' 'try' opt-bounds? fn '(' 'catch' fn ')' ')'`
 * *Example*:

```
(try :bounds [2 3]
     (initialize)
     (catch (reset-and-initialize)))
```


## **unless** [Not yet supported]
 * *Description*: If the condition is not true, execute the statement `fn` one time.  The condition is checked only once. The `:bounds`, if specified, applies to the duration of the entire statement.
 * Note the distinction of the duration and frequency in which the condition is checked among the `unless`, `when`, `whenever`, and `maintain` statements.
 * *Syntax*: `'(' 'unless' cond-expr opt-bounds? fn ')'`
 * *Example*:

```
(unless (= door "open") :bounds [2 3]
     (open-door))
```



## **when** [Not yet supported]
 * *Description*: If the condition is true, execute the statement `fn` one time.  If the condition is not initially true, execution of this `when` statement will wait until either 1) the condition becomes true, or 2) the temporal duration (`:bounds`) has been exceeded.
 * Note the distinction of the duration and frequency in which the condition is checked among the `unless`, `when`, `whenever`, and `maintain` statements.
 * *Syntax*: `'(' 'when' cond-expr opt-bounds? fn ')'`
 * *Example*:

```
(when (not (= door "open")) :bounds [2 3]
     (open-door))
```

## **whenever** [Not yet supported]
 * *Description*: During the temporal scope specified by `:bounds`, every time the condition is true, execute the statement `fn` one time.  This assumes that once execution of the `fn` has begun, the `cond-expr` won't be revisited until execution of the `fn` has completed.
 * Note the distinction of the duration and frequency in which the condition is checked among the `unless`, `when`, `whenever`, and `maintain` statements.
 * Note: as in all of Pamela, the default value for an unspecified `:bounds` is `[0 :infinity]`.  So, a `whenever` statement with an unspecified bounds should be part of a `parallel` statement.
 * *Syntax*: `'(' 'whenever' cond-expr opt-bounds? fn ')'`
 * *Example*:

```
(whenever (not (= door "open")) :bounds [2 30]
     (open-door))
```

## **maintain** [Not yet supported]
 * *Description*: Execute the statement `fn` one time.  However, while doing so, the condition `cond-expr` **can** **never** be violated.
 * Note the distinction of the duration and frequency in which the condition is checked among the `unless`, `when`, `whenever`, and `maintain` statements.
 * *Syntax*: `'(' 'maintain' cond-expr opt-bounds? fn ')'`
 * *Example*:

```
(maintain (= door "open") :bounds [2 30]
     (do-lots-of-stuff-possibly-involving-the-door))
```

## **soft-maintain** [Not yet supported] [Not yet supported by parser and IR]
 * *Description*: Execute the statement `fn` one time.  While doing so, the condition `cond-expr` **can** be violated.  However, at the completion of executing the statement `fn`, the condition `cond-expr` must be true.
 * Note the distinction of the duration and frequency in which the condition is checked among the `unless`, `when`, `whenever`, and `maintain` statements.
 * *Syntax*: `'(' 'soft-maintain' cond-expr opt-bounds? fn ')'`
 * *Example*:

```
(soft-maintain (= door "open") :bounds [2 30]
     (do-lots-of-stuff-possibly-involving-the-door))
```


## **dotimes**
 * *Description*: Repeat the `fn` the specified number of times, as part of a `sequence`.
 * *Syntax*: `'(' 'dotimes' repeat-count fn ')'`
 * This statement is implemented as a macro in terms of a `sequence`.  Since it is a macro, this statement will never appear in the Pamela Intermediate Representation (IR).
 * Note: Since `dotimes` is a macro, `repeat-count` must be a literal number (i.e., not a variable).
 * *Example*: `(dotimes 3 (open-door))`  which is macroexpanded to: `(sequence
 (open-door)
 (open-door)
 (open-door))`

