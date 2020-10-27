# Pamela Language Manual

This document is the reference manual for the Pamela language (Probabilistic Advanced Modeling and Execution Learning Architecture).

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
bound to a the class constructor method.

```
(defpclass psw [gnd pwr]
  :meta {:version "0.2.1"
         :depends [[pwrvals "0.2.0"]]
         :doc "Power Switch"}

  :fields {TP1 gnd
           TP2 pwr
           pwr (mode-of pwrvals :none)}
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

The **:inherit** option specifies a vector of Pamela superclasses.
The idea is that Pamela classes may subclass a parent class for
code reuse and specialization. Pamela superclass inheritance is **not** yet implemented.

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

The modes for a *pclass* can be specified in one of two ways:

* A vector of keywords
* A map with a keyword entry for each mode and a value representing the *mode condition*.

A *mode-condition* is a conditional expression, that when evaluated
to true, means that the given mode is *possible* for this *pclass*
instance. Certain modes (e.g. failure modes or enumerations)
may have the trivial condition expression `true` which means that
the given mode is always possible.

##### <a name="conditional-expressions"></a>Conditional expressions

A conditional expression may be comprised, recursively, of the following:

* `(and a b c...)`
* `(or a b c...)`
* `(implies a b c...)`
* `(not a)`
* `(= d e)`
* `(> h j)`
* `(>= h j)`
* `(< h j)`
* `(<= h j)`
* `(same f g)`
* `(call "foo/bar" d e)`
* `(propositions [(wm ltm (recency 5)) :is-connected-to a b) ...]
   where conditional-expression)`

In the case of `call` conditional expressions, the first argument (`"foo/bar"` in this example) specifies the name of a Clojure (or Java) function that can be dynamically called with the remaining arguments, in order to determine the *truthy* value of the condition.

For two objects `d` and `e`, (= d e) is true if the two objects are believed to have the same mode. If `d` and `e` are objects of a switch class that has modes `on` and `off`, (= d e) is true if both switches are on or if both switches are off. (same f g) is true if f and g are the same switch.  This is an essential form of comparison for retrieving objects linked by propositions.  The inequalities <, <=, >, >= can be used to compare numbers in pre and post conditions.

A proposition has the form (:proposition-type arg1 ... argn), the most common of which is the two argument case. It is useful to lookup propositions in the preconditions to methods.

For example, to find a man who is married to a woman who was born in the same town as him, we can use the following:

```
(propositions [:is-a X man]
              [:is-married-to X Y]
              [:is-a Y woman]
   where (same X.city-of-birth Y.city-of-birth))
```

Note that in this case if city-of-birth is a simple string, like "Boston", `(= X.city-of-birth Y.city-of-birth)` would poduce the same result as `(same ...)` but if the cities were objects that had fields and modes, the `=` would only compare their modes.

Cities tend not to be very dynamic, but let's say that the mode of a city were to represent whether the city was dry or not, let's say that cities were modeled to be :dry :wet :dry-on-weekends.  The law could change based on voting of the local government.

In this case using `(= X.city-of-birth Y.city-of-birth)` would be true for any two cities that were both dry, wet, or dry-on-weekends.  (same ...) would be true if and only if the X and Y were the same object -- and hence the same city.  Propositions, listed within square brackets, can be proceeded with advice on where to find the proposition.  Three constraints can be provided, 'wm' indicating working memory, 'ltm' indicating long-term memory, perhaps implemented in an object base, and (recency n) which limits the search to propositions that are no older than `n`, so to match a man, in long-term memory, which proposition is no older than `n` one would write: `(propositions [(ltm (recency n)) :is-a X man] ...)`.  It should be noted that the 'where' condition cannot be used to match values other than those in the objects that make up the propositions.  The 'where' is used to narrow the search for propositions and thus must be applied only to proposition objects.

The proposition form returns `true` or `false` but any LVARS bound in the process remain bound for the duration of the method.  In the above examples, if the class definition that contained the method had a field `[... X (lvar "a man") ...]` the LVAR `X`would be bound to one such instance found in memory that satisfies any `where` condition.  If multiple matching propositions are found, one will be chosen randomly.

* For `:pre` conditions of a pmethod, the condition is implicitly tested to be true prior to the actual invocation of the method.
* For `:post` conditions of a pmethod, the condition is implicitly asserted at the end of the actual invocation of the method. The post condition represents a guarantee of the successful application of the method.

Each operand is disambiguated based on type as follows:

  * *Operand is a keyword*: If it is a mode name, then the mode reference is made explicit with `(mode-of this :mode-name)`.
  * The operand may be an explicit field reference `X.fieldname1` where **X** may be one of the fields of the root class using arbitrary dot references. When a field is a an instance of a *pclass* the field reference is understood to refer to the mode of that *pclass instance* but the object itself can be referred to using the (same ...) predicate described above.
  * The operand may be an explicit mode reference `(mode-of pclass :mode-name)` where **pclass** may be `this` or the symbol for a previously defined *pclass* and **:mode-name** is one of the modes of that *pclass*.
  * In the future, modes may support differential equations and/or linear equations. This capability is not yet implemented.

#### :bounds

Bounds represent the lower and upper bounds that an activity is expected to take. Bounds are generally represented by numbers that refer to the time units of the model, such as `:bounds  [4 8]`, meaning that the activity in question is expected to take at least 4 and at most 8 time units.  If the time units for the model were seconds, that would mean between 4 and 8 seconds.  How these values are arrived at is up to the modeler.  Given historical data, one way would be for the lower bound to be the mean minus two standard deviations and the upper bound being the mean plus two standard deviations.  Sometimes the bounds can be learned and read in from a file; for that LVARS can be used in place of the constants.  Furthermore, limited arithmetic is permitted.  Expressions using `(+ x y)` `(- x y)` `(* x y)` or `(/ x y)` may be used and nested as long as `x` and `y` are constants or lvars.

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
  [flavor]
  (sequence (prepare-to-turn-on)(really-turn-on flavor)))
````

The first argument to `defpmethod` is the symbol for the method
to be defined. Note that this symbol cannot override the name any of the [Pamela built-in methods](#builtins).

Then there is an *optional* conditions map which may contain any
of the following keys:

* **:doc** The documentation string for this method
* **:pre** The precondition for running this method (see [conditional expressions](#conditional-expressions))
* **:post** The postcondition for running this method (see [conditional expressions](#conditional-expressions))
* **:bounds** The [bounds](#bounds) for this method
* **:cost** The cost of this method (number)
* **:reward** The reward of this method (number)
* **:controllable** By default, user-defined methods are not controllable (while the built-ins like **delay** and **ask** are controllable). This field may be set to true to indicate the method is controllable by the planner (boolean), meaning that the planner has the ability to cancel execution of that method before it finishes.
* **:primitive** By default, methods without a body are assumed to be primitive (where the implementation is defined by the plant), and methods with a body are non-primitive.  This provides an override: when `true`, the body is assumed to be documentation for the method behavior, that may optionally be used for higher-level reasoning (e.g., by the planner).
* **:display-name** This specifies the name of this method that is displayed to the user (e.g., in PlanViz).  If not specified, the `:display-name` is assumed to be a Title Case version of the method name symbol, with the hyphens converted to spaces (e.g., `"Turn On"`).

The third argument to `defpmethod` is a vector of zero or more formal arguments to the method.

The next form is the body of the method, which is a call to either a user-defined method (defined via `defpmethod`) or one of the built-in Pamela methods (see below).
If the body is empty then this is considered a *primitive method* which
is implemented by the plant (unless overridden by the **:primitive** key as described above).

##### Arguments used within calls to user-defined methods
When calling a user method (either non-primitive or primitive), each of the arguments can be a **Literal** (i.e., boolean, string, number, map, vector, keyword) or a **Reference**.  A **reference** can be either a **Symbol** or a dotted pair of symbols.

**Symbols** must have a value (through argument unification via args list) that is one of the supported literals (see above).  Alternatively, the value of a symbol could be a pclass instance.  In the case of a dotted pair of symbols, the first symbol designates a pclass instance and the second refers to a field of that pclass instance.

A **keyword** represents a specific *mode* value (if that mode is defined in the **modes** section of the current `defpclass`).  Otherwise, it's treated as a simple literal, very similar to a string.  [In fact, when generating JSON forms of the HTN/TPN, keywords and strings have the same representation type].

When specifying a complex literal as an argument value (i.e., map or vector), any of the components of that complex literal structure can be a literal of any type.  E.g., `{:age 40 :siblings ["tom" "dick" "harry"]`

Lastly, following the main body of the method, there may be zero or more `between` statements.

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

Note that the *Execution* sections describe how the Pamela execution environment deals with the Pamela statement.  There are logically 4 separate components of the Pamela execution environment:

1. *Dispatcher Manager*: Given a new HTN/TPN, the *Dispatcher Manager* creates a *Dispatcher* instance to execute that HTN/TPN.  (Note that the TPN is what is actually executed.  The HTN may have been used to generate the TPN, and may be used by Planviz to display the HTN and its execution status).
1. *Dispatcher*: This orchestrates the elements of the TPN in the proper order, sending activities to the *Belief State Manager* (BSM) and the multiple *Plant* instances.  The *Dispatcher* will dereference any symbolic references contained in the arguments (using the BSM), before sending a command statement to a Plant instance.  However, statements containing conditions will be sent to BSM in their entirety.
2. *Belief State Manager* (BSM): Based on the initial declarations in the Pamela files, along with the observations obtained during the execution of the plan, the *BSM* maintains the state of each of the: 1) global states, 2) the mode of each *Plant* instance, and 3) the fields of each *Plant* instance.  Initiated by requests from a *Dispatcher*, this information is used in 2 different ways:
	1. To dereference any symbolic references (e.g., field name) contained in the arguments of a Pamela statement.  The value of the symbolic reference is returned to the requesting *Dispatcher* as either a literal or as an error (i.e., when the reference is an invalid field name) (error representation TBD).
	1. To determine the truth value of any condition contained in a Pamela statement. The value of a condition is one of 3 values: `true`, `false`, or an error (error representation TBD).
3. *Planner*: A component of the BSM, it generates a new plan (HTN/TPN) to satisfy a condition, generally as specified by an `assert` statement.
4. *Plant*: There are one or more *Plant*s that receive and execute commands from the *Dispatcher*.

## **ask**
 * *Description*: Wait until the condition is true, and then proceed. Often `ask` is used in conjunction with a `tell` that is running in a parallel execution thread.  If the invocation takes longer than the (optionally) specified upper temporal bound, the statement will be considered to have failed.
 * *Syntax*: `'(' 'ask' cond-expr opt-bounds? ')'`
 * *Example*: `(ask (= door "open") :bounds [0 10])`
 * *Execution*:
  * The *Dispatcher* sends the `ask` statement (which includes the condition) to BSM.
  * BSM replies to *Dispatcher* with a finished status (either success or failure)
  * **TBD**: error handling (e.g., for invalid operands to conditions)

## **assert**
 * *Description*: Instruct the execution environment to make the condition true (if it isn't already true), and then proceed. Usually, a call to `assert` will cause the planner to dynamically generate a new subplan (i.e., HTN and TPN) whose post condition will satisfy the asserted condition.  If the invocation takes longer than the (optionally) specified upper temporal bound, the statement will be considered to have failed.
 * *Syntax*: `'(' 'assert' cond-expr opt-bounds? ')'`
 * *Example*: `(assert (= door "open") :bounds [0 10])`
 * *Execution*:
	 * The current *Dispatcher* (*Dispatcher-1*) sends the `assert` statement (which includes the condition) to BSM.
	 * BSM uses the *Planner* to generate a new plan (HTN/TPN) that satisfies this condition.
	 * BSM sends this new plan to the *Dispatcher Manager* which launches a new *Dispatcher* (*Dispatcher-2*) with that plan.
	 * *Dispatcher-2* then executes that plan, starting with the first activity(ies)
	 * When the plan has completed, the *Dispatcher-2* sends the status to BSM.  (There is little advantage of sending this status back to BSM via the *Dispatcher Manager*, but that alternative would be possible).
	 * BSM replies to *Dispatcher-1* with finished status of the `assert` (either success or failure).

## **choose**
 * *Description*: Select and execute one or more of the specified choices.  Syntactically, this is the same form as `choose-whenever`; the one exception is that when a choice is selected in a `choose`, that choice will never be canceled and an alternative choice selected.  By default, one of the choices is selected, but the use of `:exactly`, `:min`, and `:max` will require more than one choice to be selected.  If N choices are chosen, and any of those choices should fail, the execution environment may select an alternative choice.
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
 * *Description*: Select and execute one or more of the specified choices.  Syntactically, this is the same form as `choose`; the one exception is that when a choice is selected in a `choose`, that choice will never be canceled and an alternative choice selected.  For `choose-whenever`, the execution environment may cancel execution of a choice at any time, and begin execution of an alternative choice. By default, one of the choices is selected, but the use of `:exactly`, `:min`, and `:max` will require more than one choice to be selected.  If N choices are chosen, and any of those choices should fail, the execution environment may select an alternative choice.
 * Note: The execution environment will often rely on information contained in the `choice-opt` declarations to guide when and which choices should be canceled and selected.
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
 * *Description*: Instruct the execution environment that the specified condition is true, and then proceed. Often `ask` is used in conjunction with a `tell` that is running in a parallel execution thread.
 * *Syntax*: `'(' 'tell' cond-expr ')'`
 * *Example*: `(tell (= door "open"))`
 * *Execution*:
  * The *Dispatcher* sends the `tell` statement (which includes the condition) to BSM.
  * BSM replies to *Dispatcher* with a finished status (either success or failure)
  * **TBD**: error handling (e.g., for invalid operands to conditions)

## **try** [Not yet supported]
 * *Description*: Attempt to execute the initial `fn` within the specified temporal bounds.  If that `fn` invocation fails (because of an explicit failure of the `fn`, or because of a violation of the temporal bounds), then the `catch` `fn` is executed.
 * *Syntax*: `'(' 'try' opt-bounds? fn '(' 'catch' fn ')' ')'`
 * *Example*:

```
(try :bounds [2 3]
     (initialize)
     (catch (reset-and-initialize)))
```
 * *Execution*:
  * As with any Pamela statement, the *Dispatcher* executes the initial `fn` within the specified temporal bounds.
  * If a failure or error occurs, the *Dispatcher* executes the `catch` `fn`
  * If the initial `fn` was executed successfully, the `try` statement is considered to have executed successfully.
  * If the `catch` `fn` was executed successfully, the `try` statement is considered to have executed successfully.
  * Otherwise, the `try` statement is considered to have executed unsuccessfully (i.e., failed).

## **unless** [Not yet supported]
 * *Description*: If the condition is not true, execute the statement `fn` one time.  The condition is checked only once. The `:bounds`, if specified, applies to the duration of the entire statement.
 * Note the distinction of the duration and frequency in which the condition is checked among the `unless`, `when`, `whenever`, and `maintain` statements.
 * *Syntax*: `'(' 'unless' cond-expr opt-bounds? fn ')'`
 * *Example*:

```
(unless (= door "open") :bounds [2 3]
     (open-door))
```
* *Execution*:
  * The current *Dispatcher* (*Dispatcher-1*) sends the `unless` statement (which includes the condition and activity) to BSM.
  * BSM does a one-time evaluation of the condition.
  * If the condition is:
      * **true**: BSM replies to *Dispatcher-1* with finished status of success.
      * **false**:
         * BSM sends the activity to the *Dispatcher Manager* as an activity, which launches a new *Dispatcher* (*Dispatcher-2*) with that plan.
         * *Dispatcher-2* then executes that plan, starting with the first activity.
         * When the plan has completed, the *Dispatcher-2* sends the status to BSM.  (There is little advantage of sending this status back to BSM via the *Dispatcher Manager*, but that alternative would be possible).
         * BSM replies to *Dispatcher-1* with finished status of the activity (either success or failure).


## **when** [Not yet supported]
 * *Description*: If the condition is true, execute the statement `fn` one time.  If the condition is not initially true, execution of this `when` statement will wait until either 1) the condition becomes true, or 2) the temporal duration (`:bounds`) has been exceeded.
 * Note the distinction of the duration and frequency in which the condition is checked among the `unless`, `when`, `whenever`, and `maintain` statements.
 * *Syntax*: `'(' 'when' cond-expr opt-bounds? fn ')'`
 * *Example*:

```
(when (not (= door "open")) :bounds [2 3]
     (open-door))
```
* *Execution*:
  * The current *Dispatcher* (*Dispatcher-1*) sends the `when` statement (which includes the condition and activity) to BSM.
  * BSM does an evaluation of the condition.
  * If the condition is:
      * **false**: BSM continues check the condition, until it becomes true (or the temporal duration (`:bounds`) has been exceeded).
      * **true**:
         * BSM sends the activity to the *Dispatcher Manager* as an activity, which launches a new *Dispatcher* (*Dispatcher-2*) with that plan.
         * *Dispatcher-2* then executes that plan, starting with the first activity.
         * When the plan has completed, the *Dispatcher-2* sends the status to BSM.  (There is little advantage of sending this status back to BSM via the *Dispatcher Manager*, but that alternative would be possible).
         * BSM replies to *Dispatcher-1* with finished status of the activity (either success or failure).

## **whenever** [Not yet supported]
 * *Description*: During the temporal scope specified by `:bounds`, every time the condition is true, execute the statement `fn` one time.  This assumes that once execution of the `fn` has begun, the `cond-expr` won't be revisited until execution of the `fn` has completed.
 * **TBD**: How is the success of the `whenever` determined?  Proposal: The ``whenever`` is successful unless **any** of the attempted executions of the statement `fn` fails.
 * Note the distinction of the duration and frequency in which the condition is checked among the `unless`, `when`, `whenever`, and `maintain` statements.
 * Note: as in all of Pamela, the default value for an unspecified `:bounds` is `[0 :infinity]`.  So, a `whenever` statement with an unspecified bounds should be part of a `parallel` statement.
 * *Syntax*: `'(' 'whenever' cond-expr opt-bounds? fn ')'`
 * *Example*:

```
(whenever (not (= door "open")) :bounds [2 30]
     (open-door))
```
* *Execution*:
  1. The current *Dispatcher* (*Dispatcher-1*) sends the `whenever` statement (which includes the condition and activity) to BSM.
  2. BSM does an evaluation of the condition.
  3. If the condition is:
      * **false**: BSM continues check the condition, until it becomes true (or the temporal duration (`:bounds`) has been exceeded).
      * **true**:
         * BSM sends the activity to the *Dispatcher Manager* as an activity, which launches a new *Dispatcher* (*Dispatcher-2*) with that plan.
         * *Dispatcher-2* then executes that plan, starting with the first activity.
         * When the plan has completed, the *Dispatcher-2* sends the status to BSM.  (There is little advantage of sending this status back to BSM via the *Dispatcher Manager*, but that alternative would be possible).
         * BSM then repeats an evaluation of the condition (Step 2, above).
  4. BSM replies to *Dispatcher-1* with finished status of the activities (success if all successful, else failure).


## **maintain** [Not yet supported]
 * *Description*: Execute the statement `fn` one time.  However, while doing so, the condition `cond-expr` **can** **never** be violated.
 * Note the distinction of the duration and frequency in which the condition is checked among the `unless`, `when`, `whenever`, and `maintain` statements.
 * *Syntax*: `'(' 'maintain' cond-expr opt-bounds? fn ')'`
 * *Example*:

```
(maintain (= door "open") :bounds [2 30]
     (do-lots-of-stuff-possibly-involving-the-door))
```
* *Execution*: **TBD**: A straightforward implementation of this it to monitor the condition, returning a failure if the condition is ever violated.  However, this leaves the condition in a violated state (not the intention).  A better option would be to check the `:post` condition for every statement execution to ensure compliance. However, this has two problems:
	* This assumes that all commands have correct and comprehensive `:post` conditions
	* This requires that the condition is maintained across *Dispatcher* instances (which is quite possible, since there is only one BSM instance running).

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
