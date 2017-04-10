# PAMELA language manual

This document is the reference manual for the PAMELA language.

For background information on PAMELA please see the primary
[README](../README.md)

For the PAMELA EBNF grammar please see the [PAMELA grammar](grammar.md)


## PAMELA Introduction

The PAMELA language is intended as a declarative language for expressing
domain models which can be "executed" with a planner and potentially
augmented through machine learning.

## PAMELA source

PAMELA source code is saved in files with the extension `.pamela`
(by convention). The "flavor" of the syntax corresponds to that
of Clojure - a modern LISP.

PAMELA classes may be read from a file, from standard input, from
a string, retrieved from the model database or entered in
the Clojure REPL.

## PAMELA language

### defpclass

The `defpclass` command is the top level PAMELA construct as it
defines a PAMELA class and defines a model symbol which is
bound to a the class constructor function.

````
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
````

The first argument to `defpclass` is the PAMELA class symbol (*pclass*)
to be defined.

Then there is a vector formal arguments to the *pclass* (which may be empty).

The *pclass* is then defined by a series of options: each introduced
with a keyword and elaborated with a map.

#### :meta

The **:meta** map contains optional metadata for the PAMELA class,
with the following entries:

* **:doc** documentation string
* **:version** version string. Typically PAMELA models are versioned using the [semantic versioning](http://semver.org/) scheme.
* **:icon** pathname to file for an icon representing this *pclass*
* **:depends** The *pclasses* that this *pclass* depends upon. This is a vector of vectors expressing each dependency.
** **[dependent-pclass "dependent-version"]** a dependency is expressed as a vector with the symbol for the dependent-pclass and the string corresponding the the dependent-version of that *pclass*. If a dependent *pclass* has not been declared prior to evaluating this **defpclass** an exception will be thrown.


#### :inherit

The **:inherit** option specifies a vector of PAMELA super-classes.
The idea is that PAMELA classes may subclass a parent class for
further specialization. PAMELA superclasses are *not* yet implemented.

*NOTE*: **:inherit** may be folded in with **:meta** in the future.

#### :fields

Each field is like member data for the *pclass* and may refer
to another *pclass* instance, an *LVar* or a value. Simple
field values may be a boolean, string, keyword or a number.

A field is defined with a map of

* **:initial** The initial field value. This may be a simple value, or a symbol representing an argument passed into this *pclass* constructor, or a constructor call for another *pclass* or *LVar*.
* **:access** The access for this field :public or :private (the default).
* **:observable** True if this field is an observable input from the plant (false by default).

If the default values for **:access** and **:observable** are acceptable then
the initial field value may be used by itself (instead of in a subordinate map).

Arguments to a *pclass* constructor may be simple values, including
keywords (except for **:id** and **:interface**), or symbols. If a symbol is
used in a *pclass* constructor it may refer to a previously defined
field (but as a symbol, not a keyword) or an argument passed into this
*pclass*.

Options to a *pclass* constructor may include:
* **:id** The identifier (string) for this *pclass* instance
* **:interface** The identifier (string) for interface used in plant interactions with *pclass* instance

#### :modes

Each *pclass* instance is always in one of a set of enumerated modes.

An *enumeration-pclass* is one which defines its possible modes
simply with a vector of keywords.

Most *pclasses* define modes as a map with a keyword entry
for each mode and a value representing the *mode condition*.

A *mode-condition* is a conditional expression when is evaluated
to true means that the given mode is possible for this *pclass*
instance. Certain modes (e.g. failure modes or enumerations)
may have the trivial condition expression true which means that
being in the given mode is always possible.

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



#### :methods and defpmethod

A *pclass* may have methods specified in the vector value of **:methods**.
Each element of this vector should be a PAMELA method as constructed
by the *defpmethod* command.

````
(defpmethod turn-on
  {:pre :off :post :on :bounds [1 3]
   :doc "turns on the power supply"}
  [])
````

The first argument to `defpmethod` is the symbol for the method
to be defined.

Then there is an optional conditions map which may contain any
of the following keys:

* **:doc** The documentation string for this method
* **:pre** The precondition for running this method (see [conditional expressions](#conditional-expressions))
* **:post** The postcondition for running this method (see [conditional expressions](#conditional-expressions))
* **:bounds** The [bounds](#bounds) for this function
* **:cost** The cost of this function (number)
* **:reward** The reward of this function (number)
* **:controllable** By default methods are not controllable (except for the built-in **delay**). This field may be set to true to indicate the method is controllable by the planner (boolean).
* **:primitive** By default, methods without a body are assumed to be primitive (where the implementation is defined by the plant), and methods with a body are non-primitive.  This provides an override: when `true`, the body is assumed to be documentation for the method behavior, that may be used for higher-level reasoning.
* **:display-name** This specifies the name of this method that is displayed to the user (e.g., in PlanViz).  If not specified, the `:display-name` is assumed to be a Title Case version of the method name symbol, with the hyphens converted to spaces (e.g., `"Turn On"`).

Then, there is a vector of zero or more formal arguments to the method.

The next form is the function which comprises the body of the method.
If the body is empty then this is considered a *primitive method* which
is implemented by the plant.

Lastly, following the function form of the method, there may be zero or more `between` statements.

<!--* *TBD*
* all not controllable by default, except delay

-->
#### :transitions

This (optional) section, provides the specifications of the mode transitions for the modes defined within this `defpclass`.


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
## NOTES...

* pclass
 * A pclass may, optionally, be given an `:id` to identify this instance
   (the `:id` is assumed to be "plant" if not explicitly given)

* modes
 * Modes may be a simple enum
 * Modes may be based on a simply boolean expression involving fields
 * In the future modes may be based on differential equations or
   linear equations.

* choice
  * guard conditions may be specified to indicate a choice is to be considered
  * probability may be given to indicate the non-deterministic preference
    for a choice. If some choice conditions have guards that are not satisfied
    the probabilities are renormalized among the enabled choices prior to making
    a non-deterministic decision.

* observable
  An observable field means that it's value may be set by the
  outside world interface. (Only pclass fields may be mutated as observables,
  not lvar's).

* fields
  * access is private by default
