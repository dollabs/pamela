# Changelog for PAMELA

## Changes for 0.3.0


* Field initializer change

 The new approach is to set fields with properties associated to them,
 for example:

 :fieldname {:initial (mode-of PCLASS MODE)
             :access :private
             :observable true}

 :fieldname (mode-of PCLASS MODE) ;; may be used if no other props needed

 This means that the :initial :access and :observable options to
 a pclass constructor have been removed.

 The new property :interface is simply a string which will be used
 by (future) execution engines to provide an implementation hint.


* pclass constructor argument change

  symbols in pclass constructor arguements in field initializers
  may refer to formal arguments to the pclass or names of other fields.

  :initial mode will no longer be an optional constructor argument, but
  rather a field initializer.

  These fields will be added to the pclass constructor:
 * :interface "RMQ"


* Multiple inheritance

 We will defer *implementing* multiple inheritance, but thesyntax for
 declaring it will be:

 :inherit [super1 super2 super3]

* Mode references

 Regarding specifying modes in field initialziers or mode conditions

 To be explicit and/or override the default search order
 from the order of inherited superclass declarations one could explicitly
 specify the mode of a given type like this:

 (mode-of lightvals :dark) ;; mode-reference

* Field refernnces

 First of all we'll get rid of 'field=' in favor of the much
 easier to read '='.

 Second the order of operands should not be significant
 (i.e. whether they are field references or mode references).

 Third a naked keyword is interpreted as a field-reference
 (or, failing that, as a mode-reference).

 Forth an explicit field reference may be made in the style
 accessing a Clojure map value from the keyword:

 (:illumination this) ;; field-reference

 Where 'this' is a special symbol which refers to this class instance.
 A field-reference may be made on another class as well:

 (:pwr psw) ;; field-reference


* Removed 'pclass' constructor helper

 We decided that the constructor helper is ugly and we'll get rid of it.

* Eliminated (condition...)

 The 'condition' form was added for implementation convenience, but
 is ugly and will be removed.

* Eliminated mode=

 Also mode= will be removed in favor of the implicit mode-reference
 -or- (mode-of class :mode)

* Mode comparison

 The form (= class1 class2) means each class instance evaluates
 to the same mode.
