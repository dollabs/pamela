# Change Log

All notable ### Changes to this project will be documented in this file. This change log follows the conventions of [keepachangelog.com](http://keepachangelog.com/).

## [Unreleased]

### Changes
* Grammar ambiguity fixed in a previous commit (Closes #144)
* Fixed argument values with field references (Closes #150)
* Fixed method-opts in plant function invocations (Closes #151)
* Added probability to defpmethod (Closes #148)
* dbg-println level now controlled by pamela.utils/set-dbg-println-level (Closes #117)
* Create pclass-instance data structure for HTN's
  - Fixes #146 Extend pclass-ctor-options to be strings OR references
  - Fixes #111 0-Arg main method not correctly used as the default root task
* Further handle field-refs as part of the fix for #146
* Add a command line option to set the dbg-println-level
* Fixed some complex dereferencing cases for HTN generation.
* Added rubric symbol-ref-htn.pamela
* Add `boot local` task so local projects can use Pamela as a dependency
* Add support for new `call` conditional expression in pre and post
	conditions in pmethods.
* Extend field-inits to include :producer and :consumer options (in the
	future, we may allow any keyword option for user extensibility)
* Instantiating an unknown class now produces an error. (Closes #133).
* Implement :depends meta declaration (Closes #180).
* Incorrect arity of pclass-ctor call now errors at `build` (Closes #190).
* Allow empty maps to be specified for defpclass :fields and :modes (Closes #175).
* Extend conditions to include search and to add object equality (`same`)
* Add tests of the 'check' operation for all of the Pamela files.  We had many of the txt file rubrics in place, but there were no tests that previously used them.
* Add support for inheritance (`:inherit`)
* Extend conditions with `same`, `wm`, `ltm`, and `recency`
* Added inequalities for comparing numbers: >, <=, >, >=  These are added to allow, for example, searching for an object representing a person whose age is between 18 and 21, for example.
* Added grammar to add propositions to conditions.
* Added grammar to add limited arithmetic to bounds `(+ x y)` `(- x y)` `(* x y)` `(/ x y)`

## [0.6.2] - 2017-08-28

### Changes
* Fixed pamela launch script (Closes #72)
* Changed default output to json (Closes #74)
* Ensure that jenkins.sh will pass
* Fixed test to put temporary/generated files under target/
* Updated plan-schema dependency to 0.2.16
* Make it explicit that there can't be multiple levels of ( parallel | sequence | choose ) in a
  defpmethod used for HTN generation (Closes #57)
* HTN and TPN schema updates (one of many steps on Issue #43)
* Avoid recursive resolve-plant-class call at the end of the ancestry-path (Closes #82)
* Check method call arity in IR generation (if possible) (Closes #83)
* Improved field arg validation (Closes #87)
* Support multiple arities for methods of the same name (Closes #86)
* Ensure CLI returns same integer exit code whenever calling
  pamela.cli/pamela.
* Added cli.clj test coverage for the above.
* Add HTN/TPN support for ask, tell, and assert Pamela statements (Closes #94)
* Ensure state :end-node's are updated properly in remove-superflous (Closes #103)
* Support for plan-schema 0.3.2
* In HTN generation only state nodes which begin a sequence have
  an :end-node slot (Closes #105)
* In HTN generation handle between constraints (Closes #102)
* Adjustment of :args in HTN expanded methods (Closes #109)
* Fix using default bounds for TPN's generated with HTN method
  * (Closes #108)
* Consistently throw exceptions in HTN generation using fatal-error
* Ensure metadata in TPN's flows from method definitions when generated with the HTN method (Closes #98)
* Updated plan-schema to leverage the match-eval-out-err macro in testing
* Ensure remove-superfluous in TPN optimization does not re-visit nodes
  unnecessarily (Closes #115)
* Update doc/PAMELA.md to include documentation for all of the Pamela built-in statements
* Added unparse action (Closes #125)
* Fixed statements include ask, assert, maintain, unless, when, whenever
  (Closes #122)
* Updated plan-schema dependency to 0.3.6
* Unparse now elides default defpmethod cond-map (and default field
  initializers) (Closes #126)
* Fix IR generation defpmethods with betweens, but no body (Closes #128)
* Fix Temporal constraint not properly defined for some special cases (Closes #120)
* Support defining a pclass without defpmethods (Closes #113)
* Plant functions can begin with any of the reserved function symbols (Closes #121)
* Convert all field references to symbols (Closes #136)

## [0.6.0] - 2017-03-08

### Changes
* Grammar ### Changes: allow underscores in symbols
* Parsing ### Changes: allow plant-function symbols to refer to fields
  (if not present in method or pclass args)
* Validate pclass-ctor args
  - keywords must be one of #{:id :interface :plant-part} or a field
  - symbols must be a formal pclass arg or a field (when converted to kw)
- Complex (nested) root-task elaboration
  First symbol is HTN pclass, ultimate symbol is the method, and
  intermediate symbols (if any) are fields from the previous pclass.
  Arguments may be literals or fields qualified by pclass (and zero
  or more intermediate fields).
- HTN progagation proceeds normally when HTN and TPN are not hand-coded.
- HTN now has all the attributes required to generate the TPN
  :name
  :display-name ;; preformatted in camelCase without args
  :command (duplicate of :name)
  :args
  :argsmap
  :plantid
  :plant-part
  :interface
- TPN activities now have all plant instance details
  (:id :plant-part :interface)
- Delays are represented as type :delay-activity
- Temporal constraints for activities on attached to the activity
  (rather than the begin node).
- Fixed jenkins.sh
- Updated documents in doc/
- Update tests with new gensym to compare IR output
- Update pamela launch script to better escape root-tasks
- Updated dependencies
- Add dbg-println, a configurable alternative to using println for debugging.
   - Behavior is controlled by dbg-println-mode and *dbg-println-level* (and the current logging
     level if dbg-println-mode=:log)
- Resolution of argument resolution bug
- Determine pclass of unabiguous (but unspecified) pmethods
- Handle 5 canonical HTN source examples, including finding the appropriate plant initializer, such
  that the argument values and other plant info is propagated appropriately
- Resolved a number of problems with argument resolution (handling multiple use cases)
- Handle top-level HTN method consisting of a choice
- Updated TPN object hierarchy to include :cost<=-constraint and :reward>=-constraint
- Move constraints from Nodes to Activities when appropriate

## [0.5.0] - 2016-12-08

### Changes
* Fixed HTN generation for elaborated primitive methods (Closes #39)
* Switched build tool from lein to boot
* Streamlined the directory layout
* Updated tests
* Created master install script: pamela-setup
* Fixed HTN generation for elaborated primitive methods (Closes #39)
* Fixed parser bug with floating point numbers
* Added :plant-part to pclass-ctor (Closes #45)
* Fixed TPN generation with delay statements
* Updated documentation
* Fixed jenkins.sh

## [0.4.3] - 2016-11-18

### Added
- Better support for magic files (including --output-magic)
- Fixed minor bugs with the pamela launcher
- Ensured top level actions properly return an exit code
- Quieted the intial logging by the netty (dependency of aleph)
- Properly handle default plant bounds as lvars
- Changed default PAMELA_MODE=prod in ./bin/pamela (requires uberjar)
- Updated grammar to ensure reserved words are not interpreted
  as plant functions
- Added semantic check to validate the unqualifed plant functions
  are defined elsewhere in the PAMELA class
- Changed dependencies
  * Upgraded clojurescript, clj-time
  * Downgraded lein-cljsbuild to avoid migration to the new format
  * Downgraded org.omcljs/om to 1.0.0-alpha40 to avoid breakage
- Redesigned input-file and output-file processing (to expand-home
  and prepend the current working directory as needed)
- Fixed missing plantid slot in TPN output
- Fix --output-magic to properly list input filenames and save as EDN
  Closes #37
- This is planned to be the last version with pamelad and database
  functionality

## [0.4.2] - 2016-10-18

### Changes
- Added :primitive #23
- Added dotimes #24

## [0.4.1] - 2016-10-17

### Changes
* Fixed HTN generation for elaborated primitive methods (Closes #39)
* Fixed parser bug with floating point numbers
- Updated dependencies
- Allow plan-schema to be more flexible
  https://github.com/dollabs/planviz/issues/24
- Fixed: Planviz disagrees with Pamela (plan-schema) #18
- Added command line switch --strict to enforce schema checking
- Expand home in pathnames properly

## 0.4.0 - 2016-09-29

### Changes
- New PAMELA intermediate representation
- Reset non-passing tests as functionality is replaced with the new IR
- Complete conversion to the new Instaparse based parser
- New HTN + TPN generation from PAMELA
- TPN generation from PAMELA
- Added flexible field initializer and pclass constructor types
- Changed method invocation to use '.' instead of '$'
- Updated dependencies

## [0.3.0] - 2016-07-11

### Added
* Added support for new grammar features
  * slack-sequence
  * slack-parallel
  * optional
  * soft-sequence
  * soft-parallel
* Fixed #4 TPN output for delay should create a delay-activity
* Updated dependencies
* Changed some language features (see [grammar/doc/CHANGELOG.md](grammar/doc/CHANGELOG.md))

## [0.2.6] - 2016-05-27

### Added
- Add :controllable feature to plant functions
- Grammar ### Changes
 * Change built in function noop to delay (to better mesh with planners)
 * Updated insta2w3c script and railroad diagram

## 0.2.5 - 2016-05-16

### Added
* Updated dependencies
* Added gh-pages and API docs
* Re-implemented http-get using clj-http (as it is required by elastisch)
* Fixed db/stop-db-exit behavior while in the REPL
* Disabled web client tests for now
* Updated grammar for cost, reward, cost<=, reward>=, expanded
  between constraints to handle cost, reward as well.
* Updated grammer checking script to catch ambiguity (analyze)
* Generalized parsing in pamela.models to return a map of options
* TPN generation now uses it's own `tpnsym` function to start new
  `uid`'s at 1
* TPN generation will record the original pamela order of activities
  in parallel and choice blocks

[0.2.6]: https://github.com/dollabs/pamela/compare/0.2.5...0.2.6
[0.3.0]: https://github.com/dollabs/pamela/compare/0.2.6...0.3.0
[0.4.1]: https://github.com/dollabs/pamela/compare/0.3.0...0.4.1
[0.4.2]: https://github.com/dollabs/pamela/compare/0.4.1...0.4.2
[0.4.3]: https://github.com/dollabs/pamela/compare/0.4.1...0.4.3
[0.5.0]: https://github.com/dollabs/pamela/compare/0.4.3...0.5.0
[0.6.0]: https://github.com/dollabs/pamela/compare/0.5.0...pre-ir-updates-04-2017
[0.6.2]: https://github.com/dollabs/pamela/compare/pre-ir-updates-04-2017...0.6.2
[Unreleased]: https://github.com/dollabs/pamela/compare/0.6.2...HEAD
