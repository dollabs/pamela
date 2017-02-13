# Change Log

All notable changes to this project will be documented in this file. This change log follows the conventions of [keepachangelog.com](http://keepachangelog.com/).

### [Unreleased]

Changes
* Grammar changes: allow underscores in symbols
* Parsing changes: allow plant-function symbols to refer to fields
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

### [0.5.0] - 2016-12-08

Changes
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

### [0.4.3] - 2016-11-18

Added
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

### [0.4.2] - 2016-10-18

Changes
- Added :primitive #23
- Added dotimes #24

### [0.4.1] - 2016-10-17

Changes
* Fixed HTN generation for elaborated primitive methods (Closes #39)
* Fixed parser bug with floating point numbers
- Updated dependencies
- Allow plan-schema to be more flexible
  https://github.com/dollabs/planviz/issues/24
- Fixed: Planviz disagrees with Pamela (plan-schema) #18
- Added command line switch --strict to enforce schema checking
- Expand home in pathnames properly

### 0.4.0 - 2016-09-29

Changes
- New PAMELA intermediate representation
- Reset non-passing tests as functionality is replaced with the new IR
- Complete conversion to the new Instaparse based parser
- New HTN + TPN generation from PAMELA
- TPN generation from PAMELA
- Added flexible field initializer and pclass constructor types
- Changed method invocation to use '.' instead of '$'
- Updated dependencies

### [0.3.0] - 2016-07-11

Added
* Added support for new grammar features
  * slack-sequence
  * slack-parallel
  * optional
  * soft-sequence
  * soft-parallel
* Fixed #4 TPN output for delay should create a delay-activity
* Updated dependencies
* Changed some language features (see [grammar/doc/CHANGELOG.md](grammar/doc/CHANGELOG.md))

### [0.2.6] - 2016-05-27

Added
- Add :controllable feature to plant functions
- Grammar changes
 * Change built in function noop to delay (to better mesh with planners)
 * Updated insta2w3c script and railroad diagram

### 0.2.5 - 2016-05-16

Added
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
[Unreleased]: https://github.com/dollabs/pamela/compare/0.5.0...HEAD
