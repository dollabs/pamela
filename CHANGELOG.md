# Change Log

All notable changes to this project will be documented in this file. This change log follows the conventions of [keepachangelog.com](http://keepachangelog.com/).

### [Unreleased]

Added
- Better support for magic files (including --output-magic)
- _TBD_

### [0.4.2] - 2016-10-18

Changes
- Added :primitive #23
- Added dotimes #24

### [0.4.1] - 2016-10-17

Changes
- Allow plan-schema to be more flexible
  https://github.com/dollabs/planviz/issues/24
- Fixed: Planviz disagrees with Pamela (plan-schema) #18
- Added command line switch --strict to enforce schema checking
- Expand home in pathnames properly
- Updated dependencies

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
[Unreleased]: https://github.com/dollabs/pamela/compare/0.4.2...HEAD
