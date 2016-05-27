# Change Log

All notable changes to this project will be documented in this file. This change log follows the conventions of [keepachangelog.com](http://keepachangelog.com/).

### [Unreleased]

Added
* *TBD*

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
[Unreleased]: https://github.com/dollabs/pamela/compare/0.2.6...HEAD
