# How To Create a Pamela Test

Whenever we add a new feature to Pamela or whenever we make a bug fix for a Pamela issue, we should create a test for that particular feature.

## Steps to create a new Pamela IR test:
All Pamela tests should include a test of parse tree generation (`check`) and IR generation (`build`) (and indirectly, of unparsing).

* Create a new Pamela file in `test/pamela` that tests the Pamela feature (e.g., `test/pamela/foo.pamela`)
* Compile that Pamela file into an parse tree `check` file with the same filename as the Pamela file, but with the filename suffix of `txt`, and copy this file into the `test/pamela/IR` directory (e.g., `test/pamela/IR/foo.txt`).  
	* To create this:
```
pamela -i test/pamela/foo.pamela -o test/pamela/IR/foo.txt check
```
* Compile that Pamela file into an EDN IR with the same filename as the Pamela file, but with the filename suffix of `ir.edn`, and copy this file into the `test/pamela/IR` directory (e.g., `test/pamela/IR/foo.ir.edn`).  
	* To create this:
```
pamela -i test/pamela/foo.pamela -o test/pamela/IR/foo.ir.edn build
```

## Steps to create a new Pamela HTN test:
Include this for any tests where an HTN test is relevant.

* An HTN test assumes that the IR test files (see above) are in place (e.g., `test/pamela/foo.pamela`).
* Create a `root-task` file in the `test/pamela/IR` directory that specifies the root task argument that would be specified as part of a `pamela htn` command (e.g., `test/pamela/IR/foo.root-task`)
* Create the `htn.edn` and `tpn.edn` files that would be generated by the `pamela htn` command.  
	* For example, if the root task argument was `(foo.main)`, you could use the command `pamela -i test/pamela/coverage.pamela -o foo.edn -t "(foo.main)" htn` to generate both files.

## To run the tests:

* If your Pamela installation is up to date and compiled, just do: `boot test`.
* If you need to recompile, and then run all of the tests (including the CLI tests), do: `boot all-tests`.

