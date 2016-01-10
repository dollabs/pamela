## Testing

In order to facilitate testing please add the **bin** directory to your `PATH`;

The full suite of tests can be run with the `jenkins.sh` script which includes:

### Clojure Dependency Check

````
lein deps :tree
````

The purpose of this test is to ensure that **java** and **lein** have
been properly installed and all project depependencies have been downloaded.

### Clojure API Documention

````
lein doc
````

All variables and functions should have docstrings
and metadata. This will allow automatic generation
of API documentation via [codox](https://github.com/weavejester/codox).

Be sure to comment out the line `:aot [pamela.core]`
prior to generating the documentation with `lein doc`.

*NOTE:* the generated **doc/** output is *not* checked into git.

### Clojure Tests

````
lein test
````

This will run all the Clojure tests under [src/test/clj/testing/pamela/](https://bitbucket.org/dollinc-pamela/pamela/src/master/code/src/test/clj/testing/pamela/)

For more on the Clojure test framework please see the [API for clojure.test](https://clojure.github.io/clojure/clojure.test-api.html)

### Command line tests

````
cli-test
````

This will run all the command line tests under [src/test/cli/](https://bitbucket.org/dollinc-pamela/pamela/src/master/code/src/test/cli/) in order.

Each shell script in that diretory may use the following environment variables:

* `CODE` set to the **code/** directory
* `RESULTS` set to the **target/cli-test** directory

For these shell scripts the `CODE` directory will be added to the `PATH`
such that the `pamela` script will be found.

In order to pass each shell script must exit with return code 0 (zero) and,
if a corresponding file ending in `.out` is present, must have output which
matches the expected output file.

The invocation of the `pamela` command line script is provided in the
API documentation for [usage](http://lispmachine/pamela/code/doc/pamela.core.html#var-usage).

*NOTE:* as the `pamela` script running in development mode must invoke
both `lein` and start the PAMELA program the startup time is very
slow.


### Acknowledgement and Disclaimer:
This material is based upon work supported by the Army Contracting
and DARPA under contract No. W911NF-15-C-0005.
Any opinions, findings and conclusions or recommendations expressed
in this material are those of the author(s) and do necessarily reflect the
views of the Army Contracting Command and DARPA.
