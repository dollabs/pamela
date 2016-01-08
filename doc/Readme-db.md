#### Some examples of storing, retrieving pamela models to and from a DB.

## Usage

From the command line you can use the **pamela** script (from **bin/**):

```
tmarble@ficelle 274 :) pamela --simple list 2> /dev/null
one
two
tmarble@ficelle 275 :)
```
*NOTE:* in the example above *STDERR* has been redirected so that the database startup/shutdown messages are not intermixed with the command output.


From the **CIDER** REPL:

```
pamela.db> (pamela.core/pamela "--simple" "list")
one
two
Exception DEV MODE exit(0)  pamela.core/exit (core.clj:139)
pamela.db>
```

*NOTE:* in the example above the function `pamela.core/exit` throws an exception instead of calling `System/exit` (as that would terminate the REPL).


From the **CIDER** REPL by manually starting the database and calling functions:

```
pamela.db> (start-db-pamela {:verbose 3})
....
true
pamela.db> (list-models {:simple true})
["one" "two"]
pamela.db> (stop-db)
nil
pamela.db>
```
