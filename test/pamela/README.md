# Contents of test/pamela	
The Pamela files in this directory are used for a variety of tests:  

* **IR Generation**:  A `build` operation is performed, generating an IR file.  The IR file is compared with the content of the `test/pamela/IR` directory.
	* E.g., For `foo.pamela`, the Pamela tests will execute the command `pamela -i foo.pamela -o foo.ir.edn build`.  This output file is then compared with the file `IR/foo.ir.edn` as part of the testing.
* **HTN Generation**: An `htn` operation is performed, generating an HTN file (`*.htn.edn`).  This is done **only** for files that have a corresponding `root-task` file in the `HTN` directory.  
	* E.g., For `foo.pamela`, if there is a `HTN/foo.root-task` file, the content of that `root-task` file is used in the command: `pamela -i foo.pamela -t [ContentOf-root-task-file] -o foo.htn.edn htn`.   This output file is then compared with the file `HTN/foo.htn.edn` as part of the testing.

**Note that many of the Pamela files are used only for IR generation testing, and not for HTN generation.**