# Generating HTN's

PAMELA has support for generating HTN's (and the corresponding TPN's).

The following assumes that the `bin` directory has been added to your `PATH`.

Given a PAMELA source file with a HTN such as [biased-coin.pamela](../test/pamela/biased-coin.pamela) you can generate the HTN (and TPN) as JSON with the following command. _NOTE_: it is essential to specify the root task `(main.flip-3)` of the HTN:

````
pamela -i test/pamela/biased-coin.pamela -t '(main.flip-3)' -f json -o biased-coin htn
````


### Acknowledgement and Disclaimer:
This material is based upon work supported by the Army Contracting
and DARPA under contract No. W911NF-15-C-0005.
Any opinions, findings and conclusions or recommendations expressed
in this material are those of the author(s) and do necessarily reflect the
views of the Army Contracting Command and DARPA.
