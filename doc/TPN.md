# Generating TPN's

PAMELA has support for generating TPN's.

The following assumes that the `bin` directory has been added to your `PATH`.

Given a PAMELA source file with a TPN such as [choice-tpn.pamela](../test/pamela/choice-tpn.pamela) you can generate the TPN as JSON with this command:

````
pamela -i test/pamela/plant.pamela -i test/pamela/choice-tpn.pamela -f json -o choice-tpn.tpn.json tpn
````

For a more complete example see [tpn-demo.pamela](../test/pamela/tpn-demo.pamela)


### Acknowledgement and Disclaimer:
This material is based upon work supported by the Army Contracting
and DARPA under contract No. W911NF-15-C-0005.
Any opinions, findings and conclusions or recommendations expressed
in this material are those of the author(s) and do necessarily reflect the
views of the Army Contracting Command and DARPA.
