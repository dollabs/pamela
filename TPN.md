# Generating TPN's

PAMELA has preliminary support for generating TPN's.

The following assumes that the `bin` directory has been added to your `PATH`.

_NOTE: in the future this functionality will be supported with **pamelad** such that Java startup time can be minimized._

The basic approach is:

1. Create a plant class
   For example see `src/test/pamela/plant.pamela`

2. Create a TPN class
   For example see `src/test/pamela/choice-tpn.pamela`

3. Generate the "old" format JSON (for Cytoscape):

````
pamela -i src/test/pamela/plant.pamela -i src/test/pamela/choice-tpn.pamela -f cytoscape -o choice-tpn.cyto.json tpn
````

4. Display in Cytoscape

````
cytoscape-load choice-tpn.cyto.json tpn
````

5. Generate the "new" format JSON

````
pamela -i src/test/pamela/plant.pamela -i src/test/pamela/choice-tpn.pamela -f json -o choice-tpn.json tpn
````

-----

For a more complete example see `src/test/pamela/tpn-demo.pamela`

# Graphviz Visualization

In addition to creating JSON output PAMELA can create `.dot` files
suitable for [Graphviz](http://www.graphviz.org/)

You can generate Graphviz files by simply choosing the **dot** format:

````
pamela -i src/test/pamela/plant.pamela -i src/test/pamela/choice-tpn.pamela -f dot -o choice-tpn.dot tpn
````

Then you can use Graphviz using one of the layout commands
such as **dot** (see also [circo fdp neato osage sfdp twopi](http://www.graphviz.org/Documentation.php)).

````
dot -Tsvg choice-tpn.dot > choice-tpn.svg
````

SVG is a particularly nice format because it is resolution
independent and can be viewed in a web browser. Several
other graphics output formats are supported.

In fact if you have Graphviz installed on your system
(the **dot** program is in your **PATH**) then you can add the
`--visualize` option to generate an SVG. _NOTE: be sure
to set the `--file-format dot` as the graphviz dot file
is required to generate the SVG of the file of the same
name (but with ".svg" appended):

````
pamela -i src/test/pamela/plant.pamela -i src/test/pamela/choice-tpn.pamela -f dot -o choice-tpn.dot --visualize tpn
````

That will create the SVG file `choice-tpn.dot.svg`.


# Running pamelad to save time

You can also run PAMELA in daemon mode to make operations
significantly faster:

````
tmarble@ficelle 494 :) export SERVER_HOST=localhost
tmarble@ficelle 495 :) export SERVER_PORT=9100
tmarble@ficelle 496 :) export PAMELAD="${SERVER_HOST}:${SERVER_PORT}"
tmarble@ficelle 497 :) pamelad -v start
--- pamelad start ---
started pamela as 29163
(lein 29173)(launcher 29181)..............(pamelad 29263)
.......................... now pamelad is ready for requests
pid changed to 29315
started pamelad as 29315
started jstat as 29411
export PAMELAD=0.0.0.0:9100
tmarble@ficelle 498 :) time pamela -i src/test/pamela/plant.pamela -i src/test/pamela/choice-tpn.pamela -f dot -o choice-tpn.dot tpn

real    0m0.361s
user    0m0.008s
sys	    0m0.008s
tmarble@ficelle 499 :) pamelad -v stop
--- pamelad stop ---
asking pamelad to shut down gracefully..
......pamelad pid 29315 stopped
tmarble@ficelle 500 :)
````

# Visualizing TPN's with pamelad

You can also use `pamelad` to visualize TPN's

If you are using the command line you can generate the
Graphviz dot file and then, in a second step, download
the corresponding SVG file:

````
pamela -i src/test/pamela/plant.pamela -i src/test/pamela/choice-tpn.pamela -f dot -o choice-tpn.dot tpn
curl -O http://$PAMELAD/downloads/choice-tpn.dot.svg
````

If you are using a web browser can can setup all the arguments
as above like this (assuming you are running with PAMELAD = localhost:9100):

![pamelad](src/test/pamela/pamelad.png)

And then press **Run** to see the SVG for the TPN in your
browser (at the URL http://localhost:9100/downloads/tpn.dot.svg ).

For interactive development you can make changes to `sample.pamela`,
press the browser *Back* button, then press **Run** to visualize
the TPN instantly!

You may separately use `curl` (as shown above) to
download the files.
