[![Clojars Project](https://img.shields.io/clojars/v/dollabs/pamela.svg)](https://clojars.org/dollabs/pamela)


# PAMELA

Probabalistic Advanced Modeling and Execution Learning Architecture (PAMELA).

The goal of PAMELA is to design and implement a new probabilistic
modeling language that extends the current state of the art process modeling
languages, such as RMPL by adding first class probability
variables. The compiler will not only compile the model into an
automata representation such as a "Probabilistic Hierarchical
Constraint Automata" (PHCA), but it will also synthesize a learning
algorithm to bind the probabilistic variables using machine learning
algorithms, appropriate for the program. This approach reduces the
need for the programmer to be an expert in machine learning algorithms
freeing the programmer to write models that employ probability values.

One of our initial focuses has been on developing language constructs to support Temporal
Planning Network (TPN) for consumption by a suitable TPN execution engine.

## News

The [DOLL](http://dollabs.com/) team (including
[Paul Robertson](https://twitter.com/DrPaulRobertson),
[Dan Cerys](https://twitter.com/dcerys),
[Prakash Manghwani](https://twitter.com/manghwani), and
[Tom Marble](https://twitter.com/tmarble)) presented
the talk **In situ model-based learning in PAMELA** at
[Clojure Conj 2016](http://2016.clojure-conj.org/model-based-learning-in-pamela/).
Check out the [ClojureTV](https://youtu.be/i84i1X9k8_g) video
and the [slides](doc/slides/ClojureConjPamelaLearns.pdf).

See also our talk **Model based programming in PAMELA** from
[Clojure/west](http://clojurewest.org/speakers#tmarble).
Check out the [ClojureTV](https://youtu.be/WLovW6hlYHM) video
and the [slides](doc/slides/ClojureWestHelloPamela.pdf).

For more details visit the [PAMELA project page](https://dollabs.com/projects/pamela).

## TPN Example

Consider a simple mission to be performed by a quadcopter equipped with video cameras and
some processing power. The quadcopter's mission is to monitor, process and upload images
of the recently discovered white elephant in a land far far away.  Reconnaissance drones
have already sent the location of the elephant.  The purpose of this mission is to
collect photos of the elephant from high above the ground so as to not interfere with the
natural habitat of the elephant.

Our quadcopter(QC) is an autonomous QC that can plan and adapt its own actions to ensure
that mission is successful. For example, it could choose to the take images at full resolution or
high resolution, perform image analysis at high speed or low speed to conserve power. In
addition to video sensors, our QC is also equipped with two additional sensors for self defensive
maneuvering actions from other wild birds who mistake QC for prey.

This mission is described in `./test/pamela/tpn-demo.pamela`

Before using the `pamela` command, be sure that you have run the `pamela-setup` script (see below).

Create the Temporal Plan Network (TPN) plan from the Pamela file:

`./bin/pamela -i test/pamela/tpn-demo.pamela -c main:tpn:elephant -o tpn-demo.tpn.json tpn`


This network rendering was created with PLANVIZ as follows:

1. `../planviz/bin/planviz -i tpn-demo.tpn.edn`
2. When you see `PLANVIZ server ready`, then open your browser to [http://localhost:8080](http://localhost:8080)
3. Click into the command box and type `/show tpn-demo.tpn` and press return.
4. When you see the TPN graph, type the command `/export` and you will be prompted to save **tpn-demo.tpn.svg** as a file (which you can view in your browser or any SVG capable tool).

<img alt="[tpn-demo.tpn.png]" src="http://dollabs.github.io/pamela/doc/figures/tpn-demo.tpn.png" width="100%" height="auto">


## Installing PAMELA tools with pamela-setup

The **pamela-setup** script is intended as an tool to help you
get started quickly with the Pamela tools (including planviz).

This script will:

* Verify that **java** is installed
* Verify that **git** is installed
* Ensure that the `~/bin` directory exists (and create it if necessary)
* Check that the `~/bin` is in the `PATH`<br/>
  _NOTE:_ the script will not attempt to change the initial setting of your **PATH** (this must be done manually if required). You can add the following to your
  `.profile` or `.bashrc` as appropriate: `export PATH=$HOME/bin:$PATH`
* Ensure that the `~/src/github/dollabs` directory exists (and create it if necessary)
* Verify that **boot** is installed (and install it if necessary)
* Will install the DOLL labs repositories:
  - webkeys (will install this pre-release library locally)
  - webtasks (will install this pre-release library locally)
  - plan-schema (will install this pre-release library locally)
  - planviz (will build the PLANVIZ jar)
  - pamela (will build the PAMELA jar)
* Will run the PAMELA Clojure tests and command line tests

```
curl -fsSLo pamela-setup https://raw.githubusercontent.com/dollabs/pamela/master/bin/pamela-setup && chmod +x pamela-setup && ./pamela-setup
```

## Development status and Contributing

Please see [CONTRIBUTING](CONTRIBUTING.md) for details on
how to make a contribution.

PAMELA is currently under heavy development and has not yet been tagged with
official "release". In this *pre-release* state the PAMELA API and
functionality is subject to change.

Currently there is no mailing list setup for PAMELA (but will be
at some point soon)!

## Further Documentation

* Notes on generating [TPNs](doc/TPN.md)
* Notes on generating [HTNs](doc/HTN.md)
* PAMELA [grammar](doc/grammar.md)

## Copyright and license

Copyright © 2016 Dynamic Object Language Labs Inc.

Licensed under the [Apache License 2.0](http://opensource.org/licenses/Apache-2.0) [LICENSE](LICENSE)

## Acknowledgements and Disclaimer

Please see [ACKNOWLEDGEMENT](ACKNOWLEDGEMENT.md)
