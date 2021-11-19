# Learning Framework

* This repo is an extension of the basic [open games engine](https://github.com/philipp-zahn/open-games-hs). It provides a playground for learning games.

* The underlying repo is a Haskell combinator library implementing _open games_, a battery of examples, and a code generation tool for making the combinator library practical to work with.

If you have questions, drop me (Philipp) a [mail](mailto:philipp.zahn@unisg.ch)!

# Run the code

You can use `stack build` to compile the project, `stack test` will run the tests
`stack ghci` and `stack ghci --test` will run ghci using the main target or the test
targets respectively.


# Usage

There is a [tutorial](https://github.com/philipp-zahn/open-games-hs/blob/master/Tutorial/TUTORIAL.md) how to use the software for modelling.


# Implementation details

tbd

# Running R

Things to change when running:

* Create an ./output dir
* Change the results hash

docker run -v`pwd`/outputs:/outputs -v`pwd`/Rscripts:/Rscripts -v`pwd`/testGame-87cc1567373a3995220c7a62930e070210a77ff4:/experiment/ --rm ghcr.io/learning-games/r:2021-11-11@sha256:08975d657fd2e84e611028460e55bd32484f34479785e7de4cb52b7d1be286e8 R -f /Rscripts/asymlearners_4phases.R
