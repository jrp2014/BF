# BF
Experiments with implementing the BrainFuck language in Haskell

BrainFuck is a simple languaeg, chosen because of the availabilty of test cases.
A specification can be found in [WikiPedia](https://en.wikipedia.org/wiki/Brainfuck).
A pure specification is in [Esolang](https://esolangs.org/wiki/Pure_BF)


So far, there are a couple of different modules:

* `BF` is a naive implementation with no monads, parser combinators, etc
* `PureBF` is a reference implementation taken from [EsoLang](https://esolangs.org/wiki/Pure_BF/Implementation)

The intention is to
* Add command line features (to import source files), act as an interpreter, etc
* Include testing and benchmarking
* Use more modern / esoteric Haskell features such as State Transformers, Comonads, etc.  (see, eg, [Dave Laing](http://dlaing.org/cofun/)
