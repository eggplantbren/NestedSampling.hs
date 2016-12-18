NestedSampling.hs
=================

[![MIT License](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/eggplantbren/NestedSampling.hs/blob/master/LICENSE)

(c) 2016 Brendon J. Brewer and Jared Tobin

This is a Haskell implementation of the classic [Nested
Sampling](https://en.wikipedia.org/wiki/Nested_sampling_algorithm) algorithm
introduced by John Skilling. You can use it for Bayesian inference, statistical
mechanics, and optimisation applications.

There are a few examples included that you can run using
[Stack](https://docs.haskellstack.org/) as follows:

```
$ stack test NestedSampling-hs:test:spikeslab
$ stack test NestedSampling-hs:test:rosenbrock
```

.. and so on.  Check the .cabal file for a list of examples, the code for which
can always be found in the `test` directory.

Running any of these examples will log sampling progress to stdout and also
dump output information to a couple of CSV files:

* `nested_sampling_info.csv` includes log prior weight, log likelihood, log
  evidence, and entropy information by sampler iteration.
* `nested_sampling_parameters.csv` includes parameter information (where one
  line = one sample).

