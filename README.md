NestedSampling.hs
=================

[![MIT License](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/eggplantbren/NestedSampling.hs/blob/master/LICENSE)

This is a Haskell implementation of the classic Nested Sampling algorithm
introduced by John Skilling. You can use it for Bayesian inference,
statistical mechanics, and optimisation applications.

(c) 2016 Brendon J. Brewer and Jared Tobin

LICENSE: MIT. See the LICENSE file for details.

You can compile the library and run the demo with Stack:

```
stack build
stack exec ns-example
```

This will run the SpikeSlab example. The output files are
`sample.txt` (parameters, one line=one sample) and `sample_info.txt`
(log prior weight and log likelihood).
Posterior weight is proportional to prior weight times likelihood, and it's
your responsibility to calculate that from the information in the output files.

There is also a 10-dimensional Rosenbrock example, which you can run using
```
stack exec rosenbrock-example
```

