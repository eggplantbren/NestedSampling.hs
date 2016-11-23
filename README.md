NestedSampling.hs
=================

Eventually, this will be a Haskell implementation of classic Nested Sampling.
Warning: I am a beginner at this language so don't expect much for a while.

(c) 2016 Brendon J. Brewer and Jared Tobin

LICENSE: MIT. See the LICENSE file for details.

You can compile the library and run the demo with Stack:

```
stack build
stack exec ns-example
```

This will run the SpikeSlab example. The output files are
`sample.txt` (parameters) and `sample_info.txt`
(log prior weight and log likelihood).
Posterior weight is proportional to prior weight times likelihood - that's
your responsibility for now.
