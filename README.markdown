blaze-markup
============

This is an experimental fork of `blaze-markup` to demonstrate performance
benefits of [`text-builder-linear`](https://github.com/Bodigrim/linear-builder):

|Name            |Before, μs |After, μs |Ratio   |
|----------------|---------:|---------:|:------:|
|bigTable        |3281.51|1641.01| 0.500  |
|basic           |  13.77|   7.66| 0.556  |
|wideTree        |4230.64|2192.20| 0.518  |
|wideTreeEscaping| 818.71| 354.08| 0.432  |
|deepTree        | 787.10| 388.89| 0.494  |
|manyAttributes  |2903.38|1480.55| 0.510  |
|customAttribute |5401.18|2944.69| 0.545  |
|Geometric mean  |       |       | 0.507  |

What
----

The core modules to build a blazingly fast markup combinator library such as
[blaze-html]. Most applications should not use this package directly.

[blaze-html]: http://jaspervdj.be/blaze

Development
-----------

Running the tests:

    cabal configure --enable-tests && cabal build && cabal test

Running the benchmarks:

    make benchmark

Credits
-------

Authors:

- Jasper Van der Jeugt
- Simon Meier
- Deepak Jois
