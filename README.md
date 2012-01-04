The project directory contains three sub-directories:

1. Example Benchmarks- The directory benchmarks/ contains the tests.
   The benchmarks/tests/ directory contains the test and 
   benchmarks/src/Test.hs contain the calls to the benchmark tests.

2. Benchmark Results- The directory benchmark_results/ contain the
   timings for different benchmarks we executed.

3. The directory cu-benchmarks contains Copperhead benchmarks.

Build and Execution
--------------------

Accelerate benchmarks can be built and installed using cabal.

For executing the corresponding benchmark, use the following command
from .cabal/bin/ directory where the binary is installed:
./benchmarks <benchmark-name>

Example: ./benchmarks mydotp

Building Copperhead is slightly more involved.  There's a bunch of
dependencies (Python 2.7, CUDA 3.0+, numpy 1.3+, Boost 1.38+, PyCUDA,
CodePy, Thrust) that are required to be built and installed properly
before installing Copperhead.  There's a blog post detailing out the
process here:

http://nonzen.in/2011/10/16/copperhead.html

Once you have Copperhead up and running, running the benchmarks is
straightforward.
