#! /usr/bin/python

from copperhead import *
import numpy as np

@cu
def vector_sum(x):
    def elem_wise(xi):
        if (xi > 0):
            return xi
        else:
            return -xi
    return sum(map(elem_wise, x))
    # return sum(map((lambda xi: xi if xi > 0 else xi * -1), x))

if __name__ == '__main__':
  for sz in [1, 10, 100, 1000, 10000,
             100000, 200000, 300000, 400000, 500000,
             600000, 700000, 800000, 900000, 1000000]:
      s = vector_sum(CuArray(np.arange(sz, dtype=np.float64)))
      print "{0:7} {1}".format(sz, s)

