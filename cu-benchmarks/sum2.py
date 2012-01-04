#! /usr/bin/python

from copperhead import *
import numpy as np
import timeit
import sys

@cu
def vector_sum(x):
    def elem_wise(xi):
        if (xi < 0):
            return -xi
        else:
            return xi
    return sum(map(elem_wise, x))

# @cu
# def vector_sum(x):
#     def elem_wise(xi):
#         if (xi > 0):
#             return xi
#         else:
#             return -xi
#     # sum(map((lambda xi: xi if xi > 0 else xi * -1), x))
#     return (sum(map(elem_wise, x)))
 
def dogpu():
    with places.gpu0:
        gpu = vector_sum(x)

def docpu():
    with places.here:
        cpu = vector_sum(x)

def dobarecpu():
  res = sum(x1)

def do_run(fn, name, count=30):
    t = timeit.Timer(fn)
    n = t.timeit(count)
    return n/count

if __name__ == '__main__':

  for sz in [1, 10, 100, 1000, 10000,
             100000, 200000, 300000, 400000, 500000,
             600000, 700000, 800000, 900000, 1000000 ]:

      x1 = np.arange(sz, dtype=np.float64)
      x  = CuArray(x1)
      # x = map(elem_wise, x)
      # vector_sum(x);

      t1 = do_run(dogpu, "GPU")
      t2 = do_run(docpu, "CPU")
      t3 = do_run(dobarecpu, "Bare CPU")

      print "{0:7d}, {1:3.10f}, {2:3.10f}, {3:3.10f}".format(sz, t1, t2, t3)

