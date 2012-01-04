#! /usr/bin/python

# The first sample, from http://code.google.com/p/copperhead/

from copperhead import *
import numpy as np
import timeit
import sys

@cu
def axpy(a, x, y):
  return [a * xi + yi for xi, yi in zip(x, y)]

def dogpu():
  with places.gpu0:
    gpu = axpy(2.0, x, y)
        
def docpu():
  with places.here:
    cpu = axpy(2.0, x, y)

# def axpy0(a, x, y):
#   return [a * xi + yi for xi, yi in zip(x, y)]

def dobarecpu():
  return [2.0 * xi + yi for xi, yi in zip(x1, y1)]  

def do_run(fn, name, count=30):
    t = timeit.Timer(fn)
    n = t.timeit(count)
    return n/count

if __name__ == '__main__':

  for sz in [1, 10, 100, 1000, 10000,
             100000, 200000, 300000, 400000, 500000,
             600000, 700000, 800000, 900000, 1000000,
             2000000, 3000000, 4000000, 5000000, 
             6000000, 7000000, 8000000, 9000000,
             10000000]:

    x1 = np.arange(sz, dtype=np.float64)
    y1 = np.arange(sz, dtype=np.float64)
    x  = CuArray(x1)
    y  = CuArray(y1)
  
    t1 = do_run(dogpu, "GPU")
    t2 = do_run(docpu, "CPU")
    t3 = do_run(dobarecpu, "Bare CPU")

    print "{0:10d}, {1:3.10f}, {2:3.10f}, {3:3.10f}".format(sz, t1, t2, t3)

