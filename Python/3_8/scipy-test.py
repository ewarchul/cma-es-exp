import scipy.optimize as sc
import numpy as np
import operator
import functools
import rpy2.robjects as robjects

# Optimization staff

def prod(x) -> float:
    return functools.reduce(operator.mul, x, 1)

def optim_func(x: float) -> float:
    return sum(x**2)

def griewank_func(x: any) -> float:
    n = x.shape[0]
    sqrt = np.sqrt(np.array(range(1, n + 1)))
    return 1 + sum((x ** 2) / 4000) - prod(np.cos(x / sqrt)) 

def run_optim(eval: callable, x0: float):
    return sc.fmin(eval, x0)
    


# R staff

r_source_func = robjects.r['source']







