import cma_es_ppmf as ppmf
import cma_es_csa as csa
import numpy as np 

x0 = np.repeat(100, 10)
upper = 100
lower = -100
fn = lambda x: sum(x**2)
alg_ppmf = ppmf.CMA_ES_PPMF(x0, lower, upper)
alg_csa = csa.CMA_ES_CSA(x0, lower, upper) 
