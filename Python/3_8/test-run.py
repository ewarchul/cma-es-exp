import cma_es_ppmf as ppmf

x0 = np.repeat(100, 10)
upper = 100
lower = -100
fn = lambda x: sum(x**2)
alg = ppmf.CMA_ES_PPMF(x0, lower, upper)

