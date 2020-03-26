# CMA-ES

Different CMA-ES experiments.

## Methods

* CMA-ES classic
	- `cma_es()`
* no-CMA-ES-no-$\sigma$
	- `no_cma_es_sigma()`
* no-CMA-ES
	- `no_cma_es()`
* no-CMA-ES-new-$\sigma$-up (new rule)
 	- `no_cma_es_sigma_up()`
* no-CMA-ES-new-$\sigma$-down (new rule)
 	- `no_cma_es_sigma_down()`

## Setting
* N = 10 (dim)
* $\lambda$ = $4*N$
* $\mu$ = floor($\frac{\lambda}{2}$)
* stopfitness = $10^{-50}$
* maxiter = $100000*N$
* sigma =  0.5
