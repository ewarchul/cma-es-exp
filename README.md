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


## Ref

* A Computationally Efficient Limited Memory CMA-ES for Large Scale Optimization, Ilya Loshchilov, gecco 2014

* Learning probability distributions in continuous evolutionary algorithms – a comparative review 

* A Median Success Rule for Non-Elitist Evolution Strategies: Study of Feasibility,  Ouassim Ait Elhara, Anne Auger, Nikolaus Hansen, gecco 2013

* Improved Step Size Adaptation for the MO-CMA-ES, Thomas Voß, Nikolaus Hansen, Christian Igel, gecco 2010

