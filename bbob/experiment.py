#!/usr/bin/env python
from __future__ import division, print_function
import cocoex, cocopp
import scipy.optimize
from numpy.random import rand
import os, webbrowser

import cma_es as cma

def fmin_csa(problem, x0):
    alg = cma.CMA_ES_CSA(
        x0 = x0,
        lower = problem.lower_bounds,
        upper = problem.upper_bounds,
        full_display = False
    )
    return alg.fmin(problem)

def fmin_ppmf(problem, x0):
    alg = cma.CMA_ES_PPMF(
        x0 = x0,
        lower = problem.lower_bounds,
        upper = problem.upper_bounds,
        full_display = False
    )
    return alg.fmin(problem)


def run_experiment(suite_name, output, fmin, budget_multiplier = 1):
    """
    Run COCO benchmark.
    :param: suite_name suite name (e.g. bbob, bbob-largescale, etc.)
    :param: output directory where executor will write results
    :param: fmin optimization algorithm
    :param: budget_multiplier budget_multiplier
    """
    suite = cocoex.Suite(suite_name, "", "")
    observer = cocoex.Observer(suite_name, "result_folder: " + output)
    minimal_print = cocoex.utilities.MiniPrint()
    for problem in suite:  
        problem.observe_with(observer)
        x0 = problem.initial_solution
        while (problem.evaluations < problem.dimension * budget_multiplier
               and not problem.final_target_hit):
            fmin(problem, x0) 
            x0 = problem.lower_bounds + ((rand(problem.dimension) + rand(problem.dimension)) *
                        (problem.upper_bounds - problem.lower_bounds) / 2)
        minimal_print(problem, final=problem.index == len(suite) - 1)

if __name__ == "__main__":
    run_experiment("bbob", "CMA_ES_PPMF", fmin_ppmf)






