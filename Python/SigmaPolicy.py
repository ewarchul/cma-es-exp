import numpy as np
import math

def csa(
    sigma: float,
    ps,
    chiN: float,
    cs: float,
    damps: float
) -> float:
    return sigma * math.exp((np.linalg.norm(ps)/chiN - 1) * cs / damps) 

def ppmf(
    sigma:float,
    meanOld_fitness: [float],
    arfitness: [float],
    d_param: float,
    p_target: float
) -> float:
    p_succ = sum(arfitness < meanOld_fitness) / arfitness.size
    return sigma * math.exp(d_param * (p_succ - p_target) / (1 - p_target))

