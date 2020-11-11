import numpy as np
import math

def csa(sigma: float, ps, chiN: float, cs: float, damps: float) -> float:
    return sigma * math.exp((np.linalg.norm(ps)/chiN - 1) * cs / damps) 

