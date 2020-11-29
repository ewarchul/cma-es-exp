import numpy as np
import math

def cast_on_boundries(points, lower_bound, upper_bound):
    """
    Function casts set of points on hiperrectangle bounds.
    :param points set of points
    :param lower_bound 
    :param upper_bound 
    :return matrix 
    """
    upper_cast = cast_on_boundry(upper_bound)
    lower_cast = cast_on_boundry(lower_bound)
    return upper_cast(lower_cast(points))

def cast_on_boundry(boundry): 
    if boundry >= 0:
        f = lambda x: x if x < boundry else boundry
    else:
        f = lambda x: boundry if x < boundry else x
    return np.vectorize(f)
    
def is_pos_def(eigenvalues):
    machine_eps = 2.220446 * 10**(-16)
    if all(eigenvalues >= math.sqrt(machine_eps) * abs(eigenvalues[0])):
        return True
    else:
        return False
