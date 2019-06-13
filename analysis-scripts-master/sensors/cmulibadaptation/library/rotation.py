import numpy as np
from sqlalchemy.types import *

def mean_magnitude_rotation(g):
    return np.mean(g.magnitude)

def std_magnitude_rotation(g):
    return np.std(g.magnitude)

def median_magnitude_rotation(g):
    return np.median(g.magnitude)

def min_magnitude_rotation(g):
    return np.min(g.magnitude)

def max_magnitude_rotation(g):
    return  np.max(g.magnitude)

ROTATION_FUNCTIONS = [
    mean_magnitude_rotation,
    std_magnitude_rotation,
    median_magnitude_rotation,
    min_magnitude_rotation,
    max_magnitude_rotation,
]

ROTATION_SQL_TYPES ={
    "mean_magnitude_rotation" : Float,
    "std_magnitude_rotation"  : Float,
    "median_magnitude_rotation" : Float,
    "min_magnitude_rotation" : Float,
    "max_magnitude_rotation" : Float
}