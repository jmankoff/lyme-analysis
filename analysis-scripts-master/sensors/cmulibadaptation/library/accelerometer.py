import numpy as np
from sqlalchemy.types import *

def mean_magnitude(g):
    return np.mean(g.magnitude)

def std_magnitude(g):
    return np.std(g.magnitude)

def median_magnitude(g):
    return np.median(g.magnitude)

def min_magnitude(g):
    return np.min(g.magnitude)

def max_magnitude(g):
    return  np.max(g.magnitude)

ACCELEROMETER_FUNCTIONS = [
    mean_magnitude,
    std_magnitude,
    median_magnitude,
    min_magnitude,
    max_magnitude,
]

ACCELEROMETER_SQL_TYPES ={
    "mean_magnitude" : Float,
    "std_magnitude"  : Float,
    "median_magnitude" : Float,
    "min_magnitude" : Float,
    "max_magnitude" : Float
}