import pandas as pd
import numpy as np
from sqlalchemy.types import *

def mean_lux(g):
    return np.mean(g.double_light_lux)

def std_lux(g):
    return np.std(g.double_light_lux)

def median_lux(g):
    return np.median(g.double_light_lux)

def min_lux(g):
    return np.min(g.double_light_lux)

def max_lux(g):
    return  np.max(g.double_light_lux)

LIGHT_APPLY = [
    mean_lux,
    std_lux,
    median_lux,
    min_lux,
    max_lux,
]

LIGHT_SQL_TYPES ={
    "mean_lux" : Float,
    "std_lux"  : Float,
    "median_lux" : Float,
    "min_lux" : Float,
    "max_lux" : Float
}