import numpy as np
from sqlalchemy.types import *

def mean_temperature(g):
    return np.mean(g.temperature)

def std_temperature(g):
    return np.std(g.temperature)

def median_temperature(g):
    return np.median(g.temperature)

def min_temperature(g):
    return np.min(g.temperature)

def max_temperature(g):
    return  np.max(g.temperature)

TEMPERATURE_FUNCTIONS = [
    mean_temperature,
    std_temperature,
    median_temperature,
    min_temperature,
    max_temperature,
]

TEMPERATURE_SQL_TYPES ={
    "mean_temperature" : Float,
    "std_temperature"  : Float,
    "median_temperature" : Float,
    "min_temperature" : Float,
    "max_temperature" : Float
}