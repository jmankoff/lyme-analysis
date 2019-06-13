import pandas as pd
import numpy as np
from sqlalchemy.types import *
import os
import time

def num_rows_battery(g):
    if g is None:
        return None
    return len(g)

def length_of_charge_minutes(g):
    if g is None or len(g)==0:
        return None
    """Returns the total charge duration in a time period"""
    secs = ((g["double_end_timestamp"] - g["timestamp"]) ).sum() / 1000
    mins = secs/60.0
    return mins

BATTERY_APPLY = [
    num_rows_battery,
    length_of_charge_minutes
]

BATTERY_SQL_TYPES = {
    "num_rows_battery" : Integer,
    "length_of_charge_minutes" : Float
}
