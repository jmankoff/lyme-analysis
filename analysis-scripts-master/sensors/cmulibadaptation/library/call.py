import pandas as pd
from sqlalchemy.types import *

def number_rows_calls(g):
    if g is None:
        return None
    return len(g)

def number_outgoing_calls(g):
    if g is None or len(g) == 0:
        return None
    return g[g["call_type"] == 2]["call_type"].count()

def number_incoming_calls(g):
    if g is None or len(g) == 0:
        return None
    return  g[g["call_type"] == 1]["call_type"].count()

def number_missed_calls(g):
    if g is None or len(g) == 0:
        return None
    return  g[g["call_type"] == 3]["call_type"].count()

def duration_outgoing_calls_seconds(g):
    if g is None or len(g) == 0:
        return None
    return  int(g[g["call_type"] == 2]["call_duration"].sum())

def duration_incoming_calls_seconds(g):
    if g is None or len(g) == 0:
        return None
    return  int(g[g["call_type"] == 1]["call_duration"].sum())

def most_frequent_correspondent_phone(g):
    if g is None or len(g) == 0:
        return None
    result = g["trace"].mode()
    if result.empty:
        return g["trace"].iloc[0]
    else:
        return result[0]

def number_of_correspondents_phone(g):
    if g is None or len(g) == 0:
        return None
    return g["trace"].nunique()

def number_incoming_calls_family(g):
    if g is None or len(g) == 0:
        return None
    return number_incoming_calls(g)

def number_outgoing_calls_family(g):
    if g is None or len(g) == 0:
        return None
    return number_outgoing_calls(g)
    
def number_missed_calls_family(g):
    if g is None or len(g) == 0:
        return None
    return number_missed_calls(g)
    
def duration_incoming_calls_seconds_family(g):
    if g is None or len(g) == 0:
        return None
    return duration_incoming_calls_seconds(g)

def duration_outgoing_calls_seconds_family(g):
    if g is None or len(g) == 0:
        return None
    return duration_outgoing_calls_seconds(g)
    
def most_frequent_correspondent_phone_family(g):
    if g is None or len(g) == 0:
        return None
    return most_frequent_correspondent_phone(g)
    
def number_of_correspondents_phone_family(g):
    if g is None or len(g) == 0:
        return None
    return number_of_correspondents_phone(g)

def number_incoming_calls_friends_on_campus(g):
    if g is None or len(g) == 0:
        return None
    return number_incoming_calls(g)

def number_outgoing_calls_friends_on_campus(g):
    if g is None or len(g) == 0:
        return None
    return number_outgoing_calls(g)
    
def number_missed_calls_friends_on_campus(g):
    if g is None or len(g) == 0:
        return None
    return number_missed_calls(g)
    
def duration_incoming_calls_seconds_friends_on_campus(g):
    if g is None or len(g) == 0:
        return None
    return duration_incoming_calls_seconds(g)

def duration_outgoing_calls_seconds_friends_on_campus(g):
    if g is None or len(g) == 0:
        return None
    return duration_outgoing_calls_seconds(g)
    
def most_frequent_correspondent_phone_friends_on_campus(g):
    if g is None or len(g) == 0:
        return None
    return most_frequent_correspondent_phone(g)
    
def number_of_correspondents_phone_friends_on_campus(g):
    if g is None or len(g) == 0:
        return None
    return number_of_correspondents_phone(g)

def number_incoming_calls_friends_outside_of_campus(g):
    if g is None or len(g) == 0:
        return None
    return number_incoming_calls(g)

def number_outgoing_calls_friends_outside_of_campus(g):
    if g is None or len(g) == 0:
        return None
    return number_outgoing_calls(g)
    
def number_missed_calls_friends_outside_of_campus(g):
    if g is None or len(g) == 0:
        return None
    return number_missed_calls(g)
    
def duration_incoming_calls_seconds_friends_outside_of_campus(g):
    if g is None or len(g) == 0:
        return None
    return duration_incoming_calls_seconds(g)

def duration_outgoing_calls_seconds_friends_outside_of_campus(g):
    if g is None or len(g) == 0:
        return None
    return duration_outgoing_calls_seconds(g)
    
def most_frequent_correspondent_phone_friends_outside_of_campus(g):
    if g is None or len(g) == 0:
        return None
    return most_frequent_correspondent_phone(g)
    
def number_of_correspondents_phone_friends_outside_of_campus(g):
    if g is None or len(g) == 0:
        return None
    return number_of_correspondents_phone(g)

CALL_APPLY = [
    number_rows_calls,
    number_incoming_calls,
    number_outgoing_calls,
    number_missed_calls,
    duration_incoming_calls_seconds,
    duration_outgoing_calls_seconds,
    most_frequent_correspondent_phone,
    number_of_correspondents_phone
]

CALL_FAMILY_APPLY = [
    number_incoming_calls_family,
    number_outgoing_calls_family,
    number_missed_calls_family,
    duration_incoming_calls_seconds_family,
    duration_outgoing_calls_seconds_family,
    most_frequent_correspondent_phone_family,
    number_of_correspondents_phone_family
]

CALL_FH_APPLY = [
    number_incoming_calls_friends_on_campus,
    number_outgoing_calls_friends_on_campus,
    number_missed_calls_friends_on_campus,
    duration_incoming_calls_seconds_friends_on_campus,
    duration_outgoing_calls_seconds_friends_on_campus,
    most_frequent_correspondent_phone_friends_on_campus,
    number_of_correspondents_phone_friends_on_campus
]

CALL_FF_APPLY = [
    number_incoming_calls_friends_outside_of_campus,
    number_outgoing_calls_friends_outside_of_campus,
    number_missed_calls_friends_outside_of_campus,
    duration_incoming_calls_seconds_friends_outside_of_campus,
    duration_outgoing_calls_seconds_friends_outside_of_campus,
    most_frequent_correspondent_phone_friends_outside_of_campus,
    number_of_correspondents_phone_friends_outside_of_campus
]

CALL_SQL_TYPES = {
    "number_rows_calls" : Integer,
    "number_incoming_calls" : Integer,
    "number_outgoing_calls" : Integer,
    "number_missed_calls" : Integer,
    "duration_incoming_calls_seconds": Integer,
    "duration_outgoing_calls_seconds": Integer,
    "most_frequent_correspondent_phone" : Text,
    "number_of_correspondents_phone" : Integer,
    "number_incoming_calls_family" : Integer,
    "number_outgoing_calls_family" : Integer,
    "number_missed_calls_family" : Integer,
    "duration_incoming_calls_seconds_family": Integer,
    "duration_outgoing_calls_seconds_family": Integer,
    "most_frequent_correspondent_phone_family" : Text,
    "number_of_correspondents_phone_family" : Integer,
    "number_incoming_calls_friends_on_campus" : Integer,
    "number_outgoing_calls_friends_on_campus" : Integer,
    "number_missed_calls_friends_on_campus" : Integer,
    "duration_incoming_calls_seconds_friends_on_campus": Integer,
    "duration_outgoing_calls_seconds_friends_on_campus": Integer,
    "most_frequent_correspondent_phone_friends_on_campus" : Text,
    "number_of_correspondents_phone_friends_on_campus" : Integer,
    "number_incoming_calls_friends_outside_of_campus" : Integer,
    "number_outgoing_calls_friends_outside_of_campus" : Integer,
    "number_missed_calls_friends_outside_of_campus" : Integer,
    "duration_incoming_calls_seconds_friends_outside_of_campus": Integer,
    "duration_outgoing_calls_seconds_friends_outside_of_campus": Integer,
    "most_frequent_correspondent_phone_friends_outside_of_campus" : Text,
    "number_of_correspondents_phone_friends_outside_of_campus" : Integer
}

dffamily_cols = ["number_incoming_calls_family","number_outgoing_calls_family",
    "number_missed_calls_family","duration_incoming_calls_seconds_family",
    "duration_outgoing_calls_seconds_family","most_frequent_correspondent_phone_family",
    "number_of_correspondents_phone_family"]

dffh_cols = ["number_incoming_calls_friends_on_campus","number_outgoing_calls_friends_on_campus",
    "number_missed_calls_friends_on_campus","duration_incoming_calls_seconds_friends_on_campus",
    "duration_outgoing_calls_seconds_friends_on_campus","most_frequent_correspondent_phone_friends_on_campus",
    "number_of_correspondents_phone_friends_on_campus"]

dfff_cols = ["number_incoming_calls_friends_outside_of_campus","number_outgoing_calls_friends_outside_of_campus",
    "number_missed_calls_friends_outside_of_campus","duration_incoming_calls_seconds_friends_outside_of_campus",
    "duration_outgoing_calls_seconds_friends_outside_of_campus","most_frequent_correspondent_phone_friends_outside_of_campus",
    "number_of_correspondents_phone_friends_outside_of_campus"]