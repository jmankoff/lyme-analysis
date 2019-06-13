import pandas as pd
import numpy as np
from sqlalchemy.types import *

def number_rows_messages(g):
    if len(g) is None:
        return None
    return len(g)

def number_of_outgoing_messages(g):
    if g is None or len(g) == 0:
        return None
    return g["message_type"][g["message_type"] == 2].count()

def number_of_incoming_messages(g):
    if g is None or len(g) == 0:
        return None
    return g["message_type"][g["message_type"] == 1].count()

def most_frequent_correspondent(g):
    if g is None or len(g) == 0:
        return None
    if g.empty:
        return None
    result = g["trace"].mode()
    if result.empty:
        return g["trace"].iloc[0]
    else:
        return result[0]

def number_of_correspondents(g):
    if g is None or len(g) == 0:
        return None
    return g["trace"].nunique()

def number_of_outgoing_messages_family(g):
    if g is None or len(g) == 0:
        return None
    return number_of_outgoing_messages(g)
    
def number_of_incoming_messages_family(g):
    if g is None or len(g) == 0:
        return None
    return number_of_incoming_messages(g)
    
def most_frequent_correspondent_family(g):
    if g is None or len(g) == 0:
        return None
    return most_frequent_correspondent(g)
    
def number_of_correspondents_family(g):
    if g is None or len(g) == 0:
        return None
    return number_of_correspondents(g)

def number_of_outgoing_messages_friends_on_campus(g):
    if g is None or len(g) == 0:
        return None
    return number_of_outgoing_messages(g)
    
def number_of_incoming_messages_friends_on_campus(g):
    if g is None or len(g) == 0:
        return None
    return number_of_incoming_messages(g)
    
def most_frequent_correspondent_friends_on_campus(g):
    if g is None or len(g) == 0:
        return None
    return most_frequent_correspondent(g)
    
def number_of_correspondents_friends_on_campus(g):
    if g is None or len(g) == 0:
        return None
    return number_of_correspondents(g)

def number_of_outgoing_messages_friends_outside_of_campus(g):
    if g is None or len(g) == 0:
        return None
    return number_of_outgoing_messages(g)
    
def number_of_incoming_messages_friends_outside_of_campus(g):
    if g is None or len(g) == 0:
        return None
    return number_of_incoming_messages(g)
    
def most_frequent_correspondent_friends_outside_of_campus(g):
    if g is None or len(g) == 0:
        return None
    return most_frequent_correspondent(g)
    
def number_of_correspondents_friends_outside_of_campus(g):
    if g is None or len(g) == 0:
        return None
    return number_of_correspondents(g)

MESSAGE_APPLY = [
    number_rows_messages,
    number_of_outgoing_messages,
    number_of_incoming_messages,
    most_frequent_correspondent,
    number_of_correspondents
]

MESSAGE_FAMILY_APPLY = [
    number_of_outgoing_messages_family,
    number_of_incoming_messages_family,
    most_frequent_correspondent_family,
    number_of_correspondents_family
]

MESSAGE_FH_APPLY = [
    number_of_outgoing_messages_friends_on_campus,
    number_of_incoming_messages_friends_on_campus,
    most_frequent_correspondent_friends_on_campus,
    number_of_correspondents_friends_on_campus
]

MESSAGE_FF_APPLY = [
    number_of_outgoing_messages_friends_outside_of_campus,
    number_of_incoming_messages_friends_outside_of_campus,
    most_frequent_correspondent_friends_outside_of_campus,
    number_of_correspondents_friends_outside_of_campus
]

MESSAGE_SQL_TYPES = {
    "number_rows_messages": Integer,
    "number_of_outgoing_messages" : Integer,
    "number_of_incoming_messages" : Integer,
    "most_frequent_correspondent" : Text,
    "number_of_correspondents"    : Integer,
    "number_of_outgoing_messages_family" : Integer,
    "number_of_incoming_messages_family" : Integer,
    "most_frequent_correspondent_family" : Text,
    "number_of_correspondents_family"    : Integer,
    "number_of_outgoing_messages_friends_on_campus" : Integer,
    "number_of_incoming_messages_friends_on_campus" : Integer,
    "most_frequent_correspondent_friends_on_campus" : Text,
    "number_of_correspondents_friends_on_campus"    : Integer,
    "number_of_outgoing_messages_friends_outside_of_campus" : Integer,
    "number_of_incoming_messages_friends_outside_of_campus" : Integer,
    "most_frequent_correspondent_friends_outside_of_campus" : Text,
    "number_of_correspondents_friends_outside_of_campus"    : Integer
}

dffamily_cols = ["number_of_outgoing_messages_family","number_of_incoming_messages_family",
"most_frequent_correspondent_family","number_of_correspondents_family"]
dffh_cols = ["number_of_outgoing_messages_friends_on_campus","number_of_incoming_messages_friends_on_campus",
"most_frequent_correspondent_friends_on_campus","number_of_correspondents_friends_on_campus"]
dfff_cols = ["number_of_outgoing_messages_friends_outside_of_campus","number_of_incoming_messages_friends_outside_of_campus",
"most_frequent_correspondent_friends_outside_of_campus","number_of_correspondents_friends_outside_of_campus"]