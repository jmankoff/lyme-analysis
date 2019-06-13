import pandas as pd
import numpy as np
from sqlalchemy.types import *

#Unicode Emoji were destroyed somewhere along the way
HAPPY_REGEX = r'\|?>?[:*;Xx8=]-?o?\^?[DPpb3)}\]>]\)?'
SAD_REGEX = r'([:><].?-?[@><cC(\[{\|]\|?|[D][:8;=X]<?|v.v)'

def number_of_insertions(g):
    return g["current_text"][g["current_text"].str.len() - g["before_text"].str.len() == 1].count()

def number_of_deletions(g):
    return g["current_text"][g["current_text"].str.len() - g["before_text"].str.len() == -1].count()

def average_time_between_keypress_ms(g):

    #Defines typing segments as occurring 100 seconds between each other. This param can be tweaked

    thresh = 1e5
    ts = (g['timestamp'] - g['timestamp'].shift()) > thresh
    grp = [0]
    for i in range(1,len(ts)):
        if ts.iloc[i]:
            grp.append(grp[-1] + 1)
        else:
            grp.append(grp[-1])
    # grp.append(grp[-1])
    g['grouper'] = grp
    return g.groupby("grouper").timestamp.diff().mean()


def sad_emoticon_count(g):
    completed = g["current_text"][g["current_text"].str.len() - g["before_text"].shift(-1).str.len() > 1]
    completed_no_nan = completed.dropna()
    sad = completed_no_nan[completed_no_nan.str.contains(SAD_REGEX)]
    return sad.count()

def happy_emoticon_count(g):
    completed = g["current_text"][g["current_text"].str.len() - g["before_text"].shift(-1).str.len() > 1]
    completed_no_nan = completed.dropna()
    happy = completed_no_nan[completed_no_nan.str.contains(HAPPY_REGEX)]
    return happy.count()

def levenshtein(s1, s2):
    """
    :return: the edit distance between two strings

    Borrowed from the algorithm wiki:
     https://en.wikibooks.org/wiki/Algorithm_Implementation/Strings/Levenshtein_distance#Python
    """
    if len(s1) < len(s2):
        return levenshtein(s2, s1)

    # len(s1) >= len(s2)
    if len(s2) == 0:
        return len(s1)

    previous_row = range(len(s2) + 1)
    for i, c1 in enumerate(s1):
        current_row = [i + 1]
        for j, c2 in enumerate(s2):
            insertions = previous_row[
                             j + 1] + 1  # j+1 instead of j since previous_row and current_row are one character longer
            deletions = current_row[j] + 1  # than s2
            substitutions = previous_row[j] + (c1 != c2)
            current_row.append(min(insertions, deletions, substitutions))
        previous_row = current_row

    return previous_row[-1]

KEYBOARD_APPLY = [
    number_of_insertions,
    number_of_deletions,
    sad_emoticon_count,
    happy_emoticon_count,
    average_time_between_keypress_ms
]

KEYBOARD_SQL_TYPES = {
    "number_of_insertions" : Integer,
    "number_of_deletions"  : Integer,
    "average_time_between_keypress_ms" : Float,
    "sad_emoticon_count"   : Integer,
    "happy_emoticon_count": Integer,
}