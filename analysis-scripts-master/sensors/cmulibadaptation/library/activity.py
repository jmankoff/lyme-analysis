from sqlalchemy.types import *
import pandas as pd


def count_changes(g):
    """Applied to each groupby object to count the changes"""
    return (g["changed"][g["changed"] == True]).count()

def number_of_activities(g):
    return g["activity_type"].nunique()

def most_common_activity(g):
    if g.empty:
        return None
    result = g["activity_name"].mode()
    if result.empty:
        return g["activity_name"].iloc[0]
    else:
        return result[0]


ACTIVITY_FUNCTIONS = [
    count_changes,
    number_of_activities,
    most_common_activity
]

ACTIVITY_SQL_TYPES = {
    "count_changes" : Integer,
    "number_of_activities" : Integer,
    "most_common_activity" : String(length=20)

}
