from sqlalchemy.types import *

def number_samples_wifi(g):
    if g is None:
        return None
    return len(g)

def number_unique_wifi_hotspots(g):
    if g is None or len(g) == 0:
        return None
    return g["bssid"].nunique()

def most_frequent_wifi(g):
    if g is None or len(g) == 0:
        return None
    result = g["bssid"].mode()
    if result.empty:
        return g["bssid"].iloc[0]
    else:
        return result[0]

WIFI_APPLY = [
    number_samples_wifi,
    number_unique_wifi_hotspots,
    most_frequent_wifi
]

WIFI_SQL_TYPES = {
    "number_samples_wifi": Integer,
    "number_unique_wifi_hotspots" : Integer,
    "most_frequent_wifi" : Text
}