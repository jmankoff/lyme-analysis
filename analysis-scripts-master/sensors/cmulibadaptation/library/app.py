import pandas as pd
import numpy as np
#import urllib2 # YSS does not work with Python3
#from bs4 import BeautifulSoup # YSS not needed if not using query_google
import play_scraper # YSS
import time
from sqlalchemy.types import *

def most_common_app(g):
    if g.empty:
        return None
    result = g["package_name"].mode()
    if result.empty:
        return g["package_name"].iloc[0]
    else:
        return result[0]

def most_common_category(g):
    if g.empty:
        return None
    result = g["package_category"].mode()
    if result.empty:
        return g["package_category"].iloc[0]
    else:
        return result[0]


def number_of_unique_apps(g):
    return g["package_name"].nunique()

def apps_per_minute(g):
    count = g["package_name"].count()
    minutes = (g.index.max() - g.index.min()).total_seconds() / 60.

    if minutes == 0:
        return 0
    else:
        return count / minutes

def app_use_time_seconds(g):
    active = g[g["process_importance"] == 100]
    duration = (active["double_end_timestamp"] - active["timestamp"]) / 1000
    total_duration = duration.sum()
    return int(total_duration)

def number_of_app_changes(g):
    #Is this right? need to consult Aware Docs
    #Ask Afsaneh about this - do we only want to look at the active ones? For all of these?
    return g[g["process_importance"] == 100]["package_name"].count()


APP_FUNCTIONS = [
    number_of_unique_apps,
    apps_per_minute,
    app_use_time_seconds,
    number_of_app_changes,
    most_common_category,
    most_common_app
]
APP_SQL_TYPES = {
    "most_common_category" : String(50),
    "number_of_unique_apps" : Integer,
    "apps_per_minute" : Float,
    "app_use_time_seconds" : Integer,
    "number_of_app_changes" : Integer,
    "most_common_app" : String(50)
}

# YSS --- 
# incompatible with python3
#def query_google(appid):
#
#    """
#    Querys the google play store to find the package category given a package name.
#    If the package can't be found on the store, return the string "None"
#    :param appid: A package name
#    :return: The google play package category
#    """
#
#    time.sleep(1)
#
#    url = "https://play.google.com/store/apps/details?id=" + appid
#    print "Finding {0}".format(appid)
#
#    try:
#        response = urllib2.urlopen(url)
#    except urllib2.HTTPError:
#        return "None"
#
#    soup = BeautifulSoup(response, 'html.parser')
#    text = soup.find('span', itemprop='genre')
#
#
#
#    if text:
#        return text.contents[0]
#    else:
#        return "NoneOrUtility"
# ---- YSS

# YSS ----
# this is bad bad practice. This violates the principle of separation of modules.
# why should the library *assume* any particular handling of data (i.e. based on
# database or files) in it?
# ---- YSS
def update_app_categories(conn):
    """Maintains a SQL table of app ids and their category. Due to Google rate limits,
    only one request can be made a second

    """

    #NEED TO AUTO CREATE

    to_update = pd.read_sql(GET_UN_UPDATED_IDS,conn)

    print("Adding {} package categories...".format(len(to_update["package_name"]))) # YSS
    to_update["package_category"] = to_update["package_name"].map(query_google)

    print("Saving to database...") # YSS
    to_update.to_sql(CATEGORY_TABLE,conn, index = False, if_exists="append")
    print("Done") # YSS

# YSS
def get_app_category(pkg_name:str) -> str:
    """\
    Returns the category of the app in play store given its package name pkg_name.
    """
    
    category = ''
    try:
        category = play_scraper.details(pkg_name)['category'][0]
    except:
        print("exception occurred in retrieving app category")

    return category

# YSS
def assign_category(pkg_name:str, app_categories:dict) -> str:
    """\
    Returns the category listed in app_categories for application with package name pkg_name.
    if no entry exists for a given application, requests it dynamically.
    """

    category = ''
    if pkg_name not in app_categories:
        print('no category listed for {}... requesting it'.format(pkg_name))
        category = get_app_category(pkg_name)
        app_categories[pkg_name] = category
    elif app_categories[pkg_name] == "None":
        category = ''
    else:
        category = app_categories[pkg_name]

    return category

# YSS
def categorize(app_pkg_file:str, app_category_file:str) -> None:
    """\
    Creates app categorozation file app_category_file listing apps and their categories
    under columns package_name and package_category. app_pks_file is a list of package names
    under column package_name
    """
    apps = pd.read_csv(app_pkg_file, header=0, encoding = "ISO-8859-1")
    apps['package_category'] = apps.apply(lambda x : get_app_category(x['package_name']), axis = 1)
    apps.to_csv(app_category_file, index=False, encoding = "ISO-8859-1", mode='w')