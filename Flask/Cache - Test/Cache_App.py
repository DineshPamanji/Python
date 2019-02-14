def install_and_import(package):
    import importlib
    try:
        importlib.import_module(package)
    except ImportError:
        from pip._internal import main as pipmain
        pipmain(['install', package])
    finally:
        globals()[package] = importlib.import_module(package)

results = map(install_and_import, ('flask', 'os', 'sys', 'tablib','pandas','json'
                                   , 'scipy', 'sklearn', 'sqlalchemy', 'multiprocessing'
                                   , 'flask_caching', 'redis'))
set(results)

# Install 'Flask-Cache'

import pandas as pd
import os
import sys
from flask import Flask, request, jsonify, render_template
from sqlalchemy import create_engine
from sqlalchemy.orm import scoped_session
from sqlalchemy.orm import sessionmaker
# from flask.ext.cache import Cache
from flask_caching import Cache


app = Flask(__name__)
# cache = Cache(app, config={'CACHE_TYPE': 'redis'})
cache = Cache(app, config={'CACHE_TYPE': 'simple'})
# cache = Cache(config={'CACHE_TYPE': 'simple'})
# cache.init_app(app)

# # define the cache config keys, remember that it can be done in a settings file
# app.config['CACHE_TYPE'] = 'simple'
#
# # register the cache instance and binds it on to your app
# app.cache = Cache(app)

# User defined Functions
def get_current_dir():
   return os.path.dirname(os.path.realpath(__file__))

# Change possible windows path to unix-like paths
def get_current_dir_unixy():
   return get_current_dir().replace('\\', '/')

def get_working_dir():
    working_dir = get_current_dir_unixy()
    if not sys.platform.startswith('win'):
        working_dir = working_dir + '/'
    return working_dir


working_dir = get_working_dir()
# exec(open('C:/Projects/FCA/Codes/DB/EmissionLogic/src/1_Inputs.txt').read()); working_dir = 'C:/Projects/FCA/Codes/DB/EmissionLogic/src/'
inputfile = open(working_dir+"/1_Inputs.txt")
exec(inputfile.read(), globals())

fcafile = open(working_dir + "/fca_co2_config.txt")
exec(fcafile.read(), globals())

sys.path.insert(0, working_dir)
from Initialize import *
from Compute_co2 import *
from Compute_co2_rerun import *
from Reporting_Linearize import *
from Reporting_Linearize_batch import *
from Reporting_Combination_Matrix import *
from Reporting_Combination_Matrix_batch import *

#============================== Main Code ===============================#

credentials_file = config_path + credentials_file_name
cred = pd.read_json(credentials_file)
connection_string = 'mssql+pymssql://' + cred['Credentials']['uid'] + ':' + cred['Credentials']['pwd'] + '@' + \
                    cred['Credentials']['server'] + '/' + cred['Credentials']['database']

global engine
engine = create_engine(connection_string)
session_factory = sessionmaker(bind=engine, autocommit=True)
Session = scoped_session(session_factory)


@cache.memoize(timeout=0)
def get_orders():
    product_orders = '[' + schema + '].[' + _PATH_ORDERS_REF + ']'
    query = "SELECT TOP 1000 %s FROM %s;" % (orders_cols, product_orders)
    session = Session()
    with session.connection() as conn:
        orders = pd.read_sql_query(query, conn)
        session.flush()
    return orders


@app.route("/ClearCache")
def ClearCache():
    cache.clear()
    orders = get_orders()
    return "Cache Cleared!"


@app.route('/CacheTest', methods=['POST'])
def CacheTest():
    json_input = request.json
    # x = pd.DataFrame.from_dict(json_input['mmvs'], orient='index').T
    # mmvs_df_opt = pd.DataFrame.from_dict(json_input['Take_rates'], orient='columns').set_index(['index'])
    orders = get_orders()
    print(len(orders))
    orders_test = orders.iloc[0:1, :]#orders[orders[_BRAND_FIELD] == "\'" + x[_BRAND_FIELD][0] + "\'"]
    return orders_test.to_json(orient='records')

print(__name__)
# if __name__ == "__main__":
    # app.run()
app.run(host='0.0.0.0', port=5000, debug=True)
