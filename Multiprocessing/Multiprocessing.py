from multiprocessing import Pool
import pandas as pd
from sqlalchemy import create_engine
import logging
from datetime import datetime
import itertools

# def f(x):
#     return x*x

# ### Set up log file
logger = logging.getLogger(__name__)
logger.setLevel(logging.INFO)

# create a file handler
handler = logging.FileHandler('Log_file_'+str(datetime.now().strftime('%Y-%m-%d-%H%M%S'))+'.log')
handler.setLevel(logging.INFO)

# create a logging format
formatter = logging.Formatter('%(asctime)s - %(name)s - %(levelname)s - %(message)s')
handler.setFormatter(formatter)

# add the handlers to the logger
logger.addHandler(handler)

logger.info('Start')

# def multi_run_wrapper(args):
#    return read_db(*args)

def read_db(table_name, connection_string, logger , x, schema=None, cols=None):
    con = create_engine(connection_string)
    logger.info('Reading: %s' % str(x))
    table = pd.read_sql_table(table_name, con, columns=cols, schema=schema, coerce_float=False)
    # table = pd.read_csv('C:/Projects/FCA/Codes/DB/EmissionLogic/Test %s.csv' % x, index_col=None)
    logger.info('Saving: %s' % str(x))
    table.to_csv('C:/Projects/FCA/Codes/DB/EmissionLogic/Test %s.csv' % x, index=False)
    return print(table.ix[[x]])#.to_json(orient='records')

connection_string = 'mssql://eur_uat_db_01.msppricingcatalyst-uat.bcgtools.com/Auto1?driver=SQL+Server'
#
logger.info('Multiprocessing : START')
if __name__ == '__main__':
    p = Pool(processes=3)
    p.map_async(read_db, zip(itertools.repeat("z_py_test_options_volume"), itertools.repeat(connection_string)
                                  , itertools.repeat(logger), [1, 2, 3]))
    p.close()
    p.join()


# out_dict = results
# print(results)
    # p.starmap(read_db, [('z_py_test_options_volume',connection_string, 1)
    #     , ('z_py_test_options_volume', connection_string, 2)
    #     , ('z_py_test_options_volume', connection_string, 3)])
    # p.map(multi_run_wrapper,[('z_py_test_options_volume',connection_string, 1)
    #     , ('z_py_test_options_volume', connection_string, 2)
    #     , ('z_py_test_options_volume', connection_string, 3)])

    # M = pool.starmap(func, zip(a_args, repeat(second_arg)))
    # set(map(f, [1, 2, 3]))

logger.info('Multiprocessing : END')

# #create a list of threads
# threads = []
# # In this case 'urls' is a list of urls to be crawled.
# for ii in range(len(urls)):
#     # We start one thread per url present.
#     process = Thread(target=crawl, args=[urls[ii], result, ii])
#     process.start()
#     threads.append(process)
# # We now pause execution on the main thread by 'joining' all of our started threads.
# # This ensures that each has finished processing the urls.
# for process in threads:
#     process.join()

#### MULTI THREADING ####
# import multiprocessing
# from multiprocessing.dummy import Pool as ThreadPool
# import itertools
#
#
# multiprocessing.cpu_count()
# # make the Pool of workers
# pool = ThreadPool(4)
#
# # open the urls in their own threads
# # and return the results
# results = pool.starmap(read_db, zip(itertools.repeat("z_py_test_options_volume"), itertools.repeat(connection_string)
#                                     ,itertools.repeat(logger), [1, 2, 3, 4]))
#
#
# # close the pool and wait for the work to finish
# pool.close()
# pool.join()
# print(pd.DataFrame(results))
#
# # for i in range(1,5):
# #     read_db("z_py_test_options_volume", connection_string, logger, i)

# logger.info('Multiprocessing : END')

