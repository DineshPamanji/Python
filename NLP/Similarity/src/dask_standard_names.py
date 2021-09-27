from dask.distributed import Client, progress
from dask import delayed
import dask.dataframe as dd
from functools import partial
import dask.array as da
import dask
import joblib

client = Client(processes=False, threads_per_worker=6,
                n_workers=10, memory_limit='400GB')
# client = Client("tcp://192.168.0.4:8786")
client

import dask_ml.cluster

# Import helpers
from src.helpers import *

from difflib import SequenceMatcher
import numpy as np
import pandas as pd

# Load config
config = open("C:\Projects\OrgBuilder\Codes\Repo\TitleStandardization\src\Config.txt")
exec(config.read(), globals())


def similarity_func(u, v):
    return SequenceMatcher(None, u, v).ratio()*100

def similarity_func_calc(similarity_array, cust_names, i, j):
    if i > 0:
        similarity_array[i][j] = similarity_func(cust_names[i], cust_names[j])
    return similarity_array[i][j]


def similarity_func_all(similarity_array, cust_names, i):
    print(i)
    if i > 0:
        similarity_array[i][0:i] = np.vectorize(similarity_func)(cust_names[i], cust_names[0:i])
    # return np.array(np.vectorize(similarity_func)(cust_name, cust_names))
    # return np.array(similarity_array[i,:])
    return similarity_array[i,:]


# Create df
print('Reading File')
file =  "Input/FullStandardTitles/Emsi_Titles.csv"
df = dd.read_csv(path+file)

# Add key and index
df['key'] = 1
df = df.reset_index().compute()

# Merge - cross join
df_sub_all = dd.merge(df[[_JOB_TITLE_COL, 'index', 'key']], df[[_JOB_TITLE_COL, 'index', 'key']], on='key', how='left')

# Keep only the lower triangle -  i >= j
df_all = df_sub_all[df_sub_all['index_x']>=df_sub_all['index_y']]

%%time
df_all['Similarity'] = df_all.apply(lambda x: similarity_func(x[_JOB_TITLE_COL+'_x']
                                                                       , x[_JOB_TITLE_COL+'_y']), axis=1)

%%time
df_all['Similarity'] = df_all.map_partitions(lambda x: similarity_func(x[_JOB_TITLE_COL+'_x']
                                                                       , x[_JOB_TITLE_COL+'_y'])).compute(
                                                                    scheduler='processes')

client.close()



cust_names = df[_JOB_TITLE_COL]
print('Similarity Array')
similarity_array = da.from_array(np.ones((len(cust_names),(len(cust_names))), dtype='uint8')*100)

len(cust_names)

print('Delayed functions')
results = []
for i in range(len(cust_names)):
    # for j in range(i):
    #     y = delayed(similarity_func_calc)(similarity_array, cust_names, i, j)
    y = delayed(similarity_func_all)(similarity_array, cust_names, i)
    # y = similarity_func_all(similarity_array, cust_names, i)
    results.append(y)

print('Calculating....')
# %%time
array = np.array(dask.compute(*results, scheduler='distributed'))

pd.DataFrame(array).to_csv('Similarity_array DASK.csv', index=False)

#
# something = dask.delayed(similarity_func_calc)
#
# # this could be written as a one-line comprehension
# computations = []
# for i in range(len(cust_names)):
#     part = []
#     computations.append(part)
#     for j in range(i):
#         part.append(something(similarity_array, cust_names, i, j))
#
# results = dask.compute(*computations, scheduler='distributed',
#                        num_workers=8)