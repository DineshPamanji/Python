from multiprocessing import Pool
from time import sleep
from time import time

from copy import deepcopy


####################################################################################################################
####################################################
# Clustering
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.cluster import KMeans
from sklearn.metrics import silhouette_score
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

# Import helpers
from src.helpers import *
from src.DataPreparation import *
# import stopwords

# Load config
config = open("C:\Projects\OrgBuilder\Codes\Repo\TitleStandardization\src\Config.txt")
exec(config.read(), globals())

# Create df
df = create_df(path, file, _JOB_TITLE_COL, _DIV_COL, top10)

# Clean the text
df[_JOB_TITLE_COL + 'str'] = df[_JOB_TITLE_COL].astype(str)

# df = df[0:1000]

## Standard name generator

from difflib import SequenceMatcher
import numpy as np
import pandas as pd

def similarity_func(u, v):
    return SequenceMatcher(None, u, v).ratio()*100

def similarity_func_all(similarity_array, cust_names, i):
    # print(i)
    if i > 0:
        similarity_array[i][0:i] = np.vectorize(similarity_func)(cust_names[i], cust_names[0:i])
    # return np.array(np.vectorize(similarity_func)(cust_name, cust_names))
    # return np.array(similarity_array[i,:])
    return similarity_array[i, :]

def similarity_func_all_range(similarity_array, cust_names, k, l):
    # print(i)
    similarity_array_copy = deepcopy(similarity_array)
    r = []
    for i in range(k,l):
        if i > 0:
            similarity_array_copy[i][0:i] = np.vectorize(similarity_func)(cust_names[i], cust_names[0:i])
    # return np.array(np.vectorize(similarity_func)(cust_name, cust_names))
    # return np.array(similarity_array[i,:])
        r.append(similarity_array_copy[i,:])
    return r

def similarity_func_one_range(similarity_array, cust_names, i):
    # print(i)
    if i > 0:
        similarity_array[i][0:i] = np.vectorize(similarity_func)(cust_names[i], cust_names[0:i])
    # return np.array(np.vectorize(similarity_func)(cust_name, cust_names))
    return similarity_array[i,:]



df = clean_column(df, _JOB_TITLE_COL)

df_sub = df

cust_names = df_sub[_JOB_TITLE_COL].to_list()
similarity_array = np.ones((len(cust_names),(len(cust_names))), dtype='uint8')*100



print('Calculating........')
# import swifter
# df.reset_index(inplace=True)
#
# df_sub['key'] = 1
# df_sub_all = pd.merge(df_sub[[_JOB_TITLE_COL, 'index', 'key']], df_sub[[_JOB_TITLE_COL, 'index', 'key']], on = 'key', how='left')
# df_sub_all = df_sub_all[df_sub_all['index_x']>=df_sub_all['index_y']]
#
# # %%time
# # test = np.array(df['index'].swifter.apply(lambda x: similarity_func_all(similarity_array, cust_names, int(x))))
# %%time
# test = np.array(df_sub_all.swifter.allow_dask_on_strings(enable=True).apply(lambda x: similarity_func(x[_JOB_TITLE_COL+'_x'],
#                                                                    x[_JOB_TITLE_COL+'_y']), axis=1))

from joblib import Parallel, delayed

batch_num = 24
batch_size=len(cust_names)/batch_num

ti = time()
results=[]
from functools import partial
a = similarity_array
b = cust_names
func = partial(similarity_func_all_range, a, b)
results = Parallel(n_jobs=-2)(delayed(func)(int(i*batch_size),int((i+1)*batch_size)) for i in range(batch_num))
# func = partial(similarity_func_one_range, a, b)
# results = Parallel(n_jobs=-1)(delayed(func)(i for i in range(len(cust_names))))
duration = np.round(time() - ti, 4)
print(f"{batch_num} jobs computation: {duration} s")


final = []
for list1 in results:
    for sublist in list1:
        final.append(sublist)
array = pd.DataFrame(np.array(final))
array.to_csv("Similarity_array JOBLIB.csv", index=False)
print(similarity_array)


#
#
# if __name__ == '__main__':
#     # start 4 worker processes
#     with Pool() as pool:
#         from functools import partial
#         a = similarity_array
#         b = cust_names
#         func = partial(similarity_func_all, a, b)
#         start = time()
#         similarity_array = np.array(list(pool.map(func, np.array(range(0,len(cust_names))))))
#         similarity_array = pd.DataFrame(similarity_array)
#         similarity_array.to_csv('Similarity_array.csv', index=False)
#         print('Time taken : %s' % str(time()-start))
#         pool.close()
#         pool.join()
#
#
