
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


## Standard name generator

from difflib import SequenceMatcher
import collections
from fuzzywuzzy import fuzz
from sklearn import cluster
import numpy as np
import pandas as pd

def similarity_func(u, v):
    return SequenceMatcher(None, u, v).ratio()*100

def similarity_func_all(i, similarity_array, cust_names):
    # print(i)
    if i > 0:
        similarity_array[i][0:i] = np.vectorize(similarity_func)(cust_names[i], cust_names[0:i])
    # return np.array(np.vectorize(similarity_func)(cust_name, cust_names))
    # return np.array(similarity_array[i,:])
    return similarity_array[i,:]


def fuzz_similarity(cust_names):
    # similarity_array = np.ones((len(cust_names),(len(cust_names))), dtype='uint8')*100
    similarity_array = np.ones((10,10), dtype='uint8')*100
    similarity_array = np.array(array) #np.ones((len(cust_names),(len(cust_names))), dtype='uint8')*100
    # similarity_array = np.array(array)[0:10, 0:10]
    # np.vectorize(similarity_func_all_upper, excluded=['similarity_array', 'cust_names'])(np.array(range(len(cust_names)))
    #                                                              ,  similarity_array=similarity_array
    #                                                              , cust_names=cust_names)

    # func = lambda x: similarity_func_all(x, similarity_array=similarity_array, cust_names=cust_names)
    # similarity_array = np.array([func(i) for i in np.array(range(0,len(cust_names)))])

    # for i in range(1,len(cust_names)):
    #     print(str(i))
    #     # similarity_array[i][0:i] = np.vectorize(similarity_func)(cust_names[i], cust_names[0:i])
    #     for j in range(i):
    #         s1 = fuzz.token_set_ratio(cust_names[i],cust_names[j]) + 0.000000000001
    #         s2 = fuzz.partial_ratio(cust_names[i],cust_names[j]) + 0.00000000001
    #         similarity_array[i][j] = 2*s1*s2 / (s1+s2)
    #         # similarity_array[i][j] = SequenceMatcher(None, cust_names[i], cust_names[j]).ratio()*100
    for i in range(len(cust_names)):
        print(i)
        similarity_array[i, i + 1:len(cust_names)] = similarity_array[i + 1:len(cust_names), i]
        # for j in range(i+1,len(cust_names)):
        #     similarity_array[i][j] = similarity_array[j][i]

    np.fill_diagonal(similarity_array, 100)
    return similarity_array


def title_clusters(data, col = _JOB_TITLE_COL):
    data_clean = clean_column(data, col)
    try:
        del data_clean['Cluster']
    except:
        print('Nothing')
    # data_clean = data
    cust_names = np.array(data_clean[_JOB_TITLE_COL].to_list())
    try:
        cust_ids = data_clean[_EMP_ID_COL].to_list()
    except:
        cust_ids = list(range(len(data_clean[_JOB_TITLE_COL])))
        data_clean[_EMP_ID_COL] = cust_ids

    print('Calculating fuzz similarity')
    # similarity_array = fuzz_similarity(cust_names)
    # print(similarity_array)
    # print(len(similarity_array))

    print('Clustering the titles')
    clusters = cluster.AffinityPropagation(affinity='precomputed').fit_predict(similarity_array)
    # clusters = cluster.AffinityPropagation(affinity='euclidean').fit_predict(X[0:1000])

    df_clusters = pd.DataFrame(list(zip(cust_ids, clusters)), columns=[_EMP_ID_COL,'Cluster'])

    df_eval = df_clusters.merge(data_clean, on=_EMP_ID_COL, how='left')
    return df_eval



def standard_name(df_eval):
    # df_eval = df_new_13[0:10]
    d_standard_name = {}
    for Cluster in df_eval.Cluster.unique():
        # cluster = 13
        print('Cluster: %s' % str(Cluster))
        names = df_eval[df_eval['Cluster'] == Cluster][_JOB_TITLE_COL].to_list()
        l_common_substring = []
        if len(names) > 1:
            for i in range(0, len(names)):
                for j in range(i+1, len(names)):
                    seqMatch = SequenceMatcher(None, names[i], names[j])
                    match = seqMatch.find_longest_match(0, len(names[i]), 0, len(names[j]))
                    if match.size != 0:
                        l_common_substring.append(names[i][match.a: match.a + match.size].strip())
                n = len(l_common_substring)
                counts = collections.Counter(l_common_substring)
                get_mode = dict(counts)
                mode = [k for k, v in get_mode.items() if v == max(list(counts.values()))]
                d_standard_name[Cluster] = ';'.join(mode)
        else:
            d_standard_name[Cluster] = names[0]
    df_standard_names = pd.DataFrame(list(d_standard_name.items()), columns=['Cluster', 'StandardName'])
    df_eval = df_eval.merge(df_standard_names, on='Cluster', how='left')
    df_eval['Score_with_standard'] = df_eval.apply(lambda x: fuzz.token_set_ratio(x['StandardName'], x[_JOB_TITLE_COL])
                                                   , axis=1)
    df_eval['standard_name_withoutSpaces'] = df_eval.StandardName.apply(lambda x: x.replace(" ", ""))
    for name in df_eval.standard_name_withoutSpaces.unique():
        if len(df_eval[df_eval.standard_name_withoutSpaces == name].Cluster.unique()) > 1:
            df_eval.loc[df_eval.standard_name_withoutSpaces == name, 'StandardName'] = name

    return df_eval.drop('standard_name_withoutSpaces', axis=1)



df_test = copy(df)

# test = clean_column(df_test[0:10], _JOB_TITLE_COL)
from joblib import Parallel, delayed


%%time
df_eval = title_clusters(df)
df_new_std = standard_name(df_eval)
# df_new_std = standard_name(df)

# Save output
df_new_std.to_excel(path+'/Output/EMSI Cluster wStandardNames 0820.xlsx', index=False)


df_new_std = df_new_std[['Cluster', _JOB_TITLE_COL, 'StandardName']]


# import pandas as pd
# from scipy.spatial.distance import euclidean, pdist, squareform
#
#


cust_names = df[_JOB_TITLE_COL + 'str'][0:10].to_list()
similarity_array = np.ones((len(cust_names), (len(cust_names))), dtype='uint8') * 100
similarity_array[0,:] = np.vectorize(similarity_func)(cust_names[0], cust_names)

# DF_var = pd.DataFrame.from_dict({"s1":[1.2,3.4,10.2],"s2":[1.4,3.1,10.7],"s3":[2.1,3.7,11.3],"s4":[1.5,3.2,10.9]})
# DF_var.index = ["g1","g2","g3"]
#
# dists = pdist(df['job'], similarity_func)
# DF_matrix= pd.DataFrame(squareform(dists), columns=DF_var.index, index=DF_var.index)


