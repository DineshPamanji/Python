import numpy as np
import re
from nltk.corpus import stopwords
import os
import pandas as pd
import numpy as np

from google_trans_new import google_translator
translator = google_translator()

# Cleaning

# STOPWORDS = set(stopwords.get_stopwords('english'))
STOPWORDS = set(stopwords.words('english'))

path = "C:\\Projects\\OrgBuilder\\Codes\\DataServices\\DataServices"#os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
print(path)
replace_file ='/outputs/Short_Forms_Dictionary_KA.xlsx'
replace_dict = pd.read_excel(path+replace_file)

descriptor_cols = replace_dict.columns.tolist()[2:] #['Company title descriptor', 'Seniority descriptor', 'Division descriptor']
# fill missing values and create a copy
replace_dict[descriptor_cols] = replace_dict[descriptor_cols].fillna('Missing')
dictionary_df = replace_dict.copy()

# create a dictionary
replace_dict = {s: a for s, a in zip(replace_dict['Short'], replace_dict['Actual'])}

def clean_text(text, use_dict=1, keys=[], values=[]):
    """
    function to clean text of symbols, spaces, capitals etc.

    Parameters
    ----------
    text: a string

    Returns
    ----------
    text: modified initial string
    """
    REPLACE_BY_SPACE_RE = re.compile('[/(){}\[\]\|@,;&-]')
    BAD_SYMBOLS_RE = re.compile('[^0-9a-z #+_]')
    text = re.sub(r'([a-z](?=[A-Z])|[A-Z](?=[A-Z][a-z]))', r'\1 ', text)

    text = text.lower()  # lowercase text
    text = REPLACE_BY_SPACE_RE.sub(' ',
                                   text)  # replace REPLACE_BY_SPACE_RE symbols by space in text. substitute the matched string in REPLACE_BY_SPACE_RE with space.
    text = BAD_SYMBOLS_RE.sub('',
                              text)  # remove symbols which are in BAD_SYMBOLS_RE from text. substitute the matched string in BAD_SYMBOLS_RE with nothing.
    if use_dict == 1:
        keys = list(replace_dict.keys())
        values = list(replace_dict.values())
        for key_term in set(text.split(" ")).intersection(keys):
            n = keys.index(key_term)
            text = re.sub(r"\b%s\b" % keys[n], values[n], text)

    # Remove additional spaces
    text = re.sub(' +', ' ', text)

    return text



# reduce_noise(clean_text("Head Compliance OpsRep & Svcs", 1), column=descriptor_cols[1])
# text = 'ATM Support Supervisor (TT)'
# text = reduce_noise(clean_text("supv aml bsa compliance dru", 1), column=descriptor_cols[0])
# text = reduce_noise(text, column=descriptor_cols[0])
# reduce_noise(text, column=descriptor_cols[0])

# %%time
# clean_text("ALLIANCE TRUST BDO III", 1)


def clean_column(inputdf, colname, use_dict=1):
    """
    Clean the column - remove caps, stop words etc.

    Parameters
    ----------
    df: pandas, Data frame with column to be cleaned
    col: str, Name of the column

    Returns
    --------
    df: pandas, data frame with column cleaned
    """
    # print(f'Execution Started: {col}')
    df = inputdf.copy()
    col = colname
    df[col] = df[col].str.replace("-", ' ')
    # batch_num = 10000
    # batch_size = len(df[col]) / batch_num
    # func = partial(clean_sub_func, df, col)
    # results = Parallel(n_jobs=-2)(delayed(func)(int(i*batch_size), int((i+1)*batch_size)) for i in range(batch_num))
    # final = []
    # for list1 in results:
    #     for sublist in list1:
    #         final.append(sublist)
    # df[col] = final
    df[col] = df[col].apply(clean_text, use_dict=use_dict)
    df[col] = df[col].str.replace('\d+', '')
    return df

def extract_descriptors(dictionary_df, column):
    list_of_words = list(set(dictionary_df[dictionary_df[column] != 'Missing']['Actual'].tolist()))
    return list_of_words

def reduce_noise(text, column, extract_descriptors, dictionary_df, MINWORDS=2):
    words_list = extract_descriptors(dictionary_df, column)
    if set(text.split(" ")).intersection(words_list):
        updated_text = ' '.join(word for word in text.split() if word not in words_list)

        if len(updated_text.split(' ')) >= MINWORDS:
            return updated_text, list(list(set(text.split(' ')) - set(updated_text.split(' ')) - set([''])))
        else:
            return text, list(list(set(text.split(' ')) - set(updated_text.split(' ')) - set([''])))
    else:
        return text, list()

def call_reduce_noise(extract_descriptors, reduce_noise, dictionary_df, df, colname, args_dict, descriptor_col_num=None):
    # global df_cleaned
    # print(descriptor_col_num)
    descriptor_cols = args_dict[descriptor_col_num]
    # col = 'Cleaned_'+'_'.join(descriptor_cols).strip('descriptor')
    col = 'Cleaned_' + str(descriptor_col_num)
    if len(descriptor_cols) > 1:
        args = list(args_dict.values())
        cleaned_col = 'Cleaned_' + str(list(args_dict.keys())[args.index([descriptor_cols[0]])])
        df[col] = df[cleaned_col]
    else:
        df[col] = df[colname]
    for n in range(len(descriptor_cols)):
        df[col], df[descriptor_cols[n]] = df[col].apply(reduce_noise, column=descriptor_cols[n]
                                                        , extract_descriptors=extract_descriptors
                                                        , dictionary_df=dictionary_df).str
    if len(descriptor_cols) == 1:
        # df_cleaned = pd.concat([df[[col,descriptor_cols[n]]], df_cleaned], axis=1)
        return df[[col, descriptor_cols[n]]]
    else:
        # df_cleaned = pd.concat([df[[col]], df_cleaned], axis=1)
        return df[[col]]

    # if descriptor_col_num == max(args_dict.keys()):
    #     global df_cleaned
    #     df_cleaned = df_new.copy()


df_new = pd.read_excel(path+'/outputs//HRDataAnonModelOutput_test.xlsx', nrows=100)
df_new = df_new[['data_set_id', 'position_id', 'Title_2_1']]
df_new['CleanedTitle'] = df_new['Title_2_1']

import time
import itertools
import multiprocessing as mp
from functools import partial
from joblib import Parallel, delayed

args = []
for d in range(1, len(descriptor_cols)+1):
    for i in itertools.combinations(descriptor_cols, d):
        args.append(list(i))



# Clean column
df_new = clean_column(df_new, 'CleanedTitle')
args_dict = dict(zip(range(len(args)), args))
func = partial(call_reduce_noise, extract_descriptors, reduce_noise, dictionary_df, df_new, 'CleanedTitle', args_dict)

threading_yes = 0
if threading_yes == 1:
    a = time.time()
    import queue
    import threading

    def call_threading(func, args):
        # que = queue.Queue()
        threads_list = list()
        for i in range(len(args)):
            print(args[i])
            T1 = threading.Thread(target=func, args=[i])
            # T1 = threading.Thread(target=lambda q, arg1: q.put(func(arg1)), args=(que, [i]))
            T1.start()
            threads_list.append(T1)

        # Join all the threads
        for t in threads_list:
            t.join()

    call_threading(func, args)
    print(time.time() - a)
else:
    print('Started multiprocessing')
    a = time.time()
    # for i in range(len(args)):
    #     call_reduce_noise(df_new, 'CleanedTitle', args_dict, i)
    results = []
    for i in range(len(args)):
        results_sub = func(i)
        results.append(results_sub)
    # import multiprocess as mp
    # # import multiprocessing as mp
    # with mp.Pool(mp.cpu_count() - 1) as pool:
    #     results = pool.map(func, list(args_dict.keys()))
    # results_df = pd.concat(results, axis=1)
    # results_df = pd.concat([df_new, results_df], axis=1)
    # print(time.time() - a)

# print(len(df_cleaned))
a = time.time()

results_df = df_new
# cleaned_columns = [col for col in df_cleaned.columns if 'Cleaned' in col]
old_Calc = 0
cleaned_columns = [col for col in results_df.columns if 'Cleaned' in col]
if old_Calc:
    cleaned_columns = ['CleanedTitle']
job_title_col_new = 'UniqueTitles'

unique_titles = pd.DataFrame()
for col in cleaned_columns:
    # col = 'CleanedTitle'
    temp_df = results_df[['position_id', col]]
    temp_df.columns = ['position_id', job_title_col_new]
    temp_df['ColumnName'] = col
    if len(unique_titles):
        unique_titles = unique_titles.append(temp_df)
    else:
        unique_titles = temp_df

unique_titles.reset_index(inplace=True, drop=True)
unique_titles.drop_duplicates(subset=job_title_col_new, inplace=True)
# unique_titles = np.unique(results_df[cleaned_columns].values)
# unique_titles = pd.DataFrame(unique_titles)
# unique_titles.columns = ['UniqueTitles']

# Load model
from gensim.models import FastText
model_path_fS3 = path+'/outputs/FastText_EMSI_MODEL.model'
model_standard = FastText.load(model_path_fS3)

unique_titles[job_title_col_new + 'new'] = unique_titles['UniqueTitles']
unique_titles[job_title_col_new + 'new'] = ['_'.join(x.split()) for x in unique_titles[job_title_col_new + 'new']]

print('Check if the title is already present in the vocabulary of model')
unique_titles['Mapped_Title'] = [x if x in model_standard.wv.vocab else 'Missing' for x in unique_titles[job_title_col_new + 'new']]

similar_title_field = 'title_similar'
similar_score_field = 'similarity_score'
title_clean_field = 'title_clean'

a1 = time.time()
unique_titles['Similar'] = [list(model_standard.wv.most_similar(x, topn=1)[0]) for x in
                     unique_titles[job_title_col_new + 'new']]
print(time.time() - a1)

# a = time.time()
#
# # def predict_similar_sub(predict_similar, df, col, model_standard, i, j):
# #     df_sub = df.iloc[i:j, :]
# #     results_sub = df_sub[col].apply(predict_similar,model_standard=model_standard)
# #     return list(results_sub)
# #
# # batch_num = 4
# # batch_size = len(unique_titles[job_title_col_new + 'new']) / batch_num
# # ranges = [(int(i*batch_size), int((i+1)*batch_size)) for i in range(batch_num)]
# # func1 = partial(predict_similar_sub, predict_similar, unique_titles, job_title_col_new + 'new', model_standard)
# # with mp.Pool(mp.cpu_count() - 1) as pool:
# #     results = pool.starmap(func1, ranges)
# # unique_titles[similar_title_field], unique_titles[similar_score_field] = np.vectorize(predict_similar)(unique_titles[job_title_col_new + 'new'].to_numpy(), model_standard=model_standard)
# unique_titles['Similar1'] = unique_titles[job_title_col_new + 'new'].apply(predict_similar, model_standard=model_standard)
# print(time.time() - a)


unique_titles[[similar_title_field, similar_score_field]] = pd.DataFrame(unique_titles.Similar.tolist(), index=unique_titles.index)

mapped_available_idx = unique_titles['Mapped_Title'] != 'Missing'
unique_titles.loc[mapped_available_idx, similar_title_field] = unique_titles.loc[mapped_available_idx, 'Mapped_Title']
unique_titles.loc[mapped_available_idx, similar_score_field] = 1

final_df = unique_titles.sort_values(similar_score_field, ascending=False).drop_duplicates(['position_id'])
final_df.rename(columns={job_title_col_new + 'new':title_clean_field}, inplace=True)

print(f'Average score: {final_df[similar_score_field].mean()}')
print(time.time() - a)

print(f'100% rows : {len(final_df[final_df[similar_score_field] ==1])}')



# results_df_final = pd.merge(results_df, final_df, )

# results_df.to_excel(path + f'/outputs/HC_Master_Cleaned_Noise_v1_0216_threading_{str(threading_yes)}.xlsx', index=False)
words_list = unique_titles[job_title_col_new + 'new'].tolist()
words_list1 = ['data_scientist', 'data_analyst', 'artificial_intelligence', 'machine_learning', 'senior_data_scientist', 'manager',
 'analyst', 'chief_executive_officer', 'finance_analyst', 'product_manager', 'chief_information_officer', 'banking_manager']


from sklearn.decomposition import PCA
import matplotlib.pyplot as plt

def plot_words(model, words_list):
    word_vectors = model.wv[words_list]
    pca = PCA(n_components=2)

    p_comps = pca.fit_transform(word_vectors)
    word_names = words_list


    plt.figure(figsize=(18, 10))
    plt.scatter(p_comps[:, 0], p_comps[:, 1], c='red')

    for word_name, x, y in zip(word_names, p_comps[:, 0], p_comps[:, 1]):
        # print(word_name, x, y)
        plt.annotate(word_name, xy=(x+0.0001, y+0.0001), xytext=(0, 0), textcoords='offset points')
        # plt.annotate(word_name, (x,y))

plot_words(model_standard, words_list1)


#
# # Check thread's return value
# while not que.empty():
#     result = que.get()
#
# if __name__ == '__main__':
#     a = time.time()
#     func = partial(clean_column, df_new, 'CleanedTitle')
#     func(args[0])
#     with mp.Pool(processes=(mp.cpu_count() - 1)) as pool:
#         results = pool.map(func, args)
#     pool.close()
#     pool.join()
#     # results =  Parallel(n_jobs=-1)(delayed(func)(args))
#
#     results_df = pd.concat(results, axis=1)
#     results_df = pd.concat([df_new, results_df], axis=1)
#     results_df.to_excel(path + '/outputs/HC_Master_Cleaned_Noise_v1_0215.xlsx', index=False)
#     print(time.time() - a)


