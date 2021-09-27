FT = 1
FT_train = 1
WV_train = 0
from keras.utils.np_utils import to_categorical
from keras.callbacks import EarlyStopping
from nltk import word_tokenize
import pandas as pd
import numpy as np
import os
import time
import glob
from copy import copy
from datetime import datetime

# Import helpers
from src.helpers import *
from src.DataPreparation import *
from src.NN_helpers import *
# import stopwords

# Load config
config = open("src/Config.txt")
exec(config.read(), globals())

# Create df
df_sub = create_df(path, file, _JOB_TITLE_COL, _DIV_COL, top10)

for i in range(10):
    df_sub = df_sub.append(df_sub)

if FT == 0:
    # Clean text
    df_sub = clean_column(df_sub, _JOB_TITLE_COL)

    # Fit tokenizer
    tokenizer = prepare_tokenizer(df_sub, _JOB_TITLE_COL, MAX_NB_WORDS)

    # Prepare train test
    _DIV_COL = 'dummy'
    df_sub[_DIV_COL] = 'test'
    X, Y, X_train, X_test, Y_train, Y_test = get_train_test(tokenizer, df_sub, _JOB_TITLE_COL, _DIV_COL, MAX_SEQUENCE_LENGTH)

    # Get label count
    labels_count = Y_test.shape[1]

    # Create a basic model instance
    model = create_model(X, labels_count, MAX_NB_WORDS, EMBEDDING_DIM)

    # Fit the model
    history = model.fit(X_train, Y_train, epochs=epochs, batch_size=batch_size,validation_split=0.2
                        , callbacks=[EarlyStopping(monitor='val_loss', patience=3, min_delta=0.0001)])
    # history = model.fit(X_sm, Y_sm, epochs=epochs, batch_size=batch_size,validation_split=0.1
    #                     # ,class_weight=class_weight
    #                     ,callbacks=[EarlyStopping(monitor='val_loss', patience=3, min_delta=0.0001)])

    ## Save model
    model.save('Model/Model_EMSI.h5')
    #
    # ## Load model
    # from keras.models import load_model
    # model = load_model('Model/Model_Standard.h5')

    # Check accuracy
    accr = model.evaluate(X_test, Y_test)
    print('Test set\n  Loss: {:0.3f}\n  Accuracy: {:0.3f}'.format(accr[0],accr[1]))

    accr = model.evaluate(X_train, Y_train)
    print('Test set\n  Loss: {:0.3f}\n  Accuracy: {:0.3f}'.format(accr[0],accr[1]))

    # Get labels
    labels = pd.get_dummies(df_sub[_DIV_COL]).columns.tolist()

    # Confusion matrix
    plot_confusion(model, X_test, df_sub, labels, Y_test)

    ###############################################################################################################
    ###############################################################################################################
    ## Predict on new data
    df_new = pd.read_excel(new_file_path)

    # Get layers column
    df_new_layers = get_layer_titles_concat(df_new, _EMP_ID_COL, _JOB_Title_COL_New, _JOB_TITLE_COL, additional_columns)
    # df_new_layers[_JOB_TITLE_COL] = df_new[_JOB_Title_COL_New]

    # Clean
    df_new_layers = clean_column(df_new_layers, _JOB_TITLE_COL)

    # Transform the variables
    X_new = prepare_X(tokenizer, df_new_layers, _JOB_TITLE_COL, MAX_SEQUENCE_LENGTH)

    # Predict and get probabilities
    df_new = get_predictions(model, X_new, labels, df_new)

    # Plot predict vs LOB
    plot_prediction_distribution(df_new, 'Predict', additional_columns[0])

    # Save output
    df_new.to_excel('C:/Projects/OrgBuilder/Codes/Repo/TitleStandardization\Output\TD Bank Predicted NN 0617 EMSI.xlsx', index=False)



#########################################################################################################
############################ SIMILAR TITLES USING FAST TEXT MODEL #######################################
#########################################################################################################

if FT == 1:
    from gensim.models import FastText
    # from gensim.models import Word2Vec
    # from tqdm import tqdm
    # import phonetics
    from scipy.stats import mode

    _JOB_TITLE_COL = _DIV_COL
    df_sub_bkp = copy(df_sub)
    df_sub.shape

    # Drop duplicates
    df_sub.drop_duplicates(subset=_JOB_TITLE_COL, inplace=True)
    df_sub[_JOB_TITLE_COL + 'str'] = df_sub[_JOB_TITLE_COL].astype(str)   #+ '|' + df_sub['Function_Extracted'].astype(str)

    # Clean the text
    %%time
    df_sub = clean_column(df_sub, _JOB_TITLE_COL+'str')

    # Replace spaces
    df_sub[_JOB_TITLE_COL + 'str'] = ['_'.join(x.split()) for x in df_sub[_JOB_TITLE_COL+'str']]
    df_sub.drop_duplicates(subset=_JOB_TITLE_COL + 'str', inplace=True)

    ### MODEL TRAINING
    ## Use sg=1 to use skip gram, train the model with job title+occupation
    if FT_train == 1:
        model_ted = FastText(df_sub[_JOB_TITLE_COL + 'str'].str.split().values.tolist(), size=100, window=5, min_count=1
                                                                    , workers=4, sg=0)
        # # Load pre-trained models
        # from gensim.models.fasttext import load_facebook_model
        # from gensim.models.wrappers import FastText
        # # from gensim.models import KeyedVectors
        # wv = load_facebook_model('C:/Projects/OrgBuilder/Data/crawl-300d-2M-subword/crawl-300d-2M-subword.bin')
        # # wv = KeyedVectors.load_word2vec_format('C:/Projects/OrgBuilder/Data/wiki-news-300d-1M-subword.vec/wiki-news-300d-1M-subword.vec')
        # # wv = KeyedVectors.load_word2vec_format('C:/Projects/OrgBuilder/Data/wiki.en.vec')
        # #
        # print(wv.most_similar('artificial'))

        # import difflib
        #
        # words = ['hello', 'Hallo', 'hi', 'house', 'key', 'screen', 'hallo', 'question', 'format']
        # words = df_sub[_JOB_TITLE_COL + 'str'].values
        # difflib.get_close_matches('consumer_gwim risk', words)

        # Save the model
        model_ted.save('Model/FastText_BGT_0820.model')

    elif WV_train == 1:
        model = Word2Vec(window=10, sg=1, hs=0,
                         negative=10,  # for negative sampling
                         alpha=0.03, min_alpha=0.0007,
                         seed=14)

        model.build_vocab(df_sub[_JOB_TITLE_COL + 'str'].str.split().values.tolist(), progress_per=200)

        model.train(df_sub[_JOB_TITLE_COL + 'str'].str.split().values.tolist(), total_examples=model.corpus_count,
                    epochs=10, report_delay=1)

        # save word2vec model
        model.save("word2vec_2.model")

    if FT_train == 0:
        model_ted = FastText.load('Model/FastText_BGT_0820.model')
        # model_ted = FastText.load('C:/Projects/OrgBuilder/Codes/DataServices/DataServices/outputs/FastText_EMSI_09032020.model')
        model_ted.wv.most_similar('', topn=1)

    # New data
    df_new = pd.read_excel(new_file_path, usecols=[_EMP_ID_COL, _JOB_Title_COL_New])
    # df_new = pd.read_excel('C:/Egnyte/Shared/PDT/Omnia Data/Business/New Initiatives/OrgBuilder/Data/'
    #                        'FI Sanitized Data-Sets/BOA Full Org - 8.29.17 Project Baseline.xlsx', usecols=[_EMP_ID_COL,
    #                        _JOB_Title_COL_New])
    df_new_bkp = copy(df_new)
    df_new = copy(df_new_bkp)
    df_new.head()

    # Concatenate columns
    try:
        for i in range(len(additional_columns)):
            df_new[additional_columns[i]].fillna(' ', inplace=True)
            df_new[_JOB_Title_COL_New] = df_new[additional_columns[i]].astype(str) + ' ' + df_new[_JOB_Title_COL_New].astype(str)
    except:
        print('No additional columns to concatenate')

    # Clean columns
    df_new[_JOB_Title_COL_New] = df_new[_JOB_Title_COL_New].astype(str)
    df_new = clean_column(df_new, _JOB_Title_COL_New)

    df_new[_JOB_Title_COL_New + 'str1'] = ['_'.join(x.split()) for x in df_new[_JOB_Title_COL_New]]

    # Check if the title is already present in the vocabulary of model
    df_new['Mapped_Title'] = [x if x in model_ted.wv.vocab else 'Missing' for x in df_new[_JOB_Title_COL_New+'str1']]

    # Get similar title
    df_new['Similar'] = [model_ted.wv.most_similar(x, topn=1)[0] for x in df_new[_JOB_Title_COL_New+'str1']]
    df_new[['Similar_Title', 'Similar_Score']] = pd.DataFrame(df_new.Similar.tolist(), index=df_new.index)
    df_check = df_new[[_EMP_ID_COL, _JOB_Title_COL_New, 'Similar_Title', 'Similar_Score', 'Mapped_Title']]
    df_check.columns = [_EMP_ID_COL, _JOB_Title_COL_New+'_wReplace', 'Similar_Title', 'Similar_Score', 'Mapped_Title']

    # Use title if exactly matches
    mapped_available_idx = df_check['Mapped_Title'] != 'Missing'
    df_check['Similar_Title'][mapped_available_idx] = df_check['Mapped_Title'][mapped_available_idx]
    df_check['Similar_Score'][mapped_available_idx] = 1
    del df_check['Mapped_Title']

    # Merge with actual title
    df_check = pd.merge(df_new_bkp[[_EMP_ID_COL, _JOB_Title_COL_New]], df_check, how='left')
    df_check = pd.merge(df_check, df_sub[[_JOB_TITLE_COL, _JOB_TITLE_COL+'str']], left_on='Similar_Title'
                        , right_on=_JOB_TITLE_COL+'str', how='left')
    del df_check[_JOB_TITLE_COL+'str']


    # df_check.to_excel('C:/Projects/OrgBuilder/Codes/Repo/TitleStandardization/Output/FT/US Bank Similar FT wReplace 0611.xlsx',
    #                   index=False)
    df_check.to_excel('C:/Projects/OrgBuilder/Codes/Repo/TitleStandardization/Output/'
                      'FT/US Bank Similar FT wReplace 0820 BGT.xlsx',
                      index=False)

    df_check=pd.read_excel(
        'C:/Projects/OrgBuilder/Codes/Repo/TitleStandardization/Output/'
        'FT/BOA Bank Similar FT - EMSI vs BGT Distinct 0702.xlsx', sheet_name='Similar')

    df_all = pd.merge(df_new,df_check, how='left')

    # df_all[_EMP_ID_COL+'test'] = np.vectorize(randomize_employee_id)(df_all[_EMP_ID_COL])



## Randomize employee ID
import random
n_employees= len(df_all[_EMP_ID_COL])
df_all[_EMP_ID_COL + 'test'] = random.sample(range(10000, n_employees+100000), n_employees)

id_to_random = {w1:l1 for w1,l1 in zip(df_all[_EMP_ID_COL], df_all[_EMP_ID_COL + 'test'])}
df_all_replaced = pandas_replace(df_all,id_to_random)
del df_all_replaced[_EMP_ID_COL + 'test']

df_all_replaced.to_excel(
    'C:/Projects/OrgBuilder/Codes/Repo/TitleStandardization/Output/'
    'FT/BOA Bank Similar FT - EMSI vs BGT rawData 0713.xlsx',    index=False)



## Connect to snoflake

import snowflake.connector
import pandas as pd

con = snowflake.connector.connect(
    user='AADHVARYU',
    password='I@mjoy007',
    account='eka48603.us-east-1',
    # warehouse='BCG_BGT(XL)',
    database='BCG',
    schema='PUBLIC'
)

# Create a cursor object.
cur = con.cursor()

# Execute a statement that will generate a result set.
sql = "select top 10 * from distinct_title_dp"
cur.execute(sql)

# Fetch the result set from the cursor and deliver it as the Pandas DataFrame.
# df = cur.fetch_pandas_all()
df = pd.DataFrame(cur.fetchall())
df.columns = ['BGT_Cleaned_Title']
df.to_csv('test.csv', index=False)
df.to_pickle("BGT_Cleaned_titles")
df.to_msgpack("C:/Projects/BGT_Cleaned_titles", compress='zlib')
df_test = pd.read_msgpack("C:/Projects/BGT_Cleaned_titles")
pd.read_msgpack()
# ...

df_test1 = pd.read_pickle("C:/Projects/data_100.pkl.gz", compression="gzip")
df_test1 = pd.read_feather("C:/Projects/OrgBuilder/data.feather")
df_test1 = pd.read_feather("C:/Projects/data_75k.ft")
df_test = pd.read_feather("C:\\Projects\\OrgBuilder\\Codes\\Repo\\TitleStandardization\\Input\\FullStandardTitles\\data.feather")
df_test = pd.read_feather("C:\\Projects\\OrgBuilder\\Codes\\data.feather")
df_test = pd.read_feather("\\EURAPAN0187-00W.bcgcloud.com\\d$\\data.feather")


import pickle5 as pickle
import gzip
fname = "C:/Projects/OrgBuilder/data.pkl.gz"

with gzip.open(fname, 'r') as f:
    # obj = pickle.load(StrToBytes(f))
    df_sub = pickle.load(f)

type(obj)