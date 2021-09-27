import pickle5 as pickle
import gzip
import pandas as pd
import numpy as np
from gensim.models import FastText
import re
from nltk.corpus import stopwords
import os

fname = "C:/Projects/OrgBuilder/data_100.pkl.gz"
_JOB_TITLE_COL = 'BGT_Cleaned_Title'
replace_file ='/Input/Short_Forms_Dictionary.xlsx'
path = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

import pickle


print('Loading data')
with gzip.open(fname, 'r') as f:
    # obj = pickle.load(StrToBytes(f))
    df_sub = pickle.load(f)

print('Converting titles to string')
df_sub[_JOB_TITLE_COL + 'str'] = df_sub[_JOB_TITLE_COL].astype(str)   #+ '|' + df_sub['Function_Extracted'].astype(str)


from joblib import Parallel, delayed
from functools import partial

# from src.NN_helpers import *
# Cleaning
REPLACE_BY_SPACE_RE = re.compile('[/(){}\[\]\|@,;]')
BAD_SYMBOLS_RE = re.compile('[^0-9a-z #+_]')
# STOPWORDS = set(stopwords.get_stopwords('english'))
STOPWORDS = set(stopwords.words('english'))

print(path)
replace_dict = pd.read_excel(path+replace_file)

replace_dict = {s: a for s, a in zip(replace_dict['Short'], replace_dict['Actual'])}

def clean_text(text, use_dict=1):
    """
    function to clean text of symbols, spaces, capitals etc.

    Parameters
    ----------
    text: a string

    Returns
    ----------
    text: modified initial string
    """
    text = text.lower()  # lowercase text
    text = REPLACE_BY_SPACE_RE.sub(' ',
                                   text)  # replace REPLACE_BY_SPACE_RE symbols by space in text. substitute the matched string in REPLACE_BY_SPACE_RE with space.
    text = BAD_SYMBOLS_RE.sub('',
                              text)  # remove symbols which are in BAD_SYMBOLS_RE from text. substitute the matched string in BAD_SYMBOLS_RE with nothing.
    # text = text.replace('x', '')
    if use_dict == 1:
        keys = list(replace_dict.keys())
        values = list(replace_dict.values())
        for n in range(len(replace_dict)):
            text = re.sub(r"\b%s\b" % keys[n], values[n], text)

    #    text = re.sub(r'\W+', '', text)
    # text = ' '.join(word for word in text.split() if word not in STOPWORDS)  # remove stopwords from text
    # # Correct words
    # text = str(TextBlob(text).correct())
    return text


def clean_column(df, col, use_dict=1):
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
    df[col] = df[col].str.replace("-", ' ')
    batch_num = 10000
    batch_size = len(df[col]) / batch_num
    func = partial(clean_sub_func, df, col)
    results = Parallel(n_jobs=-2)(delayed(func)(int(i*batch_size), int((i+1)*batch_size)) for i in range(batch_num))
    final = []
    for list1 in results:
        for sublist in list1:
            final.append(sublist)
    df[col] = final
    # df[col] = df[col].apply(clean_text, use_dict=use_dict)
    df[col] = df[col].str.replace('\d+', '')
    return df

def clean_sub_func(df, col, i, j):
    df_sub = df.iloc[i:j, :]
    results = df_sub[col].apply(clean_text, use_dict=1)
    return list(results)

print('Cleaning titles')
df_sub = clean_column(df_sub, _JOB_TITLE_COL + 'str')

# Replace spaces
print('Replacing spaces')
df_sub[_JOB_TITLE_COL + 'str'] = ['_'.join(x.split()) for x in df_sub[_JOB_TITLE_COL + 'str']]
df_sub.drop_duplicates(subset=_JOB_TITLE_COL + 'str', inplace=True)

### MODEL TRAINING
## Use sg=1 to use skip gram, train the model with job title+occupation
print('Building model')
model_ted = FastText(df_sub[_JOB_TITLE_COL + 'str'].str.split().values.tolist(), size=100, window=5, min_count=1
                     , workers=4, sg=0)

print('Saving model')
model_ted.save('Model/FastText_BGT_0820.model')

