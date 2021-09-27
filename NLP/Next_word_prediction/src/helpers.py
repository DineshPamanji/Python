from keras.preprocessing.text import Tokenizer
import nltk
from nltk.tokenize import word_tokenize
from nltk.corpus import stopwords
import numpy as np
import pandas as pd
import re
from keras.utils import to_categorical
import os

path = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
print(path)
config_path = path+'/Config/'
print(config_path)
input_path = path+'/Input/'

try:
    config_file = open(config_path + "/config.txt")
    exec(config_file.read(), globals())
    print('[SUCCESS] : Reading Config')
except:
    print('[ERROR]: Reading Config')


if abbreviate:
    replace_dict = pd.read_excel(input_path+replace_file)

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
    # Cleaning
    REPLACE_BY_SPACE_RE = re.compile('[/(){}\[\]\|@,;]')
    BAD_SYMBOLS_RE = re.compile('[^0-9a-z #+_]')
    STOPWORDS = set(stopwords.words('english'))

    text = text.lower()  # lowercase text
    text = REPLACE_BY_SPACE_RE.sub(' ',
                                   text)  # replace REPLACE_BY_SPACE_RE symbols by space in text. substitute the matched string in REPLACE_BY_SPACE_RE with space.
    text = BAD_SYMBOLS_RE.sub('',
                              text)  # remove symbols which are in BAD_SYMBOLS_RE from text. substitute the matched string in BAD_SYMBOLS_RE with nothing.
    # text = text.replace('x', '')
    if abbreviate:
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
    df[col] = df[col].apply(clean_text, use_dict=use_dict)
    df[col] = df[col].str.replace('\d+', '')
    return df
