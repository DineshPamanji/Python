import pandas as pd
import numpy as np
from copy import copy
from sklearn.model_selection import train_test_split

# from src.NN_helpers import *

def create_df(path, file, _JOB_TITLE_COL, _DIV_COL, top10):
    """
    Read input data. Subset data if testing.

    Parameters
    ----------
    path: str, Path of the file present
    file: str, filename
    _JOB_TITLE_COL: str, column name of the job title/concatenation of titles
    _DIV_COL: str, column name of Division/Function
    top10: binary, 1 to use only top 10 most frequent functions

    Returns
    ----------

    df_sub: Pandas data frame
    """
    # Read file
    try:
        df = pd.read_excel(path + file)
    except:
        df = pd.read_csv(path + file, encoding='latin-1')

    # Value counts
    df[_JOB_TITLE_COL].value_counts()
    df[_DIV_COL].value_counts()

    if top10 == 1:
        # Subset data for top 10 region/ divisions
        top_divisions = df[_DIV_COL].value_counts()[0:10].index.tolist()
        df_sub = df[np.isin(df[_DIV_COL], top_divisions)]
    else:
        df_sub = copy(df)

    df_sub.reset_index(inplace=True, drop=True)
    return df_sub


def get_train_test(tokenizer, df, _JOB_TITLE_COL, _DIV_COL, MAX_SEQUENCE_LENGTH):
    """
    Create labels and features, then split into train and test

    Parameters
    ----------
    tokenizer: keras tokenizer, trained on the job title column
    df: pandas, data frame with job title, function
    _JOB_TITLE_COL: str, column name of the job title/concatenation of titles
    _DIV_COL: str, column name of Division/Function
    MAX_SEQUENCE_LENGTH: int, maximum number of words allowed to be used in the model

    Returns
    ----------
    Returns: features, labels and their train test splits
    """
    # Prepare X
    X = prepare_X(tokenizer, df, _JOB_TITLE_COL, MAX_SEQUENCE_LENGTH)

    # Create dummy column for each region/division
    Y = pd.get_dummies(df[_DIV_COL]).values
    print('Shape of label tensor:', Y.shape)

    # Train test split
    X_train, X_test, Y_train, Y_test = train_test_split(X, Y, test_size=0.20, random_state=42)
    print('Shape of X_train, Y_train tensors:', X_train.shape, Y_train.shape)
    print('Shape of X_test, Y_test tensors:',X_test.shape, Y_test.shape)
    return X, Y, X_train, X_test, Y_train, Y_test
