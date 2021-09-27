
from sklearn.feature_extraction.text import CountVectorizer
import pandas as pd
import re

path = 'C:/Projects/OrgBuilder/Codes/DataServices/DataServices/outputs/'
# filename = path+"US Bank.xlsx"
filename = path+"BOA Full Org - 8.29.17 Project Baseline.xlsx"
colname = "Job Title"
# colname = "Job Family Desc"
Ngrams = 3
NumRecurrences = 3
output_path = path
output_tag = "BOA_0305"
dict_path = path + 'Short_Forms_Dictionary_KA.xlsx'

replace_dict = pd.read_excel(dict_path)
df = pd.read_excel(filename, encoding='latin-1')
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
    REPLACE_BY_SPACE_RE = re.compile('[/(){}\[\]\|@,;&-]')
    BAD_SYMBOLS_RE = re.compile('[^0-9a-z #+_]')

    text = re.sub(r'([a-z](?=[A-Z])|[A-Z](?=[A-Z][a-z]))', r'\1 ', text)

    text = text.lower()  # lowercase text
    text = REPLACE_BY_SPACE_RE.sub(' ',
                                   text)
    # replace REPLACE_BY_SPACE_RE symbols by space in text. substitute the matched string in REPLACE_BY_SPACE_RE with space.
    text = BAD_SYMBOLS_RE.sub('',
                              text)  # remove symbols which are in BAD_SYMBOLS_RE from text. substitute the matched string in BAD_SYMBOLS_RE with nothing.
    # text = text.replace('x', '')

    # if use_dict == 1:
    #     keys = list(self.replace_dict.keys())
    #     values = list(self.replace_dict.values())
    #     for n in range(len(self.replace_dict)):
    #         text = re.sub(r"\b%s\b" % keys[n], values[n], text)

    if use_dict == 1:
        keys = list(replace_dict.keys())
        values = list(replace_dict.values())
        # for n in range(len(self.replace_dict)):
        #     text = re.sub(r"\b%s\b" % keys[n], values[n], text)
        for key_term in set(text.split(" ")).intersection(keys):
            n = keys.index(key_term)
            text = re.sub(r"\b%s\b" % keys[n], values[n], text)

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


def prepare_col(df, sourcecol, col, replace_spaces=1):
    # Drop duplicates
    df.drop_duplicates(subset=sourcecol, inplace=True)
    df[col] = df[sourcecol].str.strip()

    print('Cleaning column')
    df = clean_column(df, col)

    if replace_spaces:
        print('Replacing spaces')
        df[col] = ['_'.join(x.split()) for x in df[col]]

    return df

print('Clean the column')
df = df[[colname]]
df[colname] = df[colname].astype('str')
df = prepare_col(df, colname, colname + '_Cleaned', replace_spaces=1)
colname = colname + '_Cleaned'

print('Replace Underscores to spaces (if any).')
df[colname] = df[colname].str.replace("_", " ")

N = Ngrams  # N grams
n = NumRecurrences  # least number of recurrences

print('Get word vectorizer')
word_vectorizer = CountVectorizer(ngram_range=(1, N), analyzer='word')
sparse_matrix = word_vectorizer.fit_transform(df[colname])

print('Get frequencies')
frequencies = sum(sparse_matrix).toarray()[0]

print('Get Ngrams df')
ngrams_df = pd.DataFrame(frequencies, index=word_vectorizer.get_feature_names(), columns=['frequency'])

print('Sort on frequency and reset index')
ngrams_df.sort_values('frequency', ascending=False, inplace=True)
ngrams_df.reset_index(inplace=True)
ngrams_df.rename(columns={'index': colname}, inplace=True)

print('Add Ngram column')
ngrams_df['ngram'] = ngrams_df[colname].str.split().str.len()

print('Filter for recurrences >= {}'.format(str(n)))
ngrams_df = ngrams_df[ngrams_df['frequency'] >= n]

Ngram_path = '/Ngrams_1-{}_minCount_{}_{}.csv'.format(N, n, output_tag)
ngrams_df.to_csv(output_path + Ngram_path, index=False)
