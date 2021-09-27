import numpy as np
import re
from nltk.corpus import stopwords
import os
import pandas as pd

from google_trans_new import google_translator
translator = google_translator()

# Cleaning
REPLACE_BY_SPACE_RE = re.compile('[/(){}\[\]\|@,;&-]')
BAD_SYMBOLS_RE = re.compile('[^0-9a-z #+_]')
# STOPWORDS = set(stopwords.get_stopwords('english'))
STOPWORDS = set(stopwords.words('english'))

path = "C:\\Projects\\OrgBuilder\\Codes\\DataServices\\DataServices"#os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
print(path)
replace_file ='/outputs/Short_Forms_Dictionary_KA.xlsx'
replace_dict = pd.read_excel(path+replace_file)

descriptor_cols = ['Company title descriptor', 'Seniority descriptor', 'Division descriptor']
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
    text = re.sub(r'([a-z](?=[A-Z])|[A-Z](?=[A-Z][a-z]))', r'\1 ', text)

    text = text.lower()  # lowercase text
    text = REPLACE_BY_SPACE_RE.sub(' ',
                                   text)  # replace REPLACE_BY_SPACE_RE symbols by space in text. substitute the matched string in REPLACE_BY_SPACE_RE with space.
    text = BAD_SYMBOLS_RE.sub('',
                              text)  # remove symbols which are in BAD_SYMBOLS_RE from text. substitute the matched string in BAD_SYMBOLS_RE with nothing.
    # text = text.replace('x', '')
    if use_dict == 1:
        # for n in range(len(replace_dict)):
        #     text = re.sub(r"\b%s\b" % keys[n], values[n], text)
        keys = list(replace_dict.keys())
        values = list(replace_dict.values())
        for key_term in set(text.split(" ")).intersection(keys):
            n = keys.index(key_term)
            text = re.sub(r"\b%s\b" % keys[n], values[n], text)

    # Remove additional spaces
    text = re.sub(' +', ' ', text)

    # translate
    # text = translator.translate(text)

    #    text = re.sub(r'\W+', '', text)
    # text = ' '.join(word for word in text.split() if word not in STOPWORDS)  # remove stopwords from text
    # # Correct words
    # text = str(TextBlob(text).correct())
    # from autocorrect import Speller
    # spell = Speller(lang='en')
    # text = spell(text)
    return text

def extract_descriptors(dictionary_df, column):
    list_of_words = list(set(dictionary_df[dictionary_df[column] != 'Missing']['Actual'].tolist()))
    return list_of_words

def reduce_noise(text, column, MINWORDS=2):
    words_list = extract_descriptors(dictionary_df, column)
    updated_text = ' '.join(word for word in text.split() if word not in words_list)

    if len(updated_text.split(' ')) >= MINWORDS:
        return updated_text, list(list(set(text.split(' ')) - set(updated_text.split(' ')) - set([''])))
    else:
        return text, list(list(set(text.split(' ')) - set(updated_text.split(' ')) - set([''])))

# reduce_noise(clean_text("Head Compliance OpsRep & Svcs", 1), column=descriptor_cols[1])
text = 'ATM Support Supervisor (TT)'
# text = reduce_noise(clean_text("supv aml bsa compliance dru", 1), column=descriptor_cols[0])
# text = reduce_noise(text, column=descriptor_cols[0])
# reduce_noise(text, column=descriptor_cols[0])

# %%time
# clean_text("ALLIANCE TRUST BDO III", 1)

# clean_text("Head Compliance OpsRep & Svcs", 1)
# clean_text("MgrRegulatry&OpratnlRskMgt-C&R", 1)
# clean_text("Mgmt Rptg/Operations Manager", 1)
# clean_text("Structured Fin'l Inv Rptg Exec M&A", 1)

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
    for n in range(len(descriptor_cols)):
        df[col], df[descriptor_cols[n]] = df[col].apply(reduce_noise, column=descriptor_cols[n]).str
    df[col] = df[col].str.replace('\d+', '')
    return df


df_new = pd.read_excel(path+'/outputs//HRDataAnonModelOutput_test.xlsx')
df_new = df_new[['data_set_id', 'position_id', 'Title_2_1']]
df_new['CleanedTitle'] = df_new['Title_2_1']
# df_new_sub = df_new[0:100]
# df = df_new_sub
# col = 'Title_2_1'
# use_dict=1
# column = descriptor_cols[1]
# df_new_sub = clean_column(df_new_sub, 'Title_2_1')
#
# df_test = df_new[df_new['Title_2_1'] == 'Senior Director']
# df_test['CleanedTitle'] = df_test['Title_2_1']
#
# df_test = clean_column(df_test, 'CleanedTitle')

import time

# Clean column with noise reduction
a = time.time()
df_new = clean_column(df_new, 'CleanedTitle')
print(time.time()-a)

df_new.to_excel(path+'/outputs/HC_Master_Cleaned_Noise_v06_1229.xlsx', index=False)

########################################################################################################
def translate_column(df, column, source,chunksize=50):
    # To list
    text_list = df[column].tolist()
    text_count = len(text_list)

    translated_list = []
    for i in range(0, len(text_list), chunksize):
        print(i, min(i+chunksize, text_count))
        # Convert column to pipe separated string
        text = ' || '.join(text_list[i:min(i+chunksize, text_count)])

        # translate
        text = translator.translate(text, 'en', source)
        translated_list_sub = text.split(' || ')
        translated_list.extend(translated_list_sub)
        print(len(translated_list_sub))
        print(len(translated_list))
        if len(translated_list_sub) < chunksize:
            print(translated_list_sub)

    # Convert the text back to column
    df[column] = translated_list
    return df

df_new_test = df_new[0:1000].copy()
df_new_test['CleanedTitle'+'_original'] = df_new_test['CleanedTitle']
df_new_test['CleanedTitle'][9] = 'Lehrer'
df_new_test['CleanedTitle'][0] = 'Bonjour le monde!'

a = time.time()
df_new_test = translate_column(df_new_test, 'CleanedTitle', 'fr')
print(time.time()-a)

df_new_test['CleanedTitle'+'_original'] == df_new_test['CleanedTitle']

df_new_test = translate_column(df_new_test, 'CleanedTitle', 'de')


# import nlu
# import os
# os.environ["JAVA_HOME"]
# os.environ["PATH"]
# os.environ["PYSPARK_SUBMIT_ARGS"] = "--master local[2] pyspark-shell"
#
# pipe = nlu.load('en.stem')
#
# bert = nlu.load('bert elmo albert xlnet glove use')
# bert = nlu.load('bert')

from nltk.corpus import wordnet
syn = wordnet.synsets('hello')[0]
syn = wordnet.synsets('manager')[0]
syn.hypernyms()
syn.hypernyms()[0].hyponyms()
syn.()

import polyglot
from polyglot.text import Text, Word

text = 'rep'
word = Word(text, language="en")
print("Neighbors (Synonms) of {}".format(word)+"\n"+"-"*30)
for w in word.neighbors:
    print("{:<16}".format(w))



print("\n\nThe first 10 dimensions out the {} dimensions\n".format(word.vector.shape[0]))
print(word.vector[:10])

from polyglot.transliteration import Transliterator
transliterator = Transliterator(source_lang="fr", target_lang="en")
print(transliterator.transliterate("monde"))
