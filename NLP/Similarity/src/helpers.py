import numpy as np
import re
from nltk.corpus import stopwords
from textblob import TextBlob
from imblearn.over_sampling import SMOTE
from sklearn.utils import class_weight
import os
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.metrics import confusion_matrix
from fuzzywuzzy import fuzz
import collections
from difflib import SequenceMatcher

from joblib import Parallel, delayed
from functools import partial

# from src.NN_helpers import *


# Cleaning
REPLACE_BY_SPACE_RE = re.compile('[/(){}\[\]\|@,;&-]')
BAD_SYMBOLS_RE = re.compile('[^0-9a-z #+_]')
# STOPWORDS = set(stopwords.get_stopwords('english'))
STOPWORDS = set(stopwords.words('english'))

path = "C:\\Projects\\OrgBuilder\\Codes\\DataServices\\DataServices"#os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
print(path)
replace_file ='/outputs/Short_Forms_Dictionary_KA.xlsx'
replace_dict = pd.read_excel(path+replace_file)

descriptor_cols = ['Division descriptor', 'Seniority descriptor','Company title descriptor']
replace_dict[descriptor_cols] = replace_dict[descriptor_cols].fillna('Missing')

dictionary_df = replace_dict.copy()

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

    text =  re.sub(' +', ' ', text)
    #    text = re.sub(r'\W+', '', text)
    # text = ' '.join(word for word in text.split() if word not in STOPWORDS)  # remove stopwords from text
    # # Correct words
    # text = str(TextBlob(text).correct())
    # from autocorrect import Speller
    # spell = Speller(lang='en')
    # text = spell(text)
    return text

def extract_descriptors(dictionary_df, column):
    list_of_words = dictionary_df[dictionary_df[column] != 'Missing']['Actual'].tolist()
    return list_of_words

def reduce_noise(text, column):
    words_list = extract_descriptors(dictionary_df, column)
    updated_text = ' '.join(word for word in text.split() if word not in words_list)

    if updated_text:
        return updated_text, list(set(text.split(' ')) - set(updated_text.split(' ')))
    else:
        return [text, []]

reduce_noise(clean_text("Head Compliance OpsRep & Svcs", 1), column=descriptor_cols[1])
reduce_noise(clean_text("CEO", 1), column=descriptor_cols[1])

clean_text("ALLIANCE TRUST BDO III", 1)
clean_text("Head Compliance OpsRep & Svcs", 1)
clean_text("MgrRegulatry&OpratnlRskMgt-C&R", 1)
clean_text("Mgmt Rptg/Operations Manager", 1)
clean_text("Structured Fin'l Inv Rptg Exec M&A", 1)

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
    df[col], df[descriptor_cols[0]] = df[col].apply(reduce_noise, column=descriptor_cols[0])
    df[col], df[descriptor_cols[0]] = df[col].apply(reduce_noise, column=descriptor_cols[0])
    df[col], df[descriptor_cols[1]] = df[col].apply(reduce_noise, column=descriptor_cols[1])
    df[col] = df[col].str.replace('\d+', '')
    return df


df_new = pd.read_excel(path+'/outputs//HRDataAnonModelOutput_test.xlsx')
df_new = df_new[['data_set_id', 'position_id', 'Title_2_1']]
df_new_sub = df_new[0:100]
df = df_new_sub
df_new_sub = clean_column(df_new_sub, 'Title_2_1')



def clean_sub_func(df, col, i, j):
    df_sub = df.iloc[i:j, :]
    results = df_sub[col].apply(clean_text, use_dict=1)
    return list(results)


# def translator(user_string):
#     user_string = user_string.split(" ")
#     j = 0
#     for _str in user_string:
#         # File path which consists of Abbreviations.
#         fileName = "C:\\xxxx\\Desktop\\slang.txt"
#         # File Access mode [Read Mode]
#         accessMode = "r"
#         with open(fileName, accessMode) as myCSVfile:
#             # Reading file as CSV with delimiter as "=", so that abbreviation are stored in row[0] and phrases in row[1]
#             dataFromFile = csv.reader(myCSVfile, delimiter="=")
#             # Removing Special Characters.
#             _str = re.sub('[^a-zA-Z0-9-_.]', '', _str)
#             for row in dataFromFile:
#                 # Check if selected word matches short forms[LHS] in text file.
#                 if _str.upper() == row[0]:
#                     # If match found replace it with its appropriate phrase in text file.
#                     user_string[j] = row[1]
#             myCSVfile.close()
#         j = j + 1
#     # Replacing commas with spaces for final output.
#     print(' '.join(user_string))
#     print('===================================================')
#     print('')



def over_sample(X_train, Y_train, df_sub, _DIV_COL):
    # SMOTE Over sampling
    smote = SMOTE('minority')

    X_sm, Y_sm = smote.fit_sample(X_train, Y_train)

    # Class weight
    class_weight = class_weight.compute_class_weight('balanced', np.unique(df_sub[_DIV_COL]), df_sub[_DIV_COL])

    return X_sm, Y_sm, class_weight


def pandas_replace(df, replaceDict, verbose=1):
    """
    Replaces values in pandas using a mapped dictionary

    Parameters
    ----------
    df: pandas,
        data frame to be replaced
    dict: dictionary,
        mapping of values to be replaced for each key

    Returns
    -------
    df: pandas,
        updated dataframe with values replaced
    """
    columns = df.columns.tolist()
    for c in range(len(columns)):
        if verbose == 1:
            print('Replacing column:', columns[c])
        df[columns[c]] = df[columns[c]].map(replaceDict).fillna(df[columns[c]])

    # Fill na with empty string
    df.fillna("", inplace=True)

    return df


def get_layer_titles_concat(df, _EMP_ID_COL, _JOB_Title_COL_New, _JOB_TITLE_COL, additional_columns):
    """
     Extract layer (line of control) columns and replace with job titles. These are then concatenated along with
     any additional columns like LOB, Division etc., to be used for prediction

     Parameters
     ----------
     df: pandas,
         data frame to be utilized to extract layer columns
     _EMP_ID_COL: str,
         name of the employee id column
     _JOB_Title_COL_New: str,
         name of the job title column in the new data set
     _JOB_TITLE_COL: str,
         name of the job title column in the training data set
     additional_columns: list,
         names of addtional columns to be concatened to the final string

     Returns
     -------
     df_new_layers: pandas,
         updated dataframe with concatenated string
     """

    # Get layer columns
    layer_columns = [col for col in df if col.startswith('Layer ')]

    ### Get job titles of layers
    emp_id_jobTitle_dict = dict(zip(df[_EMP_ID_COL], df[_JOB_Title_COL_New]))

    # Subset only the layer columns - to get the span of control
    df_new_layers = df[layer_columns]
    df_new_layers.fillna("", inplace=True)
    df_new_layers.head()

    # Replace employee id with job title
    df_new_layers = pandas_replace(df_new_layers, emp_id_jobTitle_dict, verbose=1)

    ###  Concatenate the job titles
    df_new_layers = df_new_layers[layer_columns]

    if len(additional_columns) > 0:
        df_new_layers = pd.merge(df_new_layers, df[additional_columns], left_index=True, right_index=True)

    df_new_layers[_JOB_TITLE_COL] = df_new_layers.apply(lambda row: ' '.join(row.values.astype(str)), axis=1)
    return df_new_layers


def plot_confusion(model, X_test, df, labels, Y_test):
    """
    Plots the confustion matrix between y test and y predicted

    Parameters
    ----------
    model: keras model,
        model trained to predict functions
    X_test: numpy array,
        array with concatenated string of job titles
    df: pandas data frame,
        data frame
    labels: list,
        list of unique labels from the data set
    Y_test: numpy array,
        array of function from the test data set

    """

    # Get y pred
    df_sub_test = get_predictions(model, X_test, labels, df)
    trans = {l1:w1 for w1,l1 in zip(range(len(labels)), labels)}
    Y_pred_df = pandas_replace(df_sub_test[['Predict']], trans)

    # Get y test
    Y_test_df = pd.DataFrame(pd.DataFrame(Y_test, columns=labels).idxmax(axis=1))
    Y_test_df = pandas_replace(Y_test_df, trans)

    # Get confusion matrix
    conf_mat = confusion_matrix(Y_test_df, Y_pred_df)
    fig, ax = plt.subplots(figsize=(5, 3))
    sns.heatmap(conf_mat, annot=True, fmt='d',
                xticklabels=labels, yticklabels=labels)
    plt.ylabel('Actual')
    plt.xlabel('Predicted')
    plt.show()


def plot_prediction_distribution(df, prediction_col, actual_col):
    """
    Plots the distribution of prediction across each unique value in the actual col - e.g., LOB

    Parameters
    ----------
    df: pandas data frame,
        data frame with prediction and actual
    prediction_col: str,
        name of the column with prediction data
    actual_col: numpy array,
        name of the column with actual data

    """
    plt.figure(figsize=(10, 5))
    chart = sns.countplot(x=None, y=actual_col, hue=prediction_col, data=df, order=None, hue_order=None, orient=None
                          , color=None
                          , palette=None, saturation=0.75, dodge=True, ax=None)

    plt.yticks(
        rotation=45,
        horizontalalignment='right',
        fontweight='light',
        fontsize='x-small'
    )


def standard_name(df_eval, _JOB_TITLE_COL, full_words=1):
    # df_eval = test
    d_standard_name = {}
    try:
        print(df_eval.Cluster.unique())
    except:
        df_eval['Cluster'] = 1

    for Cluster in df_eval.Cluster.unique():
        # Cluster = 1
        names = df_eval[df_eval['Cluster'] == Cluster][_JOB_TITLE_COL].to_list()
        l_common_substring = []
        if len(names) > 1:
            for i in range(0, len(names)):
                # i=0
                for j in range(i+1, len(names)):
                    # j=i+1
                    seqMatch = SequenceMatcher(None, names[i], names[j])
                    match = seqMatch.find_longest_match(0, len(names[i]), 0, len(names[j]))
                    if match.size != 0:
                        match_string = names[i][match.a: match.a + match.size].strip()
                        if (len(set(names[i].split())-set(match_string.split())) == 0) & (match_string == names[i]):
                            # i
                            l_common_substring.append(match_string)
                        elif full_words == 1:
                            try:
                                # Remove non complete words
                                for substring in match_string.split():
                                    if substring not in names[i].split():
                                        match_string = re.sub(r"\b%s\b" % substring, '', match_string)
                                # Remove additional spaces
                                match_string = ' '.join(match_string.split())
                            except:
                                match_string = ''
                            if len(match_string) > 0:
                                l_common_substring.append(match_string)
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


def get_standard_names_from_similar(string, _JOB_TITLE_COL, model):
    test = pd.DataFrame(model.wv.most_similar('_'.join(string.split()))
                        , columns=[_JOB_TITLE_COL, 'Similarity_Score'])
    test[_JOB_TITLE_COL] = test[_JOB_TITLE_COL].str.replace("_", ' ')
    test = standard_name(test, _JOB_TITLE_COL)
    return test['StandardName'][0]

# NLP
def levenshtein(s1,s2):
    if len(s1) > len(s2):
        s1,s2 = s2,s1
    distances = range(len(s1) + 1)
    for index2,char2 in enumerate(s2):
        newDistances = [index2+1]
        for index1,char1 in enumerate(s1):
            if char1 == char2:
                newDistances.append(distances[index1])
            else:
                 newDistances.append(1 + min((distances[index1], distances[index1+1], newDistances[-1])))
        distances = newDistances
    return distances[-1]


def randomize_employee_id(x):
    from faker import Faker

    generator = Faker()
    generator.random.seed(1432)

    return generator.random_number()



a = np.random.randn(3, 4) # a.shape = (12288, 150)
b = np.random.randn(4, 1)
a.shape = (3, 4)
b.shape = (4, 1)

for i in range(3):
    for j in range(4):
        c[i][j] = a[i][j] + b[j]
