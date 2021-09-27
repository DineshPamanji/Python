from keras.preprocessing.text import Tokenizer
from keras.preprocessing.sequence import pad_sequences
from keras.models import Sequential
from keras.layers import Dense, Embedding, LSTM, SpatialDropout1D
from sklearn.model_selection import train_test_split
from keras.utils.np_utils import to_categorical
from keras.callbacks import EarlyStopping
from keras.layers import Dropout
import re
from nltk.corpus import stopwords
from nltk import word_tokenize
import pandas as pd
import numpy as np
import os
import glob
from copy import copy
import matplotlib.pyplot as plt
import seaborn as sns

# import stopwords

path = 'C:/Projects/OrgBuilder/Data/FI Sanitized Data-Sets/'
_DIV_COL = 'Region'
_JOB_TITLE_COL = 'Job Title'
top10 = 0

# Get all file names
os.chdir(path)
files_list = [i for i in glob.glob('*.{}'.format('xlsx'))]

file = files_list[6]

# Read file
df = pd.read_excel(path + file)

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

# # Chart
# df_sub[_DIV_COL].value_counts().sort_values(ascending=False).plot(kind='bar',
#                                                                 title='Number of candidates in each category')


# Cleaning
REPLACE_BY_SPACE_RE = re.compile('[/(){}\[\]\|@,;]')
BAD_SYMBOLS_RE = re.compile('[^0-9a-z #+_]')
# STOPWORDS = set(stopwords.get_stopwords('english'))
STOPWORDS = set(stopwords.words('english'))


def clean_text(text):
    """
    function to clean text of symbols, spaces, capitals etc.
        text: a string

        return: modified initial string
    """
    text = text.lower()  # lowercase text
    text = REPLACE_BY_SPACE_RE.sub(' ',
                                   text)  # replace REPLACE_BY_SPACE_RE symbols by space in text. substitute the matched string in REPLACE_BY_SPACE_RE with space.
    text = BAD_SYMBOLS_RE.sub('',
                              text)  # remove symbols which are in BAD_SYMBOLS_RE from text. substitute the matched string in BAD_SYMBOLS_RE with nothing.
    text = text.replace('x', '')
    #    text = re.sub(r'\W+', '', text)
    text = ' '.join(word for word in text.split() if word not in STOPWORDS)  # remove stopwors from text
    return text

df_sub[_JOB_TITLE_COL+'_COPY'] = copy(df_sub[_JOB_TITLE_COL])
df_sub[_JOB_TITLE_COL] = df_sub[_JOB_TITLE_COL].apply(clean_text)
df_sub[_JOB_TITLE_COL] = df_sub[_JOB_TITLE_COL].str.replace('\d+', '')

# The maximum number of words to be used. (most frequent)
MAX_NB_WORDS = 50000
# Max number of words in each complaint.
MAX_SEQUENCE_LENGTH = 250
# This is fixed.
EMBEDDING_DIM = 100
tokenizer = Tokenizer(num_words=MAX_NB_WORDS, filters='!"#$%&()*+,-./:;<=>?@[\]^_`{|}~', lower=True)
tokenizer.fit_on_texts(df_sub[_JOB_TITLE_COL].values)
word_index = tokenizer.word_index
print('Found %s unique tokens.' % len(word_index))

X = tokenizer.texts_to_sequences(df_sub[_JOB_TITLE_COL].values)

# Pad with zeroes
X = pad_sequences(X, maxlen=MAX_SEQUENCE_LENGTH)
print('Shape of data tensor:', X.shape)

# Create dummy column for each region/division
Y = pd.get_dummies(df_sub[_DIV_COL]).values
print('Shape of label tensor:', Y.shape)

# Train test split
X_train, X_test, Y_train, Y_test = train_test_split(X,Y, test_size = 0.10, random_state = 42)
print(X_train.shape, Y_train.shape)
print(X_test.shape, Y_test.shape)

labels_count = Y_test.shape[1]

# Build model
model = Sequential()
model.add(Embedding(MAX_NB_WORDS, EMBEDDING_DIM, input_length=X.shape[1]))
model.add(SpatialDropout1D(0.2))
model.add(LSTM(100, dropout=0.2, recurrent_dropout=0.2))
model.add(Dense(labels_count, activation='softmax'))
model.compile(loss='categorical_crossentropy', optimizer='adam', metrics=['accuracy'])

epochs = 5
batch_size = 64

history = model.fit(X_train, Y_train, epochs=epochs, batch_size=batch_size,validation_split=0.1,callbacks=[EarlyStopping(monitor='val_loss', patience=3, min_delta=0.0001)])



# Check accuracy
accr = model.evaluate(X_test, Y_test)
print('Test set\n  Loss: {:0.3f}\n  Accuracy: {:0.3f}'.format(accr[0],accr[1]))

#
# plt.title('Loss')
# plt.plot(history.history['loss'], label='train')
# plt.plot(history.history['val_loss'], label='test')
# plt.legend()
# plt.show()
#
#
# plt.title('Accuracy')
# plt.plot(history.history['acc'], label='train')
# plt.plot(history.history['val_acc'], label='test')
# plt.legend()
# plt.show();

# Get labels
labels = pd.get_dummies(df_sub[_DIV_COL]).columns.tolist()


# Test with new title
# df_sub['Predict'] = pd.Series()

# rownum = 5758
# for rownum in range(len(df_sub)):
#     new_title = [df_sub[_JOB_TITLE_COL][rownum]]
#     seq = tokenizer.texts_to_sequences(new_title)
#     padded = pad_sequences(seq, maxlen=MAX_SEQUENCE_LENGTH)
#     pred = model.predict(padded)
#     # labels = ['Credit reporting, credit repair services, or other personal consumer reports', 'Debt collection', 'Mortgage', 'Credit card or prepaid card', 'Student loan', 'Bank account or service', 'Checking or savings account', 'Consumer Loan', 'Payday loan, title loan, or personal loan', 'Vehicle loan or lease', 'Money transfer, virtual currency, or money service', 'Money transfers', 'Prepaid card']
#     df_sub['Predict'][rownum] = labels[np.argmax(pred)]
#
# print(pred, labels[np.argmax(pred)])
# print(df_sub[_DIV_COL][rownum])

df_sub_predict = df_sub[[_JOB_TITLE_COL, _DIV_COL]]

# Get probabilities for all X
probabilties = model.predict(X)
probabilties_df = pd.DataFrame(probabilties, columns=labels)
df_sub_predict = pd.merge(df_sub_predict, probabilties_df, left_index=True, right_index=True)
df_sub_predict['Max_Proba'] = df_sub_predict[labels].max(axis=1)
df_sub_predict['Predict'] = df_sub_predict[labels].idxmax(axis=1)

if top10 == 1:
    output_name = path + 'RNN_Output_' + file + 'top10.xlsx'
else:
    output_name = path+'RNN_Output_'+file+'.xlsx'

df_sub_predict.to_excel(output_name, index=False)
#sum(df_sub_predict['Predict'] == df_sub_predict[_DIV_COL])/df_sub_predict.shape[0]
