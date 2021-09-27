from keras.preprocessing.text import Tokenizer
import nltk
from nltk.tokenize import word_tokenize
from nltk.corpus import stopwords
import numpy as np
import pandas as pd
import re
from keras.utils import to_categorical
import os

# from doc3 import training_doc3
# load ascii text and covert to lowercase
# filename = 'C:/Projects/OrgBuilder/Codes/Repo/TitleStandardization/Input/BOFA/BGT_Titles.txt'
filename = 'C:/Projects/OrgBuilder/Codes/Repo/TitleStandardization/Output/FT/BOA Bank Similar FT - EMSI vs BGT 0820.txt'
raw_text = open(filename, 'r', encoding='latin-1').read()

filename = 'C:/Projects/OrgBuilder/Codes/Repo/TitleStandardization/Output/FT/BOA Bank Similar FT - EMSI vs BGT 0820.xlsx'
raw_text = pd.read_excel(filename)
raw_text['concat'] = raw_text['Job Title_wReplace'] + ' ' + raw_text['Similar_Title']
raw_text[['concat']].to_csv('C:/Projects/OrgBuilder/Codes/Repo/TitleStandardization/Output/FT/BOA Bank Similar FT - EMSI vs BGT 0820.txt', sep=' ', index=False)


abbreviate = True

path = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
print(path)
if abbreviate:
    replace_file ='/Input/Short_Forms_Dictionary.xlsx'
    replace_dict = pd.read_excel(path+replace_file)

    replace_dict = {s: a for s, a in zip(replace_dict['Short'], replace_dict['Actual'])}

def clean_text(text, use_dict=1, replace=False):
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
    if replace:
        if use_dict == 1:
            keys = list(replace_dict.keys())
            values = list(replace_dict.values())
            for n in range(len(replace_dict)):
                text = re.sub(r"\b%s\b" % keys[n], values[n], text)

    #    text = re.sub(r'\W+', '', text)
    text = ' '.join(word for word in text.split() if word not in STOPWORDS)  # remove stopwords from text
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



raw_text = raw_text.lower()


cleaned = re.sub(r'\W+', ' ', raw_text).lower()
# cleaned = raw_text['concat'].values

tokens = word_tokenize(cleaned)
train_len = 2
text_sequences = []
for i in range(train_len,len(tokens)):
  seq = tokens[i-train_len:i]
  text_sequences.append(seq)
sequences = {}
count = 1


# for df x-y mapping
text_sequences = []
raw_text['text'] = raw_text['concat'].str.strip().str.split(' ')
for i in range(len(raw_text['concat'])):
    seq = raw_text['text'][i]
    text_sequences.append(seq)
sequences = {}
count = 1

for i in range(len(tokens)):
    if tokens[i] not in sequences:
        sequences[tokens[i]] = count
        count += 1

tokenizer = Tokenizer()
tokenizer.fit_on_texts(text_sequences)
sequences = tokenizer.texts_to_sequences(text_sequences)
#vocabulary size increased by 1 for the cause of padding
vocabulary_size = len(tokenizer.word_counts)+1
n_sequences = np.empty([len(sequences),train_len], dtype='int32')
for i in range(len(sequences)):
    n_sequences[i] = sequences[i]

train_inputs = n_sequences[:,:-1]
train_targets = n_sequences[:,-1]
train_targets = to_categorical(train_targets, num_classes=vocabulary_size)
seq_len = train_inputs.shape[1]

## Train the model
from keras.models import Sequential
from keras.layers import Dense
from keras.layers import LSTM
from keras.layers import Embedding
model = Sequential()
model.add(Embedding(vocabulary_size, seq_len, input_length=seq_len))
model.add(LSTM(50, return_sequences=True))
model.add(LSTM(50))
model.add(Dense(50, activation='relu'))
model.add(Dense(vocabulary_size, activation='softmax'))
# compiling the network
model.compile(loss='categorical_crossentropy', optimizer='adam', metrics=['accuracy'])
model.fit(train_inputs,train_targets,epochs=5,verbose=1)


# Predict next word
from keras.preprocessing.sequence import pad_sequences
input_text = input().strip().lower()
encoded_text = tokenizer.texts_to_sequences([input_text])[0]
pad_encoded = pad_sequences([encoded_text], maxlen=seq_len, truncating='pre')
print(encoded_text, pad_encoded)
for i in (model.predict(pad_encoded)[0]).argsort()[-3:][::-1]:
    pred_word = tokenizer.index_word[i]
    print("Next word suggestion:",pred_word)



#########################################################################################################
# Alternate

from numpy import array
from pickle import dump
from keras.preprocessing.text import Tokenizer
from keras.preprocessing.sequence import pad_sequences

from keras.utils import to_categorical
from keras.models import Sequential
from keras.layers import Dense
from keras.layers import LSTM
from keras.layers import Embedding


# load doc into memory
def load_doc(filename):
    # open the file as read only
    file = open(filename, 'r')
    # read all text
    text = file.read()
    # close the file
    file.close()
    return text


# load
# in_filename = 'C:/Projects/OrgBuilder/Codes/Repo/TitleStandardization/Input/BOFA/BGT_Titles.txt'
in_filename = 'C:/Projects/OrgBuilder/Codes/Repo/TitleStandardization/Output/FT/BOA Bank Similar FT - EMSI vs BGT 0820.xlsx'
doc = load_doc(in_filename)
lines = doc.split('\n')

# integer encode sequences of words
tokenizer = Tokenizer()
tokenizer.fit_on_texts(lines)
sequences = tokenizer.texts_to_sequences(lines)
sequences = pad_sequences(sequences, maxlen=10, truncating='pre')
# vocabulary size
vocab_size = len(tokenizer.word_index) + 1

# separate into input and output
sequences = array(sequences)
X, y = sequences[:, :-1], sequences[:, -1]
y = to_categorical(y, num_classes=vocab_size)
seq_length = X.shape[1]

# define model
model = Sequential()
model.add(Embedding(vocab_size, 50, input_length=seq_length))
model.add(LSTM(100, return_sequences=True))
model.add(LSTM(100))
model.add(Dense(100, activation='relu'))
model.add(Dense(vocab_size, activation='softmax'))
print(model.summary())
# compile model
model.compile(loss='categorical_crossentropy', optimizer='adam', metrics=['accuracy'])
# fit model
model.fit(X, y, batch_size=128, epochs=100)

# save the model to file
model.save('Model/NWP_model.h5')
# save the tokenizer
dump(tokenizer, open('Model/NWP_tokenizer.pkl', 'wb'))

# Predict
from random import randint
from pickle import load
from keras.models import load_model
from keras.preprocessing.sequence import pad_sequences


# generate a sequence from a language model
def generate_sequence(model, tokenizer, seq_length, seed_text, n_words):
    result = list()
    in_text = seed_text
    # generate a fixed number of words
    for _ in range(n_words):
        # encode the text as integer
        encoded = tokenizer.texts_to_sequences([in_text])[0]
        # truncate sequences to a fixed length
        encoded = pad_sequences([encoded], maxlen=seq_length, truncating='pre')
        # predict probabilities for each word
        yhat = model.predict_classes(encoded, verbose=0)
        # map predicted word index to word
        out_word = ''
        for word, index in tokenizer.word_index.items():
            if index == yhat:
                out_word = word
                break
        # append to input
        in_text += ' ' + out_word
        result.append(out_word)
    return ' '.join(result)


# load the model
model = load_model('Model/NWP_model.h5')

# load the tokenizer
tokenizer = load(open('Model/NWP_tokenizer.pkl', 'rb'))

# select a seed text
seed_text = "Vice"
print(seed_text + '\n')

# generate new text
generated = generate_sequence(model, tokenizer, 9, seed_text, 2)
print(generated)

