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

filename = 'C:/Projects/OrgBuilder/Codes/Repo/TitleStandardization/Output/FT/BOA Bank Similar FT - EMSI vs BGT 0820.xlsx'
raw_text = pd.read_excel(filename)
raw_text['Job Title_wReplace'] = raw_text['Job Title_wReplace'].str.replace('no corporate title', "")
raw_text['Job Title_wReplace'] = raw_text['Job Title_wReplace'].str.strip()

raw_text['concat'] = raw_text['Job Title_wReplace'] + ' ' + raw_text['Similar_Title']
# raw_text[['concat']].to_csv('C:/Projects/OrgBuilder/Codes/Repo/TitleStandardization/Output/FT/BOA Bank Similar FT - EMSI vs BGT 0820.txt', sep=' ', index=False)

#define the function#
def find_max_list(list):
    list_len = [len(i) for i in list]
    return (max(list_len))


# for df x-y mapping
text_sequences = []
raw_text['text'] = raw_text['concat'].str.strip().str.split(' ')
for i in range(len(raw_text['concat'])):
    seq = raw_text['text'][i]
    text_sequences.append(seq)

# Convert test sequences to token(number) sequences
tokenizer = Tokenizer()
tokenizer.fit_on_texts(text_sequences)
sequences = tokenizer.texts_to_sequences(text_sequences)

#print output#
from keras.preprocessing.sequence import pad_sequences
MAX_LENGTH = find_max_list(sequences)
MAX_LENGTH = 10
sequences = pad_sequences(sequences, maxlen=MAX_LENGTH, truncating='pre')
#vocabulary size increased by 1 for the cause of padding
vocabulary_size = len(tokenizer.word_counts)+1
n_sequences = np.empty([len(sequences), MAX_LENGTH], dtype='int32')
for i in range(len(sequences)):
    n_sequences[i] = sequences[i]

train_inputs = n_sequences[:,:-1]
train_targets = n_sequences[:,-1]
train_targets = to_categorical(train_targets, num_classes=vocabulary_size)
seq_len = train_inputs.shape[1]

## Train the model
from keras.models import Sequential
from keras.layers import Dense, Embedding, LSTM, SpatialDropout1D, GRU

from keras.callbacks import EarlyStopping

model = Sequential()
model.add(Embedding(vocabulary_size, 100, input_length=seq_len))
model.add(SpatialDropout1D(0.2))
# model.add(LSTM(100))
model.add(LSTM(100, return_sequences=True))
model.add(LSTM(100, dropout=0.2, recurrent_dropout=0.2))
model.add(Dense(100, activation='relu'))
model.add(Dense(vocabulary_size, activation='softmax'))
# compiling the network
model.compile(loss='categorical_crossentropy', optimizer='adam', metrics=['accuracy'])
# model.fit(train_inputs,train_targets,epochs=5,verbose=1, batch_size=16)#, validation_split=0.2,  batch_size=64,)

# Fit the model with validation
history = model.fit(train_inputs, train_targets, epochs=100, batch_size=32, validation_split=0.2)
                    # , callbacks=[EarlyStopping(monitor='val_loss', patience=3, min_delta=0.0001)])


# Predict next word
from keras.preprocessing.sequence import pad_sequences
input_text = input().strip().lower()
input_text = 'data_scientist'.strip().lower()
encoded_text = tokenizer.texts_to_sequences([input_text])[0]
pad_encoded = pad_sequences([encoded_text], maxlen=seq_len, truncating='pre')
# print(encoded_text, pad_encoded)
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


### Another alternative with K-Fold
import pandas
from keras.models import Sequential
from keras.layers import Dense
from keras.wrappers.scikit_learn import KerasClassifier
from keras.utils import np_utils
from sklearn.model_selection import cross_val_score
from sklearn.model_selection import KFold
from sklearn.preprocessing import LabelEncoder
from sklearn.pipeline import Pipeline

# load dataset
dataframe = pandas.read_csv("iris.data", header=None)
dataset = dataframe.values
X = dataset[:, 0:4].astype(float)
Y = dataset[:, 4]
# encode class values as integers
encoder = LabelEncoder()
encoder.fit(Y)
encoded_Y = encoder.transform(Y)
# convert integers to dummy variables (i.e. one hot encoded)
dummy_y = np_utils.to_categorical(encoded_Y)
dummy_y = np_utils.to_categorical(train_targets)
dummy_y = y
Y = train_targets


# define baseline model
def baseline_model():
    # create model
    model = Sequential()
    # model.add(Dense(vocab_size, input_dim=9, activation='relu'))
    # model.add(Dense(9, activation='softmax'))
    model.add(Embedding(vocab_size, 50, input_length=seq_length))
    model.add(LSTM(100, return_sequences=True))
    model.add(LSTM(100))
    model.add(Dense(100, activation='relu'))
    model.add(Dense(vocab_size, activation='softmax'))
    # Compile model
    model.compile(loss='categorical_crossentropy', optimizer='adam', metrics=['accuracy'])
    return model


estimator = KerasClassifier(build_fn=baseline_model, epochs=200, batch_size=5, verbose=1)
kfold = KFold(n_splits=10, shuffle=True)
results = cross_val_score(estimator, X, dummy_y, cv=kfold)
print("Baseline: %.2f%% (%.2f%%)" % (results.mean() * 100, results.std() * 100))


##

import nlu


example_text = 'data scientist'
nlu.load('ner.onto').predict(example_text)


import pandas as pd
file = "C:/Projects/FCA/Phase2/Codes/OptionPricingExecutable/Output/2020-11-05_ACN_ITA_RCV.xlsx"

df = pd.read_excel(file, sheet_name='Calibration Results')

for i in range(10):
    df = df.append(df)

randint(400, )