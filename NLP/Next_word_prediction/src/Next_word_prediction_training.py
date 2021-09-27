import nltk
from nltk.tokenize import word_tokenize
import numpy as np
import re
from keras.utils import to_categorical
import os
from numpy import array
from pickle import dump
from keras.preprocessing.text import Tokenizer
from keras.preprocessing.sequence import pad_sequences

from keras.utils import to_categorical
from keras.models import Sequential
from keras.layers import Dense
from keras.layers import LSTM
from keras.layers import Embedding
from keras.callbacks import EarlyStopping


from Next_word_prediction.src.helpers import *

# path = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
path = 'C:/Projects/OrgBuilder/Codes/Repo/TitleStandardization/Next_word_prediction/'
config_path = path+'/Config/'
input_path = path+'/Input/'

try:
    config_file = open(config_path + "/config.txt")
    exec(config_file.read(), globals())
    print('[SUCCESS] : Reading Config')
except:
    print('[ERROR]: Reading Config')

# from doc3 import training_doc3
# load ascii text and covert to lowercase
file = input_path+filename
raw_text = open(file, 'r', encoding='latin-1').read()
lines = raw_text.split('\n')

# Clean text
df_lines = pd.DataFrame(lines, columns={'text'})
df_lines['text'] = df_lines['text'].astype(str)
df_lines = clean_column(df_lines, 'text')
df_lines = df_lines[~df_lines['text'].isnull()]

lines = df_lines['text'].values

# integer encode sequences of words
tokenizer = Tokenizer()
tokenizer.fit_on_texts(lines)
sequences = tokenizer.texts_to_sequences(lines)

# create sequences of length 2
train_len = 3
text_sequences = []
for l in sequences:
    for i in range(train_len, len(l)+1):
        seq = l[i-train_len:i]
        text_sequences.append(seq)

# Pad sequences
text_sequences = pad_sequences(text_sequences, maxlen=train_len, truncating='pre')

# vocabulary size
vocab_size = len(tokenizer.word_index) + 1

# separate into input and output
text_sequences = array(text_sequences)
X, y = text_sequences[:, :-1], text_sequences[:, -1]
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
model.fit(X, y, batch_size=128, epochs=100, verbose=2)

# save the model to file
model.save(path+'Model/NWP_model_Shakespeare_tri.h5')
# save the tokenizer
dump(tokenizer, open(path+'Model/NWP_tokenizer_Shakespeare_tri.pkl', 'wb'))
