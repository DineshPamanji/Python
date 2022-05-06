import nltk
from nltk.tokenize import word_tokenize
import numpy as np
import re
#from keras.utils import to_categorical
import os
from numpy import array
from pickle import dump, load
from keras.preprocessing.text import Tokenizer
from keras.preprocessing.sequence import pad_sequences

from tensorflow.keras.utils import to_categorical
from keras.models import Sequential
from keras.layers import Dense
from keras.layers import LSTM
from keras.layers import Embedding
from keras.callbacks import EarlyStopping
from keras.models import load_model


from sklearn.model_selection import train_test_split
import functools

from src.helpers import *

# path = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
path = 'C:/Projects/NBA/'

datapath= "C:/Users/703301318/Enquero/Adidas CDNA - Documents/02. Next Best Engagement/05. A-B Testing Codes/EXAP_DATA_INPUT/"


df = pd.read_csv(datapath+'Member_Eng_Data.csv')

# Only with engagements performed, not emails sent
df = df.loc[(df['EMAIL_ENGAGED_WITHIN_48H'] ==1) | 
            (df['ENGAGED_WITHOUT_EMAIL'] == 1) , :]

# Sort by recipient
df.sort_values(['IRECIPIENTID', 'TSENGMTDT'], inplace=True)


# # Group by irecipientid
# df_grp = df.groupby(['IRECIPIENTID'])['ENGMT_TYPE'].count()

# # Get engagement number
df['Engmt_Number'] = df.groupby(['IRECIPIENTID']).cumcount()+1
# df['Engmt_Number'] = 'ENGMT_' + df['Engmt_Number'].astype('str').str.zfill(2)

# # Pivot
# df_pivot = df.pivot_table(values='ENGMT_TYPE', index='IRECIPIENTID'
#                , columns='Engmt_Number', aggfunc='sum')

df_pivot1 = df.pivot_table(values='ENGMT_TYPE', index='IRECIPIENTID'
                           , aggfunc=lambda x: '|'.join(x))

# Filter for rows with more than 1 engagement
df_pivot2 = df_pivot1[df_pivot1['ENGMT_TYPE'].str.contains('\|')]
df_pivot_1eng = df_pivot1[~df_pivot1['ENGMT_TYPE'].str.contains('\|')]

# df_pivot.columns



# df = df.iloc[0:100000,:]
# config_path = path+'/Config/'
# input_path = path+'/Input/'

# try:
#     config_file = open(config_path + "/config.txt")
#     exec(config_file.read(), globals())
#     print('[SUCCESS] : Reading Config')
# except:
#     print('[ERROR]: Reading Config')

# # from doc3 import training_doc3
# # load ascii text and covert to lowercase
# file = input_path+filename
# raw_text = open(file, 'r', encoding='latin-1').read()
# lines = raw_text.split('\n')

# # Clean text
# df_lines = pd.DataFrame(lines, columns={'text'})
# df_lines['text'] = df_lines['text'].astype(str)
# df_lines = clean_column(df_lines, 'text')
# df_lines = df_lines[~df_lines['text'].isnull()]

lines = df_pivot2['ENGMT_TYPE'].values

# integer encode sequences of words
tokenizer = Tokenizer(split='|', filters='!"#$%&()*+./:;<=>?@[\\]^_`{}~\t\n'
                      , char_level=False) # remove special characters in the filter that form words
tokenizer.fit_on_texts(lines)
sequences = tokenizer.texts_to_sequences(lines)

# # create sequences
train_len = 21
# text_sequences = []
# for l in sequences:
#     for i in range(train_len, len(l)+1):
#         seq = l[i-train_len:i]
#         print(seq)
#         text_sequences.append(seq)

# Pad sequences
# text_sequences = pad_sequences(text_sequences, maxlen=train_len, truncating='pre')
text_sequences = pad_sequences(sequences, maxlen=train_len, truncating='pre')

# vocabulary size
vocab_size = len(tokenizer.word_index) + 1

# separate into input and output
text_sequences = array(text_sequences)
X, y = text_sequences[:, :-1], text_sequences[:, -1]

#Split dataset into train and test
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size = 0.2)
y_train = to_categorical(y_train, num_classes=vocab_size)
seq_length = X_train.shape[1]

# define model
batch_size = 128
epochs = 5
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
model.fit(X_train, y_train, batch_size=batch_size, epochs=epochs, verbose=1
          , validation_split=0.2
          , callbacks=[EarlyStopping(monitor='val_loss'
                                     , patience=3, min_delta=0.0001)])

# save the model to file
model.save(path+'Model/NBE_Model.h5')
# save the tokenizer
dump(tokenizer, open(path+'Model/NBE_Model.pkl', 'wb'))

# Test accuracy
y_test_dummy = to_categorical(y_test, num_classes=vocab_size)

score, acc = model.evaluate(X_test, y_test_dummy, batch_size=batch_size)
print('Test loss score:', score)
print('Test accuracy:', acc)

#####################

# load the model
model_name = 'Model/NBE_Model.h5'
model = load_model(path+model_name)

# load the tokenizer
tokenizer_name = 'Model/NBE_Model.pkl'
tokenizer = load(open(path+tokenizer_name, 'rb'))

# Labels to classes dictionary
labels_dict = tokenizer.index_word


# Test - get predictions
pred = model.predict(X_test)

# Get top recommendations
n=3
df_test = pd.DataFrame(y_test, columns=['Actual'])
df_test['recommendations'] = list(map(functools.partial(get_top_n_recommendations, n=n, tokenizer=tokenizer), pred))
df_test[['Reco1', 'Reco2', 'Reco3']] = pd.DataFrame(df_test['recommendations'].tolist(), index=df_test.index)
df_test['Actual_engagement'] = df_test["Actual"]
df_test["Actual_engagement"].replace(labels_dict, inplace=True)

# Get matches
df_test['Match'] = list(map(get_matches, df_test["Actual_engagement"],
                            df_test['Reco1'], df_test['Reco2'],
                            df_test['Reco3']))

X_test_df = pd.DataFrame(X_test)
df_test['engagements_count'] = 20-(X_test_df == 0).sum(axis=1)

df_test.reset_index(inplace=True)


# Get summary
df_summary = pd.DataFrame(df_test.groupby(["Actual_engagement", 'Match'])['Match'].count())
df_summary['%Match'] = df_summary['Match'] / df_summary.groupby('Actual_engagement')['Match'].transform('sum')
df_summary.to_csv(path+'Test_Summary.csv')

# Get summary for 1 engagement
df_test1 = df_test[df_test['engagements_count'] == 1]
df_summary = pd.DataFrame(df_test1.groupby(["Actual_engagement", 'Match'])['Match'].count())
df_summary['%Match'] = df_summary['Match'] / df_summary.groupby('Actual_engagement')['Match'].transform('sum')
df_summary.to_csv(path+'Test_Summary1.csv')




# Predict next engagement for users with 1 engagement
lines_test = df_pivot_1eng['ENGMT_TYPE'].values
encoded = tokenizer.texts_to_sequences(lines_test)
# truncate sequences to a fixed length
encoded_pad = pad_sequences(encoded, maxlen=seq_length, truncating='pre')

pred = model.predict(encoded_pad)

# Get top recommendations
n=3
df_pivot_1eng['recommendations'] = list(map(functools.partial(get_top_n_recommendations, n=n, tokenizer=tokenizer), pred))
df_pivot_1eng[['Reco1', 'Reco2', 'Reco3']] = pd.DataFrame(df_pivot_1eng['recommendations'].tolist(), index=df_pivot_1eng.index)
df_pivot_1eng.to_csv('1Engagement_Recommendations.csv')


# Check
test_eng = [0,0,2,1]
get_top_n_recommendations(model.predict(pad_sequences([test_eng], maxlen=seq_length, truncating='pre'))[0], n, tokenizer)

