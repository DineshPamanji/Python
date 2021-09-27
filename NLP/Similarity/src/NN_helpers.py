from keras.preprocessing.text import Tokenizer
from keras.preprocessing.sequence import pad_sequences
from keras.models import Sequential
from keras.layers import Dense, Embedding, LSTM, SpatialDropout1D, GRU
from keras.layers.convolutional import Conv1D
from keras.layers.convolutional import MaxPooling1D
from keras.layers import Dropout
import pandas as pd

def prepare_tokenizer(df, column, MAX_NB_WORDS):
    """
    Fits a tokenizer on the input column with job titles

    Parameters
    ----------
    df: pandas,
        data frame with job title column
    column: str,
        name of the column job title
    MAX_NB_WORDS: int,
        maximum number of words to be used in creating tokens

    Returns
    ----------
    :return: tokenizer
    """
    tokenizer = Tokenizer(num_words=MAX_NB_WORDS, filters='!"#$%&()*+,-./:;<=>?@[\]^_`{|}~', lower=True)
    tokenizer.fit_on_texts(df[column].values)
    word_index = tokenizer.word_index
    print('Found %s unique tokens.' % len(word_index))
    return tokenizer


def prepare_X(tokenizer, df, column, MAX_SEQUENCE_LENGTH):
    """
    Gives numpy array of X - (features) that are converted to sequences and padded with zeroes in the front

    Parameters
    ----------
    tokenizer: tokenizer
        tokenizer fit on the job titles
    df: pandas
        data frame with job title column
    column: str
        name of the column with job title
    MAX_SEQUENCE_LENGTH: int
        maximum number of words to be used in the model training

    Returns
    -------
    X: ndarray
        contains the features based on tokens and padded
    """
    X = tokenizer.texts_to_sequences(df[column].values)
    # tokenizer.texts_to_sequences("test sequence")
    # Pad with zeroes
    X = pad_sequences(X, maxlen=MAX_SEQUENCE_LENGTH)
    print('Shape of data tensor:', X.shape)
    return X


# Define an RNN
def create_model(X, labels_count, MAX_NB_WORDS, EMBEDDING_DIM):
    """
    Define RNN model using LSTM.

    Parameters
    ----------
    X: ndarray,
        contains the features based on tokens and padded
    labels_count: int,
        number of labels - to be used in final layer
    MAX_NB_WORDS: int,
        maximum number of words to be used in creating tokens
    EMBEDDING_DIM: int,
        number of dimensions to which the sequences are reduced to in the first layer

    Returns
    ----------
    model: keras model,
        model with LSTM
    """
    # Build model
    model = Sequential()
    model.add(Embedding(MAX_NB_WORDS, EMBEDDING_DIM, input_length=X.shape[1]))
    # model.add(Embedding(MAX_NB_WORDS, EMBEDDING_DIM, weights=[embedding_matrix], input_length=X.shape[1]))
    model.add(SpatialDropout1D(0.2))
    # model.add(Conv1D(filters=32, kernel_size=3, padding='same', activation='relu'))
    # model.add(MaxPooling1D(pool_size=2))
    # model.add(LSTM(100, return_sequences=True, dropout=0.2, recurrent_dropout=0.15))
    # model.add(LSTM(100, return_sequences=True, dropout=0.2, recurrent_dropout=0.15))
    model.add(LSTM(100, dropout=0.2, recurrent_dropout=0.2))
    # # model.add(GRU(32, dropout=0.2, recurrent_dropout=0.2))
    # model.add(Dense(64))
    # model.add(Dropout(rate=0.25))
    # model.add(Dense(16))
    # model.add(Dropout(rate=0.25))
    model.add(Dense(labels_count, activation='softmax'))
    # model.add(Dense(labels_count, activation='sigmoid'))  # Not recommended if you have more than two classes
    model.compile(loss='categorical_crossentropy', optimizer='adam', metrics=['accuracy'])
    return model

def get_predictions(model, X, labels, df, drop_labels=1):
    """
    Gives probabilities for each label and final prediction on new data

    Parameters
    ----------
    model: keras model,
        model that is trained and fit
    X: pandas,
        data frame with features from new data to be predicted
    labels: list,
        contains names of the labels
    df: pandas,
        data frame with all details to which prediction and probabilities are to be appended

    Returns
    ----------
    df: pandas, data frame with all details to which prediction and probabilities are appended
    """
    # Get probabilities for all X
    probabilities = model.predict(X)
    probabilities_df = pd.DataFrame(probabilities, columns=labels)
    df = pd.merge(df, probabilities_df, left_index=True, right_index=True)
    df['Max_Proba'] = df[labels].max(axis=1)
    df['Predict'] = df[labels].idxmax(axis=1)
    if drop_labels == 1:
        df.drop(labels, axis=1, inplace=True)
    return df
