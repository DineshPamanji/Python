import logging
from logging.handlers import RotatingFileHandler
import datetime
import pandas as pd

import os
# path = 'C:/Projects/MarketCatalyst/GDM_Curve_Proxy/Codes/ProxyEstimation/'
path = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

config_path = path+'/config/'
input_path = path+'Input/'
spend_path = path+'Input/Consolidated/'

exec(open(config_path+'config.txt').read(), globals())


prefix="Execution"
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(prefix)
log_path = path+'/Logs/'
log_size = 10000*1024*1024 # in Bytes
handler = RotatingFileHandler(log_path + 'Log_file_' + str(datetime.datetime.now().strftime('%Y-%m-%d-%H%M%S')) + '.log'
                            , maxBytes=log_size, backupCount=100)
logger.addHandler(handler)



def pandas_replace(df, replaceDict, additional_cols=[], anywhere=0, verbose=1):
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
    if len(additional_cols) > 0:
        columns = additional_cols
    else:
        columns = df.columns.tolist()

    if anywhere:
        # replaceDict = {r"\b{}\b".format(k): v for k, v in replaceDict.items()}
        replaceDict = {r"- {}$".format(k): r"- {}".format(v) for k, v in replaceDict.items()}

    for c in range(len(columns)):
        if verbose == 1:
            print('Replacing column:', columns[c])
        if anywhere:
            # replaceDict = {r"\b{}\b".format(k): v for k, v in replaceDict.items()}
            df[columns[c]] = df[columns[c]].replace(replaceDict, regex=True)
        else:
            df[columns[c]] = df[columns[c]].map(replaceDict).fillna(df[columns[c]])

    # Fill na with empty string
    df.fillna("", inplace=True)

    return df


def force_float(df, columns):
    """
    Forces all the columns to float
    :param df: pandas, dataframe to be formatted
    :param columns: list, list of columns to be formatted
    :return: pandas, df with formatted columns
    """
    for colname in columns:
        df[colname] = df[colname].astype('float')
    return df


def merge_all_files(folder, endswith):
    """
    Given a folder and file ending, all the files are appended to create on single df
    :param folder: str, path name
    :param endswith: str, suffix of the files to be appended. Make sure only the files needed are present
    :return: pandas, df with all the appended data
    """
    #folder = "C:/Projects/MarketCatalyst/GDM_Curve_Proxy/Codes/ProxyEstimation/Outputs/ConvertedCurves/"
    # endswith="Curves_converted_vf.xlsx"
    first = 1
    for file in os.listdir(folder):
        if file.endswith(endswith):
            print(file)
            try:
                df = pd.read_excel(folder+file)
            except:
                df = pd.read_csv(folder + file)
            if first:
                df_final = df.copy()
                first = 0
            else:
                df_final = df_final.append(df.copy())
    df_final.drop_duplicates(inplace=True)
    df_final.reset_index(inplace=True, drop=True)
    return df_final