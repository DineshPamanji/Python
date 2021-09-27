import logging
from logging.handlers import RotatingFileHandler
import datetime
import numpy as np
import pandas as pd
from numba import jit
import numba


path = 'C:/Projects/MarketCatalyst/GDM_Curve_Proxy/Codes/CurveAggregation/'
config_path = path+'/config/'


exec(open(config_path+'config.txt').read(), globals())


prefix="Execution"
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(prefix)
log_path = path+'/Logs/'
log_size = 10000*1024*1024 # in Bytes
handler = RotatingFileHandler(log_path + 'Log_file_' + str(datetime.datetime.now().strftime('%Y-%m-%d-%H%M%S')) + '.log'
                            , maxBytes=log_size, backupCount=100)
logger.addHandler(handler)

@numba.njit
def flash_shift(arr, num, fill_value=np.nan):
    if num > 0:
        return np.concatenate((np.full(num, fill_value), arr[:-num]))
    elif num == 0:
        return arr
    else:
        return np.concatenate((arr[-num:], np.full(-num, fill_value)))

def calc_impact(atl_all_withAgg, calc_adstock, _spend_col, _stimuli_col, _carryover_col, all_dimension_cols, dimension_cols
                , _instrument_col, _instrument_grp_col, _week_col, form_col, _lag_col, converter, p):

    atl_all_withAgg[_stimuli_col] = p * atl_all_withAgg[_stimuli_col + '_copy'].copy()
    atl_all_withAgg[_spend_col] = p * atl_all_withAgg[_spend_col + '_copy'].copy()
    # print(f'Time taken: {str(datetime.datetime.now() - a)}')

    atl_group_test = atl_all_withAgg.groupby(all_dimension_cols +
                                             [_instrument_col, _instrument_grp_col]).apply(
        lambda x: calc_adstock(x, _stimuli_col, _carryover_col)).reset_index()
    # print(f'Time taken: {str(datetime.datetime.now() - a)}')

    # Apply lag
    atl_group_test['adstock'] = (atl_group_test.groupby(all_dimension_cols +
                                                        [_instrument_col, _instrument_grp_col]).apply(
        lambda x: x['adstock'].shift(x[_lag_col].values[0]))).values

    atl_group_test['adstock'].fillna(0, inplace=True)

    # Calculate impact
    atl_group_test = converter.eval_curve_multi(atl_group_test, 'impact', form_col)
    # print(f'Time taken: {str(datetime.datetime.now() - a)}')

    # Aggregate impact
    response_curve_sub = atl_group_test.groupby(dimension_cols + [_instrument_grp_col])[
        'impact'].sum().reset_index()
    # print(f'Time taken: {str(datetime.datetime.now() - a)}')

    # Get stimuli - remove synergy stimuli
    stimuli_sub = atl_group_test.drop_duplicates(subset=dimension_cols + [_instrument_col, _instrument_grp_col
        , _week_col]).groupby(dimension_cols + [_instrument_grp_col])[[_spend_col, _stimuli_col]].sum().reset_index()
    # print(f'Time taken: {str(datetime.datetime.now() - a)}')

    # Join stimuli and impact
    response_curve_sub = response_curve_sub.merge(stimuli_sub, how='left')
    response_curve_sub['stimuli_ratio'] = p
    return response_curve_sub


def get_response_curve(atl_group_test_g, id_vars, perc_cols, perc_dict, pandas_replace, converter
                       , _stimuli_col, _spend_col, form_col, dimension_cols, _instrument_col, _instrument_grp_col
                       , _week_col, group):
    atl_group_sub = atl_group_test_g.get_group(group)
    atl_group_sub = pd.melt(atl_group_sub, id_vars=id_vars, value_name='adstock', var_name='stimuli_ratio'
                            , value_vars=perc_cols)

    # logger.info('------ Get percentage')
    atl_group_sub['p'] = atl_group_sub['stimuli_ratio'].copy()
    atl_group_sub = pandas_replace(atl_group_sub, perc_dict, ['p'], verbose=0)
    # print(f'Time taken: {str(datetime.datetime.now() - a)}')

    # atl_group_test = atl_group_test.merge(perc_df, on='stimuli_ratio', how='left')
    # print(f'Time taken: {str(datetime.datetime.now() - a)}')

    # logger.info('------ Scale spend and stimuli')
    atl_group_sub[_stimuli_col] = atl_group_sub['p'] * atl_group_sub[_stimuli_col + '_copy'].copy()
    atl_group_sub[_spend_col] = atl_group_sub['p'] * atl_group_sub[_spend_col + '_copy'].copy()
    # del atl_group_sub[_stimuli_col + '_copy'], atl_group_sub[_spend_col + '_copy']
    # print(f'Time taken: {str(datetime.datetime.now() - a)}')

    # atl_lag = atl_group_test[atl_group_test['lag']!=0]

    # logger.info('------ Calculate impact')
    atl_group_sub['impact'] = pd.Series(dtype='float')
    for form in atl_group_sub[form_col].unique():
        eq = converter.get_curve_equation(form)
        form_index = atl_group_sub[form_col] == form
        atl_group_sub.loc[form_index, 'impact'] = atl_group_sub.loc[form_index, :].eval(eq)
    # print(f'Time taken: {str(datetime.datetime.now() - a)}')

    # logger.info('------ Aggregate impact')
    response_curve_sub = atl_group_sub.groupby(dimension_cols + [_instrument_grp_col, 'stimuli_ratio'])[
        'impact'].sum().reset_index()
    # print(f'Time taken: {str(datetime.datetime.now() - a)}')

    # logger.info('------ Get stimuli - remove synergy stimuli')
    stimuli_sub = atl_group_sub.drop_duplicates(subset=dimension_cols + [_instrument_col, _instrument_grp_col
        , _week_col, 'stimuli_ratio']).groupby(dimension_cols + [_instrument_grp_col, 'stimuli_ratio'])[
        [_spend_col, _stimuli_col]].sum().reset_index()
    # print(f'Time taken: {str(datetime.datetime.now() - a)}')

    # logger.info('------ Join stimuli and impact')
    # final_response_curves = response_curve_sub.merge(stimuli_sub, how='left')
    response_curve_sub = response_curve_sub.merge(stimuli_sub, how='left')
    return response_curve_sub

@jit(nopython=True)
def exponential_sum(value, carryover):
    """
    :param value: series, values to be added to adstock
    :param carryover: carryover factor
    :return:
    """
    total = value[0]
    yield total
    for i in range(1, len(value)):
        total = (total * carryover[i]) + value[i]
        yield total

def calc_adstock_flash(df, percentage_array, _carryover_col, _stimuli_col, _lag_col):
    nrows = len(df[_stimuli_col])
    nsteps = len(percentage_array)
    lag = df[_lag_col].values[0]
    arr1 = df[_stimuli_col].to_numpy()
    arr1 = arr1.reshape((nrows, 1))
    percentage_array = percentage_array.reshape((nsteps, 1))
    stimuli_steps = np.dot(arr1, percentage_array.T)

    decomp_array = np.zeros((nrows, nrows))
    decomp_array[0] = np.power(df[_carryover_col], df['row'])
    for i in range(1, nrows):
        decomp_array[i] = flash_shift(decomp_array[0], i, fill_value=0)

    rollover_array = np.zeros((nrows, nsteps))
    for i in range(nsteps):
        rollover_array[:, i] = np.dot(stimuli_steps[:, i], decomp_array)
        rollover_array[:, i] = flash_shift(rollover_array[:, i], lag, fill_value=0)
    return pd.DataFrame(rollover_array)


def calc_adstock(df, valuecol, weight_col):
    df['adstock'] = list(exponential_sum(df[valuecol].values, df[weight_col].values))
    return df

def calc_adstock1(df, valuecol, weight_col):
    df['adstock'] = list(exponential_sum(df[valuecol].values, df[weight_col].values))
    return df['adstock']

def pandas_replace(df, replaceDict, additional_cols=[], verbose=1):
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

    for c in range(len(columns)):
        if verbose == 1:
            print('Replacing column:', columns[c])
        df[columns[c]] = df[columns[c]].map(replaceDict).fillna(df[columns[c]])

    # Fill na with empty string
    df.fillna("", inplace=True)

    return df