import pandas as pd
import numpy as np
import datetime
from datetime import date
from tqdm import tqdm

from functools import partial
import multiprocessing as mp

import json
import os

import matplotlib.pyplot as plt
import matplotlib
import warnings

matplotlib.use('Agg')

if __name__ == '__main__':
    # path = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
    path = 'C:/Projects/MarketCatalyst/GDM_Curve_Proxy/Codes/CurveAggregation/'

    print(path)
    import sys
    sys.path.append(path)
    from src.helpers.DataLoader import DataLoader, CalculationEngine
    from src.helpers.curve_converter import CurveConverter
    from src.helpers.utilities import pandas_replace, logger, calc_adstock, calc_impact\
                                        , calc_adstock_flash, get_response_curve

    logger.info('========================== CURVE AGGREGATION ==========================')
    full = datetime.datetime.now()

    # path = 'C:/Projects/MarketCatalyst/GDM_Curve_Proxy/Codes/ProxyEstimation/'
    config_path = path+'/config/'
    time = str(date.today())

    # Load column names
    exec(open(config_path+'config.txt').read(), globals())

    # Create instance with all the filenames
    ETL = DataLoader({}, filepath_dict={"functionalforms": {"file": functionalform_file},
                                        "atl": {"file": atl_file},
                                        "btl": {"file": btl_file}
                                        }, logger=logger)
    # Import all the files
    ETL.import_files(lowercase=1)

    ETL.input_dict['atl'] = ETL.convert_date(ETL.input_dict['atl'], _week_col, _week_col, datetime=1)
    ETL.input_dict['btl'] = ETL.convert_date(ETL.input_dict['btl'], _week_col, _week_col, datetime=1)

    # get calendar
    # ETL.get_calendar(_week_col, _plan_period_col, _startdate_col, _enddate_col, _planperiodname_col)

    # Create
    CalcEngine = CalculationEngine(ETL.input_dict.copy(), logger)

    dimension_cols = [_geo_col, _brand_col, _subbrand_col, _state_col]
    r_dimension_cols = ["receiving" + s for s in dimension_cols]
    all_dimension_cols = dimension_cols + r_dimension_cols

    atl = CalcEngine.input_dict['atl'].copy()
    # btl = CalcEngine.input_dict['btl'].copy()
    # # atl = atl[0:10000]
    #
    # logger.info('Append atl and btl')
    # atl = atl.append(btl)

    logger.info('Sort the records based on dimensions and week')
    atl.sort_values(all_dimension_cols+[_instrument_col, _week_col, _instrument_grp_col], inplace=True)
    atl.reset_index(inplace=True, drop=True)
    # atl['geo_brand'] = atl[_geo_col] + ' | ' + atl[_brand_col]
    # atl[_agg_col] = atl['geo_brand'] + ' - ' + atl[_instrument_grp_col]

    atl_all = atl.copy()
    atl.drop_duplicates(subset=all_dimension_cols+[_instrument_grp_col], inplace=True)

    #########################################################################################
    #################################### AGGREGATION ########################################
    #########################################################################################

    atl_all_withAgg = atl_all.copy()
    del atl_all, atl, btl
    atl_all_withAgg[_stimuli_col+'_copy'] = atl_all_withAgg[_stimuli_col].copy()
    atl_all_withAgg[_spend_col+'_copy'] = atl_all_withAgg[_spend_col].copy()

    if len(atl_all_withAgg[np.isnan(atl_all_withAgg[_coeffA_col])]):
        logger.info('----- Combinations with zero spends are not aggregated.')

    converter = CurveConverter(ETL.input_dict['functionalforms'], atl_all_withAgg, coefficient_cols, logger)

    a = datetime.datetime.now()

    logger.info('====== Calculate response curves')
    ntimes = 50
    step = 2
    step2 = 25
    step3 = 100
    percentage_array = np.array(list(np.arange(10, 120, step) / 100) + list(np.arange(120, 100 * ntimes/10, step2) / 100) +
                                                                                    list(np.arange(100 * ntimes/10, 100 * ntimes
                                                                                      , step3) / 100))

    perc_df = pd.DataFrame(percentage_array, columns=["p"])
    perc_df['stimuli_ratio'] = np.arange(len(perc_df))
    perc_dict = pd.Series(perc_df["p"].values,index=perc_df['stimuli_ratio']).to_dict()

    # perc_df['key'] = 1
    # atl_all_withAgg['key'] = 1

    logger.info('====== Calculate adstock')
    append = 0
    flash = 1
    flash_mulitprocessing = 0
    if flash == 0:
        logger.info('------ METHOD1: Simple for loop')
        for p in tqdm(percentage_array):
            # p = 10
            # atl_all_withAgg_resp = atl_all_withAgg.merge(perc_df, how='left')
            # atl_all_withAgg_resp[_stimuli_col] = atl_all_withAgg_resp['p'] * atl_all_withAgg_resp[_stimuli_col + '_copy'].copy()

            atl_all_withAgg[_stimuli_col] = p * atl_all_withAgg[_stimuli_col + '_copy'].copy()
            atl_all_withAgg[_spend_col] = p * atl_all_withAgg[_spend_col + '_copy'].copy()
            # print(f'Time taken: {str(datetime.datetime.now() - a)}')

            atl_group_test = atl_all_withAgg.groupby(all_dimension_cols +
                                                 [_instrument_col, _instrument_grp_col]).apply(
                lambda x: calc_adstock(x, _stimuli_col, _carryover_col)).reset_index()
            # print(f'Time taken: {str(datetime.datetime.now() - a)}')

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
            # response_curve_sub['stimuli_ratio'] = p
            # print(f'Time taken: {str(datetime.datetime.now() - a)}')

            # Append
            if append == 0:
                final_response_curves = response_curve_sub.copy()
                append = 1
            else:
                final_response_curves = final_response_curves.append(response_curve_sub.copy())
        print(f'Time taken: {str(datetime.datetime.now() - a)}')
    else:
        logger.info('------ METHOD2: Array and grouped calculations')
        adstock_group_cols = all_dimension_cols + [_instrument_col, _instrument_grp_col]
        atl_all_withAgg['row'] = atl_all_withAgg.groupby(adstock_group_cols).cumcount()
        carryover_array_all = atl_all_withAgg.groupby(adstock_group_cols).apply(
            lambda x: calc_adstock_flash(x, percentage_array, _carryover_col, _stimuli_col, _lag_col)).reset_index()
        print(f'Time taken: {str(datetime.datetime.now() - a)}')

        logger.info('------ Rename level col')
        carryover_array_all.rename(columns={'level_' + str(len(adstock_group_cols)): 'row'}, inplace=True)

        logger.info('------ Merge adstock columns col')
        atl_group_test = atl_all_withAgg.merge(carryover_array_all, how='left')

        logger.info('------ Get percentage columns')
        perc_cols = set(atl_group_test.columns.tolist()) - set(atl_all_withAgg.columns.tolist())
        del atl_all_withAgg, carryover_array_all

        id_vars = [col for col in atl_group_test.columns.tolist() if col not in perc_cols]

        # atl_group_test_g = atl_group_test.groupby(dimension_cols + [_instrument_grp_col])
        atl_group_test_g = atl_group_test.groupby([_brand_col] + [_instrument_grp_col])
        # atl_group_test_g = atl_group_test.groupby([_state_col] + [_instrument_grp_col])
        append_group = 0
        if flash_mulitprocessing == 0:
            logger.info('------ Loop through groups')
            for group in tqdm(atl_group_test_g.groups.keys()):
                # group = ('Rockshore', 'ATL')
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
                del atl_group_sub[_stimuli_col + '_copy'], atl_group_sub[_spend_col + '_copy']
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
                # print(f'Time taken: {str(datetime.datetime.now() - a)}')

                # Append
                if append_group == 0:
                    final_response_curves = response_curve_sub.copy()
                    append_group = 1
                else:
                    final_response_curves = final_response_curves.append(response_curve_sub.copy())
        else:
            logger.info('------ Use multiprocessing for impact calculation and spend aggregation')
            func = partial(get_response_curve, atl_group_test_g, id_vars, perc_cols, perc_dict, pandas_replace, converter
                               , _stimuli_col, _spend_col, form_col, dimension_cols, _instrument_col,
                               _instrument_grp_col
                               , _week_col)
            am = datetime.datetime.now()

            # from joblib import Parallel, delayed
            # results = Parallel(n_jobs=6)((func)(group) for group in atl_group_test_g.groups.keys())

            with mp.Pool(mp.cpu_count() - 1) as pool:
                # results, comparison_df = pool.map(func, [i for i in range(len(curves_4[0:10]))])
                # multiple_results = [pool.apply_async(func, ()) for group in atl_group_test_g.groups.keys()]
                results = (pool.map(func, [group for group in atl_group_test_g.groups.keys()]))
                # pool.close()
                # pool.join()
            print(f'Time taken: {str(datetime.datetime.now() - am)}')

            for i in range(len(results)):
                if i == 0:
                    final_response_curves = results[i]
                else:
                    final_response_curves = final_response_curves.append(results[i])
        final_response_curves.reset_index(inplace=True, drop=True)
        print(f'Time taken: {str(datetime.datetime.now() - a)}')
        del atl_group_test

    final_response_curves_id = final_response_curves.groupby(dimension_cols + [_instrument_grp_col])[_stimuli_col].idxmin().reset_index()
    final_response_curves_id.rename(columns={_stimuli_col:'CurveID'}, inplace=True)
    final_response_curves = final_response_curves.merge(final_response_curves_id, how='left')
    final_response_curves[_coeffA_col] = 1
    final_response_curves[_coeffB_col] = 1
    final_response_curves[_coeffC_col] = 1


    new_coeffs_3_all = []
    message_all = []

    logger.info('====== Starting curve estimation')
    estimate_append = 0
    for id in tqdm(final_response_curves_id["CurveID"].unique()):
        # id=5800
        # key = atl_all_withAgg[_agg_col].unique()[id]
        final_response_curves_sub = final_response_curves[final_response_curves["CurveID"] == id]
        # logger.info('Executing for combination: ' + str(id))
        # atl_all_sub = atl_all_sub.loc[atl_all_sub[_stimuli_col] > 0.1, :]
        final_response_curves_sub.reset_index(inplace=True, drop=True)

        converter = CurveConverter(ETL.input_dict['functionalforms'], final_response_curves_sub, coefficient_cols, logger)
        converter.lower_case()

        converter.Y_actual = final_response_curves_sub['impact']
        # converter.adstock = final_response_curves_sub['stimuli']
        converter.adstock = final_response_curves_sub[_spend_col]   ## Use spend so media cost need not be used later

        coefficienta = converter.Y_actual.max() #atl_all_sub[_coeffA_col].mean()*len(atl_all_sub)
        coefficientb = 1
        coefficientc = converter.adstock.mean()#atl_all_sub[_coeffC_col].mean()*len(atl_all_sub) if atl_all_sub[_coeffC_col].mean() > 0 else 2000
        coeffs = np.array([coefficienta, coefficientb, coefficientc])
        if id==5800:
            print(coeffs)

        # logger.info(f'----- Roll up to yearly for curve {id}')
        converter.fix_b = 1
        if converter.fix_b == 1:
            coeffs = np.array([coefficienta, coefficientc])
            new_coeffs_list, diff_list = converter.minimize_options([coeffs.copy()], nobounds=1)
        else:
            new_coeffs_list, diff_list = converter.minimize_options([coeffs.copy()])
        new_coeffs_3 = new_coeffs_list[np.argmin(diff_list)]
        new_coeffs_3_all.append(new_coeffs_3)
        message_all.append('Success')

        if id == 5800:
            print(new_coeffs_3)

        converter.eval_adbudg(new_coeffs_3)
        comparison_df = pd.DataFrame(data=[converter.adstock, converter.Y_actual, converter.new_adbudg]).T
        comparison_df.columns = [_stimuli_col, 'Actual', 'New_Adbudg']
        comparison_df['CurveID'] = id

        if estimate_append == 0:
            comparison_df_final = comparison_df.copy()
            estimate_append = 1
        else:
            comparison_df_final = comparison_df_final.append(comparison_df.copy())

    final_curves_yearly = final_response_curves_id #pd.DataFrame(atl_all_withAgg[_agg_col].unique(), columns=[_agg_col])
    final_curves_yearly['New_Coeffs'] = new_coeffs_3_all
    final_curves_yearly['Message'] = message_all
    if converter.fix_b == 0:
        final_curves_yearly[[_coeffA_col, _coeffB_col, _coeffC_col]] = pd.DataFrame(final_curves_yearly['New_Coeffs'].tolist()
                                                                             , index=final_curves_yearly.index)
    else:
        final_curves_yearly[[_coeffA_col, _coeffC_col]] = pd.DataFrame(
            final_curves_yearly['New_Coeffs'].tolist()
            , index=final_curves_yearly.index)
        final_curves_yearly[_coeffB_col] = 1
    print(f'Time taken: {str(datetime.datetime.now() - a)}')


    logger.info('----- Get log for all bcvs')
    bus_inputs_messsage = final_curves_yearly[dimension_cols + [_instrument_grp_col, 'Message']]


    logger.info('----- Storing outputs to: ' + path + f'/Outputs/AggregatedCurves/')
    try:
        logger.info(f'Storing data with prefix {time} to ' + path + f'/Outputs/FinalOutputs/')
        bus_inputs_messsage.to_excel(path+f'\Outputs\AggregatedCurves\{time}_BCVs_Log.xlsx', index=False)
        comparison_df_final.to_excel(path+f'\Outputs\AggregatedCurves\{time}_Aggregated_Curves_comparison_df_final.xlsx', index=False)
        final_curves_yearly.to_excel(path+f'\Outputs\AggregatedCurves\{time}_Curves_Aggregated_Yearly.xlsx', index=False)
    except Exception as e:
        logger.info(f'Exception message: {str(e)}')
        logger.info('Exception: Storing outputs to: ' + path + f'/Outputs/FinalOutputs/')



    logger.info('----- Plotting curves')
    try:
        warnings.filterwarnings("ignore")
        for curve_row in comparison_df_final['CurveID'].unique():
            comparison_df = comparison_df_final[comparison_df_final['CurveID']==curve_row]

            curve_name = 'ADBUDG'
            bcv_curve = '-'.join(final_response_curves_id.loc[final_response_curves_id['CurveID']==curve_row, dimension_cols].values[0])

            x = comparison_df['stimuli']
            actual = comparison_df['Actual']
            converted_adbudg = comparison_df['New_Adbudg']

            figure = plt.figure()

            plt.plot(x, actual)
            plt.plot(x, converted_adbudg)

            plt.legend([f'Actual_{curve_name}', 'Converted_ADBUDG'], loc='lower right')
            plt.savefig(path+f'\Outputs\AggregatedCurves\Plots\{curve_row}_{str(bcv_curve)}.png')

        logger.info('Success: Storing plots to to: ' + path + f'/Outputs/AggregatedCurves/Plots/')
    except Exception as e:
        logger.info(f'Exception message: {str(e)}')
        logger.info('Exception: Storing plots to to: ' + path + f'/Outputs/AggregatedCurves/Plots/')

    print(f'Total Time taken: {str(datetime.datetime.now() - full)}')
