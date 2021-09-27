# Import packages
import pandas as pd
import numpy as np
import json
import matplotlib.pyplot as plt
from datetime import date
import datetime

from functools import partial
import multiprocessing as mp


import os
path = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
# path = 'C:/Projects/MarketCatalyst/GDM_Curve_Proxy/Codes/ProxyEstimation/'

print(path)
import sys
sys.path.append(path)
from src.helpers.DataLoader import DataLoader, CalculationEngine
from src.helpers.curve_converter import CurveConverter
from src.helpers.utilities import pandas_replace, logger

if __name__ == '__main__':
    logger.info('========================== Non ADBUDG to ADBUDG ==========================')
    # path = 'C:/Projects/MarketCatalyst/GDM_Curve_Proxy/Codes/ProxyEstimation/'
    config_path = path+'/config/'
    time = str(date.today())

    # Load column names
    exec(open(config_path+'config.txt').read(), globals())

    # curves_file = input_path + "/ConsolidatedCurves/Consolidated Curves with Rating sub.csv"

    # Create instance with all the filenames
    ETL = DataLoader({}, filepath_dict={"functionalforms": {"file": functionalform_file},
                                        "curves": {"file": curves_consolidated_file},
                                        "spend": {"file": spends_file},
                                        "media": {"file": media_file},
                                        # "finance": {"file": finance_file},
                                        # "Calendar": {"file": calendar_file},
                                        "calendar_master": {"file": calendar_file},
                                        # "country_codes": {"file": country_code_file},
                                        # "bus_inputs": {"file": bus_inputs_file, "sheet": "Inputs"},
                                        # "geo_master": {"file": geo_file, "sheet": "Finance"},
                                        # "currency_conversion": {"file": currency_conversion_file},
                                        # "market_currency": {"file": market_currency_file},
                                        "taxonomy": {"file": taxonomy_file, "sheet": "Taxonomy changes"}
                                        }, logger=logger)
    # Import all the files
    ETL.import_files(lowercase=1)

    if align_taxonomy:
        logger.info('------ Rename according to taxonomy')
        ETL.align_taxonomy(verbose=0)

    # get calendar
    ETL.get_calendar(_week_col, _plan_period_col, _startdate_col, _enddate_col, _planperiodname_col)

    # Rename india
    ETL.input_dict['media'] = pandas_replace(ETL.input_dict['media'], {'India': 'IND'}, additional_cols=[_geo_col], verbose=1)
    ETL.input_dict['spend'] = pandas_replace(ETL.input_dict['spend'], {'India': 'IND'}, additional_cols=[_geo_col], verbose=1)
    ETL.input_dict['curves'] = pandas_replace(ETL.input_dict['curves'], {'India': 'IND'}, additional_cols=[_geo_col], verbose=1)

    # ETL.input_dict['curves'][_rating_col] = 4
    curves_4 = ETL.get_relevant_curves(_coeffA_col, _coeffB_col, _coeffC_col, form_col, _rating_col)
    filter_df = curves_4.loc[:, [_geo_col, _brand_col, _instrument_col]].drop_duplicates()
    ETL.keep_relevant_data(filter_df)

    curves = ETL.input_dict['curves']
    spend = ETL.input_dict['spend']
    Calendar = ETL.input_dict['Calendar']
    media_cost = ETL.input_dict['media']

    logger.info('====== Initiate calculation engine')
    CalcEngine = CalculationEngine(ETL.input_dict.copy(), logger=logger)

    logger.info('----- Convert date format of spend and media week columns')
    CalcEngine.input_dict['spend'] = CalcEngine.convert_date(CalcEngine.input_dict['spend'], _week_col, _week_col)
    CalcEngine.input_dict['media'] = CalcEngine.convert_date(CalcEngine.input_dict['media'], _week_col, _week_col)

    logger.info('------ Join media and spend')
    CalcEngine.calculate_stimuli(spend_cols, media_cols, _stimuli_col, _spend_col, _cost_col)

    logger.info('------ Calculate spend mean, min and max')
    CalcEngine.get_spend_mean(_spend_col, _geo_col, _brand_col, _instrument_col, _week_col
                           ,_plan_period_col, plan_period)
    ################################################################################
    ################################################################################
    # Get mean investment
    curves_4 = curves_4.merge(CalcEngine.spend_media_sub_min_max, how='left')
    curves_4[_spend_mean_col].fillna(10000, inplace=True)
    curves_4.loc[curves_4[_spend_mean_col] == 0, _spend_mean_col] = 10000

    logger.info('====== Initiate Curve Converter')
    converter = CurveConverter(CalcEngine.input_dict['functionalforms'], curves_4, coefficient_cols, logger=logger)
    converter.lower_case()

    logger.info('------ Initiate coefficients for optimization')
    coefficienta = 2000
    coefficientb = 1
    coefficientc = 1600
    coeffs = np.array([coefficienta, coefficientb, coefficientc])
    # coeffs = np.array([coefficienta, coefficientc])


    a = datetime.datetime.now()
    comparison_df_final = pd.DataFrame()

    if use_multiprocessing:
        logger.info('====== Multiprocessing all curves')
        func = partial(converter.convert_curve, curves_4, coeffs, form_col, _spend_mean_col, _stimuli_col
                       , _stimuli_type_col)
        a = datetime.datetime.now()
        # if __name__ == "__main__":
        with mp.Pool(mp.cpu_count() - 1) as pool:
            # results, comparison_df = pool.map(func, [i for i in range(len(curves_4[0:10]))])
            results = (pool.map(func, [i for i in range(len(curves_4))]))

        # Multiprocessing group of rows
        # func = partial(converter.convert_curve_group, curves_4, coeffs, form_col, _spend_mean_col, _stimuli_col)
        # a = datetime.datetime.now()
        # batch_size = 100
        # batch_num = int(len(curves_4[form_col]) / batch_size)
        # batch_size = len(curves_4[form_col]) / batch_num
        # with mp.Pool(mp.cpu_count() - 1) as pool:
        #     results = pool.starmap(func, [(int(i*batch_size), int((i+1)*batch_size)) for i in range(batch_num)])
        # print("Time Taken: " + str(datetime.datetime.now()-a))
        # results1 = []
        # for list in results:
        #     results1 = [item for sublist in results for item in sublist]

        logger.info('------ Process the results from multiprocessing')
        try:
            for i in range(len(results)):
                if i == 0:
                    final_coeffs = results[i][0]
                    comparison_df_final = results[i][1]
                else:
                    final_coeffs = final_coeffs.append(results[i][0])
                    comparison_df_final = comparison_df_final.append(results[i][1])
            logger.info(f'SUCCESS: Results processing')
        except Exception as e:
            logger.info(f'EXCEPTION: Results processing')
            logger.info(f'Exception message: {str(e)}')
    else:
        try:
            for i in range(len(curves_4)):
                # i =17
                new_coeffs, comparison_df = converter.convert_curve(curves_4, coeffs, form_col,  _spend_mean_col
                                                                    , _stimuli_col, _stimuli_type_col, curve_row=i)
                if i == 0:
                    final_coeffs = new_coeffs.copy()
                    comparison_df_final = comparison_df.copy()
                else:
                    final_coeffs = final_coeffs.append(new_coeffs.copy())
                    comparison_df_final = comparison_df_final.append(comparison_df.copy())
            logger.info(f'SUCCESS: Curve conversion')
        except Exception as e:
            logger.info(f'EXCEPTION: Curve conversion')
            logger.info(f'Exception message: {str(e)}')
    print("Time Taken: " + str(datetime.datetime.now()-a))

    final_coeffs.columns = ["coefficienta_ad", "coefficientb_ad", "coefficientc_ad"]
    final_coeffs.reset_index(inplace=True, drop=True)

    del curves_4[_spend_mean_col]
    curves_4_final = pd.concat([curves_4, final_coeffs], axis=1)
    # warnings.filterwarnings("ignore")

    logger.info('------ MAPE Calculation')
    comparison_df = comparison_df_final
    comparison_df['MAPE2'] = (((comparison_df.Actual-comparison_df.New_Adbudg).abs())/(comparison_df.Actual))

    comparison_df_final_mape = pd.DataFrame(comparison_df.groupby(['CurveID'])['MAPE2'].mean())
    comparison_df_final_mape['MAX'] = comparison_df.groupby(['CurveID'])['Actual'].max()
    comparison_df_final_mape['Accuracy'] = 1-comparison_df_final_mape['MAPE2']
    comparison_df_final_mape.reset_index(inplace=True)
    # comparison_df_final_mape['MAPE2'].max()

    figure = plt.figure()

    # TODO make the max impact threshold and accuracy threshold configurable
    comparison_df_final_mape[(comparison_df_final_mape['MAX'] > 150)]['Accuracy'].hist()
    comparison_df_final_mape['Accuracy'].hist()
    bad_curves_list = list(comparison_df_final_mape[(comparison_df_final_mape['Accuracy'] < 0.9) &
                           (comparison_df_final_mape['MAX'] > 10)]['CurveID'])

    logger.info('------ Remove bad curves and store in a separate file')
    curves_4_final.reset_index(inplace=True)
    curves_4_final.rename(columns={'index':'CurveID'}, inplace=True)
    curves_4_final_good = curves_4_final[~curves_4_final['CurveID'].isin(bad_curves_list)]
    curves_4_final_bad = curves_4_final[curves_4_final['CurveID'].isin(bad_curves_list)]

    # Store output
    time = time+"_"+_region_selected+additional_prefix
    logger.info(f'Storing data with prefix {time} to ' + path + '/Outputs/ConvertedCurves/')
    plt.savefig(path + f'\Outputs\ConvertedCurves\Plots\{time}_CurveConversion_Accuracy.png')
    curves_4_final_good.to_excel(path+f'\Outputs\ConvertedCurves\{time}_Curves_converted_vf.xlsx', index=False)
    curves_4_final_bad.to_excel(path+f'\Outputs\ConvertedCurves\{time}_Curves_converted_bad.xlsx', index=False)
    comparison_df_final_mape.to_excel(path+f'\Outputs\ConvertedCurves\{time}_Comparison_df_final_mape.xlsx', index=False)
    comparison_df_final.to_excel(path+f'\Outputs\ConvertedCurves\{time}_Converted_Curves_comparison_df_final.xlsx', index=False)
