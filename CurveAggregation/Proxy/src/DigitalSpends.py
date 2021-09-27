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
# path = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
path = 'C:/Projects/MarketCatalyst/GDM_Curve_Proxy/Codes/ProxyEstimation/'

print(path)
import sys
sys.path.append(path)
from src.helpers.DataLoader import DataLoader, CalculationEngine
from src.helpers.curve_converter import CurveConverter
from src.helpers.utilities import pandas_replace, logger, merge_all_files

logger.info('========================== Spends Distribution ==========================')
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
spend_media_sub = CalcEngine.spend_media
spend_media_sub = pd.merge(spend_media_sub, CalcEngine.input_dict['Calendar'], on=_week_col, how='left')
spend_media_sub = spend_media_sub[spend_media_sub[_plan_period_col] == plan_period]

# Get spend by Geo-Instrument
geo_instr = spend_media_sub.groupby([_geo_col, _instrument_col])[_spend_col].sum()

# Get percentage
geo_instr_pcts = geo_instr.groupby(level=0).apply(lambda x: 100 * x / float(x.sum())).reset_index()
geo_instr_pcts.rename(columns={'spend': 'spend_pct'}, inplace=True)
geo_instr = geo_instr.reset_index().merge(geo_instr_pcts, how='left')

# Save
time_prefix = time + "_" + _region_selected + additional_prefix
geo_instr.to_csv(path+f'\Outputs\SpendDistribution\{time_prefix}_SpendDistribution.csv', index=False)

merge=0
if merge:
    final_distribution = merge_all_files(path+f"/Outputs/SpendDistribution/", "SpendDistribution.csv")
    final_distribution.to_csv(path + f'\Outputs\SpendDistribution\{time}_SpendDistribution_ALL.csv', index=False)



