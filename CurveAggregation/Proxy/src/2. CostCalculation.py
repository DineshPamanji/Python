import pandas as pd
import numpy as np
import datetime
from datetime import date
import json
# from kneed import KneeLocator
import os
path = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
# path = 'C:/Projects/MarketCatalyst/GDM_Curve_Proxy/Codes/ProxyEstimation/'

print(path)
import sys
sys.path.append(path)
from src.helpers.DataLoader import DataLoader, CalculationEngine
from src.helpers.curve_converter import CurveConverter
from src.helpers.utilities import pandas_replace, logger

# path = 'C:/Projects/MarketCatalyst/GDM_Curve_Proxy/Codes/ProxyEstimation/'
config_path = path+'/config/'
time = str(date.today())

logger.info('========================== Cost Calculation ==========================')
# Load column names
exec(open(config_path+'config.txt').read(), globals())

# curves_file = input_path + "/ConsolidatedCurves/Consolidated Curves with Rating.csv"
# bus_inputs_file = config_path+'ME/AdditionalData/Diageo Catalyst - Proxy Curves - Additional Inputs v01.xlsx'
# Create instance with all the filenames
ETL = DataLoader({}, filepath_dict={"functionalforms": {"file": functionalform_file},
                                    "curves": {"file": curves_conv_file},
                                    "spend": {"file": spends_file},
                                    "media": {"file": media_file},
                                    "finance": {"file": finance_file},
                                    "calendar_master": {"file": calendar_file},
                                    # "country_codes": {"file": country_code_file},
                                    "bus_inputs": {"file": bus_inputs_file, "sheet": "Inputs"},
                                    "geo_master": {"file": geo_file, "sheet": "Finance"},
                                    "currency_conversion": {"file": currency_conversion_file},
                                    "market_currency": {"file": market_currency_file},
                                    "taxonomy": {"file": taxonomy_file, "sheet": "Taxonomy changes"},
                                    "instrument_taxonomy": {"file": instrument_taxonomy_file, "sheet": "ALL"}

                                    }, logger=logger)

# Import all the files
ETL.import_files(lowercase=1)

logger.info('------ Rename instruments according to taxonomy')
if instrument_taxonomy:
    ETL.inst_taxonomy_dict = {s: a for s, a in zip(ETL.input_dict['instrument_taxonomy']['old'], ETL.input_dict['instrument_taxonomy']['new'])}
    ETL.input_dict['bus_inputs'] = pandas_replace(ETL.input_dict['bus_inputs'], ETL.inst_taxonomy_dict
                                                  , additional_cols=['vehicle']
                                                  ,anywhere=0, verbose=1)
    ETL.input_dict['bus_inputs'] = pandas_replace(ETL.input_dict['bus_inputs'], ETL.inst_taxonomy_dict
                                                  , additional_cols=['bcv', 'selection', 'aggregation']
                                                  , anywhere=1, verbose=1)
# ETL.input_dict['bus_inputs'].to_excel(config_path + "ME/Catalyst - Proxy Curves - Input template FINAL TAXONOMY.xlsx", index=False)

if align_taxonomy:
    logger.info('------ Rename according to taxonomy')
    ETL.align_taxonomy(verbose=0)

# get calendar
ETL.get_calendar(_week_col, _plan_period_col, _startdate_col, _enddate_col, _planperiodname_col)

# Rename india
ETL.input_dict['media'] = pandas_replace(ETL.input_dict['media'], {'India': 'IND'}, additional_cols=[_geo_col], verbose=1)
ETL.input_dict['spend'] = pandas_replace(ETL.input_dict['spend'], {'India': 'IND'}, additional_cols=[_geo_col], verbose=1)


curves = ETL.input_dict['curves']
finance = ETL.input_dict['finance']
bus_inputs = ETL.input_dict['bus_inputs']
geo_master = ETL.input_dict['geo_master']
currency_conversion = ETL.input_dict['currency_conversion']
market_currency = ETL.input_dict['market_currency']


bus_inputs.rename(columns={_country_col:_geo_col, _vehicle_col:_instrument_col}, inplace=True)

ETL.filter_currency(_source_currency_col, _target_currency_symbol_col
                                                              , _conversion_factor_col, target_currency='GBP')
currency_conversion_GBP = ETL.input_dict['currency_conversion_sub']
# currency_conversion_GBP = currency_conversion.loc[currency_conversion[_target_currency_symbol_col]=='GBP'
#                                                 ,[_source_currency_col, _target_currency_symbol_col
#                                                         , _conversion_factor_col]].drop_duplicates()

# Subset for 4-star rating curves
curves_4 = curves.copy()
filter_df = curves_4.loc[:, [_geo_col, _brand_col, _instrument_col]].drop_duplicates()
ETL.keep_relevant_data(filter_df)

# bus_inputs.rename(columns={_country_col:_geo_col, _vehicle_col:_instrument_col}, inplace=True)

finance = finance[(finance[_baseline_col] == "Moderate") & (finance[_time_period_col] == 'Full')]
finance.reset_index(inplace=True, drop=True)

# Create
CalcEngine = CalculationEngine(ETL.input_dict.copy(), logger=logger)

# Get
logger.info('----- Add geography column to finance data')
CalcEngine.input_dict['finance'] = finance.copy()
CalcEngine.input_dict['finance'] = CalcEngine.input_dict['finance'].merge(CalcEngine.input_dict['geo_master'][[_geo_name_col, _geo_col]].drop_duplicates()
                                              ,left_on=_country_col, right_on=_geo_name_col, how='left')
CalcEngine.input_dict['finance'].dropna(subset=[_geo_col], inplace=True)

logger.info('----- Convert date format of spend and media week columns')
CalcEngine.input_dict['spend'] = CalcEngine.convert_date(CalcEngine.input_dict['spend'], _week_col, _week_col)
CalcEngine.input_dict['media'] = CalcEngine.convert_date(CalcEngine.input_dict['media'], _week_col, _week_col)

# Join media and spend
logger.info('----- Join media and spend data')
CalcEngine.calculate_stimuli(spend_cols, media_cols, _stimuli_col, _spend_col, _cost_col)

# Get min max across years
logger.info('----- Calculate spend mean, min, max')
CalcEngine.get_spend_mean(_spend_col, _geo_col, _brand_col, _instrument_col, _week_col
                       ,_plan_period_col, plan_period)

# Get volume
logger.info('----- Group volume across Country-Brands')
CalcEngine.get_country_brand_volume(_geo_col, _country_col, _geo_name_col, _brand_col
                                 , _plan_period_col, plan_period, _volume_col)

# Get volume info
logger.info('----- Get unique volume info at Country-Brand level')
volume_info = CalcEngine.finance_grouped_20[['Country-Brand', _volume_col]].drop_duplicates(
    'Country-Brand')  # curves_spends_mm_vol[['Country-Brand', _volume_col]].drop_duplicates('Country-Brand')

# Get grouped spend
# Calculate cost per stimuli
spend_media_sub_grouped = CalcEngine.get_cost_per_stimuli(_geo_col, _geo_name_col, _brand_col, _instrument_col
                              , _plan_period_col, plan_period, _spend_col, _stimuli_col, _cost_per_stimuli_col)
CalcEngine.spend_media_sub_grouped = spend_media_sub_grouped

non_spend_stimuli_calculation = 0
if non_spend_stimuli_calculation == 1:
    ## Get cost per stimuli of non spend curves
    curves_non_spend = curves[curves.eval(_stimuli_type_col + ' not in ' + str(['Spend']))]
    curves_non_spend.reset_index(inplace=True, drop=True)

    filter_df = curves_non_spend.loc[:, [_geo_col, _brand_col, _instrument_col]].drop_duplicates()

    # spend_media_sub = CalcEngine.spend_media_sub.copy()
    spend_media_sub = CalcEngine.spend_media.copy()
    spend_media_sub = pd.merge(spend_media_sub, CalcEngine.input_dict['Calendar'], on=_week_col, how='left')

    # Filter for only the brands/countries needed
    if not filter_df.empty:
        spend_media_sub = spend_media_sub.merge(filter_df, how='inner')

    print('----- Group spends and stimuli by Brand-Country-Vehicle')
    spend_media_sub_grouped = spend_media_sub[spend_media_sub[_plan_period_col] ==
                                                           plan_period].groupby([_instrument_col, _brand_col, _geo_col]) \
        .agg({_spend_col: np.sum, _stimuli_col: np.sum})
    spend_media_sub_grouped.reset_index(inplace=True)

    # print('----- Calculate cost per stimuli')
    # spend_media_sub_grouped[_cost_per_stimuli_col] = spend_media_sub_grouped[_spend_col] / spend_media_sub_grouped[
    #     _stimuli_col]

    print('----- Get combination')
    spend_media_sub_grouped['Combination'] = spend_media_sub_grouped[_geo_col] + " - " + spend_media_sub_grouped[
        _brand_col] + " - " + spend_media_sub_grouped[_instrument_col]

    # Join with curves
    curves_non_spend = curves_non_spend.merge(spend_media_sub_grouped, how='left')

    print('----- Group spends and stimuli by Brand-Country-Vehicle')
    curves_non_spend_grouped = curves_non_spend.groupby([_instrument_col, _stimuli_type_col, _geo_col]) \
        .agg({_spend_col: np.sum, _stimuli_col: np.sum})
    curves_non_spend_grouped.reset_index(inplace=True)

    print('----- Calculate cost per stimuli')
    curves_non_spend_grouped[_cost_per_stimuli_col] = curves_non_spend_grouped[_spend_col] / curves_non_spend_grouped[
        _stimuli_col]
    curves_non_spend_grouped.dropna(subset=[_cost_per_stimuli_col], inplace=True)



    # cost_combis = curves_4_final[curves_4_final['Combination'].notnull()][['Combination', _spend_col, _stimuli_col, _cost_per_stimuli_col]].drop_duplicates()
    cost_combis = curves_non_spend_grouped[[_instrument_col, _geo_col , _stimuli_type_col, _spend_col, _stimuli_col, _cost_per_stimuli_col]].drop_duplicates()

    # bus_inputs_data.to_excel(config_path+'ME/Inputs_Cost_data_ALL.xlsx', index=False)
    # cost_combis.to_csv(config_path+'ME/CostCombisFY20_AllCurves.csv', index=False)

    cost_combis = cost_combis[cost_combis[_cost_per_stimuli_col] != 1]

    def cost_comparison(cost_combis, country1='GBN', country2='ESP'):
        cost_combis1 = cost_combis[cost_combis.eval(_geo_col + ' in ' + str([country1]))]
        cost_combis1.columns = ['country1' + '_' + c if c not in [_instrument_col, _brand_col, _geo_col, _stimuli_type_col] else c for c in cost_combis1.columns.tolist()]
        cost_combis2 = cost_combis[cost_combis.eval(_geo_col + ' in ' + str([country2]))]
        cost_combis2.columns = ['country2' + '_' + c if c not in [_instrument_col, _brand_col, _geo_col, _stimuli_type_col] else c for c in cost_combis2.columns.tolist()]
        cost_combis_Comparison = cost_combis1.merge(cost_combis2, on=[_instrument_col, _stimuli_type_col], how='inner')
        cost_combis_Comparison['differential'] = cost_combis_Comparison['country1' + '_' +_cost_per_stimuli_col]/cost_combis_Comparison['country2' + '_'+_cost_per_stimuli_col]
        return cost_combis_Comparison

    import itertools
    cost_comparison_final = pd.DataFrame()
    # for l in itertools.combinations(cost_combis[_geo_col].unique(), 2):
    for country1 in cost_combis[_geo_col].unique():
        for country2 in cost_combis[_geo_col].unique():
            if country1 != country2:
                cost_combis_Comparison = cost_comparison(cost_combis, country1=country1, country2=country2)
                if ~ cost_combis_Comparison.empty:
                    if cost_comparison_final.empty:
                        cost_comparison_final = cost_combis_Comparison.copy()
                    else:
                        cost_comparison_final = cost_comparison_final.append(cost_combis_Comparison.copy())


    cost_comparison_final.to_csv(config_path+'ME/CostCombisComparison20.csv', index=False)

    bus_inputs_data = bus_inputs.copy()
    bus_inputs_data['BV Selection'] = bus_inputs_data[_sel_col].str.split(' - ').str[0] + " - " + \
                                      bus_inputs_data[_sel_col].str.split(' - ').str[2]

    bus_inputs_data = bus_inputs_data.merge(cost_combis, left_on='BV Selection'
                                            , right_on='Combination'
                                            , how='left')

logger.info('----- Get data filled for BCVs')
bus_inputs_data = bus_inputs.copy()

# logger.info('----- Get Country-Brand level volume info for BCVs')
# bus_inputs_data['Country-Brand'] = bus_inputs_data[_geo_col] + " - " + bus_inputs_data[_brand_col]
# bus_inputs_data = pd.merge(bus_inputs_data, volume_info, on='Country-Brand', how='left')

logger.info('----- Get spend info')
bus_inputs_data['Country-Instrument'] = bus_inputs_data[_geo_col] + " - " + bus_inputs_data[_instrument_col]
bus_inputs_data = bus_inputs_data.merge(CalcEngine.spend_media_sub_grouped[['Combination', _spend_col]]
                                    , left_on='Country-Instrument'
                                    , right_on='Combination'
                                    , how='left')
del bus_inputs_data['Combination']

logger.info('----- Get currency conversion factor from market currency')
bus_inputs_data = bus_inputs_data.merge(market_currency[[_geo_col, _brand_col, _local_currency_col]].drop_duplicates(), how='left')
bus_inputs_data = bus_inputs_data.merge(currency_conversion_GBP
                                      , left_on=_local_currency_col
                                      , right_on=_source_currency_col
                                      , how='left')

logger.info('----- Add empty columns for #weeks and MediaCost')
# bus_inputs_data['#Weeks'] = pd.Series()
# bus_inputs_data['MediaCost'] = pd.Series()

logger.info('----- Get data required for curves')
curves_4_final = curves_4.copy()
curves_4_final = curves_4_final.merge(market_currency[[_geo_col, _brand_col, _local_currency_col]].drop_duplicates(), how='left')
curves_4_final = curves_4_final.merge(currency_conversion_GBP
                                      , left_on=_local_currency_col
                                      , right_on=_source_currency_col
                                      , how='left')

logger.info('----- Get region info for curves')
curves_4_final = curves_4_final.merge(geo_master[[_region_col, _geo_col]].drop_duplicates(), how='left')

logger.info('----- Get total spend for Geo-Instrument')
curves_4_final = curves_4_final.merge(CalcEngine.spend_media_sub_grouped[[_geo_col, _instrument_col, _spend_col]]
                                      , how='left')
# curves_4_final[_cost_per_stimuli_col][curves_4_final['stimulitype']=='Spend'] = 1.0

logger.info('----- Get spend mean for curves')
curves_4_final = pd.merge(curves_4_final, CalcEngine.spend_media_sub_min_max[[_geo_col, _brand_col
    , _instrument_col, _spend_mean_col]], on=[_geo_col, _brand_col, _instrument_col], how='left')

# logger.info('----- Get volume info for curves')
# curves_4_final['Country-Brand'] = curves_4_final[_geo_col] + " - " + curves_4_final[_brand_col]
# curves_4_final = pd.merge(curves_4_final, volume_info, on='Country-Brand', how='left')

logger.info('----- Get sector info for curves')
curves_4_final = curves_4_final.merge(CalcEngine.input_dict['finance'][[_sector_col, _brand_col
                                                                            , _geo_col]].drop_duplicates(
                                                            subset=[_brand_col, _geo_col]), how='left')
# curves_4_final['MediaCost'] = pd.Series()

logger.info('----- Get cost per stimuli')
cost_per_stimuli_all = CalcEngine.spend_media[[_geo_col, _brand_col, _instrument_col, _cost_col]].drop_duplicates(
                                            subset=[_geo_col, _brand_col, _instrument_col])
cost_per_stimuli_all.reset_index(inplace=True, drop=True)
curves_4_final = curves_4_final.merge(cost_per_stimuli_all, how='left')
curves_4_final.loc[curves_4_final[_stimuli_type_col] == 'Spend', _cost_col] = 1.0
# curves_4_final_missing = curves_4_final.loc[np.isnan(curves_4_final[_cost_col]), :]

if business_inputs_selection == 1:
    logger.info('----- Keep only the columns required')
    bus_inputs_12 = bus_inputs[bus_inputs['archetype'].isin(['Archetype 1', 'Archetype 2'])]  ## TOBE CHANGED TO ARCHETYPE 3
    bus_inputs_12 = bus_inputs_12[~((bus_inputs_12[_sel_col].isnull()) | (bus_inputs_12[_sel_col] == ''))]
    bus_inputs_12.reset_index(inplace=True, drop=True)

    curves_4_final[_sel_col] = curves_4_final[_geo_col] + " - "+curves_4_final[_brand_col] + " - "+curves_4_final[_instrument_col]
    curves_1_2_selected = pd.merge(curves_4_final#.drop_duplicates(subset=_sel_col)
                                   , bus_inputs_12[[_sel_col]],
                        left_on=_sel_col
                        , right_on=_sel_col, how='inner')
    # del curves_1_2_selected[_sel_col]

    bus_inputs_3 = bus_inputs[bus_inputs['archetype'].isin(['Archetype 3', 'Archetype 4'])]  ## TOBE CHANGED TO ARCHETYPE 3
    bus_inputs_3 = bus_inputs_3[~((bus_inputs_3[_agg_col].isnull()) | (bus_inputs_3[_agg_col] == ''))]
    bus_inputs_3.reset_index(inplace=True, drop=True)
    bus_inputs_3_all = bus_inputs_3.copy()
    for i in range(len(bus_inputs_3_all)):
        # i=14
        logger.info('Executing for row: ' + str(i))
        bus_inputs_3 = pd.DataFrame(bus_inputs_3_all.iloc[i, :]).T
        bus_inputs_3.reset_index(inplace=True, drop=True)

        logger.info('----- Get all market curves for aggregation')
        region_curves = CalcEngine.filter_curves_for_aggregation(curves_4_final, aggregation=bus_inputs_3[_agg_col][0]
                                                                 , exclusions=bus_inputs_3['exclusions'][0]
                                                                 , inclusions=bus_inputs_3['inclusions'][0]
                                                                 , _instrument_col=_instrument_col, _region_col=_region_col,
                                                                 _spend_mean_col=_spend_mean_col
                                                                 , _coeffA_col=_coeffA_col, _coeffB_col=_coeffB_col,
                                                                 _coeffC_col=_coeffC_col)
        if i==0:
            region_curves_all = region_curves.copy()
        else:
            region_curves_all = region_curves_all.append(region_curves.copy())

    # Edit columns
    del curves_1_2_selected[_coeffA_col], curves_1_2_selected[_coeffB_col], curves_1_2_selected[_coeffC_col]

    # rename columns
    curves_1_2_selected.rename(columns={"coefficienta_ad": "coefficienta", "coefficientb_ad": "coefficientb",
                                  "coefficientc_ad": "coefficientc"
        , "investment_mean": _spend_mean_col}, inplace=True)

    logger.info('----- Keep unique curve rows needed')
    relevant_curves = curves_1_2_selected.append(region_curves_all)
    relevant_curves.drop_duplicates(inplace=True)

    relevant_combinations, relevant_combinations_vol, relevant_combinations_cost = CalcEngine.get_relevant_combinations(
        bus_inputs_data, relevant_curves, volume_info, _geo_col, _brand_col, _instrument_col, _spend_col)


else:
    relevant_combinations, relevant_combinations_vol, relevant_combinations_cost = CalcEngine.get_relevant_combinations(
                                    bus_inputs_data, curves_4_final, volume_info, _geo_col, _brand_col, _instrument_col
    ,_spend_col)



logger.info('----- Storing outputs to: '+config_path+f'ME/AdditionalData/')
try:
    time = time+"_"+_region_selected+additional_prefix
    logger.info(f'Storing data with prefix {time} to ' + config_path + 'ME/AdditionalData/')
    curves_4_final.to_excel(config_path+f'ME/AdditionalData/{time}_Catalyst - Proxy Curves - Addnl Data.xlsx', index=False)
    if business_inputs_selection == 1:
        relevant_curves.to_excel(config_path+f'ME/AdditionalData/{time}_Catalyst - Proxy Curves Selection - Addnl Data.xlsx', index=False)

    # initialze the excel writer
    writer = pd.ExcelWriter(config_path+f'ME/AdditionalData/{time}_Catalyst - Proxy Curves - Input All combi Addnl Data.xlsx', engine='xlsxwriter')

    # store your dataframes in a  dict, where the key is the sheet name you want
    frames = {'volume': relevant_combinations_vol, 'spend': relevant_combinations,
              'mediacost': relevant_combinations_cost}

    # now loop thru and put each on a specific sheet
    for sheet, frame in frames.items():
        frame.to_excel(writer, sheet_name=sheet, index=False)

    # critical last step
    writer.save()

    bus_inputs_data.to_excel(config_path+f'ME/AdditionalData/{time}_Catalyst - Proxy Curves - Input template Addnl Data.xlsx', index=False, sheet_name='Inputs')
    logger.info('SUCCESS: Storing outputs to: '+config_path+f'ME/AdditionalData/')
except Exception as e:
    logger.info(f'Exception message: {str(e)}')
    logger.info('Exception: Storing outputs to: '+config_path+f'ME/AdditionalData/')



