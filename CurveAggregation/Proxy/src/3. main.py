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
from src.helpers.utilities import pandas_replace, logger, force_float
from src.helpers.archetypes import Archetypes

logger.info('========================== PROXY CURVE CREATION ==========================')
# path = 'C:/Projects/MarketCatalyst/GDM_Curve_Proxy/Codes/ProxyEstimation/'
config_path = path+'/config/'
time = str(date.today())

# Load column names
exec(open(config_path+'config.txt').read(), globals())

# Create instance with all the filenames
ETL = DataLoader({}, filepath_dict={"functionalforms": {"file": functionalform_file},
                                    "curves": {"file": curves_file},
                                    "curves_consolidated": {"file": curves_consolidated_file},
                                    "spend": {"file": spends_file},
                                    "media": {"file": media_file},
                                    "finance": {"file": finance_file},
                                    "calendar_master": {"file": calendar_file},
                                    # "country_codes": {"file": country_code_file},
                                    "bus_inputs": {"file": bus_inputs_file_wData, "sheet": "Inputs"},
                                    "volume_info": {"file": addnt_bus_inputs_file, "sheet": "volume"},
                                    # "spend": {"file": addnt_bus_inputs_file, "sheet": "spend"},
                                    "mediacost": {"file": addnt_bus_inputs_file, "sheet": "mediacost"},
                                    "geo_master": {"file": geo_file, "sheet": "Finance"},
                                    # "currency_conversion": {"file": currency_conversion_file},
                                    # "market_currency": {"file": market_currency_file},
                                    # "taxonomy": {"file": taxonomy_file, "sheet": "Taxonomy changes"}
                                    "kpi_template_df": {"file": kpi_template_file},
                                    "lt_values_df_all": {"file": lt_values_file, "sheet": "ALL"},
                                    "lt_values_df_eu": {"file": lt_values_file, "sheet": "EURO"},
                                    "instrument_taxonomy": {"file": instrument_taxonomy_file, "sheet": "ALL"},
                                    "exec_status": {"file": status_file, "sheet": "Status"},
                                    "population": {"file": pop_file},
                                    "states": {"file":state_file}
                                    }, logger=logger)
# Import all the files
ETL.import_files(lowercase=1)
# ETL.align_taxonomy(verbose=0)
# ETL.input_dict['finance'].to_csv(finance_file, index=False)

logger.info('------ Get population ratio. To be used as proxy for differential cost.')
pop_ratio = ETL.get_pop_ratio(_geo_col, _pop_col, _ppp_col, parity=1)

logger.info('------ Exclude completed BCVs')
completed_bcvs = ETL.input_dict['exec_status'].loc[ETL.input_dict['exec_status']['status'] =='Complete', 'bcv'].tolist()
pending_bcv_index = ~ETL.input_dict['bus_inputs']['bcv'].isin(completed_bcvs)
ETL.input_dict['bus_inputs'] = ETL.input_dict['bus_inputs'].loc[pending_bcv_index, :]

logger.info('------ Rename instruments according to taxonomy')
if instrument_taxonomy:
    ETL.inst_taxonomy_dict = {s: a for s, a in zip(ETL.input_dict['instrument_taxonomy']['old'], ETL.input_dict['instrument_taxonomy']['new'])}
    # ETL.input_dict['lt_values_df_all'] = pandas_replace(ETL.input_dict['lt_values_df_all'], ETL.inst_taxonomy_dict
    #                                               , additional_cols=[_instrument_col]
    #                                               ,anywhere=0, verbose=1)
    ETL.input_dict['lt_values_df_eu'] = pandas_replace(ETL.input_dict['lt_values_df_eu'], ETL.inst_taxonomy_dict
                                                  , additional_cols=[_instrument_col]
                                                  ,anywhere=0, verbose=1)

    ETL.input_dict['mediacost'] = pandas_replace(ETL.input_dict['mediacost'], ETL.inst_taxonomy_dict
                                                  , additional_cols=[_instrument_col]
                                                  ,anywhere=0, verbose=1)

curves_consolidated = ETL.input_dict['curves_consolidated'].copy()

# get calendar
ETL.get_calendar(_week_col, _plan_period_col, _startdate_col, _enddate_col, _planperiodname_col, convert_datetime=1)

# Create
CalcEngine = CalculationEngine(ETL.input_dict.copy(), logger)

logger.info

logger.info('----- Add geography column to finance data')
CalcEngine.input_dict['finance'] = CalcEngine.input_dict['finance'][(CalcEngine.input_dict['finance'][_baseline_col] == "Moderate") & (CalcEngine.input_dict['finance'][_time_period_col] == 'Full')]
CalcEngine.input_dict['finance'].reset_index(inplace=True, drop=True)
CalcEngine.input_dict['finance'] = CalcEngine.input_dict['finance'].merge(CalcEngine.input_dict['geo_master'][[_geo_name_col, _geo_col]].drop_duplicates()
                                              ,left_on=_country_col, right_on=_geo_name_col, how='left')
CalcEngine.input_dict['finance'].dropna(subset=[_geo_col], inplace=True)

logger.info('----- Get volume info for curves and bus inputs')
CalcEngine.input_dict['curves'] = CalcEngine.get_volume_info(CalcEngine.input_dict['curves']
                                                             , CalcEngine.input_dict['volume_info'], _geo_col, _brand_col)
CalcEngine.input_dict['bus_inputs'] = CalcEngine.get_volume_info(CalcEngine.input_dict['bus_inputs']
                                                             , CalcEngine.input_dict['volume_info'], _geo_col, _brand_col)

CalcEngine.input_dict['bus_inputs'] = CalcEngine.get_region_col(CalcEngine.input_dict['bus_inputs'],_region_col, _geo_col)

if _region_selected != 'ALL':
    logger.info(f'-----Filtering business inputs for region: {_region_selected}')
    CalcEngine.input_dict['bus_inputs'] = CalcEngine.input_dict['bus_inputs'].loc[CalcEngine.input_dict['bus_inputs'][_region_col] == _region_selected, :]

if _inputs_region_selected != '':
    logger.info(f'-----Filtering business inputs for region: {_inputs_region_selected}')
    CalcEngine.input_dict['bus_inputs'] = CalcEngine.input_dict['bus_inputs'].loc[CalcEngine.input_dict['bus_inputs'][_region_col] == _inputs_region_selected, :]


# CalcEngine.input_dict['curves'] = pd.merge(CalcEngine.input_dict['curves'], CalcEngine.input_dict['volume_info'], how='left')
# CalcEngine.input_dict['bus_inputs'] = pd.merge(CalcEngine.input_dict['bus_inputs'], CalcEngine.input_dict['volume_info'], how='left')

logger.info('----- Get mediacost info for curves and bus inputs')
CalcEngine.input_dict['curves'] = pd.merge(CalcEngine.input_dict['curves'], CalcEngine.input_dict['mediacost'], how='left')
CalcEngine.input_dict['bus_inputs'] = pd.merge(CalcEngine.input_dict['bus_inputs'], CalcEngine.input_dict['mediacost'], how='left')


logger.info('----- Add suffix to curves columns for additional data.')
CalcEngine.input_dict['curves'].rename(columns={_volume_col: _volume_col+'_Proxy'
                                         , _spend_col: _spend_col+'_Proxy'
                                         , 'country-brand': 'country-brand'+'_Proxy'
                                         , 'mediacost': 'mediacost'+'_Proxy'}, inplace=True)
curves = CalcEngine.input_dict['curves']
bus_inputs = CalcEngine.input_dict['bus_inputs']
bus_inputs.rename(columns={_country_col:_geo_col, _vehicle_col:_instrument_col}, inplace=True)

#######################################################################################################################################
#### ARCHETYPES

curves_4_final = curves.copy()#pd.read_excel(path+'\Outputs\ConvertedCurves\Curves_converted_test_0329_vf_clean.xlsx')
curves_4_final[_sel_col] = curves_4_final[_geo_col] + " - "+curves_4_final[_brand_col] + " - "+curves_4_final[_instrument_col]
curves_4_final.drop_duplicates(subset=[_sel_col], inplace=True)

archetype=1

# bus_inputs['mediacost'] = 1.0
# curves_4_final['mediacost'+'_Proxy'] = 1.0

ArchetypesEngine = Archetypes(bus_inputs, curves_4_final, logger
                              , _archetype_col, _sel_col, _spend_mean_col, _spend_col, _volume_col
                              , _coeffA_col, _coeffB_col, _coeffC_col
                              , _stimuli_type_col, _cost_col, _geo_col, _conversion_factor_col
                              , _agg_col, _instrument_col, _region_col, _stimuli_col, _pop_col, form_col)

bus_inputs_1_final = ArchetypesEngine.exec_archetype1()

bus_inputs_2 = ArchetypesEngine.exec_archetype2(pop_ratio)

bus_inputs_3_all = ArchetypesEngine.exec_archetype3(CalcEngine, CurveConverter, ETL, coefficient_cols)

curves_4 = ETL.input_dict['curves_consolidated']
try:
    del curves_4[_geo_name_col]
except:
    logger.info('----- No geo name to delete')

logger.info('----- Get log for all bcvs')
if len(bus_inputs_1_final) > 0:
    bus_inputs_messsage = bus_inputs_1_final[['bcv', 'Message']].append(bus_inputs_2[['bcv', 'Message']]).append(bus_inputs_3_all[['bcv', 'Message']])
elif len(bus_inputs_2) > 0:
    bus_inputs_messsage = bus_inputs_2[['bcv', 'Message']].append(bus_inputs_3_all[['bcv', 'Message']])
elif len(bus_inputs_3_all) > 0:
    bus_inputs_messsage = bus_inputs_3_all[['bcv', 'Message']]


logger.info('----- Generate Curves file')
try:
    curves_t = CalcEngine.transform_curves_format(bus_inputs_1_final, curves_4
                                , _geo_col, _brand_col, _instrument_col, _coeffA_col, _coeffB_col, _coeffC_col
                                , _state_col, _subbrand_col, form_col, _stimuli_type_col, _rating_col, curves_consolidated)
except:
    logger.info('----- No Archetype 1. Creating empty df')
    curves_t = pd.DataFrame(columns=curves_4.columns.tolist())
    curves_t.rename(columns={'spend_type': 'spendtype'}, inplace=True)
    curves_t['newinstrumentname'] = pd.Series()

if len(bus_inputs_2) > 0:
    curves_t = curves_t.append(CalcEngine.transform_curves_format(bus_inputs_2, curves_4
                                    , _geo_col, _brand_col, _instrument_col, _coeffA_col, _coeffB_col, _coeffC_col
                                    , _state_col, _subbrand_col, form_col, _stimuli_type_col, _rating_col, curves_consolidated))
if len(bus_inputs_3_all) > 0:
    curves_t = curves_t.append(CalcEngine.transform_curves_format(bus_inputs_3_all, curves_4
                                    , _geo_col, _brand_col, _instrument_col, _coeffA_col, _coeffB_col, _coeffC_col
                                    , _state_col, _subbrand_col, form_col, _stimuli_type_col, _rating_col
                                                                  , curves_consolidated, _state_level))


if create_addnl_outputs == 1:
    try:
        logger.info('----- Generate instrument confidence file')
        instrumentConfidence = curves_t.loc[:, [_brand_col, _subbrand_col, _geo_col, _state_col,_instrument_col, _rating_col]]
        del curves_t[_rating_col]
        logger.info(f'SUCCESS: Generate instrument confidence file')
    except Exception as e:
        logger.info(f'Exception: Generate instrument confidence file')
        logger.info(f'Exception message: {str(e)}')

    try:
        logger.info('----- Generate media cost file')
        media_cost = CalcEngine.get_media_cost_df(curves_t, _brand_col, _subbrand_col, _geo_col
                                                  , _state_col,_instrument_col, _week_col, _cost_col)
        logger.info(f'SUCCESS: Generate media cost file')
    except Exception as e:
        logger.info(f'Exception: Generate media cost file')
        logger.info(f'Exception message: {str(e)}')

    try:
        logger.info('----- Generate Actual & Plan file')
        act_plan = CalcEngine.get_actual_plan_df(curves_t, _brand_col, _subbrand_col, _geo_col, _state_col, _instrument_col, _week_col,
                           _cost_col,  _plan_period_col, plan_period)
        logger.info(f'SUCCESS: Generate Actual & Plan file')
    except Exception as e:
        logger.info(f'Exception: Generate Actual & Plan file')
        logger.info(f'Exception message: {str(e)}')


    logger.info('----- Generate KPI Parameters  file')
    # kpi_template = config_path+'ME/AdditionalData/Templates/KPI_Parameter_Template.xlsx'
    # kpi_template_df = pd.read_excel(kpi_template)
    # kpi_template_df.columns = kpi_template_df.columns.str.lower().tolist()
    # Create instance with all the filenames
    ETL_kpi = DataLoader({}, filepath_dict={
                                        "kpi_template_df": {"file": kpi_template_file},
                                        "lt_values_df_all": {"file": lt_values_file, "sheet": "ALL"},
                                        "lt_values_df_eu": {"file": lt_values_file, "sheet": "EURO"},
                                        "taxonomy": {"file": taxonomy_file, "sheet": "Taxonomy changes"},
                                        "instrument_taxonomy": {"file": instrument_taxonomy_file, "sheet": "ALL"},
                                        "curves_cols_t": {"file": curves_col_names},
                                        "kpi_cols_t": {"file": kpi_col_names},
                                        "media_cols_t": {"file": media_col_names},

    }, logger=logger)

    # Import all the files
    ETL_kpi.import_files()

    logger.info('------ Rename according to taxonomy')
    ETL_kpi.align_taxonomy(verbose=0)

    logger.info('------ Rename instruments according to taxonomy')
    if instrument_taxonomy:
        ETL_kpi.inst_taxonomy_dict = {s: a for s, a in zip(ETL_kpi.input_dict['instrument_taxonomy']['Old'],
                                                       ETL_kpi.input_dict['instrument_taxonomy']['New'])}
        # ETL.input_dict['lt_values_df_all'] = pandas_replace(ETL.input_dict['lt_values_df_all'], ETL.inst_taxonomy_dict
        #                                               , additional_cols=[_instrument_col]
        #                                               ,anywhere=0, verbose=1)
        ETL_kpi.input_dict['lt_values_df_eu'] = pandas_replace(ETL_kpi.input_dict['lt_values_df_eu'], ETL_kpi.inst_taxonomy_dict
                                                           , additional_cols=['Instrument']
                                                           , anywhere=0, verbose=1)
        ETL_kpi.input_dict['lt_values_df_eu'].drop_duplicates(subset=['Instrument'], inplace=True)

    ETL_kpi.input_dict['kpi_template_df'].columns = ETL_kpi.input_dict['kpi_template_df'].columns.str.lower().tolist()
    ETL_kpi.input_dict['lt_values_df_eu'].columns = ETL_kpi.input_dict['lt_values_df_eu'].columns.str.lower().tolist()

    logger.info('----- Generate KPI Master')
    try:
        kpi_template_df = ETL_kpi.input_dict['kpi_template_df']
        final_kpi = CalcEngine.get_kpi_master(curves_t, kpi_template_df, _brand_col, _subbrand_col, _geo_col, _state_col
                           , _instrument_col, _region_col, curves_consolidated)
        logger.info(f'SUCCESS: Fetching KPI master')
    except Exception as e:
        logger.info(f'Exception: Fetching KPI master')
        logger.info(f'Exception message: {str(e)}')

    try:
        final_kpi_LT_ALL = CalcEngine.get_LT_ALL(ETL_kpi, final_kpi, _brand_col, _geo_col, _instrument_col, _region_col
                                                 , _region_selected)
        logger.info(f'SUCCESS: Fetching LT KPI parameters')
    except Exception as e:
        logger.info(f'Exception: Fetching LT KPI parameters')
        logger.info(f'Exception message: {str(e)}')

    # GEt finance data
    CalcEngine.input_dict['finance'].columns

    try:
        final_kpi_finance_values = CalcEngine.get_finance_KPI(final_kpi, _brand_col, _geo_col, _region_col
                            , _volume_col, _gp_per_eu_col, _nsv_per_eu_col, _gp_per_lit_col, _category_col)
        logger.info(f'SUCCESS: Fetching finance KPI parameters')
    except Exception as e:
        logger.info(f'Exception: Fetching finance KPI parameters')
        logger.info(f'Exception message: {str(e)}')

    # Get upweight data
    try:
        final_kpi_upweight = final_kpi[final_kpi['parameter_id'].isin(['AdjFactor', 'Coeff', 'Base'])]
        final_kpi_upweight['value'] = 1.0
        del final_kpi_upweight[_region_col]
        final_kpi_missing = final_kpi[final_kpi['parameter_id'].isin([np.nan, 'nan'])]
        logger.info(f'SUCCESS: Fetching upweight KPI parameters')
    except Exception as e:
        logger.info(f'Exception: Fetching upweight KPI parameters')
        logger.info(f'Exception message: {str(e)}')


    try:
        final_kpi_values = final_kpi_LT_ALL.append(final_kpi_finance_values.loc[:, final_kpi_LT_ALL.columns.tolist()]).append(
            final_kpi_upweight.loc[:, final_kpi_LT_ALL.columns.tolist()])
        final_kpi_values['key'] = 1
        logger.info(f'SUCCESS: Appending all KPI parameters')
    except Exception as e:
        logger.info(f'Exception: Appending all KPI parameters')
        logger.info(f'Exception message: {str(e)}')

    # Duplicate across all the weeks
    final_kpi_values = final_kpi_values.merge(CalcEngine.input_dict['Calendar'].loc[:, [_week_col, 'key']], how='left')
    del final_kpi_values['key']

    # TODO reorder columns

    # TODO kpi formulae
    curves_4['Country-Brand'] = curves_4[_geo_col] + " - " + CalcEngine.input_dict['finance'][_brand_col]
    bus_inputs_cb = bus_inputs.loc[:, [_geo_col, _brand_col]].drop_duplicates()
    curves_4_cb = curves_4.loc[:, [_geo_col, _brand_col]].drop_duplicates()
    curves_4_cb['test'] = 1
    bus_inputs_cb_yes = bus_inputs_cb.merge(curves_4_cb, how='left')
    bus_inputs_cb_missing = bus_inputs_cb_yes.loc[np.isnan(bus_inputs_cb_yes['test']),:]
    # curves_t.to_csv(path+'\Outputs\Final_Proxy_Curves.csv', index=False)
    # curves_final = curves_t.append(transform_curves_format(CalcEngine, bus_inputs_2, curves_4))

    # # ROI
    # act_plan_curve = act_plan.merge(curves_t[[_geo_col, _brand_col, _instrument_col
    #                                     , _coeffA_col, _coeffB_col, _coeffC_col]])
    # if _region_selected in ['EUROPE', 'EUROPE & TURKEY']:
    #     act_plan_curve = act_plan_curve.merge(final_kpi_finance_values.loc[final_kpi_finance_values['parameter_id']
    #                                                                        == 'Gross_Profit_per_Litre'])
    # else:
    #     act_plan_curve = act_plan_curve.merge(final_kpi_finance_values.loc[final_kpi_finance_values['parameter_id']
    #                                                                        == 'Profitability'])
    #
    # act_plan_curve.rename(columns={'value': 'Profitability'}, inplace=True)
    # del act_plan_curve['parameter_id']
    # act_plan_curve['curve_name'] = 'ADBUDG'
    # converter = CurveConverter(CalcEngine.input_dict['functionalforms'], curves_4, coefficient_cols, logger=logger)
    # converter.lower_case()
    # act_plan_curve['impact'] = np.vectorize(converter.curve_eval)(act_plan_curve['coefficienta'], act_plan_curve['coefficientb'],
    #                                                       act_plan_curve['coefficientc'], act_plan_curve['Investment'],
    #                                                       act_plan_curve['curve_name'])
    # act_plan_curve['ROI'] = act_plan_curve.eval('impact*Profitability/Investment')


# logger.info('----- Remove duplicates in ROI')
# act_plan_curve.drop_duplicates(subset=[_geo_col, _brand_col, _instrument_col], inplace=True)
# act_plan_curve['bcv'] = act_plan_curve[_geo_col] + " - "+act_plan_curve[_brand_col] + " - "+act_plan_curve[_instrument_col]
# act_plan_curve = act_plan_curve.merge(bus_inputs_messsage, how='left')
# act_plan_curve = act_plan_curve[act_plan_curve['Message'] == 'Success']
# del act_plan_curve['Message'], act_plan_curve['bcv']

def keep_success(df, bus_inputs_messsage, _geo_col, _brand_col, _instrument_col):
    df['bcv'] = df[_geo_col] + " - " + df[_brand_col] + " - " + df[_instrument_col]
    df = df.merge(bus_inputs_messsage, how='left')
    df = df[df['Message'] == 'Success']
    del df['Message'], df['bcv']
    return df


logger.info('keep only the successful curves')
curves_t = keep_success(curves_t, bus_inputs_messsage, _geo_col, _brand_col, _instrument_col)

if create_addnl_outputs == 1:
    media_cost = keep_success(media_cost, bus_inputs_messsage, _geo_col, _brand_col, _instrument_col)
    instrumentConfidence = keep_success(instrumentConfidence, bus_inputs_messsage, _geo_col, _brand_col, _instrument_col)
    act_plan = keep_success(act_plan, bus_inputs_messsage, _geo_col, _brand_col, _instrument_col)
    final_kpi_values = keep_success(final_kpi_values, bus_inputs_messsage, _geo_col, _brand_col, _instrument_col)



if _use_coeff == 0:
    logger.info('----- Drop Coeff rows')
    final_kpi_values = final_kpi_values[final_kpi_values['parameter_id'] != 'Coeff']
logger.info('----- Drop Missing KPI rows')
# final_kpi_values.dropna(subset=['value'], inplace=True)

logger.info('----- Order columns')
curves_t = curves_t[ETL_kpi.input_dict['curves_cols_t'].columns]
final_kpi_values = final_kpi_values[ETL_kpi.input_dict['kpi_cols_t'].columns]
media_cost = media_cost[ETL_kpi.input_dict['media_cols_t'].columns]

logger.info('----- Update inputs file with selection and execution status')
bus_inputs_status = ETL.input_dict['bus_inputs'].copy()
bus_inputs_status = bus_inputs_status[['geography', 'brand', 'instrument', 'bcv', 'archetype',
       'selection', 'aggregation', 'inclusions', 'exclusions']]
# successful_bcvs = bus_inputs_messsage.loc[bus_inputs_messsage['Message'] == 'Success', 'bcv'].values
bus_inputs_messsage = bus_inputs_status.merge(bus_inputs_messsage, on='bcv', how='right')


logger.info('----- Storing outputs to: ' + path + f'/Outputs/FinalOutputs/')
try:
    # if _region_selected != 'ALL':
    if _inputs_region_selected!="":
        _region_selected = _inputs_region_selected
    time = time+"_"+_region_selected+additional_prefix
    logger.info(f'Storing data with prefix {time} to ' + path + f'/Outputs/FinalOutputs/')
    bus_inputs_messsage.to_csv(path+f'/Outputs/FinalOutputs/{time}_BCVs_Log.csv', index=False)
    curves_t.to_csv(path+f'/Outputs/FinalOutputs/{time}_Proxy_Curves.csv', index=False)
    if create_addnl_outputs == 1:
        instrumentConfidence.to_csv(path+f'/Outputs/FinalOutputs/{time}_InstrumentConfidence.csv', index=False)
        media_cost.to_csv(path+f'/Outputs/FinalOutputs/{time}_media_cost.csv', index=False)
        # act_plan.to_csv(path+f'/Outputs/FinalOutputs/{time}_Actual & Plan.csv', index=False)
        # act_plan_curve.to_csv(path+f'/Outputs/FinalOutputs/{time}_ROI.csv', index=False)
        final_kpi_values.to_csv(path+f'/Outputs/FinalOutputs/{time}_KPI Parameters.csv', index=False)
        bus_inputs_cb_missing.to_csv(path+f'/Outputs/FinalOutputs/{time}_Missing Combinations For KPI Formula.csv', index=False)

except Exception as e:
    logger.info(f'Exception message: {str(e)}')
    logger.info('Exception: Storing outputs to: ' + path + f'/Outputs/FinalOutputs/')
