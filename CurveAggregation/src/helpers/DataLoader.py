import pandas as pd
import numpy as np
import datetime
from pandas.api.types import is_datetime64_any_dtype as is_datetime
import json

from src.helpers.utilities import pandas_replace


class DataLoader:
    """Class to load all required files
    """

    def __init__(self, config_dict, filepath_dict, logger):
        self.config_dict = config_dict
        self.filepath_dict = filepath_dict
        self.input_dict = {}
        self.logger = logger

    def load_file(self, filename, sheet_name=None, lowercase=None):
        """
        Parameters
        ----------
        filename: str, Path of the file present
        sheet_name: if reading excel file

        Returns
        ----------
        df: Pandas data frame
        """
        try:
            if filename.split(".")[-1] == 'csv':
                try:
                    df = pd.read_csv(filename)
                except:
                    df = pd.read_csv(filename)
            elif filename.split(".")[-1] == 'xlsx':
                if sheet_name:
                    df = pd.read_excel(filename, sheet_name=sheet_name)
                else:
                    df = pd.read_excel(filename)
            elif filename.split(".")[-1] == 'json':
                df = json.load(open(filename))

            if lowercase:
                # Convert the columns to lower
                if isinstance(df, pd.DataFrame):
                    df.columns = df.columns.str.lower().tolist()
            self.logger.info(f'SUCCESS: Reading file {filename}')
        except Exception as e:
            self.logger.info(f'Exception: Reading file {filename}')
            self.logger.info(f'Exception message: {str(e)}')

        return df

    def import_files(self, lowercase=None):
        """
        Loads all files in filepath dict
        :return: Output dictionary with all files loaded
        """
        for file in self.filepath_dict:
            if len(self.filepath_dict[file]) == 1:
                self.input_dict[file] = self.load_file(self.filepath_dict[file]['file'], lowercase=lowercase)
            else:
                self.input_dict[file] = self.load_file(self.filepath_dict[file]['file']
                                                       , self.filepath_dict[file]['sheet']
                                                       , lowercase=lowercase)

        # return self.input_dict

    def align_taxonomy(self, lower=1, verbose=1):
        """
        Align taxonomy across all tables loaded. Use the taxonomy dict to replace across all columns
        :param verbose: 1, if detailed logs 0 otherwise
        :return:
        """
        if lower == 1:
            self.logger.info('Convert columns to lower case')
            self.input_dict['taxonomy'].columns = self.input_dict['taxonomy'].columns.str.lower().tolist()

        self.logger.info('Create dictionary')
        self.input_dict['taxonomy'].dropna(subset=['from'], inplace=True)
        self.input_dict['taxonomy'].dropna(subset=['to'], inplace=True)
        self.taxonomy_dict = {s: a for s, a in zip(self.input_dict['taxonomy']['from'], self.input_dict['taxonomy']['to'])}
        for key in self.input_dict.keys():
            if (isinstance(self.input_dict[key], pd.DataFrame)):
                if key != 'taxonomy':
                    self.input_dict[key] = pandas_replace(self.input_dict[key], self.taxonomy_dict, verbose=verbose)

    def filter_df(self, df, filters):
        filter_string = ""
        keys = list(filters.keys())
        values = list(filters.values())
        for i in range(len(filters)):
            if not str(values[i][1]).isdigit():
                values[i][1] = "'"+str(values[i][1])+"'"
            if i > 0:
                filter_string += ('&('+keys[i]+str(values[i][0])+str(values[i][1])+')')
            else:
                filter_string += ('('+keys[i]+str(values[i][0])+str(values[i][1])+')')

        return df[df.eval(filter_string)]

    def get_relevant_curves(self, _coeffA_col, _coeffB_col, _coeffC_col, form_col, _rating_col):
        """
        Keep only the curves with confidence rating of 4 and with values that make sense
        :param _coeffA_col: str, column name of the Coefficient A
        :param _coeffB_col: str, column name of the Coefficient B
        :param _coeffC_col: str, column name of the Coefficient C
        :param form_col: str, column name of the Functional form
        :param _rating_col: str, column name of the confidence rating
        :return:
        """
        self.logger.info('----- Keeping only the 4 rating curves')
        curves_4 = self.input_dict['curves'][(self.input_dict['curves'][_rating_col] == 4)]
        curves_4 = curves_4[(curves_4[_coeffA_col] > 0) | (curves_4[_coeffB_col] > 0) | (curves_4[_coeffC_col] > 0)]
        curves_4 = curves_4[~((curves_4[_coeffA_col] == 0) & (curves_4[form_col] == 'POWER'))]
        curves_4.reset_index(inplace=True, drop=True)
        return curves_4

    def keep_relevant_data(self, filter_df):
        """
        Keep only the data that is needed based on BCVs present in curves
        :param filter_df: pandas, df with BCVs in curves
        :return:
        """
        self.input_dict['spend'] = self.input_dict['spend'].merge(filter_df, how='inner')
        self.input_dict['media'] = self.input_dict['media'].merge(filter_df, how='inner')

    def convert_date(self, df, source, destination, dateformat="%m/%d/%Y", keep=0, datetime=0):
        """
        Convert date format of a series in pandas dataframe
        :param df: pandas, dataframe where the date format is to be converted
        :param source: str, column name of the source date field
        :param destination: str, column name of the destination date field
        :param dateformat: str, format of the date
        :param keep: 1, if we need to keep both source and destination columns
        :param datetime: 1, if destination column is to be converted to datetime
        :return:
        """
        if ~ is_datetime(df[source]):
            df[source] = pd.to_datetime(df[source])
        df[destination] = df[source].dt.strftime(dateformat)
        if datetime==1:
            df[destination] = pd.to_datetime(df[destination])
        # df[destination] = pd.to_datetime(df[source], format=dateformat)

        if (keep == 0) & (source != destination):
            del df[source]
        return df

    def get_calendar(self, _week_col, _plan_period_col, _startdate_col, _enddate_col, _planperiodname_col):
        """
        Creates a master calendar file with week starts based on the master calendar info with FY week starts and ends
        :param _week_col: str, column name of the week field
        :param _plan_period_col: str, column name of the FY plan period in output calendar
        :param _startdate_col: str, column name of first week in FY
        :param _enddate_col: str, column name of last week in FY
        :param _planperiodname_col: str, column name of the FY plan period in input master calendar
        :return: pandas, Calendar with every week start between start and end weeks of every FY in master calendar
        """
        self.logger.info('------ Converting date formats for start and end dates')
        self.input_dict['calendar_master'] = self.convert_date(self.input_dict['calendar_master']
                                                               , _startdate_col, _startdate_col, datetime=1)
        self.input_dict['calendar_master'] = self.convert_date(self.input_dict['calendar_master']
                                                               , _enddate_col, _enddate_col, datetime=1)
        self.input_dict['calendar_master'].reset_index(inplace=True, drop=True)

        self.logger.info('------ Looping through different FYs')
        try:
            for i in range(len(self.input_dict['calendar_master'])):
                # i=0
                calendar_sub = pd.DataFrame()
                sdate = self.input_dict['calendar_master'][_startdate_col][i]
                edate = self.input_dict['calendar_master'][_enddate_col][i]
                calendar_sub[_week_col] = pd.date_range(sdate, edate + datetime.timedelta(days=1), freq='7D')
                calendar_sub[_plan_period_col] = self.input_dict['calendar_master'][_planperiodname_col][i]
                calendar_sub = self.convert_date(calendar_sub, _week_col, _week_col)
                if i == 0:
                    calendar = calendar_sub.copy()
                else:
                    calendar = calendar.append(calendar_sub.copy())
            self.logger.info(f'SUCCESS: Creating calendar')
        except Exception as e:
            self.logger.info(f'Exception: Creating calendar')
            self.logger.info(f'Exception message: {str(e)}')
        self.input_dict['Calendar'] = calendar

    def filter_currency(self, _source_currency_col, _target_currency_symbol_col
                                                              , _conversion_factor_col, target_currency='GBP'):
        """
        Filters out the master currency conversion table to only the relevant source target currency
        :param _source_currency_col: str, column name
        :param _target_currency_symbol_col: str, column name
        :param _conversion_factor_col: str, column name
        :param target_currency: str, currency code
        :return: Filtered currency conversion table
        """
        try:
            self.input_dict['currency_conversion_sub'] = self.input_dict['currency_conversion'].loc[
                                        self.input_dict['currency_conversion'][_target_currency_symbol_col] == target_currency
            , [_source_currency_col, _target_currency_symbol_col, _conversion_factor_col]].drop_duplicates()
            self.logger.info(f'SUCCESS: Filtering only for Target: {target_currency} currency conversion')
        except Exception as e:
            self.logger.info(f'Exception message: {str(e)}')
            self.logger.info(f'ERROR: Filtering only for Target: {target_currency} currency conversion')


class CalculationEngine:
    """Class to load all required files and calculations
    """

    def __init__(self, input_dict, logger):
        self.input_dict = input_dict
        self.logger = logger


    def convert_date(self, df, source, destination, dateformat="%m/%d/%Y", keep=0, datetime=0):
        """
        Convert date format of a pandas series
        :param df: pandas, dataframe with series to be converted
        :param source: str, column name of the source series
        :param destination: str, column name of the destination series
        :param dateformat: str, format of the destination date
        :param keep: int, 1 to keep the source column, 0 otherwise
        :param datetime: int, 1 to convert output series to datetime format
        :return: pandas, dataframe with the series converted
        """
        if ~ is_datetime(df[source]):
            df[source] = pd.to_datetime(df[source])
        df[destination] = df[source].dt.strftime(dateformat)
        if datetime == 1:
            df[destination] = pd.to_datetime(df[destination])
        # df[destination] = pd.to_datetime(df[source], format=dateformat)

        if (keep == 0) & (source != destination):
            del df[source]
        return df

    def calculate_stimuli(self, spend_cols, media_cols, _stimuli_col, _spend_col, _cost_col):
        """
        Merge spend and media info and calculate stimuli
        :param spend_cols: list, columns names of spend df to be joined on
        :param media_cols: list, columns names of media df to be joined on
        :param _stimuli_col: str, column name of the stimuli
        :param _spend_col: str, column name of spend
        :param _cost_col: str, column name of media cost
        :return: pandas, joined spend media df
        """
        try:
            spend_media = pd.merge(self.input_dict['spend'], self.input_dict['media'][media_cols]
                                   , on=spend_cols, how='inner')
            spend_media[_stimuli_col] = spend_media[_spend_col] / spend_media[_cost_col]
        except:
            raise ValueError('CalcEngine.calculate_stimuli: Issue with merging spend, media')
        self.spend_media = spend_media

    def get_region_col(self, df,_region_col, _geo_col):
        """
        Merge with geo master to get the region data
        :param df: pandas, dataframe to which region column needs to be added
        :param _region_col: str, column name of the region
        :param _geo_col: str, column name of the country
        :return: pandas, dataframe with region column added
        """
        df = df.merge(self.input_dict['geo_master'][[_region_col, _geo_col]].drop_duplicates(), how='left')
        return df

    # def get_geo_col(self, _geo_name_col, _geo_col):
    #     # Get ISO code
    #     self.input_dict['finance'] = self.input_dict['finance'].merge(self.input_dict['geo_master'][[_geo_name_col, _geo_col]].drop_duplicates()
    #                                                   , how='left')
    def get_volume_info(self, df, volume_info, _geo_col, _brand_col):
        self.logger.info('----- Get volume info for curves and bus inputs')
        # df['Country-Brand'] = df[_geo_col] + " - " +  df[_brand_col]
        df = pd.merge(df, volume_info, how='left')
        # del df['Country-Brand']
        return df

    def get_spend_mean(self, _spend_col, _geo_col, _brand_col, _instrument_col, _week_col
                       ,_plan_period_col, plan_period):
        """
        Calculate spend mean, min and max on the spend media df
        :param _spend_col: str, column name of the spend
        :param _geo_col: str, column name of the country
        :param _brand_col: str, column name of the brand
        :param _instrument_col: str, column name of the instrument/vehicle
        :param _week_col: str, column name of the week
        :param _plan_period_col: str, column name of the plan period
        :param plan_period: str, plan period to be filtered on e.g., FY-2020
        :return:
        assign filtered spend media data on plan period
        assign spend mean calculated to spend_media_sub_min_max
        """
        spend_media_sub = self.spend_media
        spend_media_sub = pd.merge(spend_media_sub, self.input_dict['Calendar'], on=_week_col, how='left')
        spend_media_sub = spend_media_sub[spend_media_sub[_plan_period_col] == plan_period]
        spend_media_sub_min_max = spend_media_sub.groupby([_geo_col, _brand_col, _instrument_col]).agg(
            {_spend_col: ['min', 'max', 'mean']})
        spend_media_sub_min_max.columns = ["_".join(x) for x in spend_media_sub_min_max.columns.ravel()]
        spend_media_sub_min_max.reset_index(inplace=True)
        self.spend_media_sub_min_max = spend_media_sub_min_max
        self.spend_media_sub = spend_media_sub

    def get_market_currency(self, to_symbol, _local_currency_col, _source_currency_col,  _target_currency_symbol_col):
        market_currency_rate = self.input_dict['market_currency'].merge(self.input_dict['currency_conversion']
                                                                        , left_on=_local_currency_col
                                                                        , right_on=_source_currency_col
                                                                        , how='left')
        self.market_currency_rate = market_currency_rate[market_currency_rate[_target_currency_symbol_col]==to_symbol]

    def get_country_brand_volume(self, _geo_col, _country_col, _geo_name_col, _brand_col
                                 , _plan_period_col, plan_period, _volume_col):
        # Get FY year
        self.input_dict['finance'][_plan_period_col] = 'FY-' + self.input_dict['finance']['year'].astype('int').astype('str')

        # Group volume for Country-Brand
        finance_grouped = pd.DataFrame(self.input_dict['finance'].groupby([_country_col, _brand_col, _plan_period_col])[_volume_col].sum())
        finance_grouped.reset_index(inplace=True)

        # Filter for the required period
        finance_grouped_20 = finance_grouped[finance_grouped[_plan_period_col] == plan_period]
        # Set columns
        finance_grouped_20.columns = [_geo_name_col, _brand_col, _plan_period_col, _volume_col]

        # Get ISO code
        finance_grouped_20 = finance_grouped_20.merge(self.input_dict['geo_master'][[_geo_name_col, _geo_col]].drop_duplicates()
                                                      , how='left')

        # Create country-brand key
        finance_grouped_20['Country-Brand'] = finance_grouped_20[_geo_col] + " - " + finance_grouped_20[_brand_col]
        self.finance_grouped_20 = finance_grouped_20


    def get_aggregated_volume(self, _region_col, _geo_name_col, _brand_col
                              , _plan_period_col, plan_period, _volume_col, filter_df=pd.DataFrame()):
        finance = self.input_dict['finance'].copy()

        # Filter for only the brands/countries needed
        if not filter_df.empty:
            finance = finance.merge(filter_df, how='inner')

        # Aggregate the volume
        finance_grouped_region = pd.DataFrame(
            finance.groupby([_region_col, _brand_col, _plan_period_col])[_volume_col].sum())
        finance_grouped_region.reset_index(inplace=True)

        # Filter for the required time period
        finance_grouped_region_20 = finance_grouped_region[finance_grouped_region[_plan_period_col] == plan_period]
        finance_grouped_region_20.columns = [_geo_name_col, _brand_col, _plan_period_col, _volume_col]

        # Create country-brand key
        finance_grouped_region_20['Country-Brand'] = finance_grouped_region_20[_geo_name_col] + " - " + \
                                                     finance_grouped_region_20[_brand_col]

        ## Append to all finance
        finance_grouped_sub = self.finance_grouped_20.append(finance_grouped_region_20)
        return finance_grouped_sub

    def get_cost_per_stimuli(self, _geo_col, _geo_name_col, _brand_col, _instrument_col
                             , _plan_period_col, plan_period, _spend_col, _stimuli_col, _cost_per_stimuli_col
                             , filter_df=pd.DataFrame()):

        spend_media_sub = self.spend_media_sub.copy()

        # Filter for only the brands/countries needed
        if not filter_df.empty:
            spend_media_sub = spend_media_sub.merge(filter_df, how='inner')

        self.logger.info('----- Group spends and stimuli by Country-Vehicle')
        spend_media_sub_grouped = spend_media_sub[spend_media_sub[_plan_period_col] ==
                                                       plan_period].groupby([_instrument_col, _geo_col]) \
            .agg({_spend_col: np.sum, _stimuli_col: np.sum})
        spend_media_sub_grouped.reset_index(inplace=True)

        self.logger.info('----- Calculate cost per stimuli')
        spend_media_sub_grouped[_cost_per_stimuli_col] = spend_media_sub_grouped[_spend_col] / spend_media_sub_grouped[
            _stimuli_col]

        self.logger.info('----- Get combination')
        spend_media_sub_grouped['Combination'] = spend_media_sub_grouped[_geo_col] + " - " + spend_media_sub_grouped[
            _instrument_col]
        # self.spend_media_sub_grouped = spend_media_sub_grouped
        return spend_media_sub_grouped

    def get_aggregated_media(self, _region_col, _geo_col, _instrument_col
                             , _spend_col, _stimuli_col, _cost_per_stimuli_col
                             , filter_df=pd.DataFrame()):

        spend_media_sub_grouped = self.spend_media_sub_grouped.merge(
            self.input_dict['geo_master'][[_region_col, _geo_col]].drop_duplicates(),
            how='left')

        # Filter for only the brands/countries needed
        if not filter_df.empty:
            spend_media_sub_grouped = spend_media_sub_grouped.merge(filter_df, how='inner')

        self.logger.info('----- Group spends and stimuli by Region-Vehicle')
        spend_media_sub_grouped_region = spend_media_sub_grouped.groupby([_region_col, _instrument_col]).agg(
            {_spend_col: np.sum, _stimuli_col: np.sum})
        spend_media_sub_grouped_region.reset_index(inplace=True)

        self.logger.info('----- Calculate cost per stimuli')
        spend_media_sub_grouped_region[_cost_per_stimuli_col] = spend_media_sub_grouped_region[_spend_col] / \
                                                                spend_media_sub_grouped_region[_stimuli_col]
        spend_media_sub_grouped_region['Combination'] = spend_media_sub_grouped_region[_region_col] + " - " + \
                                                        spend_media_sub_grouped_region[_instrument_col]

        return spend_media_sub_grouped_region

    def calc_cost_cartesian(self, grouped_spend_df, _cost_per_stimuli_col):
        self.logger.info('----- Append to all cost per stimuli')
        spend_media_sub_grouped = self.spend_media_sub_grouped.append(grouped_spend_df)
        spend_media_sub_grouped.reset_index(inplace=True)

        self.logger.info('----- Cost per stimuli ratio - cartesian')
        cost_combinations = spend_media_sub_grouped.loc[:,['Combination', _cost_per_stimuli_col]]
        cost_combinations['key'] = 1
        cost_combinations_copy = cost_combinations.copy()
        cost_combinations_copy.columns = ['Combination1', 'CostPerStimuli1', 'key']

        cost_cartesian = cost_combinations.merge(cost_combinations_copy, how='left')
        cost_cartesian['Differential_Cost'] = cost_cartesian[_cost_per_stimuli_col] / cost_cartesian['CostPerStimuli1']
        cost_cartesian['key_for_cost'] = cost_cartesian['Combination'] + " to " + cost_cartesian['Combination1']

        return cost_cartesian

    def filter_curves_for_aggregation(self, curves, aggregation, exclusions, inclusions
                                      , _instrument_col, _region_col, _spend_mean_col
                                      , _coeffA_col, _coeffB_col, _coeffC_col, adbudg_coeffs=1):
        region_curves = curves.copy()
        region_selected = aggregation.split(' - ')[0]
        instrument_selected = ' - '.join(aggregation.split(' - ')[1:])
        if region_selected != 'Global':
            self.logger.info(f'----- Fetching curves for the region {region_selected} and instrument {instrument_selected}')
            region_curves = region_curves.loc[(region_curves[_instrument_col] == instrument_selected) &
                                               (region_curves[_region_col] == region_selected), :]
            region_curves.reset_index(inplace=True, drop=True)
        else:
            self.logger.info(f'----- Global is selected. Fetching all curves with instrument {instrument_selected}.')
            region_curves = region_curves.loc[(region_curves[_instrument_col] == instrument_selected), :]
            region_curves.reset_index(inplace=True, drop=True)

        if adbudg_coeffs:
            del region_curves[_coeffA_col], region_curves[_coeffB_col], region_curves[_coeffC_col]

            # rename columns
            region_curves.rename(columns={"coefficienta_ad": "coefficienta", "coefficientb_ad": "coefficientb",
                                          "coefficientc_ad": "coefficientc"
                , "investment_mean": _spend_mean_col}, inplace=True)

        # Exclusions
        if exclusions:
            if not pd.isnull(exclusions):
                self.logger.info('----- Excluding certain curves in aggregation')
                filter_field = exclusions.split(':')[0]
                filter_value = eval(exclusions.split(':')[1])
                filter_value = filter_value if isinstance(filter_value, list) else [filter_value]
                region_curves = region_curves[region_curves.eval(filter_field + ' not in ' + str(filter_value))]
                region_curves.reset_index(inplace=True, drop=True)

        # Inclusions
        if inclusions:
            if not pd.isnull(inclusions):
                self.logger.info('----- Including certain curves in aggregation as per selection')
                filter_field = inclusions.split(':')[0]
                filter_value = [eval(inclusions.split(':')[1])]
                filter_value = filter_value if isinstance(filter_value, list) else [filter_value]
                region_curves = region_curves[region_curves.eval(filter_field + ' in ' + str(filter_value))]
                region_curves.reset_index(inplace=True, drop=True)

        return region_curves

    def transform_curves_format(self, df, curves_4
                                , _geo_col, _brand_col, _instrument_col, _coeffA_col, _coeffB_col, _coeffC_col
                                , _state_col, _subbrand_col, form_col, _stimuli_type_col, _rating_col):
        curves_1 = df.loc[:, [_geo_col, _brand_col, _instrument_col, _coeffA_col, _coeffB_col, _coeffC_col]]
        curves_1[_state_col] = '-'
        curves_1[_subbrand_col] = '-'
        dimension_cols = [_geo_col, _brand_col, _subbrand_col, _state_col]
        r_dimension_cols = ["receiving" + s for s in dimension_cols]
        curves_1[r_dimension_cols] = curves_1.loc[:, dimension_cols]
        curves_1[form_col] = 'ADBUDG'
        curves_1[_stimuli_type_col] = 'Spend'
        curves_1[_rating_col] = 3
        curves_1['carryover'] = 0.0
        curves_1['lag'] = 0.0
        curves_1['kpi'] = 'Default'
        curves_1['isdeleted'] = 'f'
        curves_1['isnonmeasured'] = 'f'
        curves_1['spend_type'] = 'Mixed'

        # curves_1 = curves_1.merge(CalcEngine.input_dict['geo_master'][[_geo_name_col, _geo_col]].drop_duplicates(), how='left')
        curves_1 = curves_1[curves_4.columns.tolist()]
        return curves_1

    def get_media_cost_df(self, curves_t, _brand_col, _subbrand_col, _geo_col, _state_col, _instrument_col, _week_col,
                          _cost_col):
        mediacost = curves_t.loc[:, [_brand_col, _subbrand_col, _geo_col, _state_col, _instrument_col]]
        mediacost.drop_duplicates(inplace=True)
        mediacost['GrowthDriver'] = 'Default'
        mediacost['Activity'] = 'Default'
        mediacost['key'] = 1
        self.input_dict['Calendar']['key'] = 1
        mediacost = mediacost.merge(self.input_dict['Calendar'].loc[:, [_week_col, 'key']], how='left')
        mediacost[_cost_col] = 1.0
        del mediacost['key']
        return mediacost

    def get_actual_plan_df(self, curves_t, _brand_col, _subbrand_col, _geo_col, _state_col, _instrument_col, _week_col,
                           _cost_col, _plan_period_col, plan_period):
        act_plan = curves_t.loc[:, [_brand_col, _subbrand_col, _geo_col, _state_col, _instrument_col]]
        act_plan.drop_duplicates(inplace=True)
        act_plan['GrowthDriver'] = 'All'
        act_plan['Activity'] = 'Activity 1'
        act_plan['key'] = 1
        self.input_dict['Calendar']['key'] = 1

        # TODO keep only 30 weeks of data
        Calendar = self.input_dict['Calendar'].loc[self.input_dict['Calendar'][_plan_period_col]==plan_period, :]
        Calendar.reset_index(inplace=True, drop=True)
        c_rows = min(max(30, Calendar.shape[0]), 30)
        Calendar = Calendar.iloc[0:c_rows, :]
        # act_plan = act_plan.merge(self.input_dict['Calendar'].loc[:, [_week_col, 'key']], how='left')
        act_plan = act_plan.merge(Calendar.loc[:, [_week_col, 'key']], how='left')

        # TODO change investment to actual sample spend -> coefficient c?
        act_plan['Investment'] = 10000.0
        act_plan['Agency'] = 'Default'
        act_plan['Type'] = 'PLAN'
        act_plan['ActualEndWeek'] = 'N'
        del act_plan['key']
        return act_plan

    def get_kpi_master(self, curves_t, kpi_template_df, _brand_col, _subbrand_col, _geo_col, _state_col
                       , _instrument_col, _region_col):

        self.logger.info('----- Get unique dimensions from curves data')
        curves_t_copy = curves_t.copy()
        curves_t_copy = self.get_region_col(curves_t_copy, _region_col, _geo_col)
        curves_t_copy[_state_col] = '-'
        curves_t_copy[_subbrand_col] = '-'
        final_kpi = curves_t_copy.loc[:,
                    [_brand_col, _subbrand_col, _geo_col, _state_col, _instrument_col, _region_col]]
        final_kpi.drop_duplicates(inplace=True)
        dimension_cols = [_brand_col, _subbrand_col, _geo_col, _state_col]
        r_dimension_cols = ["receiving" + s for s in dimension_cols]
        final_kpi[r_dimension_cols] = final_kpi.loc[:, dimension_cols]

        self.logger.info('----- Join on region/geo to get unique kpi parameter ids from template')
        NAM_index = final_kpi[_region_col] == 'NAM'
        final_kpi_nonNAM = final_kpi.loc[~NAM_index, :].merge(kpi_template_df[[_region_col, 'parameter_id']],
                                                              how='left')

        del final_kpi[_region_col]
        final_kpi_NAM = final_kpi.loc[NAM_index, :].merge(kpi_template_df[[_region_col, 'parameter_id']],
                                                          left_on=_geo_col
                                                          , right_on=_region_col, how='left')
        del final_kpi_NAM[_region_col], final_kpi_nonNAM[_region_col]

        self.logger.info('----- Append RoW with USA, CAN')
        final_kpi_NAM = final_kpi_NAM[final_kpi_nonNAM.columns.tolist()]
        final_kpi = final_kpi_nonNAM.append(final_kpi_NAM)

        self.logger.info('----- Get region column')
        final_kpi = self.get_region_col(final_kpi, _region_col, _geo_col)
        return final_kpi


    def get_LT_RoW(self, ETL_kpi, final_kpi, _brand_col, _instrument_col, _region_col):
        """
        Fetch LT parameters for Rest of World (RoW)
        :param ETL_kpi: class instance, with KPI input data
        :param final_kpi: pandas, dataframe with all KPI parameters
        :param _brand_col: str, column name of the brand
        :param _instrument_col: str, column name of the instrument/vehicle
        :param _region_col: str, column name of the region
        :return: pandas, dataframe with EURO LT kpi parameters
        """
        # Get LT values
        self.logger.info('----- Melt LT values df')
        # lt_values = config_path+'ME/AdditionalData/Templates/LT Multipliers.xlsx'
        # lt_values_df_all = pd.read_excel(lt_values, sheet_name='ALL')
        lt_values_df_all = ETL_kpi.input_dict['lt_values_df_all'].melt(id_vars='Brand', var_name='Instrument')
        lt_values_df_all.columns = lt_values_df_all.columns.str.lower().tolist()
        lt_values_df_all['parameter_id'] = 'LT_Multiplier'

        self.logger.info('----- Filter for LT multiplier')
        final_kpi_LT = final_kpi[final_kpi['parameter_id'] == 'LT_Multiplier']
        # For RoW
        EURO_index = final_kpi_LT[_region_col] == 'EUROPE & TURKEY'
        final_kpi_LT_RoW = final_kpi_LT.loc[~EURO_index, :]
        final_kpi_LT_RoW.reset_index(inplace=True, drop=True)

        self.logger.info('----- Join with lt values df')
        final_kpi_LT_RoW_t = final_kpi_LT_RoW.merge(lt_values_df_all, how='left')
        final_kpi_LT_RoW_values = final_kpi_LT_RoW_t[~np.isnan(final_kpi_LT_RoW_t['value'])]
        final_kpi_LT_RoW_missing = final_kpi_LT_RoW_t[np.isnan(final_kpi_LT_RoW_t['value'])]

        self.logger.info('----- If Brand is available but instrument not then we need to use  “Total Multiplier”')
        del final_kpi_LT_RoW_missing['value']
        final_kpi_LT_RoW_brand = final_kpi_LT_RoW_missing.merge(lt_values_df_all.loc[
                                                                    lt_values_df_all[
                                                                        _instrument_col] == 'Total Multiplier'
                                                                    , [_brand_col, 'value', 'parameter_id']]
                                                                , on=[_brand_col, 'parameter_id'], how='left')
        final_kpi_LT_RoW_values_brand = final_kpi_LT_RoW_brand[~np.isnan(final_kpi_LT_RoW_brand['value'])]
        final_kpi_LT_RoW_missing_brand = final_kpi_LT_RoW_brand[np.isnan(final_kpi_LT_RoW_brand['value'])]

        self.logger.info('----- If instrument is available but brand not then we need to use “Total”')
        del final_kpi_LT_RoW_missing_brand['value']
        final_kpi_LT_RoW_instrument = final_kpi_LT_RoW_missing_brand.merge(lt_values_df_all.loc[
                                                                               lt_values_df_all[_brand_col] == 'Total'
                                                                               , [_instrument_col, 'value',
                                                                                  'parameter_id']]
                                                                           , on=[_instrument_col, 'parameter_id'],
                                                                           how='left')

        self.logger.info('----- Append all KPI LT multipliers from RoW')
        final_kpi_LT_RoW = final_kpi_LT_RoW_values.append(final_kpi_LT_RoW_values_brand).append(
            final_kpi_LT_RoW_instrument)
        final_kpi_LT_RoW['value'].fillna(1.0, inplace=True)

        return final_kpi_LT_RoW


    def get_LT_EU(self, ETL_kpi, final_kpi,  _brand_col, _geo_col, _instrument_col, _region_col):
        """
        Fetch LT parameters for Europe
        :param ETL_kpi: class instance, with KPI input data
        :param final_kpi: pandas, dataframe with all KPI parameters
        :param _brand_col: str, column name of the brand
        :param _geo_col: str, column name of the country/geography
        :param _instrument_col: str, column name of the instrument/vehicle
        :param _region_col: str, column name of the region
        :return: pandas, dataframe with EURO LT kpi parameters
        """
        self.logger.info('Extract LT for EURO')
        # lt_values_df_eu = pd.read_excel(lt_values, sheet_name='EURO')
        # lt_values_df_eu.columns = lt_values_df_eu.columns.str.lower().tolist()
        lt_values_df_eu = ETL_kpi.input_dict['lt_values_df_eu'].loc[:, [_geo_col, _instrument_col, 'lt']]
        lt_values_df_eu['parameter_id'] = 'LT'

        self.logger.info('Filter for LT - EURO')
        final_kpi_LT = final_kpi.loc[final_kpi['parameter_id'] == 'LT', :]
        # For RoW
        EURO_index = final_kpi_LT[_region_col] == 'EUROPE & TURKEY'
        final_kpi_LT_EURO = final_kpi_LT.loc[EURO_index, :]
        final_kpi_LT_EURO.reset_index(inplace=True, drop=True)

        final_kpi_LT_EURO_t = final_kpi_LT_EURO.merge(lt_values_df_eu, how='left')
        final_kpi_LT_EURO_values = final_kpi_LT_EURO_t[~np.isnan(final_kpi_LT_EURO_t['lt'])]
        final_kpi_LT_EURO_missing = final_kpi_LT_EURO_t[np.isnan(final_kpi_LT_EURO_t['lt'])]

        self.logger.info('If a combination is not available, the average instrument level must be used')
        lt_values_df_eu_grouped = pd.DataFrame(lt_values_df_eu.groupby([_instrument_col])['lt'].mean())
        lt_values_df_eu_grouped.reset_index(inplace=True)
        del final_kpi_LT_EURO_missing['lt']
        final_kpi_LT_EURO_instrument = final_kpi_LT_EURO_missing.merge(lt_values_df_eu_grouped
                                                                       , on=[_instrument_col], how='left')

        self.logger.info('----- Append all KPI LT multipliers from EURO')
        final_kpi_LT_EURO = final_kpi_LT_EURO_values.append(final_kpi_LT_EURO_instrument)
        final_kpi_LT_EURO['lt'].fillna(1.0, inplace=True)
        final_kpi_LT_EURO.rename(columns={'lt': 'value'}, inplace=True)
        final_kpi_LT_EURO.reset_index(inplace=True, drop=True)

        return final_kpi_LT_EURO


    def get_LT_ALL(self, ETL_kpi, final_kpi, _brand_col, _geo_col, _instrument_col, _region_col, _region_selected):
        """
        Fetch LT parameters for Rest of World and Europe separately and append together
        :param ETL_kpi: class instance, with KPI input data
        :param final_kpi: pandas, dataframe with all KPI parameters
        :param _brand_col: str, column name of the brand
        :param _geo_col: str, column name of the country/geography
        :param _instrument_col: str, column name of the instrument/vehicle
        :param _region_col: str, column name of the region
        :return: pandas, dataframe with ALL LT kpi parameters
        """
        final_kpi_LT_RoW = self.get_LT_RoW(ETL_kpi, final_kpi, _brand_col, _instrument_col, _region_col)

        if _region_selected in ['EUROPE', 'EUROPE & TURKEY']:
            final_kpi_LT_EURO = self.get_LT_EU(ETL_kpi, final_kpi, _brand_col, _geo_col, _instrument_col, _region_col)
            final_kpi_LT_ALL = final_kpi_LT_EURO.append(final_kpi_LT_RoW.loc[:, final_kpi_LT_EURO.columns.tolist()])
        else:
            final_kpi_LT_ALL = final_kpi_LT_RoW

        del final_kpi_LT_ALL[_region_col]
        return final_kpi_LT_ALL

    def get_finance_KPI(CalcEngine, final_kpi, _brand_col, _geo_col, _region_col
                        , _volume_col, _gp_per_eu_col, _nsv_per_eu_col, _gp_per_lit_col, _category_col):
        CalcEngine.logger.info('----- Finance KPIs')
        CalcEngine.input_dict['finance'].loc[:, 'Country-Brand'] = CalcEngine.input_dict['finance'].loc[:, _geo_col] + " - " + \
                                                            CalcEngine.input_dict['finance'].loc[:, _brand_col]
        CalcEngine.input_dict['finance']['Profit'] = CalcEngine.input_dict['finance'][_volume_col] * \
                                                     CalcEngine.input_dict['finance'][_gp_per_eu_col]
        finance_gp_aggregated = pd.DataFrame(
            CalcEngine.input_dict['finance'].groupby(['Country-Brand']).agg({_volume_col: 'sum'
                                                                                , _nsv_per_eu_col: 'sum'
                                                                                , 'Profit': 'sum'}))
        finance_gp_aggregated.reset_index(inplace=True)

        CalcEngine.logger.info('----- Get category for country-brand')
        finance_category = CalcEngine.input_dict['finance'].loc[:, ['Country-Brand', _category_col]].drop_duplicates(subset='Country-Brand')
        finance_gp_aggregated = finance_gp_aggregated.merge(finance_category, how='left')

        CalcEngine.logger.info('----- Calculate gp per eu and gp per liter')
        finance_gp_aggregated[_gp_per_eu_col] = finance_gp_aggregated['Profit'] / finance_gp_aggregated[_volume_col]
        finance_gp_aggregated.loc[finance_gp_aggregated[_category_col] == 'Spirits', _gp_per_lit_col] = finance_gp_aggregated.loc[finance_gp_aggregated[_category_col]=='Spirits', _gp_per_eu_col]/9
        finance_gp_aggregated.loc[finance_gp_aggregated[_category_col] != 'Spirits', _gp_per_lit_col] = finance_gp_aggregated.loc[finance_gp_aggregated[_category_col]=='Spirits', _gp_per_eu_col]/90
        del finance_gp_aggregated[_category_col]

        finance_gp_aggregated.dropna(subset=[_gp_per_eu_col], inplace=True)
        finance_gp_aggregated.columns = ['Country-Brand', 'volume', 'Price', 'Profit', 'Profitability', _gp_per_lit_col]
        finance_gp_aggregated_melt = finance_gp_aggregated.melt(id_vars='Country-Brand', var_name='parameter_id',
                                                                value_name='value')

        final_kpi_finance = final_kpi.loc[
            final_kpi['parameter_id'].isin(['Price', 'Profitability', _gp_per_lit_col]), :]
        final_kpi_finance['Country-Brand'] = final_kpi_finance[_geo_col] + " - " + final_kpi_finance[_brand_col]
        final_kpi_finance_values = final_kpi_finance.merge(finance_gp_aggregated_melt, how='left')
        del final_kpi_finance_values['Country-Brand'], final_kpi_finance_values[_region_col]
        return final_kpi_finance_values


    def get_relevant_combinations(self, bus_inputs_data, curves, volume_info, _geo_col, _brand_col, _instrument_col
                                  , _spend_col):
        relevant_combinations = bus_inputs_data[[_geo_col, _brand_col, _instrument_col]].append(
            curves[[_geo_col, _brand_col, _instrument_col]])
        relevant_combinations.drop_duplicates(inplace=True)
        relevant_combinations.reset_index(inplace=True, drop=True)
        relevant_combinations[_spend_col] = pd.Series()

        self.logger.info('----- Get volume')
        relevant_combinations_vol = relevant_combinations[[_geo_col, _brand_col]].drop_duplicates()
        relevant_combinations_vol['Country-Brand'] = relevant_combinations_vol[_geo_col] + " - " + \
                                                     relevant_combinations_vol[_brand_col]
        relevant_combinations_vol = pd.merge(relevant_combinations_vol, volume_info, on='Country-Brand', how='left')
        del relevant_combinations_vol['Country-Brand']

        self.logger.info('----- Get mediacost')
        relevant_combinations_cost = relevant_combinations[[_geo_col, _instrument_col]].drop_duplicates()
        relevant_combinations_cost['MediaCost'] = pd.Series()

        return relevant_combinations, relevant_combinations_vol, relevant_combinations_cost


