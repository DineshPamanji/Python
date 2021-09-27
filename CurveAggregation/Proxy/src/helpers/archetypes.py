import pandas as pd
import numpy as np
import os

from src.helpers.utilities import force_float


class Archetypes:
    def __init__(self, bus_inputs, curves_4_final, logger
                 , _archetype_col, _sel_col, _spend_mean_col, _spend_col, _volume_col
                 , _coeffA_col, _coeffB_col, _coeffC_col
                 , _stimuli_type_col, _cost_col, _geo_col, _conversion_factor_col
                 , _agg_col, _instrument_col, _region_col, _stimuli_col, _pop_col, form_col):
        self.bus_inputs = bus_inputs
        self.curves_4_final = curves_4_final
        self.logger = logger
        self._archetype_col = _archetype_col
        self._sel_col = _sel_col
        self._spend_col = _spend_col
        self._spend_mean_col = _spend_mean_col
        self._volume_col = _volume_col
        self._coeffA_col = _coeffA_col
        self._coeffB_col = _coeffB_col
        self._coeffC_col = _coeffC_col
        self._stimuli_type_col = _stimuli_type_col
        self._cost_col = _cost_col
        self._geo_col = _geo_col
        self._conversion_factor_col = _conversion_factor_col
        self._agg_col = _agg_col
        self._instrument_col = _instrument_col
        self._region_col = _region_col
        self._stimuli_col = _stimuli_col
        self._pop_col = _pop_col
        self.form_col = form_col

    def exec_archetype1(self):
        bus_inputs_1 = self.bus_inputs[self.bus_inputs[self._archetype_col] == 'Archetype 1']
        bus_inputs_1 = bus_inputs_1[~((bus_inputs_1[self._sel_col].isnull()) | (bus_inputs_1[self._sel_col] == ''))]

        if len(bus_inputs_1) > 0:
            # Get proxy market curve
            self.logger.info('====== Executing for Archetype 1')

            try:
                # curves_4_final_USA = curves[curves[_geo_col] == 'USA']
                bus_inputs_1 = bus_inputs_1.merge(self.curves_4_final[[self._sel_col,"coefficienta_ad", "coefficientb_ad", "coefficientc_ad"
                                                                  , self._spend_mean_col
                                                                  , self._volume_col+'_Proxy'
                                                                  , self._spend_col + '_Proxy'
                                                                  # , 'country-brand' + '_Proxy'
                                                                  , 'mediacost' + '_Proxy'
                                                                  ]], left_on=self._sel_col
                                   , right_on=self._sel_col, how='left')

                # rename columns
                bus_inputs_1.rename(columns={"coefficienta_ad":"coefficienta", "coefficientb_ad":"coefficientb", "coefficientc_ad":"coefficientc"}, inplace=True)

                self.logger.info('----- Get volume ratio')
                bus_inputs_1 = force_float(bus_inputs_1, [self._volume_col+'_Proxy', self._volume_col])
                bus_inputs_1['volume_ratio'] = bus_inputs_1[self._volume_col]/bus_inputs_1[self._volume_col+'_Proxy']

                # Adjust coefficient a
                bus_inputs_1['coefficienta Selection'] = bus_inputs_1[self._coeffA_col]
                bus_inputs_1[self._coeffA_col] = bus_inputs_1[self._coeffA_col] * bus_inputs_1['volume_ratio']


                # TODO Duplicated curves - remove
                # Get duplicated rows
                ids = bus_inputs_1["bcv"]
                bus_inputs_1_final = bus_inputs_1[~ids.isin(ids[ids.duplicated()])].sort_values("bcv")
                bus_inputs_1_duplicated = bus_inputs_1[ids.isin(ids[ids.duplicated()])].sort_values("bcv")

                bus_inputs_1_final['Message'] = 'Success'

                bus_inputs_1_final = force_float(bus_inputs_1_final, [self._coeffA_col, self._coeffB_col
                    , self._coeffC_col, 'volume_ratio'
                    , self._volume_col+'_Proxy', self._volume_col, 'coefficienta Selection'])

                missing_index = (np.isnan(bus_inputs_1_final[[self._coeffA_col, self._coeffB_col
                                        , self._coeffC_col]])).any(axis="columns")
                vol_missing_index = (np.isnan(bus_inputs_1_final[['volume_ratio', self._volume_col+'_Proxy'
                                            , self._volume_col]])).any(axis="columns")
                coeff_missing_index = (np.isnan(bus_inputs_1_final[['coefficienta Selection']])).any(axis="columns")
                bus_inputs_1_final.loc[missing_index, 'Message'] = 'Missing Values'
                bus_inputs_1_final.loc[vol_missing_index, 'Message'] = 'Missing Volume'
                bus_inputs_1_final.loc[coeff_missing_index, 'Message'] = 'Missing Curves'
                self.logger.info('SUCCESS: Executing for Archetype 1')
            except Exception as e:
                self.logger.info(f'Exception message: {str(e)}')
                self.logger.info('Exception: Executing for Archetype 1')
        else:
            bus_inputs_1_final = pd.DataFrame(columns=['bcv', 'Message'])

        return bus_inputs_1_final

    def exec_archetype2(self, pop_ratio):
        bus_inputs_2 = self.bus_inputs[self.bus_inputs[self._archetype_col] == 'Archetype 2']
        bus_inputs_2 = bus_inputs_2[~bus_inputs_2[self._sel_col].isnull()]

        if len(bus_inputs_2) > 0:
            # Get proxy market curve
            try:
                self.logger.info('====== Executing for Archetype 2')
                bus_inputs_2 = bus_inputs_2.merge(
                    self.curves_4_final[[self._sel_col, "coefficienta_ad", "coefficientb_ad", "coefficientc_ad"
                        , self._spend_mean_col
                        , self._stimuli_type_col
                        , self._volume_col + '_Proxy'
                        , self._spend_col + '_Proxy'
                                    # , 'country-brand' + '_Proxy'
                        , 'mediacost' + '_Proxy'
                                    # , _conversion_factor_col
                        , self._cost_col]],
                    left_on=self._sel_col
                    , right_on=self._sel_col, how='left')

                # rename columns
                bus_inputs_2.rename(columns={"coefficienta_ad": "coefficienta", "coefficientb_ad": "coefficientb",
                                             "coefficientc_ad": "coefficientc"}, inplace=True)

                self.logger.info('----- Get volume ratio')
                bus_inputs_2 = force_float(bus_inputs_2, [self._volume_col + '_Proxy', self._volume_col])
                bus_inputs_2['volume_ratio'] = bus_inputs_2[self._volume_col] / bus_inputs_2[self._volume_col + '_Proxy']

                # # Get media cost - convert any stimuli to spend
                # bus_inputs_2 = bus_inputs_2.merge(CalcEngine.spend_media_sub_grouped[['Combination', _cost_per_stimuli_col]].drop_duplicates(), left_on='BV Selection'
                #                                        , right_on='Combination', how='left')

                # Adjust coefficient a, c
                bus_inputs_2['mediacost'] = pd.to_numeric(bus_inputs_2['mediacost'])
                bus_inputs_2['mediacost' + '_Proxy'] = pd.to_numeric(bus_inputs_2['mediacost' + '_Proxy'])

                bus_inputs_2['Differential_Cost'] = bus_inputs_2['mediacost'] / bus_inputs_2['mediacost' + '_Proxy']

                self.logger.info('----- Fill missing differential cost')
                bus_inputs_2['combination'] = bus_inputs_2[self._geo_col] + ' - ' + \
                                              bus_inputs_2[self._sel_col].str.split(" - ").str[0]
                bus_inputs_2 = bus_inputs_2.merge(pop_ratio, how='left')
                cost_missing_index = (np.isnan(bus_inputs_2[['Differential_Cost']])).any(axis="columns")
                # bus_inputs_2.to_csv(config_path+f'/ME/Pop_ratio_proxy_{_region_selected}.csv', index=False)
                bus_inputs_2.loc[cost_missing_index, 'Differential_Cost'] = bus_inputs_2.loc[
                    cost_missing_index, 'pop_ratio']

                bus_inputs_2[self._cost_col].fillna(1, inplace=True)

                # bus_inputs_2['Differential_Cost'].fillna(1.0, inplace=True)
                bus_inputs_2['coefficienta Selection'] = bus_inputs_2[self._coeffA_col]
                bus_inputs_2['coefficientc Selection'] = bus_inputs_2[self._coeffC_col]
                bus_inputs_2[self._coeffA_col] = bus_inputs_2[self._coeffA_col] * bus_inputs_2['volume_ratio']
                bus_inputs_2[self._coeffC_col] = bus_inputs_2[self._coeffC_col] * bus_inputs_2['Differential_Cost'] * \
                                            bus_inputs_2[self._cost_col] / bus_inputs_2[self._conversion_factor_col]
                # bus_inputs_2[_coeffC_col] = bus_inputs_2[_coeffC_col] * bus_inputs_2['Differential_Cost'] /bus_inputs_2[_conversion_factor_col]

                bus_inputs_2['Message'] = 'Success'

                bus_inputs_2 = force_float(bus_inputs_2, [self._coeffA_col, self._coeffB_col
                    , self._coeffC_col, 'volume_ratio'
                    , self._volume_col + '_Proxy', self._volume_col, 'coefficienta Selection', 'Differential_Cost'])

                missing_index = (np.isnan(bus_inputs_2[[self._coeffA_col, self._coeffB_col
                    , self._coeffC_col]])).any(axis="columns")
                vol_missing_index = (np.isnan(bus_inputs_2[['volume_ratio', self._volume_col + '_Proxy'
                    , self._volume_col]])).any(
                    axis="columns")
                coeff_missing_index = (np.isnan(bus_inputs_2[['coefficienta Selection']])).any(axis="columns")
                cost_missing_index = (np.isnan(bus_inputs_2[['Differential_Cost']])).any(axis="columns")
                bus_inputs_2.loc[missing_index, 'Message'] = 'Missing Values'
                bus_inputs_2.loc[vol_missing_index, 'Message'] = 'Missing Volume'
                bus_inputs_2.loc[coeff_missing_index, 'Message'] = 'Missing Curves'
                bus_inputs_2.loc[cost_missing_index, 'Message'] = 'Missing Cost'

                self.logger.info('SUCCESS: Executing for Archetype 2')
            except Exception as e:
                self.logger.info(f'Exception message: {str(e)}')
                self.logger.info('Exception: Executing for Archetype 2')
        else:
            bus_inputs_2 = pd.DataFrame(columns=['bcv', 'Message'])

        return bus_inputs_2

    def exec_archetype3(self, CalcEngine, CurveConverter, ETL, coefficient_cols):
        bus_inputs_3 = self.bus_inputs[
            self.bus_inputs['archetype'].isin(['Archetype 3', 'Archetype 4'])]  ## TOBE CHANGED TO ARCHETYPE 3
        bus_inputs_3 = bus_inputs_3[~((bus_inputs_3[self._agg_col].isnull()) | (bus_inputs_3[self._agg_col] == ''))]
        # bus_inputs_3['BV Selection'] = bus_inputs_3[_agg_col]#.str.split(' - ').str[0] + " - "+ bus_inputs_3[_agg_col].str.split(' - ').str[2]

        if len(bus_inputs_3) > 0:
            self.logger.info('====== Executing for Archetype 3, 4')
            try:
                ## LOOP starts here
                bus_inputs_3.reset_index(inplace=True, drop=True)
                bus_inputs_3_all = bus_inputs_3.copy()
                # bus_inputs_3_all['exclusions'][2] = "sector:'Beer'"
                new_coeffs_3_all = []
                message_all = []

                for i in range(len(bus_inputs_3_all)):
                    # i=10
                    self.logger.info('Executing for row: ' + str(i))
                    bus_inputs_3 = pd.DataFrame(bus_inputs_3_all.iloc[i, :]).T
                    bus_inputs_3.reset_index(inplace=True, drop=True)

                    self.logger.info('----- Get all market curves for aggregation')
                    region_curves = CalcEngine.filter_curves_for_aggregation(self.curves_4_final,
                                                                             aggregation=bus_inputs_3[self._agg_col][0]
                                                                             , exclusions=bus_inputs_3['exclusions'][0]
                                                                             , inclusions=bus_inputs_3['inclusions'][0]
                                                                             , _instrument_col=self._instrument_col,
                                                                             _region_col=self._region_col,
                                                                             _spend_mean_col=self._spend_mean_col
                                                                             , _coeffA_col=self._coeffA_col,
                                                                             _coeffB_col=self._coeffB_col,
                                                                             _coeffC_col=self._coeffC_col)
                    if len(region_curves) > 1:
                        region_curves = region_curves.loc[region_curves[self._coeffC_col] < 2000000, :]
                        region_curves.reset_index(inplace=True, drop=True)

                    if len(region_curves) == 0:
                        new_coeffs_3_all.append([0, 0, 0])
                        message_all.append('Missing Curves')
                        continue
                    self.logger.info('----- Calculate differential cost')
                    region_curves['mediacost' + '_Proxy'] = pd.to_numeric(region_curves['mediacost' + '_Proxy'])
                    region_curves[self._stimuli_col] = region_curves[self._spend_col + '_Proxy'] / region_curves[
                        'mediacost' + '_Proxy']
                    diff_cost = region_curves[self._spend_col + '_Proxy'].sum() / region_curves[self._stimuli_col].sum()
                    if (~np.isfinite(diff_cost) or (np.isnan(diff_cost))):
                        self.logger.info('----- No differential cost. Taking proxy from population')
                        region_curves = region_curves.merge(
                            CalcEngine.input_dict['population'].loc[:, [self._geo_col, self._pop_col]]
                            , on=self._geo_col, how='left')
                        bus_inputs_3 = bus_inputs_3.merge(
                            CalcEngine.input_dict['population'].loc[:, [self._geo_col, self._pop_col]]
                            , on=self._geo_col, how='left')
                        diff_cost = bus_inputs_3[self._pop_col].sum() / region_curves[self._pop_col].sum()
                    else:
                        self.logger.info('----- Keep only rows with stimuli col')
                        region_curves = region_curves.loc[~np.isnan(region_curves[self._stimuli_col]), :]
                        region_curves.reset_index(inplace=True, drop=True)
                    bus_inputs_3['Differential_Cost'] = diff_cost

                    if region_curves[self._geo_col].nunique() == 1:
                        if region_curves[self._geo_col].unique()[0] == bus_inputs_3[self._geo_col][0]:
                            bus_inputs_3['Differential_Cost'] = 1
                            bus_inputs_3['mediacost' + '_Proxy'] = 1
                            bus_inputs_3[self._conversion_factor_col] = 1

                    self.logger.info('----- Calculate volume ratio')
                    bus_inputs_3[self._volume_col + '_Proxy'] = region_curves[self._volume_col + '_Proxy'].sum()
                    bus_inputs_3['volume_ratio'] = bus_inputs_3[self._volume_col] / bus_inputs_3[self._volume_col + '_Proxy']

                    if ((np.isnan(bus_inputs_3['Differential_Cost'][0])) or
                            (~np.isfinite(bus_inputs_3['Differential_Cost'][0]))):
                        new_coeffs_3_all.append([0, 0, 0])
                        message_all.append('Missing Cost')
                        continue
                    elif ((np.isnan(bus_inputs_3['volume_ratio'][0])) or
                          (~np.isfinite(bus_inputs_3['volume_ratio'][0]))):
                        new_coeffs_3_all.append([0, 0, 0])
                        message_all.append('Missing Volume')
                        continue
                    # Get market conversion
                    # bus_inputs_3 = bus_inputs_3.merge(CalcEngine.market_currency_rate[[_geo_col, _brand_col, _conversion_factor_col]].drop_duplicates(), how='left')
                    # #  remove the hardcoding for missing currency conversion factor
                    # if np.isnan(bus_inputs_3[_conversion_factor_col][0]):
                    #     bus_inputs_3[_conversion_factor_col] = 0.87

                    ## Adjust curve
                    if len(region_curves) == 0:
                        new_coeffs_3_all.append([0, 0, 0])
                        message_all.append('No curves to aggregate.')
                    else:
                        converter = CurveConverter(ETL.input_dict['functionalforms'], region_curves, coefficient_cols,
                                                   self.logger)
                        converter.lower_case()

                        total_adstock = np.array([])
                        total_output = np.array([])
                        region_curves_sub = region_curves  # [0:2]
                        stimuli_mean = region_curves_sub[self._spend_mean_col].mean()
                        stimuli_mean = 10000 if np.isnan(stimuli_mean) else stimuli_mean
                        # stimuli_mean = 1000
                        for j in range(len(region_curves_sub)):
                            # i=0
                            curve_row = j
                            # print(curve_row)
                            # media_cost_value = region_curves_sub.loc[curve_row, _cost_per_stimuli_col]
                            media_cost_value = region_curves_sub.loc[curve_row, 'mediacost' + '_Proxy']
                            # if np.isnan(media_cost_value):
                            #     continue
                            media_cost_value = region_curves_sub.loc[:, 'mediacost' + '_Proxy'].mean() if np.isnan(
                                media_cost_value) else media_cost_value
                            media_cost_value = 1 if np.isnan(media_cost_value) else media_cost_value
                            # print(media_cost_value)
                            curr_conv_factor = region_curves_sub.loc[curve_row, self._conversion_factor_col]
                            # curr_conv_factor = 1.0 if np.isnan(region_curves_sub.loc[curve_row, _spend_mean_col]) else curr_conv_factor
                            # curr_conv_factor = 1.0 if np.isnan(curr_conv_factor) else curr_conv_factor
                            # # INR TO GBP conversion is fixed: ONLY INDIA IS MISSING FACTOR NOW - CHANGE
                            curr_conv_factor = 0.0098 if np.isnan(curr_conv_factor) else curr_conv_factor

                            # print(curr_conv_factor)
                            # Adjusting for media cost and currency conversion - divide by currency conversion to convert to local currency divide by media cost to get stimuli
                            # if stimuli_mean == 10000:
                            stimuli = stimuli_mean / (media_cost_value * curr_conv_factor)
                            # else:
                            #     stimuli = stimuli_mean
                            # print(stimuli)
                            converter.create_adstock(stimuli, curve_row, ntimes=10, step=1, step2=5)
                            curve_name = converter.get_curve_name(curve_row, self.form_col)
                            df = converter.eval_curve('ADBUDG', 'Y_actual')

                            # Change the X axis to local spends - without conversion
                            converter.create_adstock(stimuli_mean, curve_row, ntimes=10, step=1, step2=5)

                            if j == 0:
                                total_adstock = converter.adstock
                                total_output = converter.Y_actual
                            else:
                                total_adstock += converter.adstock
                                total_output += converter.Y_actual

                        converter.Y_actual = total_output

                        # Scale X for differential cost
                        adstock_factor = bus_inputs_3['Differential_Cost'][0] / (
                                    len(region_curves_sub) * bus_inputs_3[self._conversion_factor_col][0])
                        converter.scale_adstock(adstock_factor)
                        converter.set_volumes(region_curves_sub[self._volume_col + '_Proxy'].sum(),
                                              bus_inputs_3[self._volume_col][0])

                        coefficienta = region_curves_sub[self._coeffA_col].mean()
                        coefficientb = 1
                        coefficientc = region_curves_sub[self._coeffC_col].mean()  # *adstock_factor
                        coeffs = np.array([coefficienta, coefficientb, coefficientc])

                        # Append new coeffs
                        if any([np.isnan(region_curves_sub[self._volume_col + "_Proxy"].sum())
                                   , np.isnan(bus_inputs_3[self._volume_col][0])
                                # , np.isnan(total_output[0])
                                # , np.isnan(total_adstock[0])
                                   , bus_inputs_3[self._volume_col][0] == 0]):
                            new_coeffs_3_all.append([0, 0, 0])
                            message_all.append('Missing Volume')
                        elif any([np.isnan(total_output[0])
                                     , np.isnan(total_adstock[0])]):
                            new_coeffs_3_all.append([0, 0, 0])
                            message_all.append('Missing Adstock')

                        else:
                            # Get new curve parameters
                            # new_coeffs_3 = converter.get_new_coeffs_volume(coeffs)
                            new_coeffs_list, diff_list = converter.minimize_options_volume(
                                [[1600, 1, 2000], coeffs.copy()])
                            new_coeffs_3 = new_coeffs_list[np.argmin(diff_list)]
                            new_coeffs_3_all.append(new_coeffs_3)
                            message_all.append('Success')

                        # # Scale adstock back for right comparison
                        # converter.scale_adstock(1/adstock_factor)
                        #
                        # converter.eval_adbudg(new_coeffs_3)
                        # comparison_df = pd.DataFrame(data=[converter.adstock, converter.Y_actual, converter.new_adbudg]).T
                        # comparison_df.columns = [_stimuli_col, 'Actual', 'New_Adbudg']
                        # comparison_df['CurveID'] = curve_row

                        # comparison_df.to_csv(path+'\Outputs\ConvertedCurves\Converted_Curves_comparison_df_final_VNM.csv', index=False)

                bus_inputs_3_all['New_Coeffs'] = new_coeffs_3_all
                bus_inputs_3_all['Message'] = message_all
                bus_inputs_3_all[[self._coeffA_col, self._coeffB_col, self._coeffC_col]] = pd.DataFrame(
                    bus_inputs_3_all['New_Coeffs'].tolist()
                    , index=bus_inputs_3_all.index)
                self.logger.info('SUCCESS: Executing for Archetype 3, 4')
            except Exception as e:
                self.logger.info(f'Exception message: {str(e)}')
                self.logger.info('Exception: Executing for Archetype 3, 4')
        else:
            bus_inputs_3_all = pd.DataFrame(columns=['bcv', 'Message'])

        return bus_inputs_3_all