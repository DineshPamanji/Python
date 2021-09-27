import pandas as pd
import numpy as np
from scipy.optimize import minimize
from math import atan, exp


class CurveConverter:
    def __init__(self, functionalforms, curves, coefficient_cols, logger):
        self.functionalforms = functionalforms
        self.curves = curves
        self.coefficient_cols = coefficient_cols
        self.coefficienta = np.array(self.curves['coefficienta'])
        self.coefficientb = np.array(self.curves['coefficientb'])
        self.coefficientc = np.array(self.curves['coefficientc'])
        self.output_curve = 'ADBUDG'

        self.actual_field = 'Y_actual'
        self.output_field = 'Y_adbudg'
        self.adstock_field = 'adstock'
        self.logger = logger


    def lower_case(self):
        self.functionalforms = dict((k, v.lower()) for k, v in self.functionalforms.items())

    def get_curve_name(self, row, field):
        return self.curves.loc[row, field]

    def get_curve_equation(self, formname):
        return self.functionalforms[formname]

    def get_coefficienta(self, row):
        return self.coefficienta[row]

    def curve_eval(self, coefficienta, coefficientb, coefficientc, adstock, curve_name):
        return eval(self.get_curve_equation(curve_name))

    def create_adstock(self, stimuli, curve_row, ntimes=5,step=10, step2=20, nrows=None):
        self.df = pd.DataFrame(columns=self.coefficient_cols)
        # self.df['Percentage'] = pd.Series(np.arange(10, 100*ntimes, step) / 100)
        self.df['Percentage'] = pd.Series(np.array(list(np.arange(10, 120, step) / 100)+ list(np.arange(120, 100*ntimes
                                                                                                        , step2) / 100)))
        if nrows:
            try:
                self.df = self.df.iloc[0:nrows,:]
            except:
                self.df = self.df

        self.df['adstock'] = self.df['Percentage'] * stimuli
        self.adstock = self.df['adstock'].values
        # print(self.coefficienta[curve_row])
        self.df['coefficienta'] = self.coefficienta[curve_row]
        self.df['coefficientb'] = self.coefficientb[curve_row]
        self.df['coefficientc'] = self.coefficientc[curve_row]
        # return self.df

    def scale_adstock(self, multiplier=1):
        self.adstock = self.adstock * multiplier

    def eval_curve(self, curve_name, output_field):
        # self.df[output_field] = self.df.eval(self.get_curve_equation(curve_name))
        self.df['curve_name'] = curve_name
        self.df[output_field] = np.vectorize(self.curve_eval)(self.df['coefficienta'], self.df['coefficientb'], self.df['coefficientc'], self.df['adstock'], self.df['curve_name'])
        self.Y_actual = self.df[output_field].values
        return self.df

    def eval_curve_series(self, df, curve_name, output_field, coefficienta, coefficientb
                          , coefficientc):
        df['coefficienta'] = coefficienta
        df['coefficientb'] = coefficientb
        df['coefficientc'] = coefficientc
        df[output_field] = df.eval(self.get_curve_equation(curve_name))
        return df

    def eval_adbudg(self, input_dict):
        coefficienta = input_dict[0]
        coefficientb = input_dict[1]
        coefficientc = input_dict[2]
        self.new_adbudg = (coefficienta * ((self.adstock ** coefficientb) / ((self.adstock ** coefficientb) + (coefficientc ** coefficientb))))

    def set_volumes(self, actual, new):
        self.actual_volume = actual
        self.new_volume = new

    def get_objective_numpy(self, input_dict):
        coefficienta= input_dict[0]
        coefficientb= input_dict[1]
        coefficientc= input_dict[2]
        self.new_adbudg = (coefficienta * ((self.adstock ** coefficientb)/((self.adstock ** coefficientb) + (coefficientc ** coefficientb))))
        return np.sum(np.absolute(self.new_adbudg - self.Y_actual))


    def get_objective_numpy_volume(self, input_dict):
        coefficienta= input_dict[0]
        coefficientb= input_dict[1]
        coefficientc= input_dict[2]
        self.new_adbudg = (coefficienta * ((self.adstock ** coefficientb)/((self.adstock ** coefficientb) + (coefficientc ** coefficientb))))
        return np.sum(np.absolute((self.new_adbudg/self.new_volume) - (self.Y_actual/self.actual_volume)))


    def get_objective_value(self, final_curve, actual_curve):
        return np.sum(np.absolute(final_curve - actual_curve))

    def get_new_coeffs(self, coeffs):
        # bnds = ((0, float('inf')),)*len(coeffs)
        model = minimize(self.get_objective_numpy, coeffs,  method='nelder-mead'# method='SLSQP'
                         #, bounds=bnds
                         , tol=1e-2, options={'maxiter': 10000})
        if model.success:
            return np.array(model.x.tolist())
        else:
            return np.array(model.x.tolist())

    def get_new_coeffs_bounds(self, coeffs):
        bnds = ((0, float('inf')),)*len(coeffs)
        model = minimize(self.get_objective_numpy, coeffs,  method='SLSQP'
                         , bounds=bnds
                         , tol=1e-8, options={'maxiter': 10000})
        if model.success:
            return np.array(model.x.tolist())
        else:
            return [0, 0, 0]

    def get_new_coeffs_volume(self, coeffs):
        # bnds = ((0, float('inf')),)*len(coeffs)
        model = minimize(self.get_objective_numpy_volume, coeffs, method='nelder-mead'
                         # , bounds=bnds
                         , tol=1e-2, options={'maxiter': 100000})
        if model.success:
            return np.array(model.x.tolist())
        else:
            return np.array(model.x.tolist())#[0, 0, 0]

    def get_new_coeffs_volume_bnds(self, coeffs):
        bnds = ((0, float('inf')),)*len(coeffs)
        model = minimize(self.get_objective_numpy_volume, coeffs,  method='SLSQP'
                         , bounds=bnds
                         , tol=1e-2, options={'maxiter': 100000})
        if model.success:
            return np.array(model.x.tolist())
        else:
            return [0, 0, 0]

    def minimize_options(self, coeffs_list, power=0):
        new_coeffs_list = []
        for i in coeffs_list:
            new_coeffs_list.append(self.get_new_coeffs_bounds(i))
            if power == 0:
                new_coeffs_list.append(self.get_new_coeffs(i))
        diff_list = []
        for new_coeff in new_coeffs_list:
            self.eval_adbudg(new_coeff)
            diff_list.append(np.sum(np.absolute(self.new_adbudg - self.Y_actual)))
        return new_coeffs_list, diff_list

    def minimize_options_volume(self, coeffs_list, power=0):
        new_coeffs_list = []
        for i in coeffs_list:
            new_coeffs_list.append(self.get_new_coeffs_volume_bnds(i))
            new_coeffs_list.append(self.get_new_coeffs_volume(i))
        diff_list = []
        for new_coeff in new_coeffs_list:
            self.eval_adbudg(new_coeff)
            diff_list.append(np.sum(np.absolute(self.new_adbudg - self.Y_actual)))
        return new_coeffs_list, diff_list

    def convert_curve(self, curves_4, coeffs, form_col,  _spend_mean_col, _stimuli_col, _stimuli_type_col, curve_row):
        stimuli = curves_4[_spend_mean_col][curve_row]
        curve_name = self.get_curve_name(curve_row, form_col)
        power = 0
        coeffs[1] = 1
        if curve_name in ['POWER']:
            stimuli = 40000 / (curves_4['coefficienta'][curve_row] + 10)
            power = 1 if curves_4['coefficienta'][curve_row] < 5 else 0
            coeffs[1] = curves_4['coefficientb'][curve_row]
        if curve_name in ['LINEAR']:
            stimuli = 1000 / curves_4['coefficienta'][curve_row]
        if curve_name in ['NEGATIVE_EXPONENTIAL']:
            stimuli = max(curves_4['coefficienta'][curve_row], stimuli)
        # print(curves_4[_stimuli_type_col][curve_row])
        if (curves_4[_stimuli_type_col][curve_row] == 'Impressions'):
            stimuli = 10000000
        # print(stimuli)

        self.create_adstock(stimuli, curve_row, ntimes=5, step=1, step2=10)
        df = self.eval_curve(curve_name, 'Y_actual')
        if curve_name == 'ADBUDG':
            new_coeffs = df[['coefficienta', 'coefficientb', 'coefficientc']].iloc[0, :].values
        else:
            coeffs_sub = df[['coefficienta', 'coefficientb', 'coefficientc']].iloc[0, :].values
            coeffs_sub[0] = max(coeffs_sub[0], 1)
            coeffs_sub[1] = 1 if curve_name != 'POWER' else coeffs_sub[1]
            coeffs_sub[2] = max(coeffs_sub[2], 1) if curve_name != 'ATAN_LINEAR' else stimuli

            import warnings
            warnings.filterwarnings("error")

            if curve_name in ['LINEAR']:
                try:
                    new_coeffs = self.get_new_coeffs_bounds(coeffs.copy())
                except Warning:
                    new_coeffs = self.get_new_coeffs(coeffs.copy())
            else:
                warnings.filterwarnings("ignore")
                # new_coeffs = converter.get_new_coeffs(coeffs.copy())
                new_coeffs_list, diff_list = self.minimize_options([coeffs_sub, coeffs.copy()], power)
                result = np.all(diff_list == diff_list[0])
                if (result) & (power == 0):
                    self.logger.info('----- Reducing the scale of stimuli to fit the curve better')
                    self.create_adstock(stimuli / 10, curve_row, ntimes=5, step=1, step2=30)
                    df = self.eval_curve(curve_name, 'Y_actual')
                    new_coeffs_list, diff_list = self.minimize_options([coeffs_sub, coeffs.copy()])
                new_coeffs = new_coeffs_list[np.argmin(diff_list)]
        self.eval_adbudg(new_coeffs)
        comparison_df = pd.DataFrame(data=[self.adstock, self.Y_actual, self.new_adbudg]).T
        comparison_df.columns = [_stimuli_col, 'Actual', 'New_Adbudg']
        comparison_df['CurveID'] = curve_row

        return pd.DataFrame(new_coeffs).T, comparison_df

    def convert_curve_group(self, curves_4, coeffs, form_col,  _spend_mean_col, _stimuli_col, _stimuli_type_col, i, j):
        results = []
        comparison_df_final = []
        for curve_row in range(i, j):
            new_coeffs, comparison_df = self.convert_curve(curves_4, coeffs, form_col, _spend_mean_col, _stimuli_col,
                                                           _stimuli_type_col, curve_row)
            results.append(new_coeffs)
            comparison_df_final.append(comparison_df)
        return results, comparison_df_final




