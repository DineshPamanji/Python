import pandas as pd
import matplotlib.pyplot as plt
import matplotlib

matplotlib.use('Agg')
from src.helpers.curve_converter import CurveConverter
from src.helpers.DataLoader import DataLoader

path = 'C:/Projects/MarketCatalyst/GDM_Curve_Proxy/Codes/ProxyEstimation/'
config_path = path+'/config/'
input_path = path+'Input/'
spend_path = path+'Input/Final/Final'

# Filenames
functionalform_file = config_path+'FunctionalForm.json'
curves_file = input_path + "/ConsolidatedCurves/Consolidated Curves with Rating.csv"
spends_file = spend_path + "/11. Actual and Plan.csv"
media_file = spend_path + "/4. Media Cost.csv"
finance_file = path + "/Input/20210218 Finance Data v0/20210218 Data v0 Edited.csv"
calendar_file = path + "/Input/Calendar.xlsx"
country_code_file = path + "/Input/countries_codes_and_coordinates.csv"
bus_inputs_file = config_path + "Diageo Catalyst - Proxy Curves - Input template Test.xlsx"

# Column names
form_col = "functionalformname"
coefficient_cols = ['coefficienta', 'coefficientb', 'coefficientc', 'carryover', 'lag']

# Create instance with all the filenames
ETL = DataLoader({}, filepath_dict={"functionalforms": {"file": functionalform_file},
                                    "curves": {"file": curves_file},
                                    "spend": {"file": spends_file},
                                    "media": {"file": media_file},
                                    "finance": {"file": finance_file},
                                    "Calendar": {"file": calendar_file},
                                    "country_codes": {"file": country_code_file},
                                    "bus_inputs": {"file": bus_inputs_file, "sheet": "Inputs"}
                                    })
# Import all the files
all_files = ETL.import_files()

# Subset for 4-star rating curves
# curves_4 = ETL.filter_df(df=all_files['curves'], filters={'rating': ['==', 4], 'geography': ['!=', "'IND'"]})
curves_4 = all_files['curves'][(all_files['curves']['rating'] == 4) & (all_files['curves']['geography'] != 'IND')]
curves_4 = curves_4[(curves_4['coefficienta'] > 0) | (curves_4['coefficientb'] > 0) | (curves_4['coefficientc'] > 0)]
curves_4.reset_index(inplace=True, drop=True)


converter = CurveConverter(all_files['functionalforms'], curves_4, coefficient_cols)
converter.lower_case()

time = '2021-04-14'
curves_4_final = pd.read_excel(path+f'\Outputs\ConvertedCurves\{time}_Curves_converted_bad.xlsx')
comparison_df_final = pd.read_excel(path+f'\Outputs\ConvertedCurves\{time}_Converted_Curves_comparison_df_final.xlsx')

curves_4_final['Selection'] = curves_4_final['geography'] + " - "+curves_4_final['brand'] + " - "+curves_4_final['instrument']

for curve_row in range((len(curves_4_final))):
    # curve_row = 893
    # curve_row = 251

     # if curves_4_final['investment_mean'][curve_row] != 12500:
    comparison_df = comparison_df_final[comparison_df_final['CurveID']==curve_row]

    curve_name = converter.get_curve_name(curve_row, form_col)
    bcv_curve = curves_4_final['Selection'][curve_row].replace("/", " ")

    x = comparison_df['stimuli']
    actual = comparison_df['Actual']
    converted_adbudg = comparison_df['New_Adbudg']

    figure = plt.figure()

    plt.plot(x, actual)
    plt.plot(x, converted_adbudg)

    plt.legend([f'Actual_{curve_name}', 'Converted_ADBUDG'], loc='lower right')
    plt.savefig(path+f'\Outputs\ConvertedCurves\Plots\{curve_row}_{str(bcv_curve)}_vf_SUB.png')

comparison_df_final_93 = comparison_df_final[comparison_df_final['CurveID']==93]
curves_4_final.iloc[93,:]
comparison_df_final_93.to_csv(path+'\Outputs\ConvertedCurves\Converted_Curves_comparison_df_final_93.csv', index=False)
