import pandas as pd
import numpy as np

from src.helpers.utilities import pandas_replace


from src.helpers.DataLoader import DataLoader
from src.helpers.curve_converter import CurveConverter

path = 'C:/Projects/MarketCatalyst/GDM_Curve_Proxy/Codes/Proxy Estimation/'
config_path = path+'/config/'
input_path = path+'Input/'
spend_path = path+'Input/Final/Final'

# Filenames
raw_file = config_path + "Inputs/RawBCV_CR.xlsx"
taxonomy_file = config_path + "Inputs/Catalyst - GDM taxonomy changes - 17Mar2021_Final.xlsx"
available_file = config_path + "Inputs/AvailableCurves.xlsx"
atlbtl_file = input_path + "SpendsFY20/ATL BTL spend FY20.xlsx"
geo_file = input_path + "Geographies.xlsx"

# Column names
# Create instance with all the filenames
ETL = DataLoader({}, filepath_dict={
                                    "raw": {"file": raw_file, "sheet": "Raw"},
                                    "taxonomy": {"file": taxonomy_file, "sheet": "Taxonomy changes"},
                                    "available": {"file": available_file, "sheet": "Available"},
                                    "atlbtl": {"file": atlbtl_file, "sheet": "Sheet1"},
                                    "geo_master": {"file": geo_file, "sheet": "Finance"}})
# Import all the files
all_files = ETL.import_files(lowercase=1)

raw = all_files['raw']
taxonomy = all_files['taxonomy']
available = all_files['available']
atlbtl = all_files['atlbtl']
geo_master = all_files['geo_master']

# Get taxonomy dict
taxonomy.dropna(subset=['From'], inplace=True)
taxonomy.dropna(subset=['To'], inplace=True)
taxonomy_dict = {s: a for s, a in zip(taxonomy['From'], taxonomy['To'])}

# Convert raw to BCV combinations
raw_cols = raw.columns.tolist()
raw['Vehicles'] = raw['Vehicles'].str.split(',')
raw = raw.set_index(['Country', 'Brand'])['Vehicles'].apply(pd.Series).stack()
raw =pd.DataFrame(raw).reset_index()
del raw['level_2']
raw.columns = raw_cols
raw_melt = raw.copy()
raw_renamed = pandas_replace(raw_melt, taxonomy_dict,verbose=1)


# Get available
available_c = available.copy()
available_renamed = pandas_replace(available_c, taxonomy_dict,verbose=1)

available_renamed.to_csv(config_path + "Inputs/AvailableCurves_renamed.csv")
raw_renamed.to_csv(config_path + "Inputs/RawBCV_CR_renamed.csv")

# Get atlbtl
atlbtl_c = atlbtl.copy()
atlbtl_renamed = pandas_replace(atlbtl_c, taxonomy_dict,verbose=1)

atlbtl_renamed.to_csv(input_path + "SpendsFY20/ATL BTL spend FY20 renamed.csv")

## digital spends

import glob
files = glob.glob(input_path + "SpendsFY20/Digital/*")

i=0
for file in files:
    df = pd.read_excel(file, skiprows=list(range(10)))
    if i==0:
        final_df = df
        i=1
    else:
        final_df = final_df.append(df)

final_df.reset_index(inplace=True, drop=True)
final_df_sub = final_df[['Market', 'Brand', 'Channel', 'Spend', 'Impressions', 'Clicks']]

final_df_sub_grp = final_df_sub.groupby(['Market', 'Brand', 'Channel']).agg({'Spend': 'sum'
                                                                             ,'Impressions':'sum'
                                                                             ,'Clicks':'sum'})
final_df_sub_grp.reset_index(inplace=True)

final_df_sub_grp['CPM'] = final_df_sub_grp['Spend']*100/final_df_sub_grp['Impressions']
final_df_sub_grp['CPC'] = final_df_sub_grp['Spend']/final_df_sub_grp['Clicks']

final_df_sub_grp['Market'].unique()

final_df_sub_grp = final_df_sub_grp.merge(geo_master[['Geography_Name', 'geography']].drop_duplicates()
                       , left_on='Market'
                       , right_on='Geography_Name'
                       ,how='left'
                                          , copy=False)
del final_df_sub_grp['Geography_Name']



final_df_sub_grp_c = final_df_sub_grp.copy()
final_df_sub_grp_renamed = pandas_replace(final_df_sub_grp_c, taxonomy_dict,verbose=1)

final_df_sub_grp_renamed.to_csv(input_path + "SpendsFY20/Digital renamed.csv", index=False)