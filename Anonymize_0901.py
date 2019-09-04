# User inputs
path = 'C:/Projects/MarketCatalyst/DataMasking/Data/20190828/'
output_path = path+'AnonymizedToolInputs/'


##############################################################################################################
##############################################################################################################

def install_and_import(package):
    import importlib
    try:
        importlib.import_module(package)
    except ImportError:
        from pip._internal import main as pipmain
        pipmain(['install', package])
    finally:
        globals()[package] = importlib.import_module(package)


def anonymize_faker(df, col, fakerdict):
    append_list = []
    for i in range(len(df[col])):
        # Replace the field with faked fields.
        if df[col][i] == '-':
            value = '-'
        else:
            value = fakerdict[df[col][i]]
        append_list.append(value)
    df[col] = pd.Series(append_list)
    return df

# Import packages
results = map(install_and_import, ('os','pandas', 'faker', 'collections', 'unicodecsv', 'glob'))
set(results)

import pandas as pd
import unicodecsv as csv
from faker import Factory
from collections import defaultdict


# Load the faker and its providers
faker = Factory.create()
# Create mappings of names & emails to faked names & emails.
brand = defaultdict(faker.street_name)
subBrand = defaultdict(faker.street_name)
geo = defaultdict(faker.city)
growthDriver = defaultdict(faker.catch_phrase)
state = defaultdict(faker.country)
division = defaultdict(faker.color_name)

# Dictionary with column name to be anonymized and faker dictionary to be used

## Usefor TM
col_faker_dict = {'Brand': brand, 'SubBrand': brand, 'GrowthDriver': growthDriver, 'Geography': geo, 'State': state,
                  'ReceivingBrand': brand, 'ReceivingSubBrand': brand, 'ReceivingGeography': geo}

## Use for SM
# col_faker_dict = {'division':division, 'brand': brand, 'sub_brand': brand, 'individual_brand': brand, 'diageo_display_type_2': growthDriver
#                     , 'diageo_display_type': geo, 'diageo_price_tier': state}

# Get list of files in the directory
os.chdir(path)
files_list = [i for i in glob.glob('*.{}'.format('csv'))]

# Get two lists with column names and faker dictionary from the dictionary
columns = list(col_faker_dict.keys())
faker_dict = list(col_faker_dict.values())

# Loop thru all the files in the directory
for file in files_list:
    # print(path+file)
    # file = '11. Actual and Plan.csv'
    df = pd.read_csv(path+file)

    # Loop thru all the columns to be anonymized
    for col in range(len(columns)):
        # col = 3
        try:
            if len(df[columns[col]]) > 0:
                print('Unique rows in [%s] before anonymization: %d' % (columns[col],df[columns[col]].nunique()))
                df = anonymize_faker(df, columns[col], faker_dict[col])
                print('Unique rows in [%s] after anonymization: %d' % (columns[col],df[columns[col]].nunique()))
        except:
            print('Column [%s] not present in [%s]' % (columns[col], file))

    # Write anonymized tables to csv
    df.to_csv(output_path+file, index=False)

#
# for i in ['Austria', 'Belgium', 'Denmark', 'Europe', 'Finland', 'France',
#        'GB', 'Germany', 'Greece', 'Italy', 'Netherlands',
#        'Northern Ireland', 'Norway', 'Poland', 'Portugal',
#        'Republic of Ireland', 'Russia', 'Spain', 'Sweden', 'Switzerland']:
#     print(i+':'+geo[i+str('Geography')])