# User inputs
path = 'C:/Projects/MarketCatalyst/'
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
# Create mappings of fields to faked fields
# Choose the provider from the following URL
# https://faker.readthedocs.io/en/master/providers/faker.providers.address.html
brand = defaultdict(faker.company)
subBrand = defaultdict(faker.street_name)
geo = defaultdict(faker.country)
growthDriver = defaultdict(faker.catch_phrase)
state = defaultdict(faker.city)

# Dictionary with column name to be anonymized and faker dictionary to be used
col_faker_dict = {'Brand': brand, 'SubBrand': subBrand, 'GrowthDriver': growthDriver, 'Geography': geo, 'State': state,
                  'ReceivingBrand': brand, 'ReceivingSubBrand': subBrand, 'ReceivingGeography': geo}


# Get list of files in the directory
os.chdir(path)
files_list = [i for i in glob.glob('*.{}'.format('csv'))]

# Get two lists with column names and faker dictionary from the dictionary
columns = list(col_faker_dict.keys())
faker_dict = list(col_faker_dict.values())

# Loop thru all the files in the directory
for file in files_list:
    # print(path+file)
    df = pd.read_csv(path+file)

    # Loop thru all the columns to be anonymized
    for col in range(len(columns)):
        try:
            if len(df[columns[col]]) > 0:
                print('Unique rows in [%s] before anonymization: %d' % (columns[col],df[columns[col]].nunique()))
                df = anonymize_faker(df, columns[col], faker_dict[col])
                print('Unique rows in [%s] after anonymization: %d' % (columns[col],df[columns[col]].nunique()))
        except:
            print('Column [%s] not present in [%s]' % (columns[col], file))

    # Write anonymized tables to csv
    df.to_csv(output_path+file)

