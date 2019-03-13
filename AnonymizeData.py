def install_and_import(package):
    import importlib
    try:
        importlib.import_module(package)
    except ImportError:
        from pip._internal import main as pipmain
        pipmain(['install', package])
    finally:
        globals()[package] = importlib.import_module(package)

results = map(install_and_import, ('pandas', 'faker', 'collections', 'unicodecsv'))
set(results)

import pandas as pd
import unicodecsv as csv
from faker import Factory
from collections import defaultdict


# Load the faker and its providers
faker  = Factory.create()
# Create mappings of names & emails to faked names & emails.
brand  = defaultdict(faker.company)
subBrand = defaultdict(faker.street_name)
geo = defaultdict(faker.country)
growthDriver = defaultdict(faker.catch_phrase)

path = 'C:/Projects/MarketCatalyst/'

source = path+"3. Curves.csv"
target = path+"Test_fake.csv"

df = pd.read_csv(source)

def anonymize_faker(df, col, fakerdict):

    append_list = []
    for i in range(len(df[col])):
        # Replace the field with faked fields.
        value = fakerdict[df[col][i]]
        append_list.append(value)
    df[col] = pd.Series(append_list)
    return df


# Convert fields with fake names
df = anonymize_faker(df, 'Brand', brand)
df = anonymize_faker(df, 'ReceivingSubBrand', subBrand)
df = anonymize_faker(df, 'Instrument', growthDriver)
df = anonymize_faker(df, 'Geography', geo)
