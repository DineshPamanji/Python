import sys;
index = int(''.join(sys.argv[1:]));
type(index)
# index = 2
path = 'C:/Excercise/FuzzyMatching/'
input_path = path + 'Inputs/'
Output_path = path + 'Outputs/'

import pandas as pd, numpy as np
import difflib
from fuzzywuzzy import fuzz
from fuzzywuzzy import process

CityNames=pd.read_csv(input_path + 'City.csv')
MasterFile=pd.read_csv(input_path + 'States_City_Master.csv')

temp = CityNames[index:(index+1)]
temp['key']=process.extract(str(temp['Cities']), MasterFile['Name_of_City'],limit=1)[0][0]

temp.to_csv(Output_path + str(index) +'_FuzzyMatched.csv',index=False)