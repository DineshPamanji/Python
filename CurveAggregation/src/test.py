import pandas as pd
import numpy as np
import tqdm
import datetime

raw_data = pd.DataFrame(list(zip(list(range(1,1000001)),
                                 list(range(1000001, 2000001)))), columns=['a', 'b'])
raw_data['factor'] = 1000


# a = datetime.datetime.now()
# for elem in ['a', 'b']:
#     raw_data[elem] = raw_data.apply(lambda x: x[elem] * x.factor, axis=1)
# print(datetime.datetime.now()-a)

a = datetime.datetime.now()
for elem in ['a', 'b']:
    raw_data[elem+'test'] = raw_data[elem]*raw_data['factor']
print(datetime.datetime.now()-a)



a = datetime.datetime.now()
for elem in ['a', 'b']:
    raw_data[elem+'test1'] = raw_data.eval(f'{elem}*factor')
print(datetime.datetime.now()-a)

raw_data['formula'] = 'a*factor'
a = datetime.datetime.now()
for elem in ['a', 'b']:
    raw_data[elem+'test1'] = np.vectorize(raw_data.eval)(raw_data['formula'])
print(datetime.datetime.now()-a)
