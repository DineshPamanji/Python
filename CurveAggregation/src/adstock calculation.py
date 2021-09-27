import pandas as pd
import numpy as np
import scipy
from scipy.linalg import hankel

from src.helpers.utilities import calc_adstock_flash, calc_adstock
import datetime
import tqdm
import numba



df = pd.read_excel("C:\Projects\MarketCatalyst\GDM_Curve_Proxy\Codes\CurveAggregation\Input\Adstock_Test.xlsx")
df['stimuli'] = df['Spend']#/df['MediaCost']
df['Spend1'] = df['Spend']
df['row'] = df.groupby(['Group']).cumcount()
# df=df[df['Group'] == 0]
# df.loc[df['row']==0, 'Carry over']=0
ntimes = 5
step = 10
step2 = 25
step3 = 100
percentage_array = np.array(
    list(np.arange(10, 120, step) / 100) + list(np.arange(120, 100 * ntimes / 10, step2) / 100) +
    list(np.arange(100 * ntimes / 10, 100 * ntimes
                   , step3) / 100))

a = datetime.datetime.now()
carryover_array_all = df.groupby(['Group']).apply(lambda x: calc_adstock_flash(x, percentage_array
                                                                               , 'carryover', 'stimuli', 'lag')).reset_index()
carryover_array_all.rename(columns={'level_'+str(1):'row'}, inplace=True)
print(f'Time taken: {str(datetime.datetime.now() - a)}')

%%timeit
df.eval("row+Group")

append = 0
a = datetime.datetime.now()
for p in (percentage_array):
    df['Spend'] = p * df['Spend1'].copy()
    df['stimuli'] = p * df['Spend1'].copy()

    df1 = df.groupby(['Group']).apply(
        lambda x: calc_adstock(x, 'stimuli', 'Carry over')).reset_index()
    if append == 0:
        df_all = df1.copy()
        append = 1
    else:
        df_all = df_all.append(df1.copy())
print(f'Time taken: {str(datetime.datetime.now() - a)}')

df_test = df.merge(carryover_array_all, how='left')

test = calc_adstock_flash(df, percentage_array)

nrows= len(df['stimuli'])
nsteps = len(percentage_array)
arr1 = df['stimuli'].to_numpy()
arr1 = arr1.reshape((nrows, 1))
percentage_array = percentage_array.reshape((nsteps, 1))
stimuli_steps = np.dot(arr1, percentage_array.T)

decomp_array = np.zeros((nrows, nrows))
decomp_array[0] = np.power(df['Carry over'], df['row'])
for i in range(1, nrows):
    decomp_array[i] = shift4_numba(decomp_array[0], i, fill_value=0)

rollover_array = np.zeros((nrows, nsteps))
for i in range(nsteps):
    rollover_array[:, i] = np.dot(stimuli_steps[:, i], decomp_array)



