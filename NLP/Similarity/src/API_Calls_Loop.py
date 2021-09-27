import requests
import datetime

# offset = 3000
# dataset_value = 1

# url = 'http://127.0.0.1:9010/api/standardize/LoadTitles/'
url = 'http://databusinessopsdev.bcg.com/api/standardize/GetStandardTitles/'
headers = {"Content-Type": "application/json",
         "token":"eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJpYXQiOjE2MDE4NzQ0MjQsIm5iZiI6MTYwMTg3NDQyNCwianRpIjoiY2EzNmYwMmMtNWRmYy00MWY2LTg2ODItZjFmOTg1ZTI0NTEwIiwiZXhwIjoxNjAxOTYwODI0LCJpZGVudGl0eSI6WyJIYjZqdFBDbllKIiwib3JnYnVpbGRlciJdLCJmcmVzaCI6ZmFsc2UsInR5cGUiOiJhY2Nlc3MifQ.X806YMHhzVNFmbLe1od5vg7jdW2mjrhXI72QVw8STSM"}

for dataset_value in range(90, 96):
    data = '''{
      "path": "./",
      "input_table_name": "z_BGT_Cleaned_Titles",
      "hist_filename": "Mapping/StandardTitle_HistoricalMapping_EMSI.xlsx",
      "job_title_col_new": "Title_2_1",
      "employee_id_col": "Position_Id",  
      "filter_field": "Data_Set_ID",  
      "filter_value": %d,  
      "taxonomy_tablename": "EMSI_StandardTitles_Clean",
      "dictionary_filename": "Model/Data/Short_Forms_Dictionary.xlsx",
      "job_title_col": "title",
      "job_title_col_clean": "titleold",
      "output_table_name": "z_Model_Output",
      "model_path": "Model/",
      "model_tag": "EMSI_09032020",
      "user_name": "Dinesh Pamanji"
        }
    ''' % dataset_value

    print(datetime.datetime.now())
    print('Calling {} with dataset value {}'.format(url, str(dataset_value)))
    response = requests.post(url, data=data, headers=headers)
    # print(response.text + 'for offset: {}'.format(str(offset)))
    if response.status_code == 200:
        print('Time elapsed for dataset id {} is {} seconds'.format(str(dataset_value), str(response.elapsed.seconds)))
    else:
        print('Call failed for {}'.format(str(dataset_value)))
        break


import pandas as pd
import numpy as np
df = pd.read_csv('C:\\Projects\\OrgBuilder\\Data\\EMSI to BGT\\EMSI to BGT Cleaned 1005.csv')

df.sort_values(['Title_Similar'], inplace=True)

df.to_excel('C:\\Projects\\OrgBuilder\\Data\\EMSI to BGT\\EMSI to BGT Cleaned 1005.xlsx', index=False)
df.drop_duplicates(subset=['Title_Similar'], inplace=True)

bins = [0, .50, .60, .70, .80, .90, 0.99, 1]
df['binned'] = pd.cut(df['Similarity_Score'], bins)
df['binned'].value_counts()

df.to_csv('C:\\Projects\\OrgBuilder\\Data\\EMSI to BGT\\EMSI to BGT Cleaned 1005 nodup.csv', index=False)

df.Title_Similar.nunique()
df.Similarity_Score.quantile(np.linspace(.1, 1, 9, 0))


