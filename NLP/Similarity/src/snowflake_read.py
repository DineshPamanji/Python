## Connect to snoflake

import snowflake.connector
import pandas as pd

con = snowflake.connector.connect(
    user='AADHVARYU',
    password='I@mjoy007',
    account='eka48603.us-east-1',
    # warehouse='BCG_BGT(XL)',
    database='BCG',
    schema='PUBLIC'
)

# Create a cursor object.
cur = con.cursor()

# Execute a statement that will generate a result set.
sql = "select * from distinct_title_dp"
cur.execute(sql)

# Fetch the result set from the cursor and deliver it as the Pandas DataFrame.
# df = cur.fetch_pandas_all()
df = pd.DataFrame(cur.fetchall())
df.columns = ['BGT_Cleaned_Title']
df.to_csv('test.csv', index=False)


