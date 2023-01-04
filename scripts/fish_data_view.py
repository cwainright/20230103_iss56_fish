# a python equivalent to scripts/fish_data_view.R

# this script addresses https://github.com/NCRN/NCRN_DM/issues/56
# contents of `data/` is from: https://doimspp.sharepoint.com/sites/NCRNBiologicalStreamSampling/Shared%20Documents/Forms/AllItems.aspx?csf=1&web=1&e=oKeaBg&cid=46a9b284%2D304d%2D4b63%2D9d30%2D64b97cdd0314&RootFolder=%2Fsites%2FNCRNBiologicalStreamSampling%2FShared%20Documents%2FGeneral%2FAnnual%2DData%2DPackages%2F2022%2FExamples&FolderCTID=0x0120002FA57F4A20C6EC439BFEBEF4FA9D88E0
# database is from: https://doimspp.sharepoint.com/:u:/r/sites/NCRNBiologicalStreamSampling/Shared%20Documents/General/Annual-Data-Packages/2022/NCRN_MBSS/NCRN_MBSS_be_2022.mdb?csf=1&web=1&e=jjeJIg

#----- The script should
# Query the database:
  # Return data needed to populate columns in data/data_fish_MBSS.rda
# Wrangle query results to match data format in data/data_fish_MBSS.rda
  # colnames
  # column order

import pandas as pd
import numpy as np
import pyodbc

# look at example data
example_data = pd.read_csv("data/data_fish_MBSS.csv")
example_data.describe()
for col in example_data.columns:
    print(col)
example_data.groupby(['TAXAID']).count()

# look at database
driver = "{Microsoft Access Driver (*.mdb, *.accdb)}"
dbq = r"C:\Users\cwainright\OneDrive - DOI\Documents - NPS-NCRN-Biological Stream Sampling\General\Annual-Data-Packages\2022\NCRN_MBSS\NCRN_MBSS_be_2022.mdb"
con = pyodbc.connect(f'Driver={driver};DBQ={dbq};') # establish db connection
cursor = con.cursor() # establish db cursor to interact with db
for i in cursor.tables(tableType='TABLE'): # test db connection by extracting table names
    print(i.table_name)

query = 'SELECT TOP 5 * FROM tlu_Macroinverts'
# query = "SELECT 'sqlserver' dbms,t.TABLE_CATALOG,t.TABLE_SCHEMA,t.TABLE_NAME,c.COLUMN_NAME,c.ORDINAL_POSITION,c.DATA_TYPE,c.CHARACTER_MAXIMUM_LENGTH,n.CONSTRAINT_TYPE,k2.TABLE_SCHEMA,k2.TABLE_NAME,k2.COLUMN_NAME FROM INFORMATION_SCHEMA.TABLES t LEFT JOIN INFORMATION_SCHEMA.COLUMNS c ON t.TABLE_CATALOG=c.TABLE_CATALOG AND t.TABLE_SCHEMA=c.TABLE_SCHEMA AND t.TABLE_NAME=c.TABLE_NAME LEFT JOIN(INFORMATION_SCHEMA.KEY_COLUMN_USAGE k JOIN INFORMATION_SCHEMA.TABLE_CONSTRAINTS n ON k.CONSTRAINT_CATALOG=n.CONSTRAINT_CATALOG AND k.CONSTRAINT_SCHEMA=n.CONSTRAINT_SCHEMA AND k.CONSTRAINT_NAME=n.CONSTRAINT_NAME LEFT JOIN INFORMATION_SCHEMA.REFERENTIAL_CONSTRAINTS r ON k.CONSTRAINT_CATALOG=r.CONSTRAINT_CATALOG AND k.CONSTRAINT_SCHEMA=r.CONSTRAINT_SCHEMA AND k.CONSTRAINT_NAME=r.CONSTRAINT_NAME)ON c.TABLE_CATALOG=k.TABLE_CATALOG AND c.TABLE_SCHEMA=k.TABLE_SCHEMA AND c.TABLE_NAME=k.TABLE_NAME AND c.COLUMN_NAME=k.COLUMN_NAME LEFT JOIN INFORMATION_SCHEMA.KEY_COLUMN_USAGE k2 ON k.ORDINAL_POSITION=k2.ORDINAL_POSITION AND r.UNIQUE_CONSTRAINT_CATALOG=k2.CONSTRAINT_CATALOG AND r.UNIQUE_CONSTRAINT_SCHEMA=k2.CONSTRAINT_SCHEMA AND r.UNIQUE_CONSTRAINT_NAME=k2.CONSTRAINT_NAME WHERE t.TABLE_TYPE='BASE TABLE';"
df = pd.read_sql(query,con) # use pandas to extract query results as dataframe

con.close() # close db connection
