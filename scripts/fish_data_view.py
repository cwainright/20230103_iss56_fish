import pandas as pd
import numpy as np
import pyodbc

example_data = pd.read_csv("data/data_fish_MBSS.csv")
example_data.describe()
for col in example_data.columns:
    print(col)
example_data.groupby(['TAXAID']).count()
driver = "{Microsoft Access Driver (*.mdb, *.accdb)}"
dbq = r"C:\Users\cwainright\OneDrive - DOI\Documents - NPS-NCRN-Biological Stream Sampling\General\Annual-Data-Packages\2022\NCRN_MBSS\NCRN_MBSS_be_2022.mdb"
con = pyodbc.connect(f'Driver={driver};DBQ={dbq};') # establish db connection
con.close() # close db connection
