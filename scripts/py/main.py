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

# import pandas as pd
# import numpy as np
# import pyodbc

from importlib import reload
import getQueryResults
reload(getQueryResults)

dbq = r"C:\Users\cwainright\OneDrive - DOI\Documents - NPS-NCRN-Biological Stream Sampling\General\Annual-Data-Packages\2022\NCRN_MBSS\NCRN_MBSS_be_2022.mdb"
getQueryResults.getQueryResults(dbq = dbq)

# cursor.close()
# con.close() # close db connection

