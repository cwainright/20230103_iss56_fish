# this script is a workflow to build fish data views

# this script addresses https://github.com/NCRN/NCRN_DM/issues/56
# contents of `data/` is from: https://doimspp.sharepoint.com/sites/NCRNBiologicalStreamSampling/Shared%20Documents/Forms/AllItems.aspx?csf=1&web=1&e=oKeaBg&cid=46a9b284%2D304d%2D4b63%2D9d30%2D64b97cdd0314&RootFolder=%2Fsites%2FNCRNBiologicalStreamSampling%2FShared%20Documents%2FGeneral%2FAnnual%2DData%2DPackages%2F2022%2FExamples&FolderCTID=0x0120002FA57F4A20C6EC439BFEBEF4FA9D88E0
# database is from: https://doimspp.sharepoint.com/:u:/r/sites/NCRNBiologicalStreamSampling/Shared%20Documents/General/Annual-Data-Packages/2022/NCRN_MBSS/NCRN_MBSS_be_2022.mdb?csf=1&web=1&e=jjeJIg

#----- scripts/buildBioMonTools.R should
# Query the database:
    # Return dataframe that matches BioMonTools format 'data/data_fish_MBSS.csv' https://doimspp.sharepoint.com/:x:/r/sites/NCRNBiologicalStreamSampling/Shared%20Documents/General/Annual-Data-Packages/2022/BioMonTools/data_fish_MBSS.csv?d=wf74aa7432fac473dbe2565ac0380abac&csf=1&web=1&e=b30pe4
# Query Marc's 2022 fish data `data/NCRN_BSS_Fish_Monitoring_Data_2022_Marc.xlsx` tab `ElectrofishingData` https://doimspp.sharepoint.com/:x:/r/sites/NCRNBiologicalStreamSampling/Shared%20Documents/General/Annual-Data-Packages/2022/Marc_and_Bob/Fish_Template/NCRN_BSS_Fish_Monitoring_Data_2022_Marc.xlsx?d=w306357b1a43a48f4b9f598169043cc6a&csf=1&web=1&e=AeACpg
    # Return dataframe that matches BioMonTools format `data/NCRN_BSS_EDD_20230105_1300.xlsx` https://doimspp.sharepoint.com/sites/NCRNBiologicalStreamSampling/Shared%20Documents/Forms/AllItems.aspx?csf=1&web=1&e=oKeaBg&cid=46a9b284%2D304d%2D4b63%2D9d30%2D64b97cdd0314&RootFolder=%2Fsites%2FNCRNBiologicalStreamSampling%2FShared%20Documents%2FGeneral%2FAnnual%2DData%2DPackages%2F2022%2FExamples&FolderCTID=0x0120002FA57F4A20C6EC439BFEBEF4FA9D88E0
# Produce a .csv  

#----- scripts/buildEDD.R should
# Query the database:
    # Return data needed to populate columns in EDD format `data/NCRN_BSS_EDD_20230105_1300.xlsx` https://doimspp.sharepoint.com/sites/NCRNBiologicalStreamSampling/Shared%20Documents/Forms/AllItems.aspx?csf=1&web=1&e=oKeaBg&cid=46a9b284%2D304d%2D4b63%2D9d30%2D64b97cdd0314&RootFolder=%2Fsites%2FNCRNBiologicalStreamSampling%2FShared%20Documents%2FGeneral%2FAnnual%2DData%2DPackages%2F2022%2FExamples&FolderCTID=0x0120002FA57F4A20C6EC439BFEBEF4FA9D88E0
# Query Marc's 2022 fish data `data/NCRN_BSS_Fish_Monitoring_Data_2022_Marc.xlsx` tab `ElectrofishingData` https://doimspp.sharepoint.com/:x:/r/sites/NCRNBiologicalStreamSampling/Shared%20Documents/General/Annual-Data-Packages/2022/Marc_and_Bob/Fish_Template/NCRN_BSS_Fish_Monitoring_Data_2022_Marc.xlsx?d=w306357b1a43a48f4b9f598169043cc6a&csf=1&web=1&e=AeACpg
    # Return data needed to populate columns in EDD format `data/NCRN_BSS_EDD_20230105_1300.xlsx` https://doimspp.sharepoint.com/sites/NCRNBiologicalStreamSampling/Shared%20Documents/Forms/AllItems.aspx?csf=1&web=1&e=oKeaBg&cid=46a9b284%2D304d%2D4b63%2D9d30%2D64b97cdd0314&RootFolder=%2Fsites%2FNCRNBiologicalStreamSampling%2FShared%20Documents%2FGeneral%2FAnnual%2DData%2DPackages%2F2022%2FExamples&FolderCTID=0x0120002FA57F4A20C6EC439BFEBEF4FA9D88E0
# Produce an .xlsx with three tabs:
    # 1) Locations
    # 2) Activities
    # 3) Results

#----- scripts/buildMarcView.R should
# Query the database:
    # Return data needed to populate columns in Marc's format `data/NCRN_BSS_EDD_20230105_1300.xlsx` https://doimspp.sharepoint.com/:x:/r/sites/NCRNBiologicalStreamSampling/Shared%20Documents/General/Annual-Data-Packages/2022/Marc_and_Bob/Fish_Template/NCRN_BSS_Fish_Monitoring_Data_2022_Marc.xlsx?d=w306357b1a43a48f4b9f598169043cc6a&csf=1&web=1&e=AeACpg
# Query Marc's 2022 fish data `data/NCRN_BSS_Fish_Monitoring_Data_2022_Marc.xlsx` sheet `ElectrofishingData` https://doimspp.sharepoint.com/:x:/r/sites/NCRNBiologicalStreamSampling/Shared%20Documents/General/Annual-Data-Packages/2022/Marc_and_Bob/Fish_Template/NCRN_BSS_Fish_Monitoring_Data_2022_Marc.xlsx?d=w306357b1a43a48f4b9f598169043cc6a&csf=1&web=1&e=AeACpg
    # Return data needed to populate columns in Marc's format `data/NCRN_BSS_Fish_Monitoring_Data_2022_Marc.xlsx` sheet `data_model` https://doimspp.sharepoint.com/:x:/r/sites/NCRNBiologicalStreamSampling/Shared%20Documents/General/Annual-Data-Packages/2022/Marc_and_Bob/Fish_Template/NCRN_BSS_Fish_Monitoring_Data_2022_Marc.xlsx?d=w306357b1a43a48f4b9f598169043cc6a&csf=1&web=1&e=AeACpg
# Produce an .xlsx with two tabs:
    # 1) passes
    # 2) individuals

#----- load external libraries
library(data.table)
library(tidyverse)
library(dplyr)
library(readxl)
library(RODBC)
library(openxlsx)

#----- load project functions
# source("scripts/buildRealLocations.R")
# source("scripts/buildRealActivities.R")
# source("scripts/buildRealResults.R")
source("scripts/buildEDD.R")
source("scripts/buildBioMonTools.R")
source("scripts/buildMarcView.R")

# Connect to db
db <- ("C:/Users/cwainright/OneDrive - DOI/Documents - NPS-NCRN-Biological Stream Sampling/General/Annual-Data-Packages/2022/NCRN_MBSS/NCRN_MBSS_be_2022.mdb")# https://doimspp.sharepoint.com/:u:/r/sites/NCRNBiologicalStreamSampling/Shared%20Documents/General/Annual-Data-Packages/2022/NCRN_MBSS/NCRN_MBSS_be_2022.mdb?csf=1&web=1&e=jjeJIg
# db <- file.choose()
con <- RODBC::odbcConnectAccess2007(db) # open db connection
buildEDD(connection = con, write = FALSE)
buildBioMonTools(connection = con, write = FALSE)
buildMarc(connection = con, write = FALSE, addMarc = TRUE)

RODBC::odbcCloseAll() # close db connection
