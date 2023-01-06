# build example_locations
# a module for `scripts/fish_data_view.R`

#----- load external libraries
library(data.table)
library(tidyverse)
library(dplyr)
library(readxl)

#----- load project functions
source("scripts/getQueryResults.R") # equivalent to python "from x import function"


# load example data
example_results <- readxl::read_excel("data/NCRN_BSS_EDD_20230105_1300.xlsx", sheet = "Results")

# Connect to db
library(RODBC)
db <- ("C:/Users/cwainright/OneDrive - DOI/Documents - NPS-NCRN-Biological Stream Sampling/General/Annual-Data-Packages/2022/NCRN_MBSS/NCRN_MBSS_be_2022.mdb")# https://doimspp.sharepoint.com/:u:/r/sites/NCRNBiologicalStreamSampling/Shared%20Documents/General/Annual-Data-Packages/2022/NCRN_MBSS/NCRN_MBSS_be_2022.mdb?csf=1&web=1&e=jjeJIg
con <- RODBC::odbcConnectAccess2007(db) # open db connection

# Query db
db_objs <- RODBC::sqlTables(con) # test db connection
tbl_names <- db_objs %>% # choose which tables you want to query
    subset(TABLE_NAME %in% c("tbl_Fish_Events",
                             "tbl_Fish_Data",
                             "tbl_Events",
                             "tbl_Locations")) %>%
    select(TABLE_NAME)

# make list of queries so we can extract a few rows from each table
qry_list <- vector(mode="list", length=nrow(tbl_names))
names(qry_list) <- tbl_names$TABLE_NAME
for (i in 1:length(qry_list)){
    qry_list[[i]] <- paste("SELECT * FROM", names(qry_list)[i])
}


getQueryResults(qryList = qry_list, connection = con)

RODBC::odbcCloseAll() # close db connection

# re-build `example_locations` from `results_list`
ncol(example_locations)