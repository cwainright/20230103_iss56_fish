# this script addresses https://github.com/NCRN/NCRN_DM/issues/56
# contents of `data/` is from: https://doimspp.sharepoint.com/sites/NCRNBiologicalStreamSampling/Shared%20Documents/Forms/AllItems.aspx?csf=1&web=1&e=oKeaBg&cid=46a9b284%2D304d%2D4b63%2D9d30%2D64b97cdd0314&RootFolder=%2Fsites%2FNCRNBiologicalStreamSampling%2FShared%20Documents%2FGeneral%2FAnnual%2DData%2DPackages%2F2022%2FExamples&FolderCTID=0x0120002FA57F4A20C6EC439BFEBEF4FA9D88E0
# database is from: https://doimspp.sharepoint.com/:u:/r/sites/NCRNBiologicalStreamSampling/Shared%20Documents/General/Annual-Data-Packages/2022/NCRN_MBSS/NCRN_MBSS_be_2022.mdb?csf=1&web=1&e=jjeJIg

#----- The script should
# Query the database:
  # Return data needed to populate columns in data/data_fish_MBSS.rda
# Wrangle query results to match data format in data/data_fish_MBSS.rda
  # colnames
  # column order

library(data.table)
library(tidyverse)
library(prettyR)
library(dplyr)

# look at example data
example_data <- data.table::fread("data/data_fish_MBSS.csv") # https://doimspp.sharepoint.com/:x:/r/sites/NCRNBiologicalStreamSampling/Shared%20Documents/General/Annual-Data-Packages/2022/Examples/data_fish_MBSS.csv?d=wf74aa7432fac473dbe2565ac0380abac&csf=1&web=1&e=cNWRcH
# example_data <- load("data/data_fish_MBSS.rda")
dplyr::glimpse(example_data)
prettyR::describe(example_data)

# look at database
library(RODBC)
db <- ("C:/Users/cwainright/OneDrive - DOI/Documents - NPS-NCRN-Biological Stream Sampling/General/Annual-Data-Packages/2022/NCRN_MBSS/NCRN_MBSS_be_2022.mdb")# https://doimspp.sharepoint.com/:u:/r/sites/NCRNBiologicalStreamSampling/Shared%20Documents/General/Annual-Data-Packages/2022/NCRN_MBSS/NCRN_MBSS_be_2022.mdb?csf=1&web=1&e=jjeJIg
con <- RODBC::odbcConnectAccess2007(db) # open db connection
db_objs <- RODBC::sqlTables(con) # test db connection
tbl_names <- db_objs %>%
    subset(TABLE_TYPE == "TABLE") %>%
    select(TABLE_NAME)

# make list of queries so we can extract a few rows from each table
qry_list <- vector(mode="list", length=nrow(tbl_names))
names(qry_list) <- tbl_names$TABLE_NAME
for (i in 1:length(qry_list)){
    qry_list[[i]] <- paste("SELECT TOP 2 * FROM", names(qry_list)[i])
}

getQueryResults <- function(qryList, connection){
    results_list <- vector(mode="list", length=length(qryList))
    names(results_list) <- names(qryList)
    for(i in 1:length(qryList)){
        results_list[[i]] <- RODBC::sqlQuery(connection, qryList[[i]])
    }
    # return (results_list)
    assign("results_list", results_list, envir = globalenv())
}
getQueryResults(qryList = qry_list, connection = con)

RODBC::odbcCloseAll() # close db connection

# observations about `example_data` and `results_list`
colnames(example_data)
names(results_list)


#----- column-by-column figure out where the data are
# can we get lucky and just find a column name that matches?
presence_absence <- data.frame(
    example_data_colname = rep(NA,ncol(example_data)),
    present_in_results_list = rep(NA,ncol(example_data))
    )
for (i in 1:length(results_list)){
    for (j in 1:nrow(presence_absence)){
        presence_absence$example_data_colname[j] <- colnames(example_data)[j]
        presence_absence$present_in_results_list[j] <- tolower(colnames(example_data)[j]) %in% tolower(colnames(results_list[i])) # try to match colnames and ignore case: tolower()
        presence_absence$present_in_results_list <- ifelse(
            presence_absence$present_in_results_list[j] == TRUE,
            paste("column present in", results_list[i]),
            "column absent from all tables in results_list"
        )
    }
}

# try to re-build `example_data` from `results_list`

head(example_data[,1], 2) # example_data$SAMPLEID
results_list$tbl_Fish_Events$Fish_Event_ID
results_list$tbl_Fish_Events$Event_ID