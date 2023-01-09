# build example
# a module for `scripts/fish_data_view.R`

buildRealLocations <- function(database, connection){
    tryCatch(
        expr = {
            #----- load external libraries
            suppressWarnings(suppressMessages(library(data.table)))
            suppressWarnings(suppressMessages(library(tidyverse)))
            suppressWarnings(suppressMessages(library(dplyr)))
            suppressWarnings(suppressMessages(library(readxl)))
            
            #----- load project functions
            source("scripts/getQueryResults.R") # equivalent to python "from x import function"
            # load example data
            example <- readxl::read_excel("data/NCRN_BSS_EDD_20230105_1300.xlsx", sheet = "Locations") # https://doimspp.sharepoint.com/:x:/r/sites/NCRNDataManagement/Shared%20Documents/General/Standards/Data-Standards/EQuIS-WQX-EDD/NCRN_BSS_EDD_20230105_1300.xlsx?d=w8c283fde9cbd4af480945c8c8bd94ff6&csf=1&web=1&e=7Y9W1M
            
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
            
            # RODBC::odbcCloseAll() # close db connection
            
            #----- re-build `example` from `results_list`
            
            # starting point: copy the example dataframe but without data
            real <- tibble::tibble(data.frame(matrix(ncol = ncol(example), nrow = nrow(results_list$tbl_Locations)))) # empty dataframe
            colnames(real) <- colnames(example) # name columns to match example
            
            real[1] <- "NCRN"
            real[2] <- results_list$tbl_Locations$Unit_Code
            real[3] <- results_list$tbl_Locations$Location_ID
            real[4] <- results_list$tbl_Locations$Loc_Name
            real[5] <- "Creek"
            real[6] <- results_list$tbl_Locations$Dec_Degrees_North
            real[7] <- results_list$tbl_Locations$Dex_Degrees_East
            real[8] <- "GPS-Unspecified"
            real[9] <- toupper(results_list$tbl_Locations$Datum)
            real[10] <- NA
            real[11] <- NA
            real[12] <- NA
            real[13] <- NA
            real[14] <- NA
            real[15] <- NA
            real[16] <- NA
            real[17] <- results_list$tbl_Locations$HUC
            real[18] <- sprintf("%.0f", results_list$tbl_Locations$Reach_Code24) # get rid of sci notation
            real[19] <- NA
            real[20] <- NA
            for(i in 1:nrow(real)){ # loop runs once per row in `real`
                if(results_list$tbl_Locations$Elevation[i] == 0){ # change zeroes to NA
                    real[i,21] <- NA # b/c all NCRN sites are above sea level
                } else { # otherwise just keep the listed elevation
                    real[i,21] <- results_list$tbl_Locations$Elevation[i]
                }
            }
            real[22] <- "ft"
            real[23] <- NA
            real[24] <- NA
            real[25] <- NA
            real[26] <- NA
            real[27] <- "US"
            real[28] <- results_list$tbl_Locations$State
            real[29] <- results_list$tbl_Locations$County
            real[30] <- sprintf("%.3f", results_list$tbl_Locations$Catchment_Area)
            real[31] <- "acre" # design view db.tbl_Locations
            real[32] <- NA
            real[33] <- NA
            real[34] <- NA
            real[35] <- NA
            real[36] <- NA
            real[37] <- NA
            real[38] <- NA
            real[39] <- NA
            real[40] <- NA
            real[41] <- NA
            real[42] <- NA
            assign("real", real, envir = globalenv())
            # assign("example", example, envir = globalenv())
            
            # error-checking:
            check_df <- tibble::tibble(data.frame(matrix(ncol=3, nrow=ncol(real))))
            colnames(check_df) <- c("real", "example", "result")
            check_df$real <- colnames(real)
            check_df$example <- colnames(example)
            for(i in 1:nrow(check_df)){
                if(check_df$real[i] == check_df$example[i]){
                    check_df$result[i] <- "MATCH"
                } else {
                    check_df$result[i] <- "MISMATCH"
                }
            }
            
            message(
                if(length(check_df$result == "MATCH") == nrow(check_df)){
                    "`buildRealLocations()` executed successfully; `real` colnames match `example`"
                } else {
                    for(i in 1:length(check_df$result != "MATCH")){
                        cat(paste(paste0("`real.", check_df$real[i], "`"), paste0(" DID NOT MATCH `example.", check_df$example[i][i], "`"), "\n", sep = ""))
                    }
                }
            )
        }
    )
}