# build example_locations
# a module for `scripts/fish_data_view.R`

buildRealLocations <- function(){
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
            example_locations <- readxl::read_excel("data/NCRN_BSS_EDD_20230105_1300.xlsx", sheet = "Locations") # https://doimspp.sharepoint.com/:x:/r/sites/NCRNDataManagement/Shared%20Documents/General/Standards/Data-Standards/EQuIS-WQX-EDD/NCRN_BSS_EDD_20230105_1300.xlsx?d=w8c283fde9cbd4af480945c8c8bd94ff6&csf=1&web=1&e=7Y9W1M
            
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
            
            #----- re-build `example_locations` from `results_list`
            
            # starting point: copy the example dataframe but without data
            real_locations <- tibble::tibble(data.frame(matrix(ncol = ncol(example_locations), nrow = nrow(results_list$tbl_Locations)))) # empty dataframe
            colnames(real_locations) <- colnames(example_locations) # name columns to match example
            
            real_locations[1] <- "NCRN"
            real_locations[2] <- results_list$tbl_Locations$Unit_Code
            real_locations[3] <- results_list$tbl_Locations$Location_ID
            real_locations[4] <- results_list$tbl_Locations$Loc_Name
            real_locations[5] <- "Creek"
            real_locations[6] <- results_list$tbl_Locations$Dec_Degrees_North
            real_locations[7] <- results_list$tbl_Locations$Dex_Degrees_East
            real_locations[8] <- "GPS-Unspecified"
            real_locations[9] <- toupper(results_list$tbl_Locations$Datum)
            real_locations[10] <- NA
            real_locations[11] <- NA
            real_locations[12] <- NA
            real_locations[13] <- NA
            real_locations[14] <- NA
            real_locations[15] <- NA
            real_locations[16] <- NA
            real_locations[17] <- results_list$tbl_Locations$HUC
            real_locations[18] <- sprintf("%.0f", results_list$tbl_Locations$Reach_Code24) # get rid of sci notation
            real_locations[19] <- NA
            real_locations[20] <- NA
            for(i in 1:nrow(real_locations)){ # loop runs once per row in `real_locations`
                if(results_list$tbl_Locations$Elevation[i] == 0){ # change zeroes to NA
                    real_locations[i,21] <- NA # b/c all NCRN sites are above sea level
                } else { # otherwise just keep the listed elevation
                    real_locations[i,21] <- results_list$tbl_Locations$Elevation[i]
                }
            }
            real_locations[22] <- "ft"
            real_locations[23] <- NA
            real_locations[24] <- NA
            real_locations[25] <- NA
            real_locations[26] <- NA
            real_locations[27] <- "US"
            real_locations[28] <- results_list$tbl_Locations$State
            real_locations[29] <- results_list$tbl_Locations$County
            real_locations[30] <- sprintf("%.3f", results_list$tbl_Locations$Catchment_Area)
            real_locations[31] <- "acre" # design view db.tbl_Locations
            real_locations[32] <- NA
            real_locations[33] <- NA
            real_locations[34] <- NA
            real_locations[35] <- NA
            real_locations[36] <- NA
            real_locations[37] <- NA
            real_locations[38] <- NA
            real_locations[39] <- NA
            real_locations[40] <- NA
            real_locations[41] <- NA
            real_locations[42] <- NA
            assign("real_locations", real_locations, envir = globalenv())
            # assign("example_locations", example_locations, envir = globalenv())
            
            # error-checking:
            check_df <- tibble::tibble(data.frame(matrix(ncol=3, nrow=ncol(real_locations))))
            colnames(check_df) <- c("real_locations", "example_locations", "result")
            check_df$real_locations <- colnames(real_locations)
            check_df$example_locations <- colnames(example_locations)
            for(i in 1:nrow(check_df)){
                if(check_df$real_locations[i] == check_df$example_locations[i]){
                    check_df$result[i] <- "MATCH"
                } else {
                    check_df$result[i] <- "MISMATCH"
                }
            }
            
            message(
                if(length(check_df$result == "MATCH") == nrow(check_df)){
                    "buildRealLocations() executed successfully; `real_locations` colnames match `example_locations`"
                } else {
                    for(i in 1:length(check_df$result != "MATCH")){
                        cat(paste(paste0("`real_locations.", check_df$real_locations[i], "`"), paste0(" DID NOT MATCH `example_locations.", check_df$example_locations[i][i], "`"), "\n", sep = ""))
                    }
                }
            )
        }
    )
}