# build example
# a module for `scripts/fish_data_view.R`

buildRealLocations <- function(connection){
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
            
            # tidy up
            rm(db_objs)
            rm(tbl_names)
            rm(qry_list)
            
            #----- re-build `example` from `results_list`
            
            # starting point: copy the example dataframe but without data
            real <- tibble::tibble(data.frame(matrix(ncol = ncol(example), nrow = nrow(results_list$tbl_Locations)))) # empty dataframe
            colnames(real) <- colnames(example) # name columns to match example
            
            real[1] <- "NCRN" # "#Org_Code"
            real[2] <- results_list$tbl_Locations$Unit_Code # "Park_Code" 
            real[3] <- results_list$tbl_Locations$Location_ID # "Location_ID" shared field with `real_activities.Location_ID`
            real[4] <- results_list$tbl_Locations$Loc_Name # "Location_Name"
            real[5] <- "Creek" # "Location_Type"
            real[6] <- results_list$tbl_Locations$Dec_Degrees_North # "Latitude"
            real[7] <- results_list$tbl_Locations$Dex_Degrees_East # "Longitude"
            real[8] <- "GPS-Unspecified" # "Lat_Lon_Method"
            real[9] <- toupper(results_list$tbl_Locations$Datum) # "Lat_Lon_Datum"
            real[10] <- NA # "Source_Map_Scale_Numeric" 
            real[11] <- NA # "Lat_Lon_Accuracy"
            real[12] <- NA # "Lat_Lon_Accuracy_Unit"
            real[13] <- NA # "Location_Description"
            real[14] <- NA # "Travel_Directions"
            real[15] <- NA # "Location_Purpose"
            real[16] <- NA # "Establishment_Date" 
            real[17] <- results_list$tbl_Locations$HUC # "HUC8_Code"
            real[18] <- sprintf("%.0f", results_list$tbl_Locations$Reach_Code24) # "HUC12_Code"; remove sci notation
            real[19] <- NA # "Alternate_Location_ID"
            real[20] <- NA # "Alternate_Location_ID_Context"
            # "Elevation" 
            for(i in 1:nrow(real)){ # loop runs once per row in `real`
                if(results_list$tbl_Locations$Elevation[i] == 0){ # change zeroes to NA
                    real[i,21] <- NA # b/c all NCRN sites are above sea level
                } else { # otherwise just keep the listed elevation
                    real[i,21] <- results_list$tbl_Locations$Elevation[i]
                }
            }
            # "Elevation_Unit" 
            for(i in 1:nrow(real)){ # loop runs once per row in `real`
                if(is.na(real[i,21]) == FALSE){ # change zeroes to NA
                    real[i, 22] <- "ft" # b/c all NCRN sites are above sea level
                } else { # otherwise just keep the listed elevation
                    real[i,22] <- NA
                }
            }
            real[23] <- NA # "Elevation_Method" 
            real[24] <- NA # "Elevation_Datum"
            real[25] <- NA # "Elevation_Accuracy"
            real[26] <- NA # "Elevation_Accuracy_Unit"
            real[27] <- "US" # "Country_Code"
            real[28] <- results_list$tbl_Locations$State # "State_Code"
            real[29] <- results_list$tbl_Locations$County # "County_Name"
            real[30] <- sprintf("%.3f", results_list$tbl_Locations$Catchment_Area) # "Drainage_Area"
            real[31] <- "acre" # "Drainage_Area_Unit"; design view db.tbl_Locations
            real[32] <- NA # "Contributing_Area"
            real[33] <- NA # "Contributing_Area_Unit"
            real[34] <- NA # "Tribal_Land_Indicator"
            real[35] <- NA # "Tribal_Land_Name"
            real[36] <- NA # "Well_ID"
            real[37] <- NA # "Well_Type"
            real[38] <- NA # "Aquifer_Name"
            real[39] <- NA # "Formation_Type"
            real[40] <- NA # "Well_Hole_Depth"
            real[41] <- NA # "Well_Hole_Depth_Unit"
            real[42] <- NA # "Well_Status"
            assign("real_locations", real, envir = globalenv())
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
                    "`buildRealLocations()` executed successfully...\nOutput saved as `real_locations` in global environment."
                } else {
                    for(i in 1:length(check_df$result != "MATCH")){
                        cat(paste(paste0("`real.", check_df$real[i], "`"), paste0(" DID NOT MATCH `example.", check_df$example[i][i], "`"), "\n", sep = ""))
                    }
                }
            )
        }
    )
}