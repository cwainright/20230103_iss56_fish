# a module for `buildEDD()`
options(warn=-1)
getNCRNHabLocations <- function(results_list, example){
    tryCatch(
        expr = {
            #----- load external libraries
            suppressWarnings(suppressMessages(library(data.table)))
            suppressWarnings(suppressMessages(library(tidyverse)))
            suppressWarnings(suppressMessages(library(dplyr)))
            suppressWarnings(suppressMessages(library(readxl)))
            
            # make a flat dataframe from `results_list`
            df_spring <- results_list$tbl_Spring_PHI %>% dplyr::distinct(Spring_PHI_ID, .keep_all = TRUE) %>%
                select(Spring_PHI_ID, Event_ID)
            data.table::setnames(df_spring, "Spring_PHI_ID", "PHI_ID")
            df_spring <- dplyr::left_join(df_spring, results_list$tbl_Events %>% select(Event_ID, Protocol_ID, Start_Date, Start_Time, Location_ID, Comments, Event_Site_ID, Event_Group_ID), by = "Event_ID")
            df_spring <- dplyr::left_join(df_spring, results_list$tbl_Protocol %>% select(Protocol_ID, Protocol_Name, Protocol_Version, Version_Date), by = "Protocol_ID")
            df_spring <- dplyr::left_join(df_spring, results_list$tbl_Meta_Events %>% select(Event_ID, Entered_by), by = "Event_ID")
            df_spring <- dplyr::left_join(df_spring, results_list$tbl_Locations %>% select(Location_ID, Loc_Name, NCRN_Site_ID, Unit_Code, Dec_Degrees_North, Dex_Degrees_East, Datum, HUC, Reach_Code24,
                                                                             Elevation, State, County, Catchment_Area), by = "Location_ID")
            df_spring <- df_spring %>% distinct(Location_ID, .keep_all = TRUE)
            df_spring <- df_spring %>% select(Unit_Code, Location_ID, Loc_Name, Dec_Degrees_North, Dex_Degrees_East, Datum, HUC, Reach_Code24, Elevation, State, County, Catchment_Area)
            
            
            df_summer <- results_list$tbl_Summer_PHI %>% dplyr::distinct(Summer_PHI_ID, .keep_all = TRUE) %>%
                select(Summer_PHI_ID, Event_ID)
            data.table::setnames(df_summer, "Summer_PHI_ID", "PHI_ID")
            df_summer <- dplyr::left_join(df_summer, results_list$tbl_Events %>% select(Event_ID, Protocol_ID, Start_Date, Start_Time, Location_ID, Comments, Event_Site_ID, Event_Group_ID), by = "Event_ID")
            df_summer <- dplyr::left_join(df_summer, results_list$tbl_Protocol %>% select(Protocol_ID, Protocol_Name, Protocol_Version, Version_Date), by = "Protocol_ID")
            df_summer <- dplyr::left_join(df_summer, results_list$tbl_Meta_Events %>% select(Event_ID, Entered_by), by = "Event_ID")
            df_summer <- dplyr::left_join(df_summer, results_list$tbl_Locations %>% select(Location_ID, Loc_Name, NCRN_Site_ID, Unit_Code, Dec_Degrees_North, Dex_Degrees_East, Datum, HUC, Reach_Code24,
                                                                                           Elevation, State, County, Catchment_Area), by = "Location_ID")
            df_summer <- df_summer %>% distinct(Location_ID, .keep_all = TRUE)
            df_summer <- df_summer %>% select(Unit_Code, Location_ID, Loc_Name, Dec_Degrees_North, Dex_Degrees_East, Datum, HUC, Reach_Code24, Elevation, State, County, Catchment_Area)
            
            df <- rbind(df_spring, df_summer)
            #----- re-build `example` from `results_list`
            # starting point: copy the example dataframe but without data
            real <- tibble::tibble(data.frame(matrix(ncol = ncol(example), nrow = nrow(df)))) # empty dataframe
            colnames(real) <- colnames(example) # name columns to match example
            
            real[1] <- "NCRN" # "#Org_Code"
            real[2] <- df$Unit_Code # "Park_Code" 
            real[3] <- df$Location_ID # "Location_ID" shared field with `real_activities.Location_ID`
            real[4] <- df$Loc_Name # "Location_Name"
            real[5] <- "Creek" # "Location_Type"
            real[6] <- sprintf("%.7f", df$Dec_Degrees_North) # "Latitude"
            for(i in 1:nrow(real)){
                # catch unrealistic latitude values
                if(as.numeric(real[i,6])<10){
                    real[i,6] <- NA
                }
            }
            real[7] <- sprintf("%.7f", df$Dex_Degrees_East) # "Longitude"
            for(i in 1:nrow(real)){
                # catch unrealistic longitude values
                if(as.numeric(real[i,7])>(-50)){
                    real[i,7] <- NA
                }
            }
            real[8] <- "GPS-Unspecified" # "Lat_Lon_Method"
            for(i in 1:nrow(real)){
                # catch unrealistic longitude values
                if(is.na(real[i,7]) | is.na(real[i,6])){
                    real[i,8] <- NA
                }
            }
            real[9] <- toupper(df$Datum) # "Lat_Lon_Datum"
            real[10] <- NA # "Source_Map_Scale_Numeric" 
            real[11] <- NA # "Lat_Lon_Accuracy"
            real[12] <- NA # "Lat_Lon_Accuracy_Unit"
            real[13] <- NA # "Location_Description"
            real[14] <- NA # "Travel_Directions"
            real[15] <- NA # "Location_Purpose"
            real[16] <- NA # "Establishment_Date" 
            real[17] <- df$HUC # "HUC8_Code"
            real[18] <- sprintf("%.0f", df$Reach_Code24) # "HUC12_Code"; remove sci notation
            real[19] <- NA # "Alternate_Location_ID"
            real[20] <- NA # "Alternate_Location_ID_Context"
            # "Elevation" 
            for(i in 1:nrow(real)){ # loop runs once per row in `real`
                if(df$Elevation[i] == 0){ # change zeroes to NA
                    real[i,21] <- NA # b/c all NCRN sites are above sea level
                } else { # otherwise just keep the listed elevation
                    real[i,21] <- df$Elevation[i]
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
            real[28] <- df$State # "State_Code"
            real[29] <- df$County # "County_Name"
            real[30] <- df$Catchment_Area # "Drainage_Area"
            # "Drainage_Area_Unit"; design view db.tbl_Locations
            for(i in 1:nrow(real)){
                # catch NA catchment areas
                if(!is.na(real[i,30])){
                    real[i,31] <- "acre"
                }
            }
            real[30] <- sprintf("%.3f", df$Catchment_Area) # round "Drainage_Area"
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
            
            real <- as.data.frame(lapply(real, function(y) gsub("NA", NA, y))) # remove "NA" chr strings
            colnames(real)[1] <- "#Org_Code"
            
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
                    "`getNCRNHabLocations()` executed successfully..."
                } else {
                    for(i in 1:length(check_df$result != "MATCH")){
                        cat(paste(paste0("`real.", check_df$real[i], "`"), paste0(" DID NOT MATCH `example.", check_df$example[i][i], "`"), "\n", sep = ""))
                    }
                }
            )
            return(real)
        }
    )
}