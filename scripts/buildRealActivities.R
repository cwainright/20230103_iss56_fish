# build example
# a module for `scripts/fish_data_view.R`

buildRealActivities <- function(connection){
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
            example <- readxl::read_excel("data/NCRN_BSS_EDD_20230105_1300.xlsx", sheet = "Activities") # https://doimspp.sharepoint.com/:x:/r/sites/NCRNDataManagement/Shared%20Documents/General/Standards/Data-Standards/EQuIS-WQX-EDD/NCRN_BSS_EDD_20230105_1300.xlsx?d=w8c283fde9cbd4af480945c8c8bd94ff6&csf=1&web=1&e=7Y9W1M
            
            # Query db
            db_objs <- RODBC::sqlTables(con) # test db connection
            tbl_names <- db_objs %>% # choose which tables you want to query
                subset(TABLE_NAME %in% c(
                    "tbl_Events",
                    "tbl_Protocol",
                    "tbl_Fish_Events",
                    "tbl_Locations",
                    "tbl_Meta_Events",
                    "tlu_Collection_Procedures_Gear_Config"
                    )
                    ) %>%
                select(TABLE_NAME)
            
            # make list of queries so we can extract a few rows from each table
            qry_list <- vector(mode="list", length=nrow(tbl_names))
            names(qry_list) <- tbl_names$TABLE_NAME
            for (i in 1:length(qry_list)){
                qry_list[[i]] <- paste("SELECT * FROM", names(qry_list)[i])
            }
                
            getQueryResults(qryList = qry_list, connection = con)
            
            # make a flat dataframe from `results_list`
            df <- results_list$tbl_Fish_Events %>% select(-c(Fish_Move, Bottom_Visible, Water_Quality_2, Fish_Biomass_1, Fish_Biomass_2))
            df <- dplyr::left_join(df, results_list$tbl_Events %>% select(Event_ID, Protocol_ID, Start_Date, Start_Time, Location_ID, Comments, Event_Group_ID), by = "Event_ID")
            df <- dplyr::left_join(df, results_list$tbl_Protocol %>% select(Protocol_ID, Protocol_Name, Protocol_Version, Version_Date), by = "Protocol_ID")
            df <- dplyr::left_join(df, results_list$tbl_Meta_Events %>% select(Event_ID, Entered_by), by = "Event_ID")
            df <- dplyr::left_join(df, results_list$tbl_Locations %>% select(Location_ID, Loc_Name, NCRN_Site_ID), by = "Location_ID")
            
            # RODBC::odbcCloseAll() # close db connection
            
            #----- re-build `example` from `results_list`
            
            # starting point: copy the example dataframe but without data
            real <- tibble::tibble(data.frame(matrix(ncol = ncol(example), nrow = nrow(results_list$tbl_Fish_Events)))) # empty dataframe
            colnames(real) <- colnames(example) # name columns to match example
            
            real[1] <- "NCRN"
            real[2] <- df$Protocol_Name # query: SELECT e.Protocol_ID FROM tbl_Events e LEFT JOIN SELECT(Protocol_ID, Protocol_Name FROM tbl_Protocol p) ON e.Protocol_ID = p.Protocol_ID
            real[3] <- df$Event_Site_ID
            real[4] <- df$Fish_Event_ID
            real[5] <- "Field Msr/Obs" # Activity Type; choices are: 1) 'Field Msr/Obs' and 2) 'Sample-Routine'
            real[6] <- "Water" # choices are "Water", "Air", and "Other" in `example`
            real[7] <- NA # NA in `example`
            real[9] <- df$Start_Date
            real[10] <- df$Start_Time
            real[11] <- "Eastern Time - Washington, DC"
            real[12] <- NA # Activity end date
            real[13] <- NA # activity end time
            real[14] <- NA # Activity_End_Time_Zone
            real[15] <- NA # relative depth
            real[16] <- NA # depth
            real[17] <- NA # depth unit
            real[18] <- NA # activity upper depth
            real[19] <- NA # activity lower depth
            real[20] <- NA # activity depth reference
            real[21] <- df$Loc_Name # additional location info
            real[22] <- NA # activity sampler; the person who did the sampling?
            real[23] <- df$Entered_by # Activity_Recorder
            real[24] <- df$NCRN_Site_ID # Custody_ID
            real[25] <- "NCRN" # Activity_Conducting_Organization
            real[26] <- NA # Station_Visit_Comment
            real[27] <- df$Comments # Activity_Comment
            real[28] <- paste0(df$Protocol_Name, "; version", df$Protocol_Version, "; protocol date ", df$Version_Date) # "Preparation_Method_ID"
            real[29] <- "Smith Root LR-24" # "Collection_Equipment_Name" 
            real[30] <- "Smith Root LR-24" # "Collection_Equipment_Description" 
            real[31] <- subset(results_list$tlu_Collection_Procedures_Gear_Config, `Field Gear Category` == "Smith Root LR-24")$`Field Procedure ID` # "Gear_Deployment"
            real[32] <- NA # container type
            real[33] <- NA # container color
            real[34] <- NA # container size
            real[35] <- NA # container size unit
            real[36] <- paste0(df$Protocol_Name, "; version", df$Protocol_Version, "; protocol date ", df$Version_Date) # "Preparation_Method_ID"
            real[37] <- "10% buffered formalin solution (later transferable to 70% EtOH solution)" # "Chemical_Preservative" # pdf pg 124 (135) https://doimspp.sharepoint.com/:b:/r/sites/NCRNBiologicalStreamSampling/Shared%20Documents/General/Operational%20Reviews/NCRN_Biological_Stream_Survey_Protocol_Ver_2.0_NRR.pdf?csf=1&web=1&e=u0kGN9
            real[38] <- NA # "Thermal_Preservative". Fish are preserved via chemicals, not wet ice
            real[39] <- NA # "Transport_Storage_Description" 
            real[40] <- df$Event_Group_ID # "Activity_Group_ID"
            real[41] <- df$NCRN_Site_ID # "Activity_Group_Name" 
            real[42] <- paste0("Activities for: ", df$NCRN_Site_ID) # "Activity_Group_Type"  
            real[43] <- NA # we don't record stop time for fish events e-fishing, so duration is unknown
            real[44] <- NA # we don't record stop time for fish events e-fishing, so duration unit is unknown
            real[45] <- "Two-pass backpack electrofishing" # Sampling_Component_Name
            real[46] <- NA # Sampling_Component_Place_In_Series
            real[47] <- df$Seg_Length # "Reach_Length"
            # Reach_Length_Unit
            for (i in 1:nrow(real)){ # for each row
                if(!is.na(real[i,47])){ # 75 m is the prescribed e-fishing reach distance; 
                    real[i,48] <- "m" # units are meters
                    } else {# otherwise
                        real[i,48] <- NA #assign NA when "Reach_Length" is blank
                    }
                }
            real[49] <- NA # Reach_Width
            real[50] <- NA # Reach_Width_Unit
            real[51] <- 2 # [electrofishing] pass count
            real[52] <- NA # net type
            real[53] <- NA # net surface area
            real[54] <- NA # net surface area unit
            real[55] <- NA # net mesh size
            real[56] <- NA # net mesh size unit
            real[57] <- NA # boat speed
            real[58] <- NA # boat speed unit
            real[59] <- NA # Current speed
            real[60] <- NA # current speed unit
            real[61] <- NA # toxicity test type
            real[62] <- NA # effort # we don't record stop time for fish events, so effort is unknown
            real[63] <- NA # effort unit
            
            assign("real_activities", real, envir = globalenv())
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
                    "`buildRealActivities()` executed successfully...\nOutput saved as `real_activities` in global environment."
                } else {
                    for(i in 1:length(check_df$result != "MATCH")){
                        cat(paste(paste0("`real.", check_df$real[i], "`"), paste0(" DID NOT MATCH `example.", check_df$example[i][i], "`"), "\n", sep = ""))
                    }
                }
            )
        }
    )
}