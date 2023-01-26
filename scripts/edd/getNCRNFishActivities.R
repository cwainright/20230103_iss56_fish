# a module for `buildEDD()`
options(warn=-1)
getNCRNFishActivities <- function(results_list, example){
    tryCatch(
        expr = {
            #----- load external libraries
            suppressWarnings(suppressMessages(library(data.table)))
            suppressWarnings(suppressMessages(library(tidyverse)))
            suppressWarnings(suppressMessages(library(dplyr)))
            suppressWarnings(suppressMessages(library(readxl)))
            
            # make a flat dataframe from `results_list`
            df <- results_list$tbl_Fish_Events %>% select(-c(Fish_Move, Bottom_Visible, Water_Quality_2, Fish_Biomass_1, Fish_Biomass_2))
            df <- dplyr::left_join(df, results_list$tbl_Events %>% select(Event_ID, Protocol_ID, Start_Date, Start_Time, Location_ID, Comments, Event_Site_ID, Event_Group_ID), by = "Event_ID")
            df <- dplyr::left_join(df, results_list$tbl_Protocol %>% select(Protocol_ID, Protocol_Name, Protocol_Version, Version_Date), by = "Protocol_ID")
            df <- dplyr::left_join(df, results_list$tbl_Meta_Events %>% select(Event_ID, Entered_by), by = "Event_ID")
            df <- dplyr::left_join(df, results_list$tbl_Locations %>% select(Location_ID, Loc_Name, NCRN_Site_ID), by = "Location_ID")
            df2 <- results_list$tbl_Electro_Fish_Details %>% dplyr::distinct(Fish_Event_ID, .keep_all = TRUE)
            df <- dplyr::left_join(df, df2 %>% select(Fish_Event_ID, Pass_1_Start, Pass_1_End, Pass_2_Start, Pass_2_End), by = "Fish_Event_ID")
            df2 <- df %>% select(-c(Pass_1_Start, Pass_1_End)) # make a copy to use for pass 2
            df <- df %>% select(-c(Pass_2_Start, Pass_2_End))
            # make one row one pass to match Marc's format
            # pass1
            data.table::setnames(df, "Pass_1_Start", "Pass_Start_seconds")
            data.table::setnames(df, "Pass_1_End", "Pass_End_seconds")
            df$Fish_Event_ID <- paste0(df$Fish_Event_ID, ",", "pass_2")
            # pass2
            data.table::setnames(df2, "Pass_2_Start", "Pass_Start_seconds")
            data.table::setnames(df2, "Pass_2_End", "Pass_End_seconds")
            df2$Fish_Event_ID <- paste0(df2$Fish_Event_ID, ",", "pass_2")
            #combine
            df <- rbind(df, df2)
            
            #----- re-build `example` from `results_list`
            # starting point: copy the example dataframe but without data
            real <- tibble::tibble(data.frame(matrix(ncol = ncol(example), nrow = nrow(df)))) # empty dataframe
            colnames(real) <- colnames(example) # name columns to match example
            
            real[1] <- "NCRN" # "#Org_Code" 
            real[2] <- df$Protocol_Name # "Project_ID"
            real[3] <- df$Event_Site_ID # "Location_ID" shared field with `real_locations.Location_ID`
            real[4] <- df$Fish_Event_ID # "Activity_ID" shared field with `real_locations.Activity_ID` and `real_results.Activity_ID`
            real[5] <- "Field Msr/Obs" # "Activity_Type"; choices are: 1) 'Field Msr/Obs' and 2) 'Sample-Routine'
            real[6] <- "Water" # "Medium"  choices are "Water", "Air", and "Other" in `example`
            real[7] <- NA # "Medium_Subdivision"
            real[8] <- "Stream Fish" # "Assemblage_Sampled_Name"
            real[9] <- format(df$SampleDate, "%Y-%m-%d") # "Activity_Start_Date"
            real[10] <- format(df$Start_Time, "%H:%M") # "Activity_Start_Time" 
            real[11] <- "Eastern Time - Washington, DC" # "Activity_Start_Time_Zone" 
            real[12] <- NA # "Activity_End_Date" 
            real[13] <- NA # "Activity_End_Time"
            real[14] <- NA # "Activity_End_Time_Zone"  
            real[15] <- NA # "Activity_Relative_Depth" 
            real[16] <- NA # "Activity_Depth"
            real[17] <- NA # "Activity_Depth_Unit"
            real[18] <- NA # "Activity_Upper_Depth"
            real[19] <- NA # "Activity_Lower_Depth"
            real[20] <- NA # "Activity_Depth_Reference"
            real[21] <- df$Loc_Name # "Additional_Location_Info"
            real[22] <- NA # "Activity_Sampler"; the person who did the sampling?
            # "Activity_Recorder"
            real[23] <- df$Entered_by
            for (i in 1:nrow(real)){ # for each row
                ifelse(stringr::str_detect(real$Activity_Recorder[i], "Event") == TRUE, # 'Event' ends up in some records, so we regex to remove
                       # regex to extract strings matching pattern:
                       # String starts with (^) any character (*)
                       # followed by one or more (+) numbers ([0-9])
                       # followed by a dash (-)
                       # followed by one or more (+) numbers ([0-9])
                       # followed by a period (.)
                       # followed by one or more (+) numbers ([0-9])
                       real[i,23] <- stringr::str_extract(real[i,23], "^*([0-9])+-([0-9])+.([0-9])+"),
                       real[i,23] <- real[i,23])# units are meters
            }
            real[24] <- df$NCRN_Site_ID # "Custody_ID" 
            real[25] <- "NCRN" # "Activity_Conducting_Organization" 
            real[26] <- NA # "Station_Visit_Comment" 
            real[27] <- df$Comments # "Activity_Comment
            real[28] <- paste0(df$Protocol_Name, "; version ", df$Protocol_Version, "; protocol date ", df$Version_Date) # "Collection_Method_ID" 
            real[29] <- NA # Possibly a Smith Root LR-24 but not known; "Collection_Equipment_Name" 
            real[30] <- NA # Possibly a Smith Root LR-24 but not known; # "Collection_Equipment_Description" 
            real[31] <- NA # subset(results_list$tlu_Collection_Procedures_Gear_Config, `Field Gear Category` == "Smith Root LR-24")$`Field Procedure ID` # "Gear_Deployment"
            real[32] <- NA # "Container_Type"
            real[33] <- NA # "Container_Color"
            real[34] <- NA # "Container_Size"
            real[35] <- NA # "Container_Size_Unit"
            real[36] <- paste0(df$Protocol_Name, "; version ", df$Protocol_Version, "; protocol date ", df$Version_Date) # "Preparation_Method_ID"
            real[37] <- "10% buffered formalin solution (later transferable to 70% EtOH solution)" # "Chemical_Preservative" # pdf pg 124 (135) https://doimspp.sharepoint.com/:b:/r/sites/NCRNBiologicalStreamSampling/Shared%20Documents/General/Operational%20Reviews/NCRN_Biological_Stream_Survey_Protocol_Ver_2.0_NRR.pdf?csf=1&web=1&e=u0kGN9
            real[38] <- NA # "Thermal_Preservative". Fish are preserved via chemicals, not wet ice
            real[39] <- NA # "Transport_Storage_Description" 
            real[40] <- df$Event_Group_ID # "Activity_Group_ID"
            real[41] <- df$NCRN_Site_ID # "Activity_Group_Name" 
            real[42] <- ifelse(is.na(real[41])==TRUE,
                               paste0("Activities for: ", df$NCRN_Site_ID),
                               NA)# "Activity_Group_Type"  
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
            real[49] <- NA # "Reach_Width"
            real[50] <- NA # "Reach_Width_Unit" 
            # [electrofishing] "Pass_Count"
            # only assign a "Pass_Count" value if we know the reach length
            # because this gives us higher confidence that the record used the 2-pass e-fishing protocol
            for (i in 1:nrow(real)){ # for each row
                if(!is.na(real[i,47])){ # 75 m is the prescribed e-fishing reach distance; 
                    real[i,51] <- 2 # units are meters
                } else {# otherwise
                    real[i,51] <- NA #assign NA when "Reach_Length" is blank
                }
            }
            real[52] <- NA # "Net_Type"
            real[53] <- NA # "Net_Surface_Area"
            real[54] <- NA # "Net_Surface_Area_Unit"
            real[55] <- NA # "Net_Mesh_Size"
            real[56] <- NA # "Net_Mesh_Size_Unit"  
            real[57] <- NA # "Boat_Speed"
            real[58] <- NA # "Boat_Speed_Unit"
            real[59] <- NA # "Current_Speed"
            real[60] <- NA # "Current_Speed_Unit"
            real[61] <- NA # "Toxicity_Test_Type"
            # "Effort"
            # we need to account for possible NA values since we're doing math
            # we need to only do math if we have a valid start & end pair (i.e., use either pass 2 or pass 1 if only one pair is NA-free)
            # if we have two valid start & end pairs, use both and sum them
            for(i in 1:nrow(real)){
                if(!is.na(df$Pass_End_seconds[i]) & !is.na(df$Pass_Start_seconds[i])){
                    real[i,62] <- (df$Pass_End_seconds[i]-df$Pass_Start_seconds[i])
                } else {
                    real[i,62] <- NA
                }
            }
            # "Effort_Unit"
            for (i in 1:nrow(real)){ # for each row
                if(!is.na(real[i,62])){
                    real[i,63] <- "seconds" # units are seconds
                }
            }
            real <- as.data.frame(lapply(real, function(y) gsub("NA", NA, y))) # remove "NA" chr strings
            colnames(real)[1] <- "#Org_Code"
            # test <- cbind(real_activities[62:63], df$Pass_1_End, df$Pass_1_Start, df$Pass_2_End, df$Pass_2_Start) # check the `$Effort` math in real[62]

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
                    "`getNCRNFishActivities()` executed successfully..."
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