#--------------------------------------------------------------------------
#----- Make Marc's 2022 data the required `EDD` format-----------------
#--------------------------------------------------------------------------
# a module for `buildEDD()`
options(warn=-1)
getMarc2022Activities <- function(marc2022, example){
    tryCatch(
        expr = {
            # make a flat dataframe where one row is one e-fishing pass from `df`
            # format from wide-format data: $Total_Pass_1 and $Total_Pass_2 to long-format $value and $pass
            df <- marc2022
            counts <- df %>% 
                group_by(Pass_ID, species_id) %>%
                summarize(count=n()) %>%
                mutate(dummy = paste0(Pass_ID, ".", species_id)) %>%
                ungroup()
            
            df <- unique(setDT(df), by=c("Pass_ID", "common_name"))
            df$dummy <- paste0(df$Pass_ID, ".", df$species_id)
            df <- dplyr::left_join(df, counts %>% select(dummy, count), by=c("dummy" = "dummy"))
            
            #----- re-build `example` from `results_list`
            acts <- tibble::tibble(data.frame(matrix(ncol = ncol(example), nrow = nrow(df)))) # empty dataframe
            colnames(acts) <- colnames(example) # name columns to match example
            
            acts[1] <- "NCRN" # "#Org_Code" 
            acts[2] <- "Biological Stream Survey Protocol" # "Project_ID"
            acts[3] <- df$Station_ID # "Location_ID" 
            acts[4] <- df$Pass_ID # "Activity_ID"
            acts[5] <- "Field Msr/Obs" # "Activity_Type"; choices are: 1) 'Field Msr/Obs' and 2) 'Sample-Routine'
            acts[6] <- "Water" # "Medium"  choices are "Water", "Air", and "Other" in `example`
            acts[7] <- NA # "Medium_Subdivision"
            acts[8] <- "Stream Fish" # "Assemblage_Sampled_Name"
            acts[9] <- format(df$SampleDate, "%Y-%m-%d") # "Activity_Start_Date"
            acts[10] <- format(df$SampleDate, "%H:%M") # "Activity_Start_Time" 
            acts[11] <- "Eastern Time - Washington, DC" # "Activity_Start_Time_Zone" 
            acts[12] <- NA # "Activity_End_Date" 
            acts[13] <- NA # "Activity_End_Time"
            acts[14] <- NA # "Activity_End_Time_Zone"  
            acts[15] <- NA # "Activity_Relative_Depth" 
            acts[16] <- NA # "Activity_Depth"
            acts[17] <- NA # "Activity_Depth_Unit"
            acts[18] <- NA # "Activity_Upper_Depth"
            acts[19] <- NA # "Activity_Lower_Depth"
            acts[20] <- NA # "Activity_Depth_Reference"
            acts[21] <- df$Station_Name # "Additional_Location_Info"
            acts[22] <- NA # "Activity_Sampler"; the person who did the sampling?
            # "Activity_Recorder"
            acts[23] <- NA
            for (i in 1:nrow(acts)){ # for each row
                ifelse(stringr::str_detect(acts$Activity_Recorder[i], "Event") == TRUE, # 'Event' ends up in some records, so we regex to remove
                       # regex to extract strings matching pattern:
                       # String starts with (^) any character (*)
                       # followed by one or more (+) numbers ([0-9])
                       # followed by a dash (-)
                       # followed by one or more (+) numbers ([0-9])
                       # followed by a period (.)
                       # followed by one or more (+) numbers ([0-9])
                       acts[i,23] <- stringr::str_extract(acts[i,23], "^*([0-9])+-([0-9])+.([0-9])+"),
                       acts[i,23] <- acts[i,23])# units are meters
            }
            acts[24] <- df$Station_Name # "Custody_ID" 
            acts[25] <- "NCRN" # "Activity_Conducting_Organization" 
            acts[26] <- NA # "Station_Visit_Comment" 
            acts[27] <- NA # "Activity_Comment
            acts[28] <- "Biological Stream Survey Protocol" # "Collection_Method_ID" 
            acts[29] <- NA # Possibly a Smith Root LR-24 but not known; "Collection_Equipment_Name" 
            acts[30] <- NA # Possibly a Smith Root LR-24 but not known; # "Collection_Equipment_Description" 
            acts[31] <- NA # subset(results_list$tlu_Collection_Procedures_Gear_Config, `Field Gear Category` == "Smith Root LR-24")$`Field Procedure ID` # "Gear_Deployment"
            acts[32] <- NA # "Container_Type"
            acts[33] <- NA # "Container_Color"
            acts[34] <- NA # "Container_Size"
            acts[35] <- NA # "Container_Size_Unit"
            acts[36] <- "Biological Stream Survey Protocol" # "Preparation_Method_ID"
            acts[37] <- "10% buffered formalin solution (later transferable to 70% EtOH solution)" # "Chemical_Preservative" # pdf pg 124 (135) https://doimspp.sharepoint.com/:b:/r/sites/NCRNBiologicalStreamSampling/Shared%20Documents/General/Operational%20Reviews/NCRN_Biological_Stream_Survey_Protocol_Ver_2.0_NRR.pdf?csf=1&web=1&e=u0kGN9
            acts[38] <- NA # "Thermal_Preservative". Fish are preserved via chemicals, not wet ice
            acts[39] <- NA # "Transport_Storage_Description" 
            acts[40] <- df$Pass_ID # "Activity_Group_ID"
            acts[41] <- df$Station_Name # "Activity_Group_Name" 
            acts[42] <- ifelse(is.na(acts[41])==TRUE,
                               paste0("Activities for: ", df$Station_Name),
                               NA)# "Activity_Group_Type"  
            acts[43] <- NA # we don't record stop time for fish events e-fishing, so duration is unknown
            acts[44] <- NA # we don't record stop time for fish events e-fishing, so duration unit is unknown
            acts[45] <- "Two-pass backpack electrofishing" # Sampling_Component_Name
            acts[46] <- NA # Sampling_Component_Place_In_Series
            acts[47] <- 75 # "Reach_Length"
            # Reach_Length_Unit
            for (i in 1:nrow(acts)){ # for each row
                if(!is.na(acts[i,47])){ # 75 m is the prescribed e-fishing reach distance; 
                    acts[i,48] <- "m" # units are meters
                } else {# otherwise
                    acts[i,48] <- NA #assign NA when "Reach_Length" is blank
                }
            }
            acts[49] <- NA # "Reach_Width"
            acts[50] <- NA # "Reach_Width_Unit" 
            # [electrofishing] "Pass_Count"
            # only assign a "Pass_Count" value if we know the reach length
            # because this gives us higher confidence that the record used the 2-pass e-fishing protocol
            for (i in 1:nrow(acts)){ # for each row
                if(!is.na(acts[i,47])){ # 75 m is the prescribed e-fishing reach distance; 
                    acts[i,51] <- 2 # units are meters
                } else {# otherwise
                    acts[i,51] <- NA #assign NA when "Reach_Length" is blank
                }
            }
            acts[52] <- NA # "Net_Type"
            acts[53] <- NA # "Net_Surface_Area"
            acts[54] <- NA # "Net_Surface_Area_Unit"
            acts[55] <- NA # "Net_Mesh_Size"
            acts[56] <- NA # "Net_Mesh_Size_Unit"  
            acts[57] <- NA # "Boat_Speed"
            acts[58] <- NA # "Boat_Speed_Unit"
            acts[59] <- NA # "Current_Speed"
            acts[60] <- NA # "Current_Speed_Unit"
            acts[61] <- NA # "Toxicity_Test_Type"
            acts[62] <- NA # "Effort"
            acts[63] <- NA # "Effort_Unit"
            acts <- as.data.frame(lapply(acts, function(y) gsub("NA", NA, y))) # remove "NA" chr strings
            colnames(acts)[1] <- "#Org_Code"
            # pass2022[32] <- '"data/NCRN_BSS_Fish_Monitoring_Data_2022_Marc.xlsx", sheet = "ElectrofishingData"' # Source
                
            # error-checking:
            check_df <- tibble::tibble(data.frame(matrix(ncol=3, nrow=ncol(acts))))
            colnames(check_df) <- c("acts", "example", "result")
            check_df$acts <- colnames(acts)
            check_df$example <- colnames(example)
            for(i in 1:nrow(check_df)){
                if(check_df$acts[i] == check_df$example[i]){
                    check_df$result[i] <- "MATCH"
                } else {
                    check_df$result[i] <- "MISMATCH"
                }
            }
            
            message(
                if(length(check_df$result == "MATCH") == nrow(check_df)){
                    "`getMarc2022Activities()` executed successfully..."
                } else {
                    for(i in 1:length(check_df$result != "MATCH")){
                        cat(paste(paste0("`acts", check_df$acts[i], "`"), paste0(" DID NOT MATCH `example.", check_df$example[i][i], "`"), "\n", sep = ""))
                    }
                }
            )
            # assign("pass2022", pass2022, envir = globalenv())
            
            return(acts)
        }
    )
}