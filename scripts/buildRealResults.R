# build example
# a module for `scripts/fish_data_view.R`

buildRealResults <- function(connection){
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
            example <- readxl::read_excel("data/NCRN_BSS_EDD_20230105_1300.xlsx", sheet = "Results") # https://doimspp.sharepoint.com/:x:/r/sites/NCRNDataManagement/Shared%20Documents/General/Standards/Data-Standards/EQuIS-WQX-EDD/NCRN_BSS_EDD_20230105_1300.xlsx?d=w8c283fde9cbd4af480945c8c8bd94ff6&csf=1&web=1&e=7Y9W1M
            
            # Query db
            db_objs <- RODBC::sqlTables(con) # test db connection
            tbl_names <- db_objs %>% # choose which tables you want to query
                subset(TABLE_NAME %in% c(
                    # "tbl_Events",
                    # "tbl_Protocol",
                    # "tbl_Fish_Events",
                    # "tbl_Locations",
                    # "tbl_Meta_Events",
                    # "tlu_Collection_Procedures_Gear_Config",
                    # "tbl_Electro_Fish_Details"
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
            df <- dplyr::left_join(df, results_list$tbl_Events %>% select(Event_ID, Protocol_ID, Start_Date, Start_Time, Location_ID, Comments, Event_Site_ID, Event_Group_ID), by = "Event_ID")
            df <- dplyr::left_join(df, results_list$tbl_Protocol %>% select(Protocol_ID, Protocol_Name, Protocol_Version, Version_Date), by = "Protocol_ID")
            df <- dplyr::left_join(df, results_list$tbl_Meta_Events %>% select(Event_ID, Entered_by), by = "Event_ID")
            df <- dplyr::left_join(df, results_list$tbl_Locations %>% select(Location_ID, Loc_Name, NCRN_Site_ID), by = "Location_ID")
            df2 <- results_list$tbl_Electro_Fish_Details %>% dplyr::distinct(Fish_Event_ID, .keep_all = TRUE)
            df <- dplyr::left_join(df, df2 %>% select(Fish_Event_ID, Pass_1_Start, Pass_1_End, Pass_2_Start, Pass_2_End), by = "Fish_Event_ID")
            
            # RODBC::odbcCloseAll() # close db connection
            
            #----- re-build `example` from `results_list`
            
            # starting point: copy the example dataframe but without data
            real <- tibble::tibble(data.frame(matrix(ncol = ncol(example), nrow = nrow(results_list$tbl_Fish_Events)))) # empty dataframe
            colnames(real) <- colnames(example) # name columns to match example
            
            real[1] <- "NCRN" # "#Org_Code" 
            real[2] <- df$Fish_Event_ID # "Activity_ID" shared field with `real_activities.Activity_ID`
            real[3] <-  # "Characteristic_Name"  
            real[4] <-  # "Method_Speciation"
            real[5] <- # "Filtered_Fraction"
            real[6] <- # "Result_Detection_Condition"
            real[7] <- # "Result_Text"
            real[8] <- # "Result_Unit"
            real[9] <- # "Result_Qualifier"
            real[10] <- # "Result_Status" 
            real[11] <- # "Result_Type" 
            real[12] <- # "Result_Comment" 
            real[13] <- # "Method_Detection_Limit"
            real[14] <- # "Lower_Quantification_Limit"
            real[15] <- # "Upper_Quantification_Limit" 
            real[16] <- # "Limit_Comment"
            real[17] <- # "Temperature_Basis"
            real[18] <- # "Statistical_Basis"
            real[19] <- # "Time_Basis" 
            real[20] <- # "Weight_Basis"
            real[21] <- # "Particle_Size_Basis"
            real[22] <- # "Precision"
            real[23] <- # "Bias"
            real[24] <- # "Confidence_Interval" 
            real[25] <- # "Upper_Confidence_Limit" 
            real[26] <- # "Lower_Confidence_Limit" 
            real[27] <- # "Result_Sampling_Point_Name"
            real[28] <- # "Result_Depth_Height_Measure"
            real[29] <- # "Result_Depth_Height_Measure_Unit" 
            real[30] <- # "Result_Depth_Altitude_Reference_Point"
            real[31] <- # "Analytical_Method_ID"
            real[32] <- # "Analytical_Remark"
            real[33] <- # "Lab_ID"
            real[34] <- # "Lab_Remark_Code"
            real[35] <- # "Analysis_Start_Date"
            real[36] <- # "Analysis_Start_Time" 
            real[37] <- # "Analysis_Start_Time_Zone"
            real[38] <- # "Lab_Accreditation_Indicator"
            real[39] <- # "Lab_Accreditation_Authority_Name" 
            real[40] <- # "Lab_Batch_ID"
            real[41] <- # "Lab_Sample_Preparation_ID" 
            real[42] <- # "Lab_Sample_Preparation_Start_Date"  
            real[43] <- # "Lab_Sample_Preparation_Start_Time"
            real[44] <- # "Lab_Sample_Preparation_Start_Time_Zone" 
            real[45] <- # "Dilution_Factor"
            real[46] <- # "Num_of_Replicates"
            real[47] <- # "Data_Logger_Line_Name"
            real[48] <- # "Biological_Intent"
            real[49] <- # "Biological_Individual_ID"
            real[50] <- # "Subject_Taxon"
            real[51] <- # "Unidentified_Species_ID"
            real[52] <- # "Tissue_Anatomy"
            real[53] <- # "Group_Summary_Count_or_Weight"
            real[54] <- # "Group_Summary_Count_or_Weight_Unit"
            real[55] <- # "Cell_Form"
            real[56] <- # "Cell_Shape"  
            real[57] <- # "Habit_Name_1"
            real[58] <- # "Habit_Name_2"
            real[59] <- # "Habit_Name_3"
            real[60] <- # "Voltinism"
            real[61] <- # "Pollution_Tolerance"
            real[62] <- # "Pollution_Tolerance_Scale"
            real[63] <- # "Trophic_Level"
            real[64] <- # "Functional_Feeding_Group_1"
            real[65] <- # "Functional_Feeding_Group_2"
            real[66] <- # "Functional_Feeding_Group_3"
            real[67] <- # "Resource_ID"
            real[68] <- # "Resource_Date"
            real[69] <- # "Resource_Title_Name"
            real[70] <- # "Resource_Creator_Name"
            real[71] <- # "Resource_Publisher_Name"
            real[72] <- # "Resource_Publication_Year"
            real[73] <- # "Resource_Volume_Pages"
            real[74] <- # "Resource_Subject_Text"
            real[75] <- # "Frequency_Class_Descriptor_1"
            real[76] <- # "Frequency_Class_Bounds_Unit_1"
            real[77] <- # "Frequency_Class_Lower_Bound_1"
            real[78] <- # "Frequency_Class_Upper_Bound_1"
            real[79] <- # "Frequency_Class_Descriptor_2"
            real[80] <- # "Frequency_Class_Bounds_Unit_2"
            real[81] <- # "Frequency_Class_Lower_Bound_2"
            real[82] <- # "Frequency_Class_Upper_Bound_2"
            real[83] <- # "Frequency_Class_Descriptor_3"
            real[84] <- # "Frequency_Class_Bounds_Unit_3"
            real[85] <- # "Frequency_Class_Lower_Bound_3"
            real[86] <- # "Frequency_Class_Upper_Bound_3"
            real[87] <- # "Taxonomist_Accreditation_Indicator"
            real[88] <- # "Taxonomist_Accreditation_Authority_Name"
            real[89] <- # "Result_File_Name"
            
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