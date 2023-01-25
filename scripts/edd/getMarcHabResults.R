# a module for `buildEDD()`
options(warn = -1)
getMarcHabResults <- function(habitat_marc2022, habitat_marc2021, example, results_list){
    tryCatch(
        expr = {
            #----- load external libraries
            suppressWarnings(suppressMessages(library(data.table)))
            suppressWarnings(suppressMessages(library(tidyverse)))
            suppressWarnings(suppressMessages(library(dplyr)))
            suppressWarnings(suppressMessages(library(readxl)))
            
            #----- wrangle 2021
            df2021 <- habitat_marc2021
            df2021 <- df2021 %>% select(NCRN_Site_ID, c(7:61))
            measure_vars <- colnames(habitat_marc2021 %>% select(c(7:ncol(habitat_marc2021)))) # the columns we want to melt into `value` and `variable` columns
            df2021 <- melt(data.table::setDT(df2021), id.vars = c("NCRN_Site_ID"),
                           measure.vars = measure_vars)
            df2021 <- dplyr::left_join(df2021, habitat_marc2021 %>% select(NCRN_Site_ID, Unit_Code, Loc_Name, Event_Site_ID, Year, Start_Date), by="NCRN_Site_ID")
            for(i in 1:nrow(df2021)){
                df2021$Site_ID[i] <- stringr::str_extract(df2021$Event_Site_ID[i], "[A-Z][A-Z][A-Z][A-Z]-[0-9][0-9][0-9]-[A-Z]")
            }
            df2021$Start_Date <- as.character(df2021$Start_Date)
            
            #----- wrangle 2022
            df2022 <- habitat_marc2022
            id_vars <- colnames(habitat_marc2022)[1] # the column(s) we want to keep as columns
            measure_vars <- colnames(habitat_marc2022 %>% select(c(2:ncol(habitat_marc2022)))) # the columns we want to melt into `value` and `variable` columns
            df2022 <- melt(data.table::setDT(df2022), id.vars = id_vars,
                           measure.vars = measure_vars)
            data.table::setnames(df2022, "variable", "Site_ID") # rename columns to match df2021
            data.table::setnames(df2022, "Site", "variable") # rename columns to match df2021
            df2022$Site_ID <- paste0(df2022$Site_ID, "-N") # add the '-N' suffix so we can xref against `results_list$tbl_Locations`
            df2022 <- dplyr::left_join(df2022, results_list$tbl_Locations %>% select(Site_ID, NCRN_Site_ID, Unit_Code, Loc_Name), by=c("Site_ID")) %>%
                mutate(Event_Site_ID = paste0(Site_ID, "-2022"), # create a column `Event_Site_ID`
                       Year = "2022", # create a column `Year`
                       Start_Date = NA) # create a column `Start_Date`
            df2022 <- df2022 %>% select(colnames(df2021)) # set order of cols to match
            # extract date
            tlu_date <- df2022 %>% select(variable, value, Event_Site_ID) %>% subset(variable == "Date" & value != "8") # make a lookup table to grab sample dates
            tlu_date$value <- as.Date(as.numeric(trimws(tlu_date$value)), origin = "1899-12-30") # 1899-12-30 is MS Excel epoch start
            df2022 <- subset(df2022, variable != "Date") # get rid of variable == "Date" because Date is its own column
            df2022 <- dplyr::left_join(df2022, tlu_date %>% select(value, Event_Site_ID), by="Event_Site_ID")
            df2022$Start_Date <- df2022$value.y
            df2022$value.y <- NULL
            data.table::setnames(df2022, "value.x", "value")
            df2022$Start_Date <- as.character(df2022$Start_Date)
            
            #----- combine 2021 and 2022
            df <- rbind(df2021, df2022) # combine
            df$Activity_ID <- paste0(df$Event_Site_ID, "_", df$variable)
            
            #----- use NCRN lookup tables to add values that are missing from Marc's data
            df <- dplyr::left_join(df, results_list$tbl_Locations %>% select(-c(Unit_Code, Loc_Name, Site_ID)), by=c("NCRN_Site_ID")) # add lookup values
            
            #----- build to match example
            real <- tibble::tibble(data.frame(matrix(ncol = ncol(example), nrow = nrow(df)))) # empty dataframe
            colnames(real) <- colnames(example) # name columns to match example

            real[1] <- "NCRN" # "#Org_Code" 
            real[2] <- paste0(df$Activity_ID, "\\", df$Location_ID) # "Activity_ID"
            real[3] <- "Stream habitat inventory" # "Characteristic_Name"  
            real[4] <- df$variable # "Method_Speciation"
            real[5] <- NA # "Filtered_Fraction"
            real[6] <- NA # "Result_Detection_Condition"
            real[7] <- df$value # "Result_Text"
            real[8] <- NA # "Result_Unit"
            real[9] <- NA # "Result_Qualifier"
            real[10] <- "Final" # "Result_Status" 
            real[11] <- "Actual" # "Result_Type" 
            real[12] <- NA # "Result_Comment" 
            real[13] <- NA # "Method_Detection_Limit"
            real[14] <- NA # "Lower_Quantification_Limit"
            real[15] <- NA # "Upper_Quantification_Limit" 
            real[16] <- NA # "Limit_Comment"
            real[17] <- NA # "Temperature_Basis"
            real[18] <- NA # "Statistical_Basis"
            real[19] <- NA # "Time_Basis" 
            real[20] <- NA # "Weight_Basis"
            real[21] <- NA # "Particle_Size_Basis"
            real[22] <- NA # "Precision"
            real[23] <- NA # "Bias"
            real[24] <- NA # "Confidence_Interval" 
            real[25] <- NA # "Upper_Confidence_Limit" 
            real[26] <- NA # "Lower_Confidence_Limit" 
            real[27] <- df$Loc_Name # "Result_Sampling_Point_Name"
            real[28] <- NA # "Result_Depth_Height_Measure"
            real[29] <- NA # "Result_Depth_Height_Measure_Unit" 
            real[30] <- NA # "Result_Depth_Altitude_Reference_Point"
            real[31] <- NA # "Analytical_Method_ID"
            real[32] <- NA # "Analytical_Remark"
            real[33] <- NA # "Lab_ID"
            real[34] <- NA # "Lab_Remark_Code"
            real[35] <- format(as.Date(df$Start_Date), "%Y-%m-%d") # "Analysis_Start_Date"
            real[36] <- format(as.Date(df$Start_Date), "%Y-%m-%d") # "Analysis_Start_Time" 
            real[37] <- "Eastern Time - Washington, DC" # "Analysis_Start_Time_Zone"
            real[38] <- NA # "Lab_Accreditation_Indicator"
            real[39] <- NA # "Lab_Accreditation_Authority_Name" 
            real[40] <- NA # "Lab_Batch_ID"
            real[41] <- NA # "Lab_Sample_Preparation_ID" 
            real[42] <- NA # "Lab_Sample_Preparation_Start_Date"  
            real[43] <- NA # "Lab_Sample_Preparation_Start_Time"
            real[44] <- NA # "Lab_Sample_Preparation_Start_Time_Zone" 
            real[45] <- NA # "Dilution_Factor"
            real[46] <- NA # "Num_of_Replicates"
            real[47] <- NA # "Data_Logger_Line_Name"
            real[48] <- NA # "Biological_Intent"
            real[49] <- NA # "Biological_Individual_ID"
            real[50] <- NA # "Subject_Taxon"
            real[51] <- NA # "Unidentified_Species_ID"
            real[52] <- NA # "Tissue_Anatomy"
            real[53] <- NA # "Group_Summary_Count_or_Weight"
            real[54] <- NA # "Group_Summary_Count_or_Weight_Unit"
            real[55] <- NA # "Cell_Form"
            real[56] <- NA # "Cell_Shape"  
            real[57] <- NA # "Habit_Name_1"
            real[58] <- NA # "Habit_Name_2"
            real[59] <- NA # "Habit_Name_3"
            real[60] <- NA # "Voltinism"
            real[61] <- NA # "Pollution_Tolerance"
            real[62] <- NA # "Pollution_Tolerance_Scale"
            real[63] <- NA # "Trophic_Level"
            real[64] <- NA # "Functional_Feeding_Group_1"
            real[65] <- NA # "Functional_Feeding_Group_2"
            real[66] <- NA # "Functional_Feeding_Group_3"
            real[67] <- NA # "Resource_ID"
            real[68] <- NA # "Resource_Date"
            real[69] <- NA # "Resource_Title_Name"
            real[70] <- NA # "Resource_Creator_Name"
            real[71] <- NA # "Resource_Publisher_Name"
            real[72] <- NA # "Resource_Publication_Year"
            real[73] <- NA # "Resource_Volume_Pages"
            real[74] <- NA # "Resource_Subject_Text"
            real[75] <- NA # "Frequency_Class_Descriptor_1"
            real[76] <- NA # "Frequency_Class_Bounds_Unit_1"
            real[77] <- NA # "Frequency_Class_Lower_Bound_1"
            real[78] <- NA # "Frequency_Class_Upper_Bound_1"
            real[79] <- NA # "Frequency_Class_Descriptor_2"
            real[80] <- NA # "Frequency_Class_Bounds_Unit_2"
            real[81] <- NA # "Frequency_Class_Lower_Bound_2"
            real[82] <- NA # "Frequency_Class_Upper_Bound_2"
            real[83] <- NA # "Frequency_Class_Descriptor_3"
            real[84] <- NA # "Frequency_Class_Bounds_Unit_3"
            real[85] <- NA # "Frequency_Class_Lower_Bound_3"
            real[86] <- NA # "Frequency_Class_Upper_Bound_3"
            real[87] <- NA # "Taxonomist_Accreditation_Indicator"
            real[88] <- NA # "Taxonomist_Accreditation_Authority_Name"
            real[89] <- NA # "Result_File_Name"
                        
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
                    "`getMarcHabLocations()` executed successfully..."
                } else {
                    for(i in 1:length(check_df$result != "MATCH")){
                        cat(paste(paste0("`real", check_df$real[i], "`"), paste0(" DID NOT MATCH `example.", check_df$example[i][i], "`"), "\n", sep = ""))
                    }
                }
            )
            # assign("real", real, envir = globalenv())
            return(real)
        }
    )
}