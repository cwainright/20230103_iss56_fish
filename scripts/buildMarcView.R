# build example
# a module for `scripts/fish_data_view.R`

buildMarc <- function(connection, write){
    tryCatch(
        expr = {
            #----- load external libraries
            suppressWarnings(suppressMessages(library(data.table)))
            suppressWarnings(suppressMessages(library(tidyverse)))
            suppressWarnings(suppressMessages(library(dplyr)))
            
            #----- load project functions
            source("scripts/getQueryResults.R") # equivalent to python "from x import function"
            # load example data
            example <- readxl::read_excel("data/NCRN_BSS_Fish_Monitoring_Data_2022_Marc.xlsx", sheet = "data_model") # https://doimspp.sharepoint.com/:x:/r/sites/NCRNBiologicalStreamSampling/Shared%20Documents/General/Annual-Data-Packages/2022/Marc_and_Bob/Fish_Template/NCRN_BSS_Fish_Monitoring_Data_2022_Marc.xlsx?d=w306357b1a43a48f4b9f598169043cc6a&csf=1&web=1&e=BC5i5n
            marc2021 <- readxl::read_excel("data/NCRN_BSS_Fish_Monitoring_Data_2021_Marc.xlsx", sheet = "Fish Data (Individuals)")
            marc2022 <- readxl::read_excel("data/NCRN_BSS_Fish_Monitoring_Data_2022_Marc.xlsx", sheet = "ElectrofishingData")
            
            # Query db
            db_objs <- RODBC::sqlTables(con) # test db connection
            tbl_names <- db_objs %>% # choose which tables you want to query
                subset(TABLE_NAME %in% c(
                    "tbl_Events",
                    "tbl_Protocol",
                    "tbl_Fish_Events",
                    "tbl_Locations",
                    "tbl_Meta_Events",
                    "tbl_Fish_Data",
                    "tbl_GameFish",
                    "tlu_Fish",
                    "tlu_Collection_Procedures_Gear_Config",
                    "tbl_Electro_Fish_Details",
                    "tlu_Basin_Code"
                )
                ) %>%
                select(TABLE_NAME)
            
            # make list of queries so we can extract a few rows from each table
            qry_list <- vector(mode="list", length=nrow(tbl_names))
            names(qry_list) <- tbl_names$TABLE_NAME
            for (i in 1:length(qry_list)){
                qry_list[[i]] <- paste("SELECT * FROM", names(qry_list)[i])
            }
            
            results_list <- getQueryResults(qryList = qry_list, connection = con)
            
            # tidy up
            rm(db_objs)
            rm(tbl_names)
            rm(qry_list)
            
            # make a flat dataframe from `results_list`
            df <- results_list$tbl_Fish_Data
            df <- dplyr::left_join(df, results_list$tlu_Fish %>% select(Latin_Name, Common_Name, Nativity, Tolerance, `Trophic Status`), by = c("Fish_Species" = "Latin_Name"))
            df$total_n <- sum(df$Total_Pass_1, df$Total_Pass_2)
            df <- dplyr::left_join(df, results_list$tbl_Fish_Events %>% select(Fish_Event_ID, Event_ID, Seg_Length, Fish_Biomass_1, Fish_Biomass_2), by = "Fish_Event_ID")
            df <- dplyr::left_join(df, results_list$tbl_Events %>% select(Event_ID, Location_ID, Start_Date, Start_Time), by = "Event_ID")
            df <- dplyr::left_join(df, results_list$tbl_Locations %>% select(Location_ID, Loc_Name, NCRN_Site_ID, Basin_Code), by = "Location_ID")
            df <- dplyr::left_join(df, results_list$tlu_Basin_Code %>% select(Basin_Code, Basin), by = "Basin_Code")
            df <- dplyr::left_join(df, results_list$tbl_Meta_Events %>% select(Event_ID, Entered_Date), by = "Event_ID")
            
            
            # format from wide-format data: $Total_Pass_1 and $Total_Pass_2 to long-format $value and $pass
            keepcols <- df %>% select(-c("Total_Pass_1", "Total_Pass_2"))
            keepcols <- colnames(keepcols)
            df <- data.table::melt(setDT(df), measure.vars = c("Total_Pass_1","Total_Pass_2"), id.vars = keepcols, variable.name = "pass")
            df$pass <- stringr::str_extract(df$pass, "[A-Za-z]+_[0-9]+") # regex extracts only Pass_x
            df$pass <- gsub("_", "", df$pass) # gsub replaces underscore "_" with nothing ""
            df$id <- paste0(df$Data_ID, "_", df$pass)
            # RODBC::odbcCloseAll() # close db connection
            
            #----- re-build `example` from `results_list`
            
            # starting point: copy the example dataframe but without data
            real <- tibble::tibble(data.frame(matrix(ncol = ncol(example), nrow = nrow(df)))) # empty dataframe
            colnames(real) <- colnames(example) # name columns to match example
            
            real[1] <- df$id # "FishObsID"
            real[2] <- format(df$Start_Date, "%Y")# "Year"
            real[3] <- format(df$Start_Date, "%Y-%m-%d") # "SampleDate"
            real[4] <-  format(df$Start_Time, "%H:%M") # "SampleTime"
            real[5] <-  df$NCRN_Site_ID# "Station_ID"
            real[6] <-  paste0(format(df$Start_Date, "%Y-%m-%d"), "_", format(df$Start_Time, "%H:%M"), "_", df$pass) # "Pass_ID"
            real[7] <- format(df$Entered_Date, "%Y-%m-%d") # "Entry_Date"
            real[8] <- format(df$Entered_Date, "%H:%M") # "Entry_Time"
            real[9] <- df$Common_Name # "Subject_Taxon"
            real[10] <- # "Species_ID" 
            real[11] <-  # "Status"
            real[12] <- df$retained # "Disposition"
            for(i in 1:nrow(real)){
                if(real$Disposition[i] >0){
                    real$Disposition[i] <- paste0("Retained ", real$Disposition[i], " individuals")
                } else {
                    real$Disposition[i] <- "Released"
                }
            }
            real[13] <-  # "Picture"
            real[14] <-  # "Camera"  
            real[15] <-  # "Photo" 
            real[16] <-  # "TL_mm"
            real[17] <-  # "Wt_g"
            real[18] <-  # "Calc_wt_TL_g"
            real[19] <-  # "Wt_AddedOn"
            real[20] <- df$Comments # "Note"
            real[21] <- df$Basin# "Basin"
            real[22] <- df$Loc_Name # "Branch"
            real[23] <- df$NCRN_Site_ID # "Reach_Name" 
            real[24] <-  # "Station_Name" 
            real[25] <-  # "Delt_deformities" 
            real[26] <-  #"Delt_erodedfins" 
            real[27] <-  # "Delt_lesions" 
            real[28] <-  # "Delt_tumors" 
            real[29] <-  # "Delt_other" 
            
            real <- as.data.frame(lapply(real, function(y) gsub("NA", NA, y))) # remove "NA" chr strings    
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
                    "`buildFish()` executed successfully...\nOutput saved as `real_fish` in global environment."
                } else {
                    for(i in 1:length(check_df$result != "MATCH")){
                        cat(paste(paste0("`real.", check_df$real[i], "`"), paste0(" DID NOT MATCH `example.", check_df$example[i][i], "`"), "\n", sep = ""))
                    }
                }
            )
            assign("real_marc", real, envir = globalenv())
            if(write == TRUE){
                data.table::fwrite(real, file.choose())
            }
            return(real)
        }
    )
}