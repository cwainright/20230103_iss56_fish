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
            
            marc2022 <- rename(marc2022, common_name = `Species_ID...12`)
            marc2022 <- rename(marc2022, species_id = `Species_ID...13`)
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
            
            #--------------------------------------------
            #----- Marc's 2022 format--------------------
            #--------------------------------------------
            
            # make a flat dataframe where one row is one e-fishing pass from `results_list`
            # this will have data that matches Marc's 2022 format: readxl::read_excel("data/NCRN_BSS_Fish_Monitoring_Data_2022_Marc.xlsx", sheet = "ElectrofishingData")
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
            
            # make a lookup table to find $Species_ID values
            tlu_species <- marc2022 %>% select(common_name, species_id) # select the two columns we're interested in
            tlu_species$dummy <- paste0(tlu_species$common_name, tlu_species$species_id) # concatenate these values so we know unique pairs
            tlu_species <- tlu_species[!duplicated(tlu_species$dummy), ] # (referential integrity: making sure there's no repeated key-value pairs or keys with more than one value or values with more than one key)
            tlu_species <- tlu_species %>% select(common_name, species_id) # keep only the key-value pairs
            tlu_species$common_name <- tolower(tlu_species$common_name)
            for(i in 1:nrow(tlu_species)){
                if(stringr::str_detect(tlu_species$common_name[i], "\\(")==TRUE){
                    tlu_species$common_name[i] <- stringr::str_extract(tlu_species$common_name[i], "(?<=\\().+?(?=\\))") # https://stackoverflow.com/questions/8613237/extract-info-inside-all-parenthesis-in-r
                }
            }
            
            
            #----- re-build `example` from `results_list`
            
            # starting point: copy the example dataframe but without data
            real <- tibble::tibble(data.frame(matrix(ncol = ncol(example), nrow = nrow(df)))) # empty dataframe
            colnames(real) <- colnames(example) # name columns to match example
            
            real[1] <- df$id # "FishObsID"
            real[2] <- format(df$Start_Date, "%Y")# "Year"
            real[3] <- format(df$Start_Date, "%Y-%m-%d") # "SampleDate"
            real[4] <- format(df$Start_Time, "%H:%M") # "SampleTime"
            real[5] <- df$NCRN_Site_ID# "Station_ID"
            real[6] <- paste0(format(df$Start_Date, "%Y-%m-%d"), "_", format(df$Start_Time, "%H:%M"), "_", df$pass) # "Pass_ID"
            real[7] <- format(df$Entered_Date, "%Y-%m-%d") # "Entry_Date"
            real[8] <- format(df$Entered_Date, "%H:%M") # "Entry_Time"
            real[9] <- trimws(tolower(df$Common_Name)) # "Subject_Taxon"
            # "Species_ID"
            real <- dplyr::left_join(real, tlu_species, by=c("Subject_Taxon" = "common_name"))
            real[10] <- real$species_id # "Species_ID"
            real$species_id <- NULL # delete the joined column
            
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
                
            #--------------------------------------------
            #----- Marc's 2021 format--------------------
            #--------------------------------------------
            
            # make a flat dataframe where one row is one individual from `results_list`
            # this will have data that matches Marc's 2021 format: readxl::read_excel("data/NCRN_BSS_Fish_Monitoring_Data_2021_Marc.xlsx", sheet = "Fish Data (Individuals)")
            
            df2 <- results_list$tbl_GameFish
            df2$Characteristic_Name <- "Individual fish total length"
            df2$Result_Text <- df2$LENGTH
            df2$Result_Unit <- "mm"
            results_list$tlu_Fish$Common_Name <- trimws(results_list$tlu_Fish$Common_Name, which=c("both"))
            df2$SPECIES <- toupper(df2$SPECIES)
            df2 <- dplyr::left_join(df2, results_list$tlu_Fish %>% select(Common_Name, Latin_Name), by = c("SPECIES" = "Common_Name"))
            df2 <- rename(df2, species = Latin_Name)
            df2 <- rename(df2, Common_Name = SPECIES)
            df2$Comments <- NA
            
            
            
            #----- re-build `example` from `results_list`
            
            # starting point: copy the example dataframe but without data
            real2 <- tibble::tibble(data.frame(matrix(ncol = ncol(example), nrow = nrow(df2)))) # empty dataframe
            colnames(real2) <- colnames(example) # name columns to match example
            
            real2[1] <- # "FishObsID"
            real2[2] <- # "Year"
            real2[3] <- # "SampleDate"
            real2[4] <- # "SampleTime"
            real2[5] <- # "Station_ID"
            real2[6] <- # "Pass_ID"
            real2[7] <- # "Entry_Date"
            real2[8] <- # "Entry_Time"
            real2[9] <- # "Subject_Taxon"
            real2[10] <- # "Species_ID"
            real2[11] <- # "Status"
            real2[12] <- # "Disposition"
            real2[13] <- # "Picture"
            real2[14] <- # "Camera"  
            real2[15] <- # "Photo" 
            real2[16] <- # "TL_mm"
            real2[17] <- # "Wt_g"
            real2[18] <- # "Calc_wt_TL_g"
            real2[19] <- # "Wt_AddedOn"
            real2[20] <- # "Note"
            real2[21] <- # "Basin"
            real2[22] <- # "Branch"
            real2[23] <- # "Reach_Name" 
            real2[24] <- # "Station_Name" 
            real2[25] <-  # "Delt_deformities" 
            real2[26] <-  #"Delt_erodedfins" 
            real2[27] <-  # "Delt_lesions" 
            real2[28] <-  # "Delt_tumors" 
            real2[29] <-  # "Delt_other" 

            combined <- rbind(df,df2)
            combined <- as.data.frame(lapply(combined, function(y) gsub("NA", NA, y))) # remove "NA" chr strings  
            
            
            
            
            
            
            # error-checking:
            check_df <- tibble::tibble(data.frame(matrix(ncol=3, nrow=ncol(combined))))
            colnames(check_df) <- c("combined", "example", "result")
            check_df$combined <- colnames(combined)
            check_df$example <- colnames(example)
            for(i in 1:nrow(check_df)){
                if(check_df$combined[i] == check_df$example[i]){
                    check_df$result[i] <- "MATCH"
                } else {
                    check_df$result[i] <- "MISMATCH"
                }
            }
            
            message(
                if(length(check_df$result == "MATCH") == nrow(check_df)){
                    "`buildMarcView()` executed successfully...\nOutput saved as `marc_view` in global environment."
                } else {
                    for(i in 1:length(check_df$result != "MATCH")){
                        cat(paste(paste0("`combined.", check_df$combined[i], "`"), paste0(" DID NOT MATCH `example.", check_df$example[i][i], "`"), "\n", sep = ""))
                    }
                }
            )
            assign("combined_marc", combined, envir = globalenv())
            if(write == TRUE){
                data.table::fwrite(combined, file.choose())
            }
            return(combined)
        }
    )
}