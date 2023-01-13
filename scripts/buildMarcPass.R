#--------------------------------------------------------------------------
#----- Make our access db data match the required `example` format---------
#----- One row is one e-fishing pass---------------------------------------
#--------------------------------------------------------------------------
# a module for `scripts/buildMarcView.R`

buildMarcPass <- function(connection, add2022){
    tryCatch(
        expr = {
            #----- load external libraries
            suppressWarnings(suppressMessages(library(data.table)))
            suppressWarnings(suppressMessages(library(tidyverse)))
            suppressWarnings(suppressMessages(library(dplyr)))
            
            #----- load project functions
            source("scripts/getQueryResults.R") # equivalent to python "from x import function"
            source("scripts/buildMarc2022Pass.R") # equivalent to python "from x import function"
            # load example data
            example <- readxl::read_excel("data/NCRN_BSS_Fish_Monitoring_Data_2022_Marc.xlsx", sheet = "data_model_pass") # https://doimspp.sharepoint.com/:x:/r/sites/NCRNBiologicalStreamSampling/Shared%20Documents/General/Annual-Data-Packages/2022/Marc_and_Bob/Fish_Template/NCRN_BSS_Fish_Monitoring_Data_2022_Marc.xlsx?d=w306357b1a43a48f4b9f598169043cc6a&csf=1&web=1&e=BC5i5n
            marc2021 <- readxl::read_excel("data/NCRN_BSS_Fish_Monitoring_Data_2021_Marc.xlsx", sheet = "Fish Data (Individuals)")
            marc2022 <- readxl::read_excel("data/NCRN_BSS_Fish_Monitoring_Data_2022_Marc.xlsx", sheet = "ElectrofishingData")
            
            data.table::setnames(marc2022, "Species_ID...12", "common_name")
            data.table::setnames(marc2022, "Species_ID...13", "species_id")
            
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
                    "tlu_Basin_Code",
                    "tbl_Photos",
                    "tlu_Park_Code"
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
            
            #--------------------------------------------------------------------------
            #----- Make our access db data match the required `example` format---------
            #----- One row is one e-fishing pass---------------------------------------
            #--------------------------------------------------------------------------
            
            # make a flat dataframe where one row is one unique combination of `pass` and `species` from `results_list`
            
            df <- results_list$tbl_Fish_Data
            df <- rename(df, Comments_fishdata = Comments)
            df <- dplyr::left_join(df, results_list$tlu_Fish %>% select(Latin_Name, Common_Name, Nativity, Tolerance, `Trophic Status`), by = c("Fish_Species" = "Latin_Name"))
            df$total_n <- sum(df$Total_Pass_1, df$Total_Pass_2)
            df <- dplyr::left_join(df, results_list$tbl_Fish_Events %>% select(Fish_Event_ID, Event_ID, Seg_Length, Fish_Biomass_1, Fish_Biomass_2), by = "Fish_Event_ID")
            df <- dplyr::left_join(df, results_list$tbl_Events %>% select(Event_ID, Location_ID, Start_Date, Start_Time, Comments), by = "Event_ID")
            df <- rename(df, Comments_tblevents = Comments)
            df <- dplyr::left_join(df, results_list$tbl_Locations %>% select(Location_ID, Loc_Name, NCRN_Site_ID, Basin_Code, Site_ID, Unit_Code), by = "Location_ID")
            df <- dplyr::left_join(df, results_list$tlu_Basin_Code %>% select(Basin_Code, Basin), by = "Basin_Code")
            df <- dplyr::left_join(df, results_list$tbl_Meta_Events %>% select(Event_ID, Entered_Date), by = "Event_ID")
            df <- dplyr::left_join(df, results_list$tbl_Photos %>% select(Event_ID, Photo_num), by = "Event_ID")
            df <- dplyr::left_join(df, results_list$tlu_Park_Code %>% select(PARKCODE, PARKNAME), by = c("Unit_Code" = "PARKCODE"))
            
            
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
            pass <- tibble::tibble(data.frame(matrix(ncol = ncol(example), nrow = nrow(df)))) # empty dataframe
            colnames(pass) <- colnames(example) # name columns to match example
            
            pass[1] <- df$id # "FishObsID"
            pass[2] <- format(df$Start_Date, "%Y")# "Year"
            pass[3] <- as.character(format(df$Start_Date, "%Y-%m-%d")) # "SampleDate"
            pass[4] <- format(df$Start_Time, "%H:%M") # "SampleTime"
            pass[5] <- df$Site_ID # "Station_ID"
            pass[6] <- df$NCRN_Site_ID # "Station_Name"
            pass[7] <- paste0(format(df$Start_Date, "%Y-%m-%d"), "_", format(df$Start_Time, "%H:%M"), "_", df$pass) # "Pass_ID"
            pass[8] <- as.character(format(df$Entered_Date, "%Y-%m-%d")) # "Entry_Date"
            pass[9] <- as.character(format(df$Entered_Date, "%H:%M")) # "Entry_Time"
            pass[10] <- trimws(tolower(df$Common_Name)) # "Subject_Taxon"
            # "Species_ID"
            pass <- dplyr::left_join(pass, tlu_species, by=c("Subject_Taxon" = "common_name"))
            pass[11] <- pass$species_id # "Species_ID"
            pass$species_id <- NULL # delete the joined column
            pass[12] <- df$value # "Count"
            pass[13] <- NA # "Status"
            pass[14] <- df$retained # "Disposition"
            for(i in 1:nrow(pass)){
                if(pass$Disposition[i] >0){
                    pass$Disposition[i] <- paste0("Retained ", pass$Disposition[i], " individuals")
                } else {
                    pass$Disposition[i] <- "Released"
                }
            }
            pass[15] <- df$Photo_num # "Picture"
            pass[16] <- NA # "Camera" 
            # "Photo"
            # the value of $Photo should be TRUE or FALSE
            # depending on whether there is a picture
            # in our access db, there's a picture if there's a value in $Photo_num
            for(i in 1:nrow(pass)){
                if(is.na(pass$Picture[i])){
                    pass[i,17] <- FALSE
                } else {
                    pass[i,17] <- TRUE
                }
            }
            pass[18] <- NA # "TL_mm" NA because `pass` aggregates to e-fishing pass (refer to `indiv` for $TL_mm data)
            pass[19] <- NA # "Wt_g" NA because `pass` aggregates to e-fishing pass (refer to `indiv` for $Wt_g data)
            pass[20] <- NA # "Calc_wt_TL_g" NA because `pass` aggregates to e-fishing pass (refer to `indiv` for $Calc_wt_TL_g data)
            pass[21] <- NA # "Wt_AddedOn"
            pass[22] <- df$Comments_tblevents # "Note"
            pass[23] <- df$Basin# "Basin"
            pass[24] <- df$Loc_Name # "Branch"
            pass[25] <- df$PARKNAME # "Reach_Name"
            pass[26] <- df$Anom # "Delt_deformities" 
            pass[26] <- as.character(pass$Delt_deformities) # "Delt_deformities" 
            # pass[25] <- as.character(pass[25])
            for(i in 1:nrow(pass)){
                if(pass[i,26] != "0"){
                    pass[i,26] <- "DELT reported for this pass"
                } else {
                    pass[i,26] <- "No DELT"
                }
            }    
            pass[27] <- NA #"Delt_erodedfins" 
            pass[28] <- NA # "Delt_lesions" 
            pass[29] <- NA # "Delt_tumors" 
            for(i in 1:nrow(pass)){
                if(pass[i,26] == "No DELT"){
                    pass[i,30] <- NA
                } else {
                    pass[i,30] <- df$Comments_fishdata[i] # "Delt_other"
                }
            }
            pass[31] <- 'NCRN MBSS Access db: "~Documents - NPS-NCRN-Biological Stream Sampling\General\Annual-Data-Packages\2022\NCRN_MBSS\NCRN_MBSS_be_2022.mdb"'
            pass <- unique(setDT(pass), by=c("FishObsID")) 

                
            # error-checking:
            check_df <- tibble::tibble(data.frame(matrix(ncol=3, nrow=ncol(pass))))
            colnames(check_df) <- c("pass", "example", "result")
            check_df$pass <- colnames(pass)
            check_df$example <- colnames(example)
            for(i in 1:nrow(check_df)){
                if(check_df$pass[i] == check_df$example[i]){
                    check_df$result[i] <- "MATCH"
                } else {
                    check_df$result[i] <- "MISMATCH"
                }
            }
            #--------------------------------------------
            #----- Add Marc's 2022 data to indiv format--
            #--------------------------------------------
            if(add2022 == TRUE){
                pass2022 <- buildMarc2022Pass()
                pass <- rbind(pass, pass2022)
            }
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            message(
                if(length(check_df$result == "MATCH") == nrow(check_df)){
                    "`buildMarcPass()` executed successfully..."
                } else {
                    for(i in 1:length(check_df$result != "MATCH")){
                        cat(paste(paste0("`pass_marc", check_df$pass_marc[i], "`"), paste0(" DID NOT MATCH `example.", check_df$example[i][i], "`"), "\n", sep = ""))
                    }
                }
            )
            # assign("pass_marc", pass_marc, envir = globalenv())

            return(pass)
        }
    )
}