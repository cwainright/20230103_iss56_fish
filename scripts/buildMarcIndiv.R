#--------------------------------------------------------------------------
#----- Make our access db data match the required `example` format---------
#----- One row is one individual fish--------------------------------------
#--------------------------------------------------------------------------
# a module for `scripts/buildMarcView.R`




#!!!!!!!!! USE GAME-FISH DATA FOR INDIVIDUALS


buildMarcIndiv <- function(connection, addMarc){
    tryCatch(
        expr = {
            #----- load external libraries
            suppressWarnings(suppressMessages(library(data.table)))
            suppressWarnings(suppressMessages(library(tidyverse)))
            suppressWarnings(suppressMessages(library(dplyr)))
            
            #----- load project functions
            source("scripts/getQueryResults.R") # equivalent to python "from x import function"
            source("scripts/buildMarc2022Indiv.R") # equivalent to python "from x import function"
            source("scripts/buildMarc2021Indiv.R") # equivalent to python "from x import function"
            # load example data
            example <- readxl::read_excel("data/NCRN_BSS_Fish_Monitoring_Data_2022_Marc.xlsx", sheet = "data_model") # https://doimspp.sharepoint.com/:x:/r/sites/NCRNBiologicalStreamSampling/Shared%20Documents/General/Annual-Data-Packages/2022/Marc_and_Bob/Fish_Template/NCRN_BSS_Fish_Monitoring_Data_2022_Marc.xlsx?d=w306357b1a43a48f4b9f598169043cc6a&csf=1&web=1&e=BC5i5n
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
            #----- One row is one fish-------------------------------------------------
            #--------------------------------------------------------------------------
            
            # make a flat dataframe where one row is one individual fish from `results_list`
            
            df <- results_list$tbl_GameFish
            df$count <- 1
            df <- dplyr::left_join(df, results_list$tbl_Fish_Events %>% select(Fish_Event_ID, Event_ID, Seg_Length, Fish_Biomass_1, Fish_Biomass_2), by = "Fish_Event_ID")
            df <- dplyr::left_join(df, results_list$tbl_Events %>% select(Event_ID, Location_ID, Start_Date, Start_Time, Comments), by = "Event_ID")
            data.table::setnames(df, "Comments", "Comments_tblevents")
            df <- dplyr::left_join(df, results_list$tbl_Locations %>% select(Location_ID, Loc_Name, NCRN_Site_ID, Basin_Code, Site_ID, Unit_Code), by = "Location_ID")
            df <- dplyr::left_join(df, results_list$tlu_Basin_Code %>% select(Basin_Code, Basin), by = "Basin_Code")
            df <- dplyr::left_join(df, results_list$tbl_Meta_Events %>% select(Event_ID, Entered_Date), by = "Event_ID")
            df <- dplyr::left_join(df, results_list$tlu_Park_Code %>% select(PARKCODE, PARKNAME), by = c("Unit_Code" = "PARKCODE"))
            tbl_Photos <- aggregate(Photo_num~Event_ID, results_list$tbl_Photos %>% select(Event_ID, Photo_num), paste, collapse = ', ')
            df <- dplyr::left_join(df, tbl_Photos, by = "Event_ID")
            tlu_Fish <- results_list$tlu_Fish %>% select(Latin_Name, Common_Name, Nativity, Tolerance, `Trophic Status`)
            tlu_Fish$Common_Name <- trimws(tlu_Fish$Common_Name)
            df <- dplyr::left_join(df, tlu_Fish, by = c("SPECIES" = "Common_Name"))
            
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
            indiv <- tibble::tibble(data.frame(matrix(ncol = ncol(example), nrow = nrow(df)))) # empty dataframe
            colnames(indiv) <- colnames(example) # name columns to match example
            
            indiv[1] <- df$Data_ID # "FishObsID"
            indiv[2] <- format(df$Start_Date, "%Y")# "Year"
            indiv[3] <- as.character(format(df$Start_Date, "%Y-%m-%d")) # "SampleDate"
            indiv[4] <- format(df$Start_Time, "%H:%M") # "SampleTime"
            indiv[5] <- df$Site_ID # "Station_ID"
            indiv[6] <- df$NCRN_Site_ID # "Station_Name"
            indiv[7] <- paste0(format(df$Start_Date, "%Y-%m-%d"), "_", format(df$Start_Time, "%H:%M"), "_", df$pass) # "Pass_ID"
            indiv[8] <- as.character(format(df$Entered_Date, "%Y-%m-%d")) # "Entry_Date"
            indiv[9] <- as.character(format(df$Entered_Date, "%H:%M")) # "Entry_Time"
            indiv[10] <- trimws(tolower(df$SPECIES)) # "Subject_Taxon"
            # "Species_ID"
            indiv <- dplyr::left_join(indiv, tlu_species, by=c("Subject_Taxon" = "common_name"))
            indiv[11] <- indiv$species_id # "Species_ID"
            indiv$species_id <- NULL # delete the joined column
            
            indiv[12] <- df$count# "Count"
            indiv[13] <- NA # "Status"
            indiv[14] <- NA # "Disposition"
            indiv[15] <- df$Photo_num # "Picture"
            indiv[16] <- NA # "Camera" 
            # "Photo"
            # the value of $Photo should be TRUE or FALSE
            # depending on whether there is a picture
            # in our access db, there's a picture if there's a value in $Photo_num
            for(i in 1:nrow(indiv)){
                if(is.na(indiv$Picture[i])){
                    indiv[i,17] <- FALSE
                } else {
                    indiv[i,17] <- TRUE
                }
            }
                
            indiv[18] <- df$LENGTH # "TL_mm"
            indiv[19] <- NA # "Wt_g"
            indiv[20] <- NA # "Calc_wt_TL_g"
            indiv[21] <- NA # "Wt_AddedOn"
            indiv[22] <- df$Comments_tblevents # "Note"
            indiv[23] <- df$Basin# "Basin"
            indiv[24] <- df$Loc_Name # "Branch"
            indiv[25] <- df$PARKNAME # "Reach_Name"
            indiv[26] <- NA # "Delt_deformities" 
            indiv[27] <- NA #"Delt_erodedfins" 
            indiv[28] <- NA # "Delt_lesions" 
            indiv[29] <- NA # "Delt_tumors"
            indiv[30] <- NA # "Delt_other"
            indiv[31] <- 'NCRN MBSS Access db: tbl_GameFish, "~Documents - NPS-NCRN-Biological Stream Sampling/General/Annual-Data-Packages/2022/NCRN_MBSS/NCRN_MBSS_be_2022.mdb"'
            # indiv <- unique(setDT(indiv), by=c("FishObsID")) 
            
            
            # error-checking:
            check_df <- tibble::tibble(data.frame(matrix(ncol=3, nrow=ncol(indiv))))
            colnames(check_df) <- c("indiv", "example", "result")
            check_df$indiv <- colnames(indiv)
            check_df$example <- colnames(example)
            for(i in 1:nrow(check_df)){
                if(check_df$indiv[i] == check_df$example[i]){
                    check_df$result[i] <- "MATCH"
                } else {
                    check_df$result[i] <- "MISMATCH"
                }
            }
            
            #---------------------------------------------------
            #----- Add Marc's 2021 & 2022 data to indiv format--
            #---------------------------------------------------
            if(addMarc == TRUE){
                indiv2021 <- buildMarc2021Indiv(connection)
                indiv2022 <- buildMarc2022Indiv(connection)
                indiv <- rbind(indiv, indiv2021, indiv2022)
            }
            
            message(
                if(length(check_df$result == "MATCH") == nrow(check_df)){
                    "`buildMarcindiv()` executed successfully..."
                } else {
                    for(i in 1:length(check_df$result != "MATCH")){
                        cat(paste(paste0("`indiv_marc", check_df$indiv_marc[i], "`"), paste0(" DID NOT MATCH `example.", check_df$example[i][i], "`"), "\n", sep = ""))
                    }
                }
            )
            # assign("indiv_marc", indiv_marc, envir = globalenv())
            
            return(indiv)
        }
    )
}