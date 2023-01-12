# build example
# a module for `scripts/fish_data_view.R`

buildBioMonTools <- function(connection, write){
    tryCatch(
        expr = {
            #----- load external libraries
            suppressWarnings(suppressMessages(library(data.table)))
            suppressWarnings(suppressMessages(library(tidyverse)))
            suppressWarnings(suppressMessages(library(dplyr)))
            
            #----- load project functions
            source("scripts/getQueryResults.R") # equivalent to python "from x import function"
            # load example data
            example <- data.table::fread("data/data_fish_MBSS.csv") # https://doimspp.sharepoint.com/:x:/r/sites/NCRNDataManagement/Shared%20Documents/General/Standards/Data-Standards/EQuIS-WQX-EDD/NCRN_BSS_EDD_20230105_1300.xlsx?d=w8c283fde9cbd4af480945c8c8bd94ff6&csf=1&web=1&e=7Y9W1M

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
                    "tbl_Electro_Fish_Details"
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
            df <- dplyr::left_join(df, results_list$tbl_Events %>% select(Event_ID, Location_ID, Start_Date), by = "Event_ID")
            df <- dplyr::left_join(df, results_list$tbl_Locations %>% select(Location_ID, Loc_Name), by = "Location_ID")
    
            # RODBC::odbcCloseAll() # close db connection
            
            #----- re-build `example` from `results_list`
            
            # starting point: copy the example dataframe but without data
            real <- tibble::tibble(data.frame(matrix(ncol = ncol(example), nrow = nrow(df)))) # empty dataframe
            colnames(real) <- colnames(example) # name columns to match example
            
            real[1] <- df$Fish_Event_ID # "SAMPLEID"
            real[2] <- df$Common_Name # "TAXAID"
            for(i in 1:nrow(real)){ # NA is not an acceptable value for `Common_Name`
                if(is.na(real$TAXAID[i])){
                    real$TAXAID[i] <- toupper(real$TAXAID[i])
                }
            }
            real[3] <- df$total_n # "N_TAXA"
            real[4] <- NA # "TYPE"
            real[5] <- df$Tolerance # "TOLER"
            real[6] <- df$Nativity # "NATIVE"
            real[7] <- df$`Trophic Status` # "TROPHIC"
            real[8] <- NA # "SILT"
            real[9] <-  paste0("Freshwater creek: ", df$Loc_Name)# "INDEX_CLASS"
            real[10] <- df$Seg_Length# "SAMP_LENGTH_M"
            real[11] <- NA # "SAMP_WIDTH_M"
            # "SAMP_BIOMASS" 
            for(i in 1:nrow(df)){
                real[i,12] <- df$Fish_Biomass_1[i] + df$Fish_Biomass_2[i]
            }
            real[13] <- paste0("MBSS_", format(df$Start_Date, "%Y"), "_Fish") # "INDEX_NAME"
            real[14] <- NA # "EXCLUDE"  
            real[15] <- NA # "BCG_ATTR" 
            real[16] <- NA # "DA_MI2"
            real[17] <- df$Anom # "N_ANOMALIES"
            real[18] <- NA # "FAMILY"
            # "GENUS"
            real[19] <- df$Fish_Species
            for(i in 1:nrow(real)){
                real[i,19] <- stringr::str_extract(real[i,19], "^*([A-Z])+([a-z])+")
            }
            real[20] <- NA # "THERMAL_INDICATOR"
            real[21] <- NA # "ELEVATION_ATTR"
            real[22] <- NA # "GRADIENT_ATTR"
            real[23] <- NA # "WSAREA_ATTR" 
            real[24] <- NA # "REPRODUCTION" 
            real[25] <- NA # "HABITAT" 
            real[26] <- NA # "CONNECTIVITY" 
            real[27] <- NA # "SCC" 
            real[28] <- NA # "HYBRID" 
            
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
            assign("real_fish", real, envir = globalenv())
            if(write == TRUE){
                data.table::fwrite(real, file.choose())
            }
            return(real)
        }
    )
}