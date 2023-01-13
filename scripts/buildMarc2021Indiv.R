#--------------------------------------------------------------------------
#----- Make Marc's 2021 data the required `example` format-----------------
#----- One row is one individual fish--------------------------------------
#--------------------------------------------------------------------------
# a module for `scripts/buildMarcView.R`


buildMarc2021Indiv <- function(connection){
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
            marc2022 <- readxl::read_excel("data/NCRN_BSS_Fish_Monitoring_Data_2022_Marc.xlsx", sheet = "ElectrofishingData")
            marc2021 <- readxl::read_excel("data/NCRN_BSS_Fish_Monitoring_Data_2021_Marc.xlsx", sheet = "Fish Data (Individuals)")

            data.table::setnames(marc2022, "Species_ID...12", "common_name")
            data.table::setnames(marc2022, "Species_ID...13", "species_id")
            
            # Query db
            db_objs <- RODBC::sqlTables(con) # test db connection
            tbl_names <- db_objs %>% # choose which tables you want to query
                subset(TABLE_NAME %in% c(
                    "tlu_Fish"
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
            
            
            
            #--------------------------------------------------------------------------
            #----- Make Marc's 2022 data match the required `example` format-----------
            #--------------------------------------------------------------------------
            
            # make a flat dataframe where one row is one e-fishing pass from `results_list`
            # this will have data that matches Marc's 2022 format: readxl::read_excel("data/NCRN_BSS_Fish_Monitoring_Data_2022_Marc.xlsx", sheet = "ElectrofishingData")
            
            df <- marc2021
            df$count <- 1
            for(i in 1:nrow(df)){
                if(stringr::str_detect(df$Species_ID[i], "\\(")==TRUE){
                    df$Species_ID[i] <- tolower(stringr::str_extract(df$Species_ID[i], "(?<=\\().+?(?=\\))")) # https://stackoverflow.com/questions/8613237/extract-info-inside-all-parenthesis-in-r
                }
            }
            # tlu_Fish <- results_list$tlu_Fish
            # tlu_Fish$Common_Name <- trimws(tolower(tlu_Fish$Common_Name))
            # df$Species_ID <- trimws(tolower(df$Species_ID))
            # df <- dplyr::left_join(df, results_list$tlu_Fish %>% select(Latin_Name, Common_Name), by = c("Species_ID" = "Common_Name"))
            
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
            indiv <- tibble::tibble(data.frame(matrix(ncol = ncol(example), nrow = nrow(marc2021)))) # empty dataframe
            colnames(indiv) <- colnames(example) # name columns to match example
            
            indiv[1] <- df$FishObsID # "FishObsID"
            indiv[2] <- as.character(df$Year) # "Year"
            indiv[3] <- as.character(format(df$SampleDate, "%Y-%m-%d")) # "SampleDate"
            indiv[4] <- df$SampleTime # "SampleTime"
            indiv[5] <- df$Station_ID# "Station_ID"
            indiv[6] <- df$Station_Name # "Station_Name"
            indiv[7] <- df$Pass_ID # "Pass_ID"
            indiv[8] <- as.character(format(df$Entry_Date, "%Y-%m-%d")) # "Entry_Date"
            indiv[9] <- as.character(format(df$Entry_Time, "%H:%M")) # "Entry_Time"
            indiv[10] <- df$Species_ID# "Subject_Taxon"
            # "Species_ID"
            indiv <- dplyr::left_join(indiv, tlu_species, by=c("Subject_Taxon" = "common_name"))
            indiv[11] <- indiv$species_id # "Species_ID"
            indiv$species_id <- NULL # delete the joined column
            indiv[12] <- df$count # "Count"
            indiv[13] <- df$Status # "Status"
            indiv[14] <- df$Disposition # "Disposition"
            indiv[15] <- df$Picture # "Picture"  
            indiv[16] <- df$Camera # "Camera" 
            indiv[17] <- df$Photo # "Photo"
            indiv[18] <- df$TL_mm # "TL_mm"
            indiv[19] <- df$Wt_g # "Wt_g"
            indiv[20] <- NA # "Calc_wt_TL_g"
            indiv[21] <- NA # "Wt_AddedOn"
            indiv[22] <- df$Note # "Note"
            indiv[23] <- df$Basin # "Basin"
            indiv[24] <- df$Branch # "Branch" 
            indiv[25] <- df$Reach_Name # "Reach_Name" 
            indiv[26] <- df$Delt_deformities # "Delt_deformities" 
            indiv[27] <- df$Delt_erodedfins #"Delt_erodedfins" 
            indiv[28] <- df$Delt_lesions# "Delt_lesions" 
            indiv[29] <- df$Delt_tumors
            indiv[30] <- df$Delt_other # "Delt_other" 
            indiv[31] <- '"data/NCRN_BSS_Fish_Monitoring_Data_2021_Marc.xlsx", sheet = "Fish Data (Individuals)"'
                
                
                
                
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
            
            message(
                if(length(check_df$result == "MATCH") == nrow(check_df)){
                    "`buildMarc2021Indiv()` executed successfully..."
                } else {
                    for(i in 1:length(check_df$result != "MATCH")){
                        cat(paste(paste0("`indiv_2021", check_df$indiv[i], "`"), paste0(" DID NOT MATCH `example.", check_df$example[i][i], "`"), "\n", sep = ""))
                    }
                }
            )
            # assign("pass2022", pass2022, envir = globalenv())
            
            return(indiv)
        }
    )
}