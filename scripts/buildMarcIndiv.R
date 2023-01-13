#--------------------------------------------------------------------------
#----- Make our access db data match the required `example` format---------
#----- One row is one individual fish--------------------------------------
#--------------------------------------------------------------------------
# a module for `scripts/buildMarcView.R`

buildMarcIndiv <- function(connection, add2022){
    tryCatch(
        expr = {
            #----- load external libraries
            suppressWarnings(suppressMessages(library(data.table)))
            suppressWarnings(suppressMessages(library(tidyverse)))
            suppressWarnings(suppressMessages(library(dplyr)))
            
            #----- load project functions
            source("scripts/getQueryResults.R") # equivalent to python "from x import function"
            source("scripts/buildMarc2022Indiv.R") # equivalent to python "from x import function"
            
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
            
            
                
            #--------------------------------------------------------------------------
            #----- Make our access db data match the required `example` format---------
            #----- One row is one individual fish--------------------------------------
            #--------------------------------------------------------------------------
            
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
            indiv <- tibble::tibble(data.frame(matrix(ncol = ncol(example), nrow = nrow(df2)))) # empty dataframe
            colnames(indiv) <- colnames(example) # name columns to match example
            
            indiv[1] <- # "FishObsID"
                indiv[2] <- # "Year"
                indiv[3] <- # "SampleDate"
                indiv[4] <- # "SampleTime"
                indiv[5] <- # "Station_ID"
                indiv[6] <- # "Pass_ID"
                indiv[7] <- # "Entry_Date"
                indiv[8] <- # "Entry_Time"
                indiv[9] <- # "Subject_Taxon"
                indiv[10] <- # "Species_ID"
                indiv[11] <- # "Status"
                indiv[12] <- # "Disposition"
                indiv[13] <- # "Picture"
                indiv[14] <- # "Camera"  
                indiv[15] <- # "Photo" 
                indiv[16] <- # "TL_mm"
                indiv[17] <- # "Wt_g"
                indiv[18] <- # "Calc_wt_TL_g"
                indiv[19] <- # "Wt_AddedOn"
                indiv[20] <- # "Note"
                indiv[21] <- # "Basin"
                indiv[22] <- # "Branch"
                indiv[23] <- # "Reach_Name" 
                indiv[24] <- # "Station_Name" 
                indiv[25] <-  # "Delt_deformities" 
                indiv[26] <-  #"Delt_erodedfins" 
                indiv[27] <-  # "Delt_lesions" 
                indiv[28] <-  # "Delt_tumors" 
                indiv[29] <-  # "Delt_other" 
                

                
            #--------------------------------------------
            #----- Add Marc's 2022 data to indiv format--
            #--------------------------------------------
            
            if(add2022 == TRUE){
                indiv2022 <- buildMarc2022Indiv()
                indiv <- rbind(indiv, indiv2022)
            }
            
            
            
            
            
            
            
            
            indiv <- as.data.frame(lapply(indiv, function(y) gsub("NA", NA, y))) # remove "NA" chr strings  

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
                    "`buildMarcIndiv()` executed successfully..."
                } else {
                    for(i in 1:length(check_df$result != "MATCH")){
                        cat(paste(paste0("`indiv.", check_df$indiv[i], "`"), paste0(" DID NOT MATCH `example.", check_df$example[i][i], "`"), "\n", sep = ""))
                    }
                }
            )

            return(indiv)
        }
    )
}