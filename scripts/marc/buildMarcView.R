#--------------------------------------------------------------------------
#----- Synthesize fish data into the format Marc wants-----------------
#----- One row is one individual fish--------------------------------------
#--------------------------------------------------------------------------
# database is from: https://doimspp.sharepoint.com/:u:/r/sites/NCRNBiologicalStreamSampling/Shared%20Documents/General/Annual-Data-Packages/2022/NCRN_MBSS/NCRN_MBSS_be_2022.mdb?csf=1&web=1&e=jjeJIg

buildMarc <- function(connection, write, addMarc){
    tryCatch(
        expr = {
            #----- load external libraries
            suppressWarnings(suppressMessages(library(openxlsx)))
            suppressWarnings(suppressMessages(library(data.table)))
            suppressWarnings(suppressMessages(library(tidyverse)))
            suppressWarnings(suppressMessages(library(dplyr)))
            
            #----- load project functions
            source("scripts/marc/buildMarcPass.R")
            source("scripts/marc/buildMarcIndiv.R")
            source("scripts/marc/buildLookup.R")
            source("scripts/getQueryResults.R") # equivalent to python "from x import function"

            #----- pre-process and cache assets for functions
            example <- readxl::read_excel("data/NCRN_BSS_Fish_Monitoring_Data_2022_Marc.xlsx", sheet = "data_model") # https://doimspp.sharepoint.com/:x:/r/sites/NCRNBiologicalStreamSampling/Shared%20Documents/General/Annual-Data-Packages/2022/Marc_and_Bob/Fish_Template/NCRN_BSS_Fish_Monitoring_Data_2022_Marc.xlsx?d=w306357b1a43a48f4b9f598169043cc6a&csf=1&web=1&e=BC5i5n
            marc2021 <- readxl::read_excel("data/NCRN_BSS_Fish_Monitoring_Data_2021_Marc.xlsx", sheet = "Fish Data (Individuals)")
            marc2022 <- readxl::read_excel("data/NCRN_BSS_Fish_Monitoring_Data_2022_Marc.xlsx", sheet = "ElectrofishingData")
            data.table::setnames(marc2022, "Species_ID...12", "common_name")
            data.table::setnames(marc2022, "Species_ID...13", "species_id")
            for(i in 1:nrow(marc2021)){
                if(stringr::str_detect(marc2021$Species_ID[i], "\\(")==TRUE){
                    marc2021$Species_ID[i] <- tolower(stringr::str_extract(marc2021$Species_ID[i], "(?<=\\().+?(?=\\))")) # https://stackoverflow.com/questions/8613237/extract-info-inside-all-parenthesis-in-r
                }
            }
            marc2021$Species_ID <- tolower(marc2021$Species_ID)
            for(i in 1:nrow(marc2022)){
                if(stringr::str_detect(marc2022$common_name[i], "\\(")==TRUE){
                    marc2022$common_name[i] <- tolower(stringr::str_extract(marc2022$common_name[i], "(?<=\\().+?(?=\\))")) # https://stackoverflow.com/questions/8613237/extract-info-inside-all-parenthesis-in-r
                }
            }
            marc2022$common_name <- tolower(marc2022$common_name)
            
            
            #----- static assets for query
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
            
            # make list of queries
            qry_list <- vector(mode="list", length=nrow(tbl_names))
            names(qry_list) <- tbl_names$TABLE_NAME
            for (i in 1:length(qry_list)){
                qry_list[[i]] <- paste("SELECT * FROM", names(qry_list)[i])
            }
            
            results_list <- getQueryResults(qry_list = qry_list, connection = con) # execute query
            RODBC::odbcCloseAll() # close db connection
            tlu_species <- buildLookup(marc2022 = marc2022, results_list = results_list)
            
            #----- route assets to build views
            pass <- buildMarcPass(addMarc = addMarc, example = example, marc2021 = marc2021, marc2022 = marc2022, tlu_species = tlu_species, results_list = results_list)
            indiv <- buildMarcIndiv(addMarc = addMarc, example = example, marc2021 = marc2021, marc2022 = marc2022, tlu_species = tlu_species, results_list = results_list)
            
            #----- receive views and build final product
            list_of_datasets <- list("by_pass" = pass, "individuals" = indiv) # list object that can be saved as xlsx
            if(length(list_of_datasets)==2){
                if(nrow(list_of_datasets[[1]]>0) & nrow(list_of_datasets[[2]]>0)){
                    message("\n\n`buildMarc()` successfully produced data views.\nOutput saved as `marc_views` in global environment.\n\n")
                    assign("marc_views", list_of_datasets, envir = globalenv()) # save final product to global environment
                }
            } else {
                message("An error occurred when compiling results.")
                break
            }
            
            if(write == TRUE){
                openxlsx::write.xlsx(list_of_datasets, file = file.choose())
            }
            
        }
    )
}