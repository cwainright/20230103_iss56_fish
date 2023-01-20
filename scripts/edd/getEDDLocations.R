# a module for `buildEDD()`
options(warn=-1)
getEDDLocations <- function(results_list, marc2022, marc2021, habitat_marc2021, habitat_marc2022, addMarc){
    tryCatch(
        expr = {
            #----- load external libraries
            suppressWarnings(suppressMessages(library(data.table)))
            suppressWarnings(suppressMessages(library(tidyverse)))
            suppressWarnings(suppressMessages(library(dplyr)))
            suppressWarnings(suppressMessages(library(readxl)))
            
            #----- load project functions
            source("scripts/edd/getMarc2022Locations.R")
            source("scripts/edd/getMarcHabLocations.R")
            source("scripts/edd/getNCRNFishLocations.R")
            source("scripts/edd/getNCRNHabLocations.R")
            source("scripts/edd/getNCRNChemLocations.R")
            
            # load example data
            example <- readxl::read_excel("data/NCRN_BSS_EDD_20230105_1300.xlsx", sheet = "Locations") # https://doimspp.sharepoint.com/:x:/r/sites/NCRNDataManagement/Shared%20Documents/General/Standards/Data-Standards/EQuIS-WQX-EDD/NCRN_BSS_EDD_20230105_1300.xlsx?d=w8c283fde9cbd4af480945c8c8bd94ff6&csf=1&web=1&e=7Y9W1M
            
            #----- call NCRN project functions
            ncrn_fish_locations <- getNCRNFishLocations(results_list, example)
            ncrn_chem_locations <- getNCRNChemLocations(results_list, example)
            ncrn_hab_locations <- getNCRNHabLocations(results_list, example)
            real <- rbind(ncrn_fish_locations, ncrn_chem_locations, ncrn_hab_locations) # row bind project function returns
            
            #----- if TRUE add Marc's 2022 data to dataframe `real` NCRN db data
            if(addMarc==TRUE){
                marc2022_locations <- getMarc2022Locations(marc2022, example, results_list)
                marc_habitat_locations <- getMarcHabLocations(habitat_marc2022, habitat_marc2021, example, results_list) # NCRN doesn't have any of Marc's habitat data, so add both 2021 and 2022
                real <- rbind(real, marc2022_locations, marc_habitat_locations)
            }
            
            #----- keep only unique Locations
            real <- real %>% distinct(Location_ID, .keep_all = TRUE)
            
            #----- error-checking:
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
                    "`getEDDLocations()` executed successfully..."
                } else {
                    for(i in 1:length(check_df$result != "MATCH")){
                        cat(paste(paste0("`real.", check_df$real[i], "`"), paste0(" DID NOT MATCH `example.", check_df$example[i][i], "`"), "\n", sep = ""))
                    }
                }
            )
            return(real)
        }
    )
}