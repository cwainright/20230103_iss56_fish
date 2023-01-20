# a module for `buildEDD()`
options(warn=-1)
getEDDActivities <- function(results_list, marc2022, marc2021, habitat_marc2021, habitat_marc2022, addMarc){
    tryCatch(
        expr = {
            #----- load external libraries
            suppressWarnings(suppressMessages(library(data.table)))
            suppressWarnings(suppressMessages(library(tidyverse)))
            suppressWarnings(suppressMessages(library(dplyr)))
            suppressWarnings(suppressMessages(library(readxl)))
            
            #----- load project functions
            source("scripts/edd/getMarc2022Activities.R")
            source("scripts/edd/getMarcHabActivities.R")
            source("scripts/edd/getNCRNFishActivities.R")
            source("scripts/edd/getNCRNChemActivities.R")
            source("scripts/edd/getNCRNHabActivities.R")
            
            # load example data
            example <- readxl::read_excel("data/NCRN_BSS_EDD_20230105_1300.xlsx", sheet = "Activities") # https://doimspp.sharepoint.com/:x:/r/sites/NCRNDataManagement/Shared%20Documents/General/Standards/Data-Standards/EQuIS-WQX-EDD/NCRN_BSS_EDD_20230105_1300.xlsx?d=w8c283fde9cbd4af480945c8c8bd94ff6&csf=1&web=1&e=7Y9W1M
            
            #----- call NCRN project functions
            ncrn_fish_activities <- getNCRNFishActivities(results_list, example)
            ncrn_chem_activities <- getNCRNChemActivities(results_list, example)
            ncrn_hab_activities <- getNCRNHabActivities(results_list, example)
            real <- rbind(ncrn_fish_activities, ncrn_chem_activities, ncrn_hab_activities) # row bind project function returns
            
            #----- if TRUE, call Marc project functions
            if(addMarc==TRUE){
                marc2022_activities <- getMarc2022Activities(marc2022, example) # NCRN already has Marc's 2021 activities in db, so only add 2022
                marc_habitat_activities <- getMarcHabActivities(habitat_marc2022, habitat_marc2021, example, results_list) # NCRN doesn't have any of Marc's habitat data, so add both 2021 and 2022
                real <- rbind(real, marc2022_activities, marc_habitat_activities) # row bind project function returns
            }
            
            #----- keep only unique Activities
            real <- real %>% distinct(Activity_ID, .keep_all = TRUE)
            
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
                    "`getEDDActivities()` executed successfully..."
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