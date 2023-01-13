#--------------------------------------------------------------------------
#----- Make Marc's 2022 data the required `example` format-----------------
#----- One row is one e-fishing pass---------------------------------------
#--------------------------------------------------------------------------
# a module for `scripts/buildMarcView.R`

buildMarc2022Pass <- function(){
    tryCatch(
        expr = {
            #----- load external libraries
            suppressWarnings(suppressMessages(library(data.table)))
            suppressWarnings(suppressMessages(library(tidyverse)))
            suppressWarnings(suppressMessages(library(dplyr)))
            
            #----- load project functions
            # load example data
            example <- readxl::read_excel("data/NCRN_BSS_Fish_Monitoring_Data_2022_Marc.xlsx", sheet = "data_model") # https://doimspp.sharepoint.com/:x:/r/sites/NCRNBiologicalStreamSampling/Shared%20Documents/General/Annual-Data-Packages/2022/Marc_and_Bob/Fish_Template/NCRN_BSS_Fish_Monitoring_Data_2022_Marc.xlsx?d=w306357b1a43a48f4b9f598169043cc6a&csf=1&web=1&e=BC5i5n
            marc2022 <- readxl::read_excel("data/NCRN_BSS_Fish_Monitoring_Data_2022_Marc.xlsx", sheet = "ElectrofishingData")
            marc2022 <- rename(marc2022, common_name = `Species_ID...12`)
            marc2022 <- rename(marc2022, species_id = `Species_ID...13`)
            
            #--------------------------------------------------------------------------
            #----- Make Marc's 2022 data match the required `example` format-----------
            #--------------------------------------------------------------------------
            
            # make a flat dataframe where one row is one e-fishing pass from `results_list`
            # this will have data that matches Marc's 2022 format: readxl::read_excel("data/NCRN_BSS_Fish_Monitoring_Data_2022_Marc.xlsx", sheet = "ElectrofishingData")
            
            
            
            #----- re-build `example` from `results_list`
            
            # starting point: copy the example dataframe but without data
            pass2022 <- tibble::tibble(data.frame(matrix(ncol = ncol(example), nrow = nrow(df)))) # empty dataframe
            colnames(pass2022) <- colnames(example) # name columns to match example
            
            pass2022[1] <- df$id # "FishObsID"
            pass2022[2] <- format(df$Start_Date, "%Y")# "Year"
            pass2022[3] <- format(df$Start_Date, "%Y-%m-%d") # "SampleDate"
            pass2022[4] <- format(df$Start_Time, "%H:%M") # "SampleTime"
            pass2022[5] <- df$NCRN_Site_ID# "Station_ID"
            pass2022[6] <- paste0(format(df$Start_Date, "%Y-%m-%d"), "_", format(df$Start_Time, "%H:%M"), "_", df$pass2022) # "pass2022_ID"
            pass2022[7] <- format(df$Entered_Date, "%Y-%m-%d") # "Entry_Date"
            pass2022[8] <- format(df$Entered_Date, "%H:%M") # "Entry_Time"
            pass2022[9] <- trimws(tolower(df$Common_Name)) # "Subject_Taxon"
            # "Species_ID"
            pass2022 <- dplyr::left_join(pass2022, tlu_species, by=c("Subject_Taxon" = "common_name"))
            pass2022[10] <- pass2022$species_id # "Species_ID"
            pass2022$species_id <- NULL # delete the joined column
            
            pass2022[11] <-  # "Status"
                pass2022[12] <- df$retained # "Disposition"
            for(i in 1:nrow(pass2022)){
                if(pass2022$Disposition[i] >0){
                    pass2022$Disposition[i] <- paste0("Retained ", pass2022$Disposition[i], " individuals")
                } else {
                    pass2022$Disposition[i] <- "Released"
                }
            }
            pass2022[13] <-  # "Picture"
                pass2022[14] <-  # "Camera"  
                pass2022[15] <-  # "Photo" 
                pass2022[16] <-  # "TL_mm"
                pass2022[17] <-  # "Wt_g"
                pass2022[18] <-  # "Calc_wt_TL_g"
                pass2022[19] <-  # "Wt_AddedOn"
                pass2022[20] <- df$Comments # "Note"
            pass2022[21] <- df$Basin# "Basin"
            pass2022[22] <- df$Loc_Name # "Branch"
            pass2022[23] <- df$NCRN_Site_ID # "Reach_Name" 
            pass2022[24] <-  # "Station_Name" 
                pass2022[25] <-  # "Delt_deformities" 
                pass2022[26] <-  #"Delt_erodedfins" 
                pass2022[27] <-  # "Delt_lesions" 
                pass2022[28] <-  # "Delt_tumors" 
                pass2022[29] <-  # "Delt_other" 
                
                
                
                
            # error-checking:
            check_df <- tibble::tibble(data.frame(matrix(ncol=3, nrow=ncol(pass2022))))
            colnames(check_df) <- c("pass2022", "example", "result")
            check_df$pass2022 <- colnames(pass2022)
            check_df$example <- colnames(example)
            for(i in 1:nrow(check_df)){
                if(check_df$pass2022[i] == check_df$example[i]){
                    check_df$result[i] <- "MATCH"
                } else {
                    check_df$result[i] <- "MISMATCH"
                }
            }
            
            message(
                if(length(check_df$result == "MATCH") == nrow(check_df)){
                    "`buildMarc2022Pass()` executed successfully..."
                } else {
                    for(i in 1:length(check_df$result != "MATCH")){
                        cat(paste(paste0("`pass2022", check_df$pass2022[i], "`"), paste0(" DID NOT MATCH `example.", check_df$example[i][i], "`"), "\n", sep = ""))
                    }
                }
            )
            # assign("pass2022", pass2022, envir = globalenv())
            
            return(pass2022)
        }
    )
}