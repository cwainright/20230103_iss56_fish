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
            example <- readxl::read_excel("data/NCRN_BSS_Fish_Monitoring_Data_2022_Marc.xlsx", sheet = "data_model_pass") # https://doimspp.sharepoint.com/:x:/r/sites/NCRNBiologicalStreamSampling/Shared%20Documents/General/Annual-Data-Packages/2022/Marc_and_Bob/Fish_Template/NCRN_BSS_Fish_Monitoring_Data_2022_Marc.xlsx?d=w306357b1a43a48f4b9f598169043cc6a&csf=1&web=1&e=BC5i5n
            df <- readxl::read_excel("data/NCRN_BSS_Fish_Monitoring_Data_2022_Marc.xlsx", sheet = "ElectrofishingData")
            
            
            data.table::setnames(df, "Species_ID...12", "common_name")
            data.table::setnames(df, "Species_ID...13", "species_id")
            
            #--------------------------------------------------------------------------
            #----- Make Marc's 2022 data match the required `example` format-----------
            #--------------------------------------------------------------------------
            
            # make a flat dataframe where one row is one e-fishing pass from `df`
            
            # format from wide-format data: $Total_Pass_1 and $Total_Pass_2 to long-format $value and $pass
            counts <- df %>% 
                group_by(Pass_ID, species_id) %>%
                summarize(count=n()) %>%
                mutate(dummy = paste0(Pass_ID, ".", species_id)) %>%
                ungroup()
            
            df <- unique(setDT(df), by=c("Pass_ID", "common_name"))
            
                
            
            df$dummy <- paste0(df$Pass_ID, ".", df$species_id)
            df <- dplyr::left_join(df, counts %>% select(dummy, count), by=c("dummy" = "dummy"))
            for(i in 1:nrow(df)){
                if(stringr::str_detect(df$common_name[i], "\\(")==TRUE){
                    df$common_name[i] <- stringr::str_extract(df$common_name[i], "(?<=\\().+?(?=\\))") # https://stackoverflow.com/questions/8613237/extract-info-inside-all-parenthesis-in-r
                }
            }
            
            #----- re-build `example` from `results_list`
            
            # starting point: copy the example dataframe but without data
            pass2022 <- tibble::tibble(data.frame(matrix(ncol = ncol(example), nrow = nrow(df)))) # empty dataframe
            colnames(pass2022) <- colnames(example) # name columns to match example
            
            pass2022[1] <- paste0(df$Pass_ID, ".", df$species_id) # "FishObsID" is now a unique combination of species and pass
            pass2022[2] <- df$Year # "Year"
            pass2022[3] <- as.character(df$SampleDate) # "SampleDate"
            pass2022[4] <- NA # "SampleTime"
            pass2022[5] <- df$Station_ID# "Station_ID"
            pass2022[6] <- NA # "Station_Name"
            pass2022[7] <- df$Pass_ID # "Pass_ID"
            pass2022[8] <- as.character(df$Entry_Date) # "Entry_Date"
            pass2022[9] <- as.character(df$Entry_Time) # "Entry_Time"
            pass2022[10] <- tolower(df$common_name) # "Subject_Taxon"
            pass2022[11] <- df$species_id # "Species_ID"
            pass2022[12] <- df$count # "Count"
            pass2022[13] <- df$Status # "Status"
            pass2022[14] <- NA # "Disposition"
            pass2022[15] <- df$Picture # "Picture"  
            pass2022[16] <- NA # "Camera" 
            # "Photo"
            for(i in 1:nrow(pass2022)){
                if(is.na(pass2022$Picture[i])){
                    pass2022[i,17] <- FALSE
                } else {
                    pass2022[i,17] <- TRUE
                }
            }
            pass2022[18] <- NA # "TL_mm" NA because `pass` aggregates to e-fishing pass (refer to `indiv` for $TL_mm data)
            pass2022[19] <- NA # "Wt_g" NA because `pass` aggregates to e-fishing pass (refer to `indiv` for $Wt_g data)
            pass2022[20] <- NA # "Calc_wt_TL_g" NA because `pass` aggregates to e-fishing pass (refer to `indiv` for $Calc_wt_TL_g data)
            pass2022[21] <- NA # "Wt_AddedOn" NA because `pass` aggregates to e-fishing pass (refer to `indiv` for $Wt_AddedOn data)
            pass2022[22] <- df$Note # "Note"
            pass2022[23] <- df$Basin # "Basin"
            pass2022[24] <- df$Branch # "Branch" 
            pass2022[25] <- df$Reach_Name # "Reach_Name" 
            pass2022[26] <- as.character(df$Delt_deformities) # "Delt_deformities" 
            for(i in 1:nrow(pass2022)){
                if(pass2022[i,26] == "FALSE"){
                    pass2022[i,26] <- "No DELT"
                } else {
                    pass2022[i,26] <- "DELT reported for this pass"
                }
            }
            pass2022[27] <- NA #"Delt_erodedfins" 
            pass2022[28] <- NA # "Delt_lesions" 
            pass2022[29] <- NA # "Delt_tumors" 
            # "Delt_other"
            # build a concat column in df to hold the value we need to route to $Delt_other
            for(i in 1:nrow(df)){
                if(df$Delt_erodedfins[i]==TRUE & df$Delt_lesions[i]==TRUE & df$Delt_tumors[i]==TRUE){
                    df$deltconcat[i] <- "Delt_erodedfins, Delt_lesions, Delt_tumors"
                } else if(df$Delt_erodedfins[i]==TRUE & df$Delt_lesions[i]==FALSE & df$Delt_tumors[i]==FALSE){
                    df$deltconcat[i] <- "Delt_erodedfins"
                } else if(df$Delt_erodedfins[i]==FALSE & df$Delt_lesions[i]==TRUE & df$Delt_tumors[i]==FALSE){
                    df$deltconcat[i] <- "Delt_lesions"
                } else if(df$Delt_erodedfins[i]==FALSE & df$Delt_lesions[i]==FALSE & df$Delt_tumors[i]==TRUE){
                    df$deltconcat[i] <- "Delt_tumors"
                } else if(df$Delt_erodedfins[i]==FALSE & df$Delt_lesions[i]==FALSE & df$Delt_tumors[i]==FALSE){
                    df$deltconcat[i] <- NA
                } else if(df$Delt_erodedfins[i]==TRUE & df$Delt_lesions[i]==TRUE & df$Delt_tumors[i]==FALSE){
                    df$deltconcat[i] <- "Delt_erodedfins, Delt_lesions"
                } else if(df$Delt_erodedfins[i]==TRUE & df$Delt_lesions[i]==FALSE & df$Delt_tumors[i]==TRUE){
                    df$deltconcat[i] <- "Delt_erodedfins, Delt_tumors"
                } else if(df$Delt_erodedfins[i]==FALSE & df$Delt_lesions[i]==TRUE & df$Delt_tumors[i]==TRUE){
                    df$deltconcat[i] <- "Delt_lesions, Delt_tumors"
                } 
            }
            for(i in 1:nrow(pass2022)){
                if(pass2022[i,26] == "No DELT"){
                    pass2022[i,30] <- NA
                } else {
                    pass2022[i,30] <- df$deltconcat[i]
                }
            }
            pass2022[31] <- '"data/NCRN_BSS_Fish_Monitoring_Data_2022_Marc.xlsx", sheet = "ElectrofishingData"' # Source
                
                
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