#--------------------------------------------------------------------------
#----- Make Marc's 2022 data the required `example` format-----------------
#----- One row is one e-fishing pass---------------------------------------
#--------------------------------------------------------------------------
# a module for `scripts/buildMarcView.R`

buildMarc2022Pass <- function(marc2022, example, tlu_species, results_list){
    tryCatch(
        expr = {
            # make a flat dataframe where one row is one e-fishing pass from `df`
            
            # format from wide-format data: $Total_Pass_1 and $Total_Pass_2 to long-format $value and $pass
            df <- marc2022
            counts <- df %>% 
                group_by(Pass_ID, species_id) %>%
                summarize(count=n()) %>%
                mutate(dummy = paste0(Pass_ID, ".", species_id)) %>%
                ungroup()
            
            df <- unique(setDT(df), by=c("Pass_ID", "common_name"))
            df$dummy <- paste0(df$Pass_ID, ".", df$species_id)
            df <- dplyr::left_join(df, counts %>% select(dummy, count), by=c("dummy" = "dummy"))
            
            #----- re-build `example` from `results_list`
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
            pass2022 <- dplyr::left_join(pass2022, tlu_species, by=c("Subject_Taxon" = "Common_Name"))
            pass2022[11] <- pass2022$Latin_Name # "Scientific_Name"
            pass2022$Latin_Name <- NULL # remove the joined columns
            pass2022$species_id <- NULL # remove the joined columns
            pass2022[12] <- df$species_id # "Species_ID"
            pass2022[13] <- df$count # "Count"
            pass2022[14] <- df$Status # "Status"
            pass2022[15] <- NA # "Disposition"
            pass2022[16] <- df$Picture # "Picture"  
            pass2022[17] <- NA # "Camera" 
            # "Photo"
            for(i in 1:nrow(pass2022)){
                if(is.na(pass2022$Picture[i])){
                    pass2022[i,18] <- FALSE
                } else {
                    pass2022[i,18] <- TRUE
                }
            }
            pass2022[19] <- NA # "TL_mm" NA because `pass` aggregates to e-fishing pass (refer to `indiv` for $TL_mm data)
            pass2022[20] <- NA # "Wt_g" NA because `pass` aggregates to e-fishing pass (refer to `indiv` for $Wt_g data)
            pass2022[21] <- NA # "Calc_wt_TL_g" NA because `pass` aggregates to e-fishing pass (refer to `indiv` for $Calc_wt_TL_g data)
            pass2022[22] <- NA # "Wt_AddedOn" NA because `pass` aggregates to e-fishing pass (refer to `indiv` for $Wt_AddedOn data)
            pass2022[23] <- df$Note # "Note"
            pass2022[24] <- df$Basin # "Basin"
            pass2022[25] <- df$Branch # "Branch" 
            pass2022[26] <- df$Reach_Name # "Reach_Name" 
            pass2022[27] <- as.character(df$Delt_deformities) # "Delt_deformities" 
            # for(i in 1:nrow(pass2022)){
            #     if(pass2022[i,27] == "FALSE"){
            #         pass2022[i,27] <- "No DELT"
            #     } else {
            #         pass2022[i,27] <- "DELT reported for this pass"
            #     }
            # }
            pass2022[28] <- NA #"Delt_erodedfins" 
            pass2022[29] <- NA # "Delt_lesions" 
            pass2022[30] <- NA # "Delt_tumors" 
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
                if(pass2022[i,27] == "FALSE"){
                    pass2022[i,31] <- NA
                } else {
                    pass2022[i,31] <- df$deltconcat[i]
                }
            }
            pass2022[32] <- '"data/NCRN_BSS_Fish_Monitoring_Data_2022_Marc.xlsx", sheet = "ElectrofishingData"' # Source
                
                
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