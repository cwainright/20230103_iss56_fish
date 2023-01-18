#--------------------------------------------------------------------------
#----- Make Marc's 2022 data the required `example` format-----------------
#----- One row is one individual fish--------------------------------------
#--------------------------------------------------------------------------
# a module for `scripts/buildMarcView.R`


buildMarc2022Indiv <- function(example, marc2022, tlu_species){
    tryCatch(
        expr = {
            # make a flat dataframe where one row is one e-fishing pass from `results_list`
            df <- marc2022
            df$count <- 1

            #----- re-build `example` from `results_list`
            indiv <- tibble::tibble(data.frame(matrix(ncol = ncol(example), nrow = nrow(marc2022)))) # empty dataframe
            colnames(indiv) <- colnames(example) # name columns to match example
            
            indiv[1] <- df$FishObsID # "FishObsID"
            indiv[2] <- as.character(df$Year) # "Year"
            indiv[3] <- as.character(format(df$SampleDate, "%Y-%m-%d")) # "SampleDate"
            indiv[4] <- NA # "SampleTime"
            indiv[5] <- df$Station_ID# "Station_ID"
            indiv[6] <- df$Station_Name # "Station_Name"
            indiv[7] <- df$Pass_ID # "Pass_ID"
            indiv[8] <- as.character(format(df$Entry_Date, "%Y-%m-%d")) # "Entry_Date"
            indiv[9] <- as.character(format(df$Entry_Time, "%H:%M")) # "Entry_Time"
            indiv[10] <- df$common_name# "Subject_Taxon"
            # "Species_ID" and "Scientific_Name"
            indiv <- dplyr::left_join(indiv, tlu_species, by=c("Subject_Taxon" = "Common_Name"))
            indiv[11] <- indiv$Latin_Name # "Scientific_Name"
            indiv[12] <- indiv$species_id # "Species_ID"
            indiv$species_id <- NULL # delete the joined column
            indiv$Latin_Name <- NULL # delete the joined column
            indiv[13] <- df$count # "Count"
            indiv[14] <- df$Status # "Status"
            indiv[15] <- NA # "Disposition"
            indiv[16] <- df$Picture # "Picture"  
            indiv[17] <- NA # "Camera" 
            indiv[18] <- NA # "Photo"
            indiv[19] <- df$TL_mm # "TL_mm"
            indiv[20] <- df$Wt_g # "Wt_g"
            indiv[21] <- df$Calc_wt_TL_g # "Calc_wt_TL_g"
            indiv[22] <- df$Wt_AddedOn # "Wt_AddedOn"
            indiv[23] <- df$Note # "Note"
            indiv[24] <- df$Basin # "Basin"
            indiv[25] <- df$Branch # "Branch" 
            indiv[26] <- df$Reach_Name # "Reach_Name" 
            indiv[27] <- df$Delt_deformities # "Delt_deformities" 
            indiv[28] <- df$Delt_erodedfins #"Delt_erodedfins" 
            indiv[29] <- df$Delt_lesions# "Delt_lesions" 
            indiv[30] <- df$Delt_tumors
            indiv[31] <- df$Delt_other # "Delt_other" 
            indiv[32] <- '"data/NCRN_BSS_Fish_Monitoring_Data_2022_Marc.xlsx", sheet = "ElectrofishingData"'

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
                    "`buildMarc2022Indiv()` executed successfully..."
                } else {
                    for(i in 1:length(check_df$result != "MATCH")){
                        cat(paste(paste0("`indiv_2022", check_df$indiv[i], "`"), paste0(" DID NOT MATCH `example.", check_df$example[i][i], "`"), "\n", sep = ""))
                    }
                }
            )
            # assign("pass2022", pass2022, envir = globalenv())
            
            return(indiv)
        }
    )
}