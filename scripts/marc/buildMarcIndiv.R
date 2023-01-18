#--------------------------------------------------------------------------
#----- Make our access db data match the required `example` format---------
#----- One row is one individual fish--------------------------------------
#--------------------------------------------------------------------------
# a module for `scripts/buildMarcView.R`

buildMarcIndiv <- function(addMarc, example, marc2021, marc2022, tlu_species, results_list){
    tryCatch(
        expr = {
            #----- load project functions
            source("scripts/marc/buildMarc2022Indiv.R") # equivalent to python "from x import function"
            source("scripts/marc/buildMarc2021Indiv.R") # equivalent to python "from x import function"

            #--------------------------------------------------------------------------
            #----- Make our access db data match the required `example` format---------
            #----- One row is one fish-------------------------------------------------
            #--------------------------------------------------------------------------
            
            # make a flat dataframe where one row is one individual fish from `results_list`
            
            df <- results_list$tbl_GameFish
            df$count <- 1
            df <- dplyr::left_join(df, results_list$tbl_Fish_Events %>% select(Fish_Event_ID, Event_ID, Seg_Length, Fish_Biomass_1, Fish_Biomass_2), by = "Fish_Event_ID")
            df <- dplyr::left_join(df, results_list$tbl_Events %>% select(Event_ID, Location_ID, Start_Date, Start_Time, Comments), by = "Event_ID")
            data.table::setnames(df, "Comments", "Comments_tblevents")
            df <- dplyr::left_join(df, results_list$tbl_Locations %>% select(Location_ID, Loc_Name, NCRN_Site_ID, Basin_Code, Site_ID, Unit_Code), by = "Location_ID")
            df <- dplyr::left_join(df, results_list$tlu_Basin_Code %>% select(Basin_Code, Basin), by = "Basin_Code")
            df <- dplyr::left_join(df, results_list$tbl_Meta_Events %>% select(Event_ID, Entered_Date), by = "Event_ID")
            df <- dplyr::left_join(df, results_list$tlu_Park_Code %>% select(PARKCODE, PARKNAME), by = c("Unit_Code" = "PARKCODE"))
            tbl_Photos <- aggregate(Photo_num~Event_ID, results_list$tbl_Photos %>% select(Event_ID, Photo_num), paste, collapse = ', ')
            df <- dplyr::left_join(df, tbl_Photos, by = "Event_ID")
            tlu_Fish <- results_list$tlu_Fish %>% select(Latin_Name, Common_Name, Nativity, Tolerance, `Trophic Status`)
            tlu_Fish$Common_Name <- trimws(tlu_Fish$Common_Name)
            df <- dplyr::left_join(df, tlu_Fish, by = c("SPECIES" = "Common_Name"))
            
            #----- re-build `example` from `results_list`
            
            # starting point: copy the example dataframe but without data
            indiv <- tibble::tibble(data.frame(matrix(ncol = ncol(example), nrow = nrow(df)))) # empty dataframe
            colnames(indiv) <- colnames(example) # name columns to match example
            
            indiv[1] <- df$Data_ID # "FishObsID"
            indiv[2] <- format(df$Start_Date, "%Y")# "Year"
            indiv[3] <- as.character(format(df$Start_Date, "%Y-%m-%d")) # "SampleDate"
            indiv[4] <- format(df$Start_Time, "%H:%M") # "SampleTime"
            indiv[5] <- df$Site_ID # "Station_ID"
            indiv[6] <- df$NCRN_Site_ID # "Station_Name"
            indiv[7] <- paste0(format(df$Start_Date, "%Y-%m-%d"), "_", format(df$Start_Time, "%H:%M"), "_", df$pass) # "Pass_ID"
            indiv[8] <- as.character(format(df$Entered_Date, "%Y-%m-%d")) # "Entry_Date"
            indiv[9] <- as.character(format(df$Entered_Date, "%H:%M")) # "Entry_Time"
            indiv[10] <- trimws(tolower(df$SPECIES)) # "Subject_Taxon"
            # "Species_ID" and "Scientific_Name"
            indiv <- dplyr::left_join(indiv, tlu_species, by=c("Subject_Taxon" = "Common_Name"))
            indiv[11] <- indiv$Latin_Name # "Scientific_Name"
            indiv[12] <- indiv$species_id # "Species_ID"
            indiv$species_id <- NULL # delete the joined column
            indiv$Latin_Name <- NULL # delete the joined column
            indiv[13] <- df$count# "Count"
            indiv[14] <- NA # "Status"
            indiv[15] <- NA # "Disposition"
            indiv[16] <- df$Photo_num # "Picture"
            indiv[17] <- NA # "Camera" 
            # "Photo"
            # the value of $Photo should be TRUE or FALSE
            # depending on whether there is a picture
            # in our access db, there's a picture if there's a value in $Photo_num
            for(i in 1:nrow(indiv)){
                if(is.na(indiv$Picture[i])){
                    indiv[i,18] <- FALSE
                } else {
                    indiv[i,18] <- TRUE
                }
            }
                
            indiv[19] <- df$LENGTH # "TL_mm"
            indiv[20] <- NA # "Wt_g"
            indiv[21] <- NA # "Calc_wt_TL_g"
            indiv[22] <- NA # "Wt_AddedOn"
            indiv[23] <- df$Comments_tblevents # "Note"
            indiv[24] <- df$Basin# "Basin"
            indiv[25] <- df$Loc_Name # "Branch"
            indiv[26] <- df$PARKNAME # "Reach_Name"
            indiv[27] <- NA # "Delt_deformities" 
            indiv[28] <- NA #"Delt_erodedfins" 
            indiv[29] <- NA # "Delt_lesions" 
            indiv[30] <- NA # "Delt_tumors"
            indiv[31] <- NA # "Delt_other"
            indiv[32] <- 'NCRN MBSS Access db: tbl_GameFish, "~Documents - NPS-NCRN-Biological Stream Sampling/General/Annual-Data-Packages/2022/NCRN_MBSS/NCRN_MBSS_be_2022.mdb"'
            # indiv <- unique(setDT(indiv), by=c("FishObsID")) 
            
            
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
            
            #---------------------------------------------------
            #----- Add Marc's 2021 & 2022 data to indiv format--
            #---------------------------------------------------
            if(addMarc == TRUE){
                indiv2021 <- buildMarc2021Indiv(example, marc2021, tlu_species)
                indiv2022 <- buildMarc2022Indiv(example, marc2022, tlu_species)
                indiv <- rbind(indiv, indiv2021, indiv2022)
            }
            
            message(
                if(length(check_df$result == "MATCH") == nrow(check_df)){
                    "`buildMarcindiv()` executed successfully..."
                } else {
                    for(i in 1:length(check_df$result != "MATCH")){
                        cat(paste(paste0("`indiv_marc", check_df$indiv_marc[i], "`"), paste0(" DID NOT MATCH `example.", check_df$example[i][i], "`"), "\n", sep = ""))
                    }
                }
            )
            # assign("indiv_marc", indiv_marc, envir = globalenv())
            
            return(indiv)
        }
    )
}