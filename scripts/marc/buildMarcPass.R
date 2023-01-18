#--------------------------------------------------------------------------
#----- Make our access db data match the required `example` format---------
#----- One row is one e-fishing pass---------------------------------------
#--------------------------------------------------------------------------
# a module for `scripts/buildMarcView.R`

buildMarcPass <- function(addMarc, example, marc2021, marc2022, tlu_species, results_list){
    tryCatch(
        expr = {
            #----- load project functions
            source("scripts/marc/buildMarc2022Pass.R") # equivalent to python "from x import function"
            source("scripts/marc/buildGameFishPass.R") # equivalent to python "from x import function"

            #----- make a flat dataframe where one row is one unique combination of `pass` and `species` from `results_list`
            df <- results_list$tbl_Fish_Data
            df <- rename(df, Comments_fishdata = Comments)
            df <- dplyr::left_join(df, results_list$tlu_Fish %>% select(Latin_Name, Common_Name, Nativity, Tolerance, `Trophic Status`), by = c("Fish_Species" = "Latin_Name"))
            df$total_n <- df$Total_Pass_1 + df$Total_Pass_2
            df <- dplyr::left_join(df, results_list$tbl_Fish_Events %>% select(Fish_Event_ID, Event_ID, Seg_Length, Fish_Biomass_1, Fish_Biomass_2), by = "Fish_Event_ID")
            df <- dplyr::left_join(df, results_list$tbl_Events %>% select(Event_ID, Location_ID, Start_Date, Start_Time, Comments), by = "Event_ID")
            df <- rename(df, Comments_tblevents = Comments)
            df <- dplyr::left_join(df, results_list$tbl_Locations %>% select(Location_ID, Loc_Name, NCRN_Site_ID, Basin_Code, Site_ID, Unit_Code), by = "Location_ID")
            df <- dplyr::left_join(df, results_list$tlu_Basin_Code %>% select(Basin_Code, Basin), by = "Basin_Code")
            df <- dplyr::left_join(df, results_list$tbl_Meta_Events %>% select(Event_ID, Entered_Date), by = "Event_ID")
            df <- dplyr::left_join(df, results_list$tbl_Photos %>% select(Event_ID, Photo_num), by = "Event_ID")
            df <- dplyr::left_join(df, results_list$tlu_Park_Code %>% select(PARKCODE, PARKNAME), by = c("Unit_Code" = "PARKCODE"))
            
            #----- format from wide-format data: $Total_Pass_1 and $Total_Pass_2 to long-format $value and $pass
            keepcols <- df %>% select(-c("Total_Pass_1", "Total_Pass_2"))
            keepcols <- colnames(keepcols)
            df <- data.table::melt(setDT(df), measure.vars = c("Total_Pass_1","Total_Pass_2"), id.vars = keepcols, variable.name = "pass")
            df$pass <- stringr::str_extract(df$pass, "[A-Za-z]+_[0-9]+") # regex extracts only Pass_x
            df$pass <- gsub("_", "", df$pass) # gsub replaces underscore "_" with nothing ""
            # df$id2 <- df$Data_ID
            df$id <- paste0(df$Data_ID, "_", df$pass)
            # RODBC::odbcCloseAll() # close db connection
            
            #----- re-build `example` from `results_list`
            pass <- tibble::tibble(data.frame(matrix(ncol = ncol(example), nrow = nrow(df)))) # empty dataframe
            colnames(pass) <- colnames(example) # name columns to match example
            
            pass[1] <- df$Data_ID # "FishObsID"
            pass[2] <- format(df$Start_Date, "%Y")# "Year"
            pass[3] <- as.character(format(df$Start_Date, "%Y-%m-%d")) # "SampleDate"
            pass[4] <- format(df$Start_Time, "%H:%M") # "SampleTime"
            pass[5] <- df$Site_ID # "Station_ID"
            pass[6] <- df$NCRN_Site_ID # "Station_Name"
            pass[7] <- paste0(format(df$Start_Date, "%Y-%m-%d"), "_", format(df$Start_Time, "%H:%M"), "_", df$pass) # "Pass_ID"
            pass[8] <- as.character(format(df$Entered_Date, "%Y-%m-%d")) # "Entry_Date"
            pass[9] <- as.character(format(df$Entered_Date, "%H:%M")) # "Entry_Time"
            pass[10] <- trimws(tolower(df$Common_Name)) # "Subject_Taxon"
            # "Species_ID"
            pass <- dplyr::left_join(pass, tlu_species, by=c("Subject_Taxon" = "Common_Name"))
            pass[11] <- pass$Latin_Name # "Scientific_Name"
            pass$Latin_Name <- NULL
            pass[12] <- pass$species_id # "Species_ID"
            pass$species_id <- NULL # delete the joined column
            pass[13] <- df$value # "Count"
            pass[14] <- NA # "Status"
            pass[15] <- df$retained # "Disposition"
            for(i in 1:nrow(pass)){
                if(pass$Disposition[i] >0){
                    pass$Disposition[i] <- paste0("Retained ", pass$Disposition[i], " individuals")
                } else {
                    pass$Disposition[i] <- "Released"
                }
            }
            pass[16] <- df$Photo_num # "Picture"
            pass[17] <- NA # "Camera" 
            # "Photo"
            # the value of $Photo should be TRUE or FALSE
            # depending on whether there is a picture
            # in our access db, there's a picture if there's a value in $Photo_num
            for(i in 1:nrow(pass)){
                if(is.na(pass$Picture[i])){
                    pass[i,18] <- FALSE
                } else {
                    pass[i,18] <- TRUE
                }
            }
            pass[19] <- NA # "TL_mm" NA because `pass` aggregates to e-fishing pass (refer to `indiv` for $TL_mm data)
            pass[20] <- NA # "Wt_g" NA because `pass` aggregates to e-fishing pass (refer to `indiv` for $Wt_g data)
            pass[21] <- NA # "Calc_wt_TL_g" NA because `pass` aggregates to e-fishing pass (refer to `indiv` for $Calc_wt_TL_g data)
            pass[22] <- NA # "Wt_AddedOn"
            pass[23] <- df$Comments_tblevents # "Note"
            pass[24] <- df$Basin# "Basin"
            pass[25] <- df$Loc_Name # "Branch"
            pass[26] <- df$PARKNAME # "Reach_Name"
            pass[27] <- df$Anom # "Delt_deformities" 
            pass[27] <- as.character(pass$Delt_deformities) # "Delt_deformities" 
            # pass[25] <- as.character(pass[25])
            for(i in 1:nrow(pass)){
                if(pass[i,27] != "0"){
                    pass[i,27] <- "TRUE"
                } else {
                    pass[i,27] <- "FALSE"
                }
            }    
            pass[28] <- NA #"Delt_erodedfins" 
            pass[29] <- NA # "Delt_lesions" 
            pass[30] <- NA # "Delt_tumors" 
            for(i in 1:nrow(pass)){
                if(pass[i,27] == "FALSE"){
                    pass[i,31] <- NA
                } else {
                    pass[i,31] <- df$Comments_fishdata[i] # "Delt_other"
                }
            }
            pass[32] <- 'NCRN MBSS Access db: tbl_Fish_Data, "~Documents - NPS-NCRN-Biological Stream Sampling/General/Annual-Data-Packages/2022/NCRN_MBSS/NCRN_MBSS_be_2022.mdb"'
            pass$id <- df$id
            pass <- unique(setDT(pass), by=c("id")) 
            pass$id <- NULL

                
            # error-checking:
            check_df <- tibble::tibble(data.frame(matrix(ncol=3, nrow=ncol(pass))))
            colnames(check_df) <- c("pass", "example", "result")
            check_df$pass <- colnames(pass)
            check_df$example <- colnames(example)
            for(i in 1:nrow(check_df)){
                if(check_df$pass[i] == check_df$example[i]){
                    check_df$result[i] <- "MATCH"
                } else {
                    check_df$result[i] <- "MISMATCH"
                }
            }
            
            #-------------------------------------------------
            #----- Add pre-2020 gamefish data to pass format--
            #-------------------------------------------------
            gamefish_pass <- buildGameFishPass(results_list=results_list, example=example, tlu_species=tlu_species, marc2022=marc2022)
            pass <- rbind(pass, gamefish_pass)
            
            #--------------------------------------------
            #----- Add Marc's 2022 data to pass format--
            #--------------------------------------------
            if(addMarc == TRUE){
                pass2022 <- buildMarc2022Pass(marc2022=marc2022, example=example, tlu_species=tlu_species, results_list=results_list)
                pass <- rbind(pass, pass2022)
            }

            message(
                if(length(check_df$result == "MATCH") == nrow(check_df)){
                    "`buildMarcPass()` executed successfully..."
                } else {
                    for(i in 1:length(check_df$result != "MATCH")){
                        cat(paste(paste0("`pass_marc", check_df$pass_marc[i], "`"), paste0(" DID NOT MATCH `example.", check_df$example[i][i], "`"), "\n", sep = ""))
                    }
                }
            )
            pass$TL_mm <- NA # total length should always be NA for pass-level data because TL is an individual-level metric
            pass$Wt_g <- NA # weight should always be NA for pass-level data because TL is an individual-level metric
            # assign("pass_marc", pass_marc, envir = globalenv())

            return(pass)
        }
    )
}