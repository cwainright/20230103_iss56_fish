# build electronic data deliverable (EDD)
# EDD is a data format https://doimspp.sharepoint.com/sites/NCRNBiologicalStreamSampling/Shared%20Documents/Forms/AllItems.aspx?csf=1&web=1&e=oKeaBg&cid=46a9b284%2D304d%2D4b63%2D9d30%2D64b97cdd0314&RootFolder=%2Fsites%2FNCRNBiologicalStreamSampling%2FShared%20Documents%2FGeneral%2FAnnual%2DData%2DPackages%2F2022%2FExamples&FolderCTID=0x0120002FA57F4A20C6EC439BFEBEF4FA9D88E0
# database is from: https://doimspp.sharepoint.com/:u:/r/sites/NCRNBiologicalStreamSampling/Shared%20Documents/General/Annual-Data-Packages/2022/NCRN_MBSS/NCRN_MBSS_be_2022.mdb?csf=1&web=1&e=jjeJIg
# a module for `scripts/fish_data_view.R`



# build example
# a module for `scripts/fish_data_view.R`

buildEDD <- function(connection, write){
    tryCatch(
        expr = {
            #----- load external libraries
            suppressWarnings(suppressMessages(library(openxlsx)))
            suppressWarnings(suppressMessages(library(data.table)))
            
            #----- load project functions
            source("scripts/edd/buildEDDLocations.R")
            source("scripts/edd/buildEDDActivities.R")
            source("scripts/edd/buildEDDResults.R") # equivalent to python "from x import function"
            source("scripts/getQueryResults.R") # equivalent to python "from x import function"
            
            #-----  Query db
            db_objs <- RODBC::sqlTables(con) # test db connection
            tbl_names <- db_objs %>% # choose which tables you want to query
                subset(TABLE_NAME %in% c(
                    "tbl_Events",
                    "tbl_Protocol",
                    "tbl_Fish_Events",
                    "tbl_Locations",
                    "tbl_Meta_Events",
                    "tlu_Collection_Procedures_Gear_Config",
                    "tbl_Electro_Fish_Details",
                    "tbl_Fish_Data",
                    "tbl_GameFish",
                    "tlu_Fish"
                )
                ) %>%
                select(TABLE_NAME)
            
            # make list of queries so we can extract a few rows from each table
            qry_list <- vector(mode="list", length=nrow(tbl_names))
            names(qry_list) <- tbl_names$TABLE_NAME
            for (i in 1:length(qry_list)){
                qry_list[[i]] <- paste("SELECT * FROM", names(qry_list)[i])
            }
            
            results_list <- getQueryResults(qry_list = qry_list, connection = con)
            RODBC::odbcCloseAll() # close db connection
            
            # tidy up
            rm(db_objs)
            rm(tbl_names)
            rm(qry_list)
            
            #----- call functions that build data for EDD tabs
            activities <- buildEDDActivities(results_list)
            locations <- buildEDDLocations(results_list)
            results <- buildEDDResults(results_list)
            
            #----- compile data for EDD tabs into a list
            list_of_datasets <- list("Locations" = locations, "Activities" = activities, "Results" = results)
            if(length(list_of_datasets)==3){
                if(nrow(list_of_datasets[[1]]>0) & nrow(list_of_datasets[[2]]>0) & nrow(list_of_datasets[[3]]>0)){
                    message("\n\n`buildEDD() successfully produced data views.\nOutput saved as `EDD` in global environment.\n\n")
                    assign("EDD", list_of_datasets, envir = globalenv()) # save final product to global environment
                }
            } else {
                message("An error occurred when compiling results.")
                break
            }
            
            #----- write list to xlsx if `write` flag is TRUE
            if(write == TRUE){
                openxlsx::write.xlsx(list_of_datasets, file = file.choose())
            }
        }
    )
}