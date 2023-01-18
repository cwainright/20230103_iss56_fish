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
            source("scripts/buildEDDLocations.R")
            source("scripts/buildEDDActivities.R")
            source("scripts/buildEDDResults.R") # equivalent to python "from x import function"
            
            activities <- buildEDDActivities(connection = con)
            locations <- buildEDDLocations(connection = con)
            results <- buildEDDResults(connection = con)
            
            list_of_datasets <- list("Locations" = locations, "Activities" = activities, "Results" = results)
            assign("EDD", list_of_datasets, envir = globalenv())
            if(write == TRUE){
                openxlsx::write.xlsx(list_of_datasets, file = file.choose())
            }
        },
        finally = {
            "`buildEDD() executed successfully"
        }
    )
}