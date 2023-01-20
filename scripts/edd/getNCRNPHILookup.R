# a module for `buildEDD()`
options(warn=-1)
getNCRNPHILookup <- function(springvars, summervars){
    tryCatch(
        expr = {
            #----- load external libraries
            suppressWarnings(suppressMessages(library(data.table)))
            suppressWarnings(suppressMessages(library(tidyverse)))
            suppressWarnings(suppressMessages(library(dplyr)))
            suppressWarnings(suppressMessages(library(readxl)))
            
            springvar <- data.frame(variable = springvars,
                                    source = "tbl_Spring_PHI")
            summervar <- data.frame(variable = summervars,
                                    source = "tbl_Summer_PHI")
            lookup <- rbind(springvar, summervar)
            
            meta_tbl_Spring_PHI <- readxl::read_excel("data/doc_def_tbl_Spring_PHI.xls") # from database tools -> database documenter
            meta_tbl_Summer_PHI <- readxl::read_excel("data/doc_def_tbl_Summer_PHI.xls")
            data.table::setnames(meta_tbl_Spring_PHI, "...8", "var")
            data.table::setnames(meta_tbl_Summer_PHI, "...8", "var")
            data.table::setnames(meta_tbl_Spring_PHI, "...9", "val")
            data.table::setnames(meta_tbl_Summer_PHI, "...9", "val")
            
            meta_tbl_Spring_PHI <- meta_tbl_Spring_PHI %>% select(var,val)
            meta_tbl_Spring_PHI <- meta_tbl_Spring_PHI %>% subset(var %in% c("SourceField:", "Description:"))
            meta_tbl_Spring_PHI$source <- "tbl_Spring_PHI"
            meta_tbl_Spring_PHI <- meta_tbl_Spring_PHI %>% subset(val != "Spring_PHI_ID")
            meta_tbl_Spring_PHI <- meta_tbl_Spring_PHI %>% subset(val != "Event_ID")
            meta_tbl_Spring_PHI <- meta_tbl_Spring_PHI %>% subset(val != "Sampleability_Habitat")
            test_desc <- meta_tbl_Spring_PHI %>% subset(var=="Description:")
            test_source <- meta_tbl_Spring_PHI %>% subset(var=="SourceField:")
            test <- cbind(test_desc, test_source)
            
            meta_tbl_Summer_PHI <- meta_tbl_Summer_PHI %>% select(var,val)
            meta_tbl_Summer_PHI <- meta_tbl_Summer_PHI %>% subset(var %in% c("SourceField:", "Description:"))
            meta_tbl_Summer_PHI$source <- "tbl_Summer_PHI"
            meta_tbl_Summer_PHI <- meta_tbl_Summer_PHI %>% subset(val != "Summer_PHI_ID")
            meta_tbl_Summer_PHI <- meta_tbl_Summer_PHI %>% subset(val != "Event_ID")
            meta_tbl_Summer_PHI <- meta_tbl_Summer_PHI %>% subset(val != "Sampleability_Habitat")
            test_desc <- meta_tbl_Summer_PHI %>% subset(var=="Description:")
            test_source <- meta_tbl_Summer_PHI %>% subset(var=="SourceField:")
            test2 <- cbind(test_desc, test_source)
            
            test <- rbind(test, test2)
            test$var <- NULL
            test$var <- NULL
            test$source <- NULL
            colnames(test) <- c("long", "short", "source")
            
            
            lookup <- dplyr::left_join(lookup, metadata, )
            
            message(
                if(length(check_df$result == "MATCH") == nrow(check_df)){
                    "`getNCRNPHILookup()` executed successfully..."
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