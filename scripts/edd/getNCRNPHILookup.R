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
            # this regex grabs most of the correct units
            for(i in 1:nrow(test)){
                test$unit[i] <- stringr::str_extract(test$long[i], "(?<=\\().+?(?=\\))")
            }
            # the units the regex can't extract, we fix manually:
            test$unit[4] <- NA # temp logger serial number
            for(i in 1:nrow(test))(
                if(stringr::str_detect(test$long[i], "presence/absence")){
                    test$unit[i] <- "0 = absence, 1 = presence"
                } else
                if(stringr::str_detect(test$long[i], "present/absent")){
                    test$unit[i] <- "A = absent, P = present"
                } else
                if(stringr::str_detect(test$long[i], "yes/no")){
                    test$unit[i] <- "0 = no, i.e., absent; 1 = yes, i.e, present"
                } else
                if(stringr::str_detect(test$long[i], "present, absent, extensive")){
                    test$unit[i] <- "A = absent, P = present, E = extensive"
                } else {
                    test$unit[i] <- test$unit[i]
                }
            )
            
            message(
                "`getNCRNPHILookup()` executed successfully..."
            )
            return(test)
        }
    )
}