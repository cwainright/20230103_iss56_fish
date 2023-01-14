# query a database and return results to a list
# a module for `scripts/fish_data_view.R`

getQueryResults <- function(connection){ # function with one argument: 1.`connection` an active odbc connection
    tryCatch(
        expr = {
            
            # Query db
            db_objs <- RODBC::sqlTables(con) # test db connection
            tbl_names <- db_objs %>% # choose which tables you want to query
                subset(TABLE_NAME %in% c(
                    "tbl_Events",
                    "tbl_Protocol",
                    "tbl_Fish_Events",
                    "tbl_Locations",
                    "tbl_Meta_Events",
                    "tbl_Fish_Data",
                    "tbl_GameFish",
                    "tlu_Fish",
                    "tlu_Collection_Procedures_Gear_Config",
                    "tbl_Electro_Fish_Details",
                    "tlu_Basin_Code",
                    "tbl_Photos",
                    "tlu_Park_Code"
                )
                ) %>%
                select(TABLE_NAME)
            
            # make list of queries so we can extract a few rows from each table
            qry_list <- vector(mode="list", length=nrow(tbl_names))
            names(qry_list) <- tbl_names$TABLE_NAME
            for (i in 1:length(qry_list)){
                qry_list[[i]] <- paste("SELECT * FROM", names(qry_list)[i])
            }
            
            results_list <- vector(mode="list", length=length(qry_list)) # instantiate empty list to hold query results
            names(results_list) <- names(qry_list) # name elements in `results_list` to match `qry_list` element names
            for(i in 1:length(qry_list)){ # loop runs once per query
                results_list[[i]] <- RODBC::sqlQuery(connection, qry_list[[i]]) # query the db and save result to `results_list`
            }
            # assign("results_list", results_list, envir = globalenv()) # save query results as environment object `results_list`
            message( # console messaging
                for(j in 1:length(qry_list)){ # print a message for each query
                    if(class(results_list[[j]]) != "data.frame"){ # if the query result isn't a data frame
                        e <- results_list[[j]][1] # grab the ODBC error message from [[j]][1]
                        cat(paste("'", names(results_list)[j], "'", " query failed. Error message: ", "'", e, "'", "\n", sep = "")) # print to console that an error occurred
                    } else {
                        cat(paste("Query for ", "'", names(qry_list)[j], "'", " executed successfully", "\n", sep = "")) # print to console that query ran successfully
                    }
                }
            )
            return(results_list)
        },
        error = {
            function(error_msg){
                message("An error occurred when running the query:")
                message(error_msg)
            }
        },
        finally = {
            message("All queries have been executed\n") # message indicating the function job completed
        }
    )
}