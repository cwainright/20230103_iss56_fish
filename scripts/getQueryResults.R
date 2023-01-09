# query a database and return results to a list
# a module for `scripts/fish_data_view.R`

getQueryResults <- function(qryList, connection){ # function with two arguments: 1.`qryList` (a list-object of query char strings) 2. `connection` an active odbc connection
    tryCatch(
        expr = {
            results_list <- vector(mode="list", length=length(qryList)) # instantiate empty list to hold query results
            names(results_list) <- names(qryList) # name elements in `results_list` to match `qryList` element names
            for(i in 1:length(qryList)){ # loop runs once per query
                results_list[[i]] <- RODBC::sqlQuery(connection, qryList[[i]]) # query the db and save result to `results_list`
            }
            # return(results_list)
            assign("results_list", results_list, envir = globalenv()) # save query results as environment object `results_list`
            message( # console messaging
                for(j in 1:length(qryList)){ # print a message for each query
                    if(class(results_list[[j]]) != "data.frame"){ # if the query result isn't a data frame
                        e <- results_list[[j]][1] # grab the ODBC error message from [[j]][1]
                        cat(paste("'", names(results_list)[j], "'", " query failed. Error message: ", "'", e, "'", "\n", sep = "")) # print to console that an error occurred
                    } else {
                        cat(paste("Query for ", "'", names(qryList)[j], "'", " executed successfully", "\n", sep = "")) # print to console that query ran successfully
                    }
                }
            )
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