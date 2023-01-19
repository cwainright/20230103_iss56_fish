options(warn=-1)
buildLookup <- function(marc2022, results_list){
    
    # make a lookup table to find $Species_ID values
    
    # grab key-value (species-id) pairs from `marc2022` (this key-value pairing doesn't exist in `marc2021`)
    tlu_species <- marc2022 %>% select(common_name, species_id) # select the two columns we're interested in
    tlu_species$dummy <- paste0(tlu_species$common_name, tlu_species$species_id) # concatenate these values so we know unique pairs
    tlu_species <- tlu_species[!duplicated(tlu_species$dummy), ] # (referential integrity: making sure there's no repeated key-value pairs or keys with more than one value or values with more than one key)
    tlu_species <- tlu_species %>% select(common_name, species_id) # keep only the key-value pairs
    tlu_species$common_name <- tolower(tlu_species$common_name)
    for(i in 1:nrow(tlu_species)){
        if(stringr::str_detect(tlu_species$common_name[i], "\\(")==TRUE){
            tlu_species$common_name[i] <- stringr::str_extract(tlu_species$common_name[i], "(?<=\\().+?(?=\\))") # https://stackoverflow.com/questions/8613237/extract-info-inside-all-parenthesis-in-r
        }
    }
    # grab key-value (common name-latin name) pairs from `results_list$tlu_Fish`
    tlu_species2 <- results_list$tlu_Fish %>%
        select(Latin_Name, Common_Name)
    tlu_species2$Common_Name <- trimws(tolower(tlu_species2$Common_Name))
    tlu_species2$dummy <- paste0(tlu_species2$Common_Name, tlu_species2$Latin_Name)
    tlu_species2 <- tlu_species2[!duplicated(tlu_species2$dummy), ] # (referential integrity: making sure there's no repeated key-value pairs or keys with more than one value or values with more than one key)
    tlu_species2 <- tlu_species2 %>% select(Common_Name, Latin_Name) # keep only the key-value pairs
    
    # join to produce a lookup table with latin name, common name, species id
    tlu_species <- dplyr::left_join(tlu_species2, tlu_species, by=c("Common_Name" = "common_name"))
    
    # tidy up
    rm(tlu_species2)
    
    # return lookup table
    return(tlu_species)
}




