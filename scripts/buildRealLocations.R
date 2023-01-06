# build example_locations
# a module for `scripts/fish_data_view.R`

#----- load external libraries
library(data.table)
library(tidyverse)
library(dplyr)
library(readxl)

#----- load project functions
source("scripts/getQueryResults.R") # equivalent to python "from x import function"


# load example data
example_locations <- readxl::read_excel("data/NCRN_BSS_EDD_20230105_1300.xlsx", sheet = "Locations")

# Connect to db
library(RODBC)
db <- ("C:/Users/cwainright/OneDrive - DOI/Documents - NPS-NCRN-Biological Stream Sampling/General/Annual-Data-Packages/2022/NCRN_MBSS/NCRN_MBSS_be_2022.mdb")# https://doimspp.sharepoint.com/:u:/r/sites/NCRNBiologicalStreamSampling/Shared%20Documents/General/Annual-Data-Packages/2022/NCRN_MBSS/NCRN_MBSS_be_2022.mdb?csf=1&web=1&e=jjeJIg
con <- RODBC::odbcConnectAccess2007(db) # open db connection

# Query db
db_objs <- RODBC::sqlTables(con) # test db connection
tbl_names <- db_objs %>% # choose which tables you want to query
    subset(TABLE_NAME %in% c("tbl_Fish_Events",
                             "tbl_Fish_Data",
                             "tbl_Events",
                             "tbl_Locations")) %>%
    select(TABLE_NAME)

# make list of queries so we can extract a few rows from each table
qry_list <- vector(mode="list", length=nrow(tbl_names))
names(qry_list) <- tbl_names$TABLE_NAME
for (i in 1:length(qry_list)){
    qry_list[[i]] <- paste("SELECT * FROM", names(qry_list)[i])
}

getQueryResults(qryList = qry_list, connection = con)

RODBC::odbcCloseAll() # close db connection

#----- re-build `example_locations` from `results_list`

# starting point: copy the example dataframe but without data
real_locations <- tibble::tibble(data.frame(matrix(ncol = ncol(example_locations), nrow = nrow(results_list$tbl_Locations)))) # empty dataframe
colnames(real_locations) <- colnames(example_locations) # name columns to match example

# head(example_locations[1]) # what does the next column look like?
# colnames(example_locations)[1]
# length(unique(example_locations[1])) # how many unique org codes are there?
real_locations[1] <- "NCRN"

# head(example_locations[2]) # what does the next column look like?
# colnames(example_locations)[2]
# length(unique(example_locations$`Park_Code`)) # how many unique org codes are there?
# colnames(results_list$tbl_Locations)
# length(unique(results_list$tbl_Locations$Unit_Code))
# head(results_list$tbl_Locations$Unit_Code)
real_locations[2] <- results_list$tbl_Locations$Unit_Code

# head(example_locations[3]) # what does the next column look like?
# colnames(example_locations)[3]
# head(results_list$tbl_Locations$Location_ID)
# length(unique(results_list$tbl_Locations$Location_ID))
real_locations[3] <- results_list$tbl_Locations$Location_ID

# head(example_locations[4]) # what does the next column look like?
# colnames(example_locations)[4]
# colnames(results_list$tbl_Locations)
# head(results_list$tbl_Locations$Loc_Name)
# length(unique(results_list$tbl_Locations$Loc_Name))
real_locations[4] <- results_list$tbl_Locations$Loc_Name

# head(example_locations[5]) # what does the next column look like?
# colnames(example_locations)[5]
# colnames(results_list$tbl_Locations)
# head(results_list$tbl_Locations$Loc_Type)
# length(unique(results_list$tbl_Locations$Loc_Type))
real_locations[5] <- "Creek"

# head(example_locations[6]) # what does the next column look like?
# colnames(example_locations)[6]
# colnames(results_list$tbl_Locations)
# head(results_list$tbl_Locations$Dec_Degrees_North)
real_locations[6] <- results_list$tbl_Locations$Dec_Degrees_North

# head(example_locations[7]) # what does the next column look like?
# colnames(example_locations)[7]
# colnames(results_list$tbl_Locations)
# head(results_list$tbl_Locations$Dex_Degrees_East)
real_locations[7] <- results_list$tbl_Locations$Dex_Degrees_East

# head(example_locations[8]) # what does the next column look like?
# colnames(example_locations)[8]
# colnames(results_list$tbl_Locations)
# head(results_list$tbl_Locations$Coord_System)
real_locations[8] <- "GPS-Unspecified"

# head(example_locations[9]) # what does the next column look like?
# colnames(example_locations)[9]
# colnames(results_list$tbl_Locations)
# head(results_list$tbl_Locations$Datum)
real_locations[9] <- toupper(results_list$tbl_Locations$Datum)

# head(example_locations[10]) # what does the next column look like?
# colnames(example_locations)[10]
real_locations[10] <- NA

# head(example_locations[11]) # what does the next column look like?
# colnames(example_locations)[11]
# colnames(results_list$tbl_Locations)
# head(results_list$tbl_Locations$Accuracy_Notes)
# unique(results_list$tbl_Locations$Accuracy_Notes)
real_locations[11] <- NA

# head(example_locations[12]) # what does the next column look like?
# colnames(example_locations)[12]
real_locations[12] <- NA

# head(example_locations[13]) # what does the next column look like?
# colnames(example_locations)[13]
real_locations[13] <- NA

# head(example_locations[14]) # what does the next column look like?
# colnames(example_locations)[14]
# colnames(results_list$tbl_Locations)
real_locations[14] <- NA

# head(example_locations[15]) # what does the next column look like?
# colnames(example_locations)[15]
# unique(example_locations$Location_Purpose)
# colnames(results_list$tbl_Locations)
real_locations[15] <- NA

# head(example_locations[16]) # what does the next column look like?
# colnames(example_locations)[16]
# unique(example_locations[16])
# colnames(results_list$tbl_Locations)
real_locations[16] <- NA

# head(example_locations[17]) # what does the next column look like?
# colnames(example_locations)[17]
# unique(example_locations[17])
# colnames(results_list$tbl_Locations)
# head(results_list$tbl_Locations$HUC)
# unique(results_list$tbl_Locations$HUC)
real_locations[17] <- results_list$tbl_Locations$HUC

# head(example_locations[18]) # what does the next column look like?
# colnames(example_locations)[18]
# unique(example_locations[18])
# colnames(results_list$tbl_Locations)
# head(results_list$tbl_Locations$Reach_Code24)
# unique(results_list$tbl_Locations$Reach_Code24)
real_locations[18] <- sprintf("%.0f", results_list$tbl_Locations$Reach_Code24) # get rid of sci notation

# head(example_locations[19]) # what does the next column look like?
# colnames(example_locations)[19]
# unique(example_locations[19])
real_locations[19] <- NA

# head(example_locations[20]) # what does the next column look like?
# colnames(example_locations)[20]
# unique(example_locations[20])
real_locations[20] <- NA

# head(example_locations[21]) # what does the next column look like?
# colnames(example_locations)[21]
# unique(example_locations[21])
# colnames(results_list$tbl_Locations)
# head(results_list$tbl_Locations$Elevation)
# unique(results_list$tbl_Locations$Elevation)
for(i in 1:nrow(real_locations)){ # loop runs once per row in `real_locations`
    if(results_list$tbl_Locations$Elevation[i] == 0){ # change zeroes to NA
        real_locations[i,21] <- NA # b/c all NCRN sites are above sea level
    } else { # otherwise just keep the listed elevation
        real_locations[i,21] <- results_list$tbl_Locations$Elevation[i]
    }
}

# head(example_locations[22]) # what does the next column look like?
# colnames(example_locations)[22]
# unique(example_locations[22])
# colnames(results_list$tbl_Locations)
real_locations[22] <- "ft"

# head(example_locations[23]) # what does the next column look like?
# colnames(example_locations)[23]
# unique(example_locations[23])
# colnames(results_list$tbl_Locations)
real_locations[23] <- NA

# head(example_locations[24]) # what does the next column look like?
# colnames(example_locations)[24]
# unique(example_locations[24])
# colnames(results_list$tbl_Locations)
real_locations[24] <- NA

# head(example_locations[25]) # what does the next column look like?
# colnames(example_locations)[25]
# unique(example_locations[25])
# colnames(results_list$tbl_Locations)
real_locations[25] <- NA

# head(example_locations[26]) # what does the next column look like?
# colnames(example_locations)[26]
# unique(example_locations[26])
# colnames(results_list$tbl_Locations)
real_locations[26] <- NA

# head(example_locations[27]) # what does the next column look like?
# colnames(example_locations)[27]
# unique(example_locations[27])
real_locations[27] <- "US"

# head(example_locations[28]) # what does the next column look like?
# colnames(example_locations)[28]
# unique(example_locations[28])
# colnames(results_list$tbl_Locations)
# unique(results_list$tbl_Locations$State)
real_locations[28] <- results_list$tbl_Locations$State

# head(example_locations[29]) # what does the next column look like?
# colnames(example_locations)[29]
# unique(example_locations[29])
# colnames(results_list$tbl_Locations)
# unique(results_list$tbl_Locations$County)
real_locations[29] <- results_list$tbl_Locations$County

# head(example_locations[30]) # what does the next column look like?
# colnames(example_locations)[30]
# unique(example_locations[30])
# colnames(results_list$tbl_Locations)
# unique(results_list$tbl_Locations$Catchment_Area)
real_locations[30] <- sprintf("%.3f", results_list$tbl_Locations$Catchment_Area)

# head(example_locations[31]) # what does the next column look like?
# colnames(example_locations)[31]
# unique(example_locations[31])
# colnames(results_list$tbl_Locations)
real_locations[31] <- "acre" # design view db.tbl_Locations

# head(example_locations[32]) # what does the next column look like?
# colnames(example_locations)[32]
# unique(example_locations[32])
# colnames(results_list$tbl_Locations)
real_locations[32] <- NA # design view db.tbl_Locations

# head(example_locations[33]) # what does the next column look like?
# colnames(example_locations)[33]
# unique(example_locations[33])
# colnames(results_list$tbl_Locations)
real_locations[33] <- NA # design view db.tbl_Locations

# head(example_locations[34]) # what does the next column look like?
# colnames(example_locations)[34]
# unique(example_locations[34])
# colnames(results_list$tbl_Locations)
real_locations[34] <- NA # design view db.tbl_Locations

# head(example_locations[35]) # what does the next column look like?
# colnames(example_locations)[35]
# unique(example_locations[35])
# colnames(results_list$tbl_Locations)
real_locations[35] <- NA # design view db.tbl_Locations

# head(example_locations[36]) # what does the next column look like?
# colnames(example_locations)[36]
# unique(example_locations[36])
# colnames(results_list$tbl_Locations)
real_locations[36] <- NA # design view db.tbl_Locations

# head(example_locations[37]) # what does the next column look like?
# colnames(example_locations)[37]
# unique(example_locations[37])
# colnames(results_list$tbl_Locations)
real_locations[37] <- NA # design view db.tbl_Locations

# head(example_locations[38]) # what does the next column look like?
# colnames(example_locations)[38]
# unique(example_locations[38])
# colnames(results_list$tbl_Locations)
real_locations[38] <- NA # design view db.tbl_Locations

# head(example_locations[39]) # what does the next column look like?
# colnames(example_locations)[39]
# unique(example_locations[39])
# colnames(results_list$tbl_Locations)
real_locations[39] <- NA # design view db.tbl_Locations

# head(example_locations[40]) # what does the next column look like?
# colnames(example_locations)[40]
# unique(example_locations[40])
# colnames(results_list$tbl_Locations)
real_locations[40] <- NA # design view db.tbl_Locations

# head(example_locations[41]) # what does the next column look like?
# colnames(example_locations)[41]
# unique(example_locations[41])
# colnames(results_list$tbl_Locations)
real_locations[41] <- NA # design view db.tbl_Locations

# head(example_locations[42]) # what does the next column look like?
# colnames(example_locations)[42]
# unique(example_locations[42])
# colnames(results_list$tbl_Locations)
real_locations[42] <- NA # design view db.tbl_Locations


# check colname and order
for(i in 1:ncol(real_locations)){
    if(colnames(real_locations)[i] == colnames(example_locations)[i]){
        cat(paste("real_locations.", colnames(real_locations)[i], " matched example_locations.", colnames(example_locations)[i]), "\n", sep = "")
    } else {
        cat(paste("mismatching columns: real_locations.", colnames(real_locations)[i], " and example_locations.", colnames(example_locations)[i]), "\n", sep = "")
    }
}


