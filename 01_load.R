# Copyright 2018 Province of British Columbia
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.


#######################################################################################
# This script uses the bcgroundwater R package (https://github.com/bcgov/bcgroundwater)
# to download groundwater level data from the B.C. Data Catalogue 
# (https://catalogue.data.gov.bc.ca/dataset/57c55f10-cf8e-40bb-aae0-2eff311f1685), 
# provided under the Open Government Licence - British Columbia.
# The data is generated through the B.C. Ministry of Environment's
# Provincial Groundwater Observation Well Network Monitoring Program
# (https://www2.gov.bc.ca/gov/content?id=B03D0994BB5C4F98B6F7D4FD8610C836).
#
# Groundwater well attribute data and Natural Resource (NR) Regions are also
# sourced from the B.C. Data Catalogue, both released udner the Open Government
# Licence - British Columbia.
# Groundwater Wells: 
# https://catalogue.data.gov.bc.ca/dataset/e4731a85-ffca-4112-8caf-cb0a96905778
# Natural Resource (NR) Regions: 
# https://catalogue.data.gov.bc.ca/dataset/dfc492c0-69c5-4c20-a6de-2c9bc999301f
# Here we automatically grab the NR Regions using the 'bcmaps' R package 
########################################################################################


## Source package libraries and the bcdc_map() function
if (!exists(".header_sourced")) source("header.R")
source("func.R")

## Well metadata from gwells. In the future this can probably replace the 
## databc object, but for now use it to get aquifer number
if(!dir.exists('data'))dir.create("data", showWarnings = FALSE)
# tmpzip <- download.file("https://s3.ca-central-1.amazonaws.com/gwells-export/gwells.zip", 
#                         destfile = "data/gwells.zip")
# unzip("data/gwells.zip", exdir = "data")
# gwells <- read_csv("data/well.csv") %>% 
#   select(well_tag_number, aquifer_id)

# Above download link broken. Attempting to get data directly from BC Data Catalogue.
obs_wells = bcdata::bcdc_query_geodata('groundwater-wells') %>% 
  collect() %>% 
  setNames(snakecase::to_snake_case(colnames(.)))

# obs_wells_lookup_table = obs_wells %>% 
#   dplyr::select(well_tag_number, aquifer_id)
# #May be unnecessary.


obs_wells = obs_wells %>% 
  #Filter out observations with no observation well status or number (the vast majority!)
  filter(!is.na(observation_well_status)) %>% 
  filter(!is.na(observation_well_number)) %>% 
  #Join the natural resource region names etc. to data
  st_join(nr_regions() %>% dplyr::select(-id)) %>% 
  #Make column names 'R-friendly'
  setNames(snakecase::to_snake_case(colnames(.))) %>% 
  #Narrow down columns.
  dplyr::select(observation_well_number, id, 
                well_tag_number, construction_end_date, #general_remarks, other_information,
                observation_well_status, aquifer_type = aquifer_material,
                aquifer_id,
                finished_well_depth, static_water_level, region_name, feature_area_sqm) %>% 
  # Clean up natural resource region name and aquifer type text fields.
  mutate(region_name = str_remove_all(region_name, " Natural Resource Region"),
         region_name = str_replace(region_name, "-", " / "),
         aquifer_type = case_when(
           aquifer_type == "Unconsolidated" ~ "Sand and Gravel",
           is.na(aquifer_type) ~ "Unknown",
           T ~ aquifer_type
         )) %>% 
  #Make aquifer type into an ordered factor.
  mutate(aquifer_type = factor(aquifer_type, levels = c("Bedrock","Sand and Gravel","Unknown"))) %>% 
  #Remove any duplicated observation well numbers.
  filter(!duplicated(observation_well_number))

# gwells_intermediate %>% st_drop_geometry() %>% 
#   count(region_name, sort = T)
# ## Get groundwater well attribute data & add NR Region info from `bcmaps`
# obs_wells_raw <- bcdc_map("WHSE_WATER_MANAGEMENT.GW_WATER_WELLS_WRBC_SVW", 
#                       query = "OBSERVATION_WELL_NUMBER IS NOT NULL") %>%
#   collect()
#   filter(!is.na(MINISTRY_OBSERVATION_WELL_STAT) | 
#            !is.na(OBSERVATION_WELL_NUMBER)) %>% 
#   mutate(LONGITUDE = -LONGITUDE) %>% # LONGITUDE needs to be negative
#   sf::st_join(nr_regions()) %>%
#   as_tibble() %>%
#   select(OBSERVATION_WELL_NUMBER, CHEMISTRY_SITE_ID, WELL_TAG_NUMBER, 
#          CONSTRUCTION_END_DATE, GENERAL_REMARKS, OTHER_INFORMATION,
#          MINISTRY_OBSERVATION_WELL_STAT, 
#          AQUIFER_TYPE = AQUIFER_LITHOLOGY_CODE, 
#          DEPTH_WELL_DRILLED, WATER_DEPTH, LONGITUDE, LATITUDE,
#          REGION_NAME, SITE_AREA) %>%
#   mutate(REGION_NAME = str_replace(REGION_NAME, " Natural Resource Region", ""),
#          REGION_NAME = str_replace(REGION_NAME, "-", " / "), # To fit labeller later
#          AQUIFER_TYPE = as.character(AQUIFER_TYPE),
#          AQUIFER_TYPE = replace(AQUIFER_TYPE, is.na(AQUIFER_TYPE), "Unknown"),
#          AQUIFER_TYPE = factor(AQUIFER_TYPE, levels = c("BED", "UNC", "Unknown"), 
#                                labels = c("Bedrock", "Sand and Gravel", "Unknown")))


# Download all data for wells from https://www.env.gov.bc.ca/wsd/data_searches/obswell/map/data/
# If the url for a given well doesn't work, the code creates an empty data.frame to 
# make sure such a result can still be combined with the successful data reading attempts. In this way,
# the function does not break if one or more wells has no available data to download.
wells_data_raw = obs_wells$observation_well_number %>% 
  map( ~ {
    tryCatch(read_csv(paste0("https://www.env.gov.bc.ca/wsd/data_searches/obswell/map/data/OW",.x,"-data.csv")), 
             #If the url for this repetition doesn't exist / work, we create an empty data.frame to 
             # make sure such a result can still be combined with the successful data reading attempts.
             error = function(e) data.frame(Time = structure(numeric(0), 
                                         tzone = "UTC", 
                                         class = c("POSIXct","POSIXt")), 
                        Value = numeric(0), 
                        Approval = character(0), 
                        myLocation = character(0)))
             
  }) %>% 
  bind_rows()


# # Check for duplicate Well numbers in the well metadata
# dup_wells <- obs_wells_raw$OBSERVATION_WELL_NUMBER[duplicated(obs_wells_raw$OBSERVATION_WELL_NUMBER)]
# obs_wells_raw[obs_wells_raw$OBSERVATION_WELL_NUMBER %in% dup_wells,]
# 
# # Looking at the comments in GENERAL_REMARKS and OTHER_INFORMATION, they are 
# # deep and shallow variants of the same obs well number, omit the shallow version
# obs_wells <- filter(obs_wells_raw, WELL_TAG_NUMBER != 93712) %>% 
#   left_join(gwells, by = c("WELL_TAG_NUMBER" = "well_tag_number"))


## Download the Groundwater Level Data using the `bcgroundwater` package
## WARNING: This takes quite a bit of time

# Get raw well data (warnings reflect wells with no data)
#wells_data_raw <- get_gwl(wells = obs_wells$OBSERVATION_WELL_NUMBER, which = "all")
# The above code did not work for me - it reported a 404 error, and did not run to completion. I wrote
# a map function above that attempts to download all data available for each well in our table, and when
# that is not possible, it returns an empty table and it continues to the next well in the list.

## Save raw data objects in a temporary directory
save(obs_wells, file = "./tmp/clean_attr_data.RData")
save(wells_data_raw, file = "./tmp/raw_well_data.RData")


## Alternative source of groundwater well metadata in the B.C. Data Catalogue (OGL-British Columbia)
# gw_lithology <- bcdc_map("WHSE_WATER_MANAGEMENT.GW_WATER_WELLS_LITHOLOGY_SP",
# query = "OBSERVATION_WELL_NUMBER IS NOT NULL") %>%
#   select(OBSERVATION_WELL_NUMBER,
#          WELL_USE_NAME,
#          AQUIFER_ID,
#          AQUIFER_TYPE = AQUIFER_LITHOLOGY_CODE,
#          DEPTH_WELL_DRILLED) %>%
#   as_tibble() %>%
#   distinct(OBSERVATION_WELL_NUMBER, .keep_all = TRUE) %>%
#   filter(WELL_USE_NAME == "Observation Well")
# The above alternate code did not work for me: HTTP 400.
