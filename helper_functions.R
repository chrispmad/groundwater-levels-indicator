# Copyright 2015 Province of British Columbia
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

# construct_url <- function(obj, epsg, query) {
#   baseurl <- "https://openmaps.gov.bc.ca/geo/pub/{obj}/ows?service=WFS&version=2.0.0&request=GetFeature&typeName={obj}&SRSNAME=epsg:{epsg}&outputFormat=json{query}"
#   
#   if (!is.null(query)) {
#     query <- paste0("&CQL_FILTER=", query)
#   } else {
#     query <- ""
#   }
#   URLencode(glue::glue(baseurl, obj = obj, epsg = epsg, query = query))
# }
# 
# 
# bcdc_map <- function(id, epsg = 3005, query = NULL) {
#   url <- construct_url(id, epsg, query)
#   tmp <- tempfile(fileext = ".json")
#   on.exit(unlink(tmp))
#   res <- httr::GET(url, httr::write_disk(tmp))
#   httr::stop_for_status(res)
#   sf::st_read(tmp)
# }

get_obs_wells_data = function(){
  ## Source package libraries and the bcdc_map() function
  # if (!exists(".header_sourced")) source("header.R")
  # source("R/func.R")
  
  if(!dir.exists('data'))dir.create("data", showWarnings = FALSE)
  
  # Download data directly from BC Data Catalogue.
  obs_wells = bcdata::bcdc_query_geodata('groundwater-wells') %>% 
    collect() %>% 
    setNames(snakecase::to_snake_case(colnames(.)))
  
  obs_wells = obs_wells %>% 
    #Filter out observations with no observation well status or number (the vast majority!)
    dplyr::filter(!is.na(observation_well_status)) %>% 
    dplyr::filter(!is.na(observation_well_number)) %>% 
    #Join the natural resource region names etc. to data
    sf::st_join(bcmaps::nr_regions() %>% dplyr::select(-id)) %>% 
    #Make column names 'R-friendly'
    setNames(snakecase::to_snake_case(colnames(.))) %>% 
    #Narrow down columns.
    dplyr::select(observation_well_number, id, 
                  well_tag_number, construction_end_date, #general_remarks, other_information,
                  observation_well_status, aquifer_type = aquifer_material,
                  aquifer_id,
                  finished_well_depth, static_water_level, region_name, feature_area_sqm) %>% 
    # Clean up natural resource region name and aquifer type text fields.
    dplyr::mutate(region_name = str_remove_all(region_name, " Natural Resource Region"),
           region_name = str_replace(region_name, "-", " / "),
           aquifer_type = case_when(
             aquifer_type == "Unconsolidated" ~ "Sand and Gravel",
             is.na(aquifer_type) ~ "Unknown",
             T ~ aquifer_type
           )) %>% 
    #Make aquifer type into an ordered factor.
    dplyr::mutate(aquifer_type = factor(aquifer_type, levels = c("Bedrock","Sand and Gravel","Unknown"))) %>% 
    #Remove any duplicated observation well numbers.
    dplyr::filter(!duplicated(observation_well_number))
  
  write_sf(obs_wells, 'data/obs_wells_data.gpkg')
  #return(obs_wells)
}

get_raw_wells_data = function(){
  # Download all data for wells from https://www.env.gov.bc.ca/wsd/data_searches/obswell/map/data/
  # If the url for a given well doesn't work, the code creates an empty data.frame to 
  # make sure such a result can still be combined with the successful data reading attempts. In this way,
  # the function does not break if one or more wells has no available data to download.
  obs_wells = read_sf('data/obs_wells_data.gpkg')
  
  wells_data_raw = obs_wells$observation_well_number %>% 
    purrr::map( ~ {
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
    dplyr::bind_rows()
  
  ## Save raw data objects in a temporary directory
  # save(obs_wells, file = "./tmp/clean_attr_data.RData")
  # save(wells_data_raw, file = "./tmp/raw_well_data.RData")
  write.csv(wells_data_raw, file = 'data/raw_well_data.csv',row.names=F)
  #return(wells_data_raw)
}
