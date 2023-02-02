#!/usr/bin/env Rscript

# Set Options.
create_ggmaps = F
update.PDF.print.version = F

# Load in R scripts that list all the packages to import and the helper functions.
source('header.R')
source('helper_functions.R')
# If we have not downloaded the well attribute data or the raw groundwater level data (big!),
# download those now. Otherwise, those data should be in the "./data" folder.
if(!file.exists('data/obs_wells_data.gpkg')) get_obs_wells_data()
if(!file.exists('data/raw_well_data.csv')) get_raw_wells_data()

# Visualize targets pipeline.

# Run targets pipeline.
targets::tar_make()
# targets::tar_make_clustermq(workers = 2) # nolint
# targets::tar_make_future(workers = 2) # nolint
