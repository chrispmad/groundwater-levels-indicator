#!/usr/bin/env Rscript

# This is a helper script to run the pipeline.
# Choose how to execute the pipeline below.
# See https://books.ropensci.org/targets/hpc.html
# to learn about your options.

# Set Options.
create_ggmaps = F
update.PDF.print.version = F

# Download the data.
source('header.R')
source('helper_functions.R')
if(!file.exists('data/obs_wells_data.gpkg')) get_obs_wells_data()
if(!file.exists('data/raw_well_data.csv')) get_raw_wells_data()

# Run targets pipeline.
targets::tar_make()
# targets::tar_make_clustermq(workers = 2) # nolint
# targets::tar_make_future(workers = 2) # nolint
