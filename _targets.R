# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed. # nolint

# Set target options:
tar_option_set(
  # packages that your targets need to run
  packages = c("tibble","rgdal", "sp", "tidyverse", "lubridate", #"zoo"
               "grid", "scales", 'bcgroundwater',#ggmap", 
               "devtools", "rvest", "RColorBrewer", "envreportutils",
               "purrr", "sf", "gridExtra", "bcmaps", "scales", "forcats",
               "rmapshaper", "janitor", "readr", "cowplot", "glue","targets",
               "patchwork",
               "future","future.callr","future.batchtools"),
  format = "rds" # default storage format
  # Set other options as needed.
)

# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multiprocess")

# tar_make_future() configuration (okay to leave alone):
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# Source other scripts as needed. # nolint
# source("R/functions.R")


# Replace the target list below with your own:
list(
  #Set data paths.
  tar_target(obs_wells_data_path, './data/obs_wells_data.gpkg', format = 'file'),
  tar_target(raw_well_data_path, './data/raw_well_data.csv', format = 'file'),
  #Read in data.
  tar_target(obs_wells, read_obs_wells_dat(obs_wells_data_path)),
  tar_target(wells_data_raw, read_raw_wells_dat(raw_well_data_path)),
  #Calculate various summaries
  tar_target(monthlywells_ts, calculate_monthlywells_ts(wells_data_raw)),
  tar_target(welldata_attr, generate_welldata_attr_summary(monthlywells_ts)),
  tar_target(results_out, generate_well_results(monthlywells_ts, welldata_attr, obs_wells)),
  tar_target(monthly_out, calculate_monthly_results(monthlywells_ts, results_out)),
  #Generate data necessary to make output.
  tar_target(results_viz, set_results_viz(results_out,monthly_out)),
  #Generate output.
  tar_target(make_final_output, generate_output(results_viz, results_out, monthlywells_ts))
)
