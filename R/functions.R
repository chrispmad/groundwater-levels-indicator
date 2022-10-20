# Functions that are called by targets package

# ===== HEADER ========
# header = function(){
#   ## Install the packages we will need from CRAN:
#   package_list <- c("dplyr", "rgdal", "sp", "ggplot2", "stringr", #"lubridate", "zoo"
#                     "grid", "scales", #ggmap", 
#                     "devtools", "rvest", "RColorBrewer",
#                     "purrr", "sf", "gridExtra", "bcmaps", "scales", "forcats",
#                     "rmapshaper", "janitor", "readr", "cowplot", "glue","targets","future","future.callr","future.batchtools")
#   package_new <- package_list[!(package_list %in% installed.packages()[,"Package"])]
#   if(length(package_new)) install.packages(package_new)
#   
#   
#   ## Install the packages we will need from GitHub:
#   package_github <- c(bcgov = "bcgroundwater", bcgov = "envreportutils", 
#                       bcgov = "bcmapsdata", thomasp85 = "patchwork")
#   package_new <- package_github[!(package_github %in% installed.packages()[,"Package"])]
#   if(length(package_new)) {
#     devtools::install_github(paste(names(package_new), package_new, sep = "/"))
#   }
#   
#   
#   ## ggmap needs to be installed from GitHub, using the `tidyup` branch 
#   # from a known working commit https://github.com/dkahle/ggmap/tree/3ffc51b965709162dbaa62b3baa6106f59eba27b.
#   # See https://github.com/dkahle/ggmap/issues/51
#   if (!"ggmap" %in% installed.packages()[, "Package"] || 
#       utils::packageVersion("ggmap") < package_version("2.7.902")) {
#     devtools::install_github("dkahle/ggmap", ref = "3ffc51b965709162")
#   }
#   
#   ## Load required packages
#   library(sf)
#   library(dplyr)
#   library(purrr)
#   library(tidyr)
#   library(bcgroundwater)
#   library(sp)
#   library(rgdal)
#   library(rvest)
#   library(stringr)
#   library(bcmaps)
#   library(ggplot2)
#   library(grid)
#   library(RColorBrewer)
#   library(scales)
#   library(ggmap)
#   library(forcats)
#   library(rmapshaper)
#   library(gridExtra)
#   library(envreportutils)
#   library(janitor)
#   library(readr)
#   library(glue)
#   library(patchwork)
#   library(cowplot)
#   
#   ## Create project directories
#   if (!exists("tmp")) dir.create("tmp", showWarnings = FALSE)
#   if (!exists("out")) dir.create("out", showWarnings = FALSE)
#   if (!exists("out/figs")) dir.create("out/figs", showWarnings = FALSE)
#   if (!exists("leaflet_map/well_plots")) dir.create("leaflet_map/well_plots", showWarnings = FALSE)
#   if (!exists("leaflet_map/regional_plots")) dir.create("leaflet_map/regional_plots", showWarnings = FALSE)
#   
#   ## Invisible header object
#   # .header_sourced <- TRUE
# }

# === READ IN DATA ====

read_obs_wells_dat = function(file) {
  obs_wells = read_sf(file)
  return(obs_wells)
}

read_raw_wells_dat = function(file) {
  wells_data_raw = read_csv(file)
  return(wells_data_raw)
}
# ===DATA CLEANING AND ANALYSIS FUNCTION(S)====
# 

# function 'calculate_monthly_well_summaries' takes the wells_data_raw output 
# from 'get_raw_wells_data' function and outputs clean, monthly well summaries.
calculate_monthlywells_ts = function(data){
  
  ## Source package libraries
  # if (!exists(".header_sourced")) source("header.R")
  
  ## Load saved raw data if necessary
  # if (!exists("wells_data_raw")) load("./tmp/raw_well_data.RData")
  
  ## Clean raw groundwater level data
  
  date.limit = Sys.Date()
  
  # Nest data by Well_Num. As we don't have EMS_IDS, use Well_Num
  # so we get a clear idea of which well has convergence issues
  wells_prep <- data %>%
    rename(Date = Time, GWL = Value, Well_Num = myLocation) %>% 
    filter(Date <= as.POSIXct(date.limit)) %>% 
    mutate(EMS_ID = Well_Num) %>%      
    group_by(Well_Num1 = Well_Num) %>%
    nest()
  
  # Create monthly time series for each well - takes a minute or two.
  wells_month <- mutate(wells_prep, data = map(data, ~bcgroundwater::monthly_values(.x)))
  
  # Get time series, remove consecutive strings of missing values from the
  # beginning and end of each time series, interpolate over missing values. Takes a few minutes.
  wells_ts <- mutate(wells_month, data = map(data, ~bcgroundwater::make_well_ts(.x)))
  
  # Note that wells with 4 or more consecutive days missing data should be flagged and removed from the dataset.
  
  # The following code applies a function to each row of the dataset. This function repeats most of the logic
  # of the {bcgroundwater} function 'make_well_ts', which outputs to the console whether or not a well 
  # has data gaps that are sufficiently large to be a problem. The issue is that then the user must 
  # write (by hand!) the list of well identity numbers and then filter them out... we can do better!
  # The function below adds a column to each well's dataframe indicating whether or not such a data gap exists,
  # which we can easily use in the following code to filter out such problematic wells.
  wells_ts = wells_ts$data %>% 
    map( ~ {
      .x %>% cbind(.x %>% slice_head(prop = 0.1) %>% 
                     bind_rows(.x %>% slice_tail(prop = 0.1)) %>% 
                     filter(is.na(dev_med_GWL)) %>% 
                     filter(Date %m+% months(1) == lead(Date)) %>% 
                     summarise(data_missing = n()) > 1
      )
    }) %>% 
    bind_rows() %>% 
    group_by(EMS_ID) %>% 
    nest()
  
  # Unnest data for full timeseries
  monthlywells_ts <- unnest(wells_ts, data) %>%
    ungroup() %>% 
    mutate(Well_Num = as.numeric(str_remove(Well_Num, "OW")),
           EMS_ID = NA)

  ## Save clean data object in a temporary directory
  ## save(monthlywells_ts, file = "./tmp/clean_well_data.RData")
  return(monthlywells_ts)
}

generate_welldata_attr_summary = function(data = monthlywells_ts){
  ## Generate summary data for each well
  welldata_attr <- data %>%
    group_by(EMS_ID, Well_Num) %>%
    summarise(dataStart = as.Date(min(Date)), 
              dataEnd = as.Date(max(Date)), 
              dataYears = as.numeric(dataEnd - dataStart) / 365, 
              nObs = n(), 
              nMissing = length(med_GWL[nReadings == 0]), 
              percent_missing = round(nMissing/nObs*100, 1))
  return(welldata_attr)
}
  
generate_well_results = function(monthlywells_ts, welldata_attr, obs_wells){
  ## Source package libraries
  # if (!exists(".header_sourced")) source("header.R")
  
  ## Load saved clean data objects if necessary
  # if (!exists("monthlywells_ts")) load("./tmp/clean_well_data.RData")
  # if (!exists("obs_wells")) load("./tmp/clean_attr_data.RData")
  
  ## Analysis of mean annual groundwater levels, using a Mann-Kendall trend test
  ## with trend-free prewhitening, as implemented in the package 'zyp'. Methods are 
  ## documented here: 
  ## http://www.env.gov.bc.ca/soe/indicators/water/groundwater-levels.html
  
  ## Only use wells with relatively current data, more than 10 years of data, and 
  ## less than 25% missing monthly observations. Use lubridate to subtract 10 years from current date.
  latest_date <- Sys.Date() %m+% -years(10)
  
  wells_nums <- filter(welldata_attr, 
                       dataYears >= 10, 
                       percent_missing < 25, 
                       dataEnd > latest_date) %>% 
    pull(Well_Num)
  
  ## Summarise as mean annual values and filter to subset of wells
  annualwells_ts <- monthlywells_ts %>%
    group_by(EMS_ID, Well_Num, Year) %>%
    summarize(mean_GWL = mean(med_GWL), SD = sd(med_GWL), n_months = n()) %>%
    filter(Well_Num %in% wells_nums, n_months == 12)
  
  ## Perform the analysis
  results_annual <- gwl_zyp_test(dataframe = annualwells_ts, byID = "Well_Num", 
                                 col = "mean_GWL", method = "both") %>%
    mutate(Well_Num = as.numeric(Well_Num)) %>%
    filter(test_type == "yuepilon")
  
  ## Join the analysis results to the well summary data
  ## Full join to add in missing and old data so we can mark it as such below
  wells_results <- full_join(results_annual, welldata_attr, by = "Well_Num") %>% as_tibble()
  
  ## Assign each well to a trend category according to the slope and significance 
  ## of the trend
  wells_results <- mutate(wells_results,
                          state = case_when(trend >= 0.1 & sig < 0.05 ~ "Large Rate of Decline",
                                            trend >= 0.03 & trend < 0.1 & sig < 0.05 ~ "Moderate Rate of Decline",
                                            trend <= -0.03 & sig < 0.05 ~ "Increasing",
                                            TRUE ~ "Stable"))
  
  ## Join this to the well attribute data
  results_out <- obs_wells %>% 
    mutate(observation_well_number = as.numeric(observation_well_number)) %>% 
    right_join(wells_results, 
               by = c("observation_well_number" = "Well_Num")) %>%
    # Reproject the coordinates of the wells into lat/long, add them to dataframe
    st_transform(crs = 4326) %>% 
    mutate(Long = st_coordinates(.)[,1],
           Lat = st_coordinates(.)[,2]) %>%
    mutate(Lat = round(Lat, 4), 
           Long = round(Long, 4), 
           wellDepth_m = round(finished_well_depth * 0.3048), 
           waterDepth_m = round(static_water_level * 0.3048), 
           dataYears = round(dataYears, 1),
           trend_line_int = round(intercept, 4), 
           trend_line_slope = round(trend, 4),
           sig = round(sig, 4), 
           percent_missing = round(percent_missing, 1)) %>%
    select(EMS_ID = id, 
           Well_Num = observation_well_number, 
           Aquifer_Type = aquifer_type,
           region_name, 
           aquifer_id,
           Lat, Long, 
           wellDepth_m, waterDepth_m, 
           start_date = dataStart, 
           last_date = dataEnd, 
           nYears = dataYears, 
           percent_missing, trend_line_int, trend_line_slope, sig, state) %>%
    mutate(Well_Name = paste0("Observation Well #", Well_Num), 
           state = case_when(is.na(trend_line_int) & (nYears < 10 | is.na(last_date)) ~ 
                               "Recently established well; time series too short for trend analysis",
                             is.na(trend_line_int) & (percent_missing >= 25 | last_date < latest_date) ~
                               "Too many missing observations to perform trend analysis",
                             TRUE ~ state),
           category = case_when(state %in% c("Increasing", "Stable") ~ "Stable or Increasing", 
                                grepl("Recently|missing", state) ~ "N/A",
                                TRUE ~ state)) %>% 
    filter(!(state == "Too many missing observations to perform trend analysis" & last_date < latest_date)) %>% 
    select(EMS_ID, Well_Num, Well_Name, everything())
  
  
  ## Save results in a temporary directory
  #save(results_out, file = "./tmp/analysis_data.RData")
  #save(welldata_attr, file = "./tmp/well_data_attributes.RData")
  
  
  ## Write out clean data and attributes file
  # attr.out.file <- "out/GW_Well_Attributes.csv"
  # write.csv(results_out, attr.out.file, row.names = FALSE)
  return(results_out)
}

calculate_monthly_results = function(monthlywells_ts, results_out){
  ## Write out clean groundwater level data file
  # remove interpolated values and NA values and make sure only
  # keep wells which match results_out dataset
  
  monthly_out <- monthlywells_ts %>%
    filter(nReadings > 0, 
           Well_Num %in% results_out$Well_Num) %>%
    select(EMS_ID, Well_Num, Date, Year, Month, med_GWL, dev_med_GWL, nReadings)
  
  #gwl.out.file <- "out/GWL_Monthly.csv"
  #write.table(monthly_out, file = gwl.out.file, sep = ",", row.names = FALSE)
  return(monthly_out)
}

# ======OUTPUT FUNCTION(S)========

set_results_viz = function(results_out,monthly_out){
#GWL_Monthly = read_csv('out/GWL_Monthly.csv')

## Select wells analyzed and create factors
results_viz <- results_out[results_out$category != "N/A",] %>%
  mutate(region_name_short = str_replace(region_name, "( / )|( )", "_"),
         state = factor(state, levels = c("Increasing", 
                                          "Stable",
                                          "Moderate Rate of Decline",
                                          "Large Rate of Decline"),
                        ordered = TRUE),
         category = factor(category, levels = c("Stable or Increasing", 
                                                "Moderate Rate of Decline",
                                                "Large Rate of Decline"),
                           ordered = TRUE))

##save results_viz df to tmp folder for use in gwl.Rmd
#save(results_viz, file = "tmp/results_viz.RData")
return(results_viz)
}

# Note: below function still does not work to update print PDF map(s) if you
# do not have a google cloud API, which I do not!

generate_output = function(results_viz, results_out, monthlywells_ts, update.PDF.print.version = F){
  # ## Source package libraries
  # if (!exists(".header_sourced")) source("header.R")
  # 
  # ## Load saved data if necessary
  # if (!exists("results_out"))  load("./tmp/analysis_data.RData")
  # if (!exists("monthlywells_ts")) load("./tmp/clean_well_data.RData")
## Bar chart theme
theme_barcharts <- theme(
  axis.text = element_text(size = 14),
  axis.title = element_blank(), 
  plot.title = element_text(size = 17, hjust = 0.5),
  plot.margin = unit(c(6,8,6,2),"mm")
)

## Plot settings 
label.colour <- "black" 
colour.scale <- brewer.pal(3,"Blues")

## Paths for saving plots
status.bc <- "out/figs/status-bc"
status.reg <- "out/figs/status-by-reg"
status.reg.bc <- "out/figs/status-by-reg-bc"
status.well <- "leaflet_map/well_plots"
status.reg.all <- "leaflet_map/regional_plots"


# #facet label function
nLabeller <- function(n, singular, sep = " ") {
  suffix <- ifelse(n == 1, singular, paste0(singular,"s"))
  label <- paste(n, suffix, sep = sep)
  label
}

## Provincial & Regional Summary Plots (Web & PDF)------------------------------

#summary df & provincial summary bar chart of categories
bc_bar_chart <- results_viz %>%
  group_by(category) %>%
  summarise(frequency = n()) %>%
  arrange(desc(category)) %>%
  mutate(percent = round(frequency/sum(frequency), digits = 2),
         position = cumsum(percent) - percent/2,
         geography = "British Columbia") %>% 
  ggplot(aes(x = geography, y = percent, fill = category)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  scale_fill_manual(name = "", values = colour.scale) +
  geom_text(aes(y = position, label = paste0(frequency, " wells")),
            colour = label.colour, size = 5) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 1, .2),
                     limits = c(0, 1.04), labels = percent) +
  labs(y = "Percent of Groundwater Wells", x = " ") +
  theme_soe() +
  theme_barcharts +
  theme(panel.grid.major.x = element_blank(),
        legend.text = element_text(size = 16),
        axis.text.x = element_text(size = 16))


#regional summary df
sum_data_reg <- results_viz %>%
  group_by(region_name, region_name_short, category) %>%
  summarise(frequency = n()) %>%
  mutate(proportion = frequency/sum(frequency),
         #region_lab = paste0(gsub("(\\s)","\\\n",
         #                    gsub("\\s/\\s*", "/\\\n", REGION_NAME)),
         #                    "\n(", nLabeller(sum(frequency), "well"), ")")) %>%
         region_lab = paste0(region_name,
                             "\n(", nLabeller(sum(frequency), "well"), ")")) #%>%
# complete(nesting(region_name), category,
#          fill = list(frequency = 0, proportion = 0))

#regional bar chart plot with percentage on y and sample size labels
regional_bar_chart <- ggplot(sum_data_reg,
                             aes(x = fct_reorder2(region_lab, category, proportion), 
                                 y = proportion, fill = category)) + 
  geom_bar(stat = 'identity', alpha = 0.7) +
  coord_flip() +
  scale_fill_manual(name = "", values = colour.scale) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     expand = c(0, 0), limits = c(0, 1.04),
                     breaks = seq(0, 1, .2)) +
  theme_soe() +
  theme_barcharts +
  theme(panel.grid.major.y = element_blank(),
        axis.title.y = element_blank(),
        legend.text = element_text(size = 16),
        legend.position = "bottom",
        legend.direction = "vertical")


#combined bc & regional bar chart plots with one legend using cowplot + patchwork
bc_bar_nolegend <- bc_bar_chart + theme(legend.position = 'none')
regional_nolegend <- regional_bar_chart + theme(legend.position = 'none')

legend <- ggdraw(get_legend(regional_bar_chart + 
                              theme(legend.direction = "horizontal")))

combined_bc_summary <- bc_bar_nolegend + 
  regional_nolegend - 
  legend + 
  plot_layout(ncol = 1, heights = c(5, 0.5)) + 
  plot_annotation(caption = "*Note that only wells with enough data for trend analysis are included in these figures.", 
                  theme = theme(plot.caption = element_text(size = 14)))


## Save bar chart plots for Web
#bc bar chart
png_retina(filename = paste0(status.bc,'.png'), width = 500, height = 600, type = 'windows')
plot(bc_bar_chart)
dev.off()

svg_px(glue(status.bc, ".svg"), width = 500, height = 600)
plot(bc_bar_chart)
dev.off()

#regional bar chart
png_retina(glue(status.reg, ".png"), width = 500, height = 600, type = 'windows')
plot(regional_bar_chart)
dev.off()

svg_px(glue(status.reg, ".svg"), width = 500, height = 600)
plot(regional_bar_chart)
dev.off()

#bar charts combined bc + regions
png_retina(glue(status.reg.bc, ".png"), width = 900, height = 600, type = 'windows')
plot(combined_bc_summary)
dev.off()

svg_px(glue(status.reg.bc, ".svg"), width = 900, height = 600)
plot(combined_bc_summary)
dev.off()

#Individual Summary Plots for Each NR region (Web only)-------------------------

# Saved to a list object
regional_plots <- sum_data_reg %>%
  split(.$region_name_short) %>%
  map(~ ggplot(.) +
        geom_col(aes(category, proportion, fill = category), alpha = 0.7) +
        coord_flip() +
        scale_fill_manual(name = "", values = colour.scale) +
        scale_y_continuous(labels = percent_format(accuracy = 1),
                           expand = c(0,0), limits = c(0, 1.04),
                           breaks = seq(0, 1, .2)) +
        theme_soe() +
        theme_barcharts +
        theme(panel.grid.major.y = element_blank(),
              legend.position = "none",
              legend.text = element_text(size = 16),
              plot.margin = unit(c(6,12,6,2),"mm")))

# To look at one plot in list object:
# regional_plots[["Northeast"]]

# To look at all the plots in the list object:
# walk(regional_plots, ~ plot(.x))

## Save individual regional bar charts
for (i in seq_along(regional_plots)) {
  svg_px(file.path(status.reg.all, 
                   glue("summary_", names(regional_plots)[i], ".svg")), 
         width = 800, height = 400)
  plot(regional_plots[[i]])
  dev.off()
}

# Individual Obs Well Plots (Web & PDF) ----------------------------------------
well_plots <- monthlywells_ts %>%
  mutate(Well_Num1 = Well_Num#,
         #Date1 = Date
  ) %>% # both top level and nested data need Well_Num and Date as month
  nest(-Well_Num1, -EMS_ID, #-Date
  ) %>%
  rename(Well_Num = Well_Num1) %>%
  right_join(results_viz, by = c("Well_Num")) %>%
  mutate(colour = case_when(category == "Large Rate of Decline" ~ colour.scale[3],
                            category == "Stable or Increasing" ~ colour.scale[1],
                            TRUE ~ colour.scale[2]),
         state_chr = as.character(state)) %>% 
  mutate(month_plot = map(data, ~gwl_monthly_plot(dataframe = .x, splines = TRUE,
                                                  save = FALSE))) %>% 
  mutate(area_plot = pmap(list(data, trend_line_slope, trend_line_int, state_chr, sig),
                          ~gwl_area_plot(data = ..1, trend = ..2, intercept = ..3,
                                         trend_category = ..4, sig = ..5,
                                         showInterpolated = TRUE, save = FALSE,
                                         mkperiod = "annual", 
                                         show_stable_line = FALSE) +
                            theme(plot.title = element_text(lineheight = 1,
                                                            margin = margin(b = -10)),
                                  plot.subtitle = element_blank(),
                                  axis.title.x = element_blank(),
                                  plot.margin = unit(c(5, 1, 2, 5), units = "pt"),
                                  legend.box.spacing = unit(c(0, 0, 0, 0), units = "pt"),
                                  legend.margin = margin(0, 0, 0, 0),
                                  legend.position = "top")))


## Print Obs Well Plots
for (i in seq_len(nrow(well_plots))) {
  # # Month plots
  # svg_px(file.path(status.well,
  #                  glue("month_", well_plots$Well_Num[i], ".svg")),
  #        width = 350, height = 220)
  # plot(well_plots$month_plot[[i]])
  # dev.off()
  
  # Area plots
  svg_px(file.path(status.well,
                   glue("area_", well_plots$Well_Num[i], ".svg")),
         width = 600, height = 200)
  plot(well_plots$area_plot[[i]])
  dev.off()
}

if(update.PDF.print.version == T){
  ## Map Summary (for PDF print version)------------------------------------------
  
  #get natural resource regions
  bc <- bc_bound(class = "sf")
  nrr <- nr_regions(class = "sf")
  nrr_clip <- ms_clip(nrr, bc)
  nrr_simp <-  ms_simplify(nrr_clip) %>% 
    st_transform(3857)
  
  # Save nrr_simp for use in leaflet map
  write_rds(nrr_simp, "out/nr_polygons.rds")
  
  #Provincial summary map
  styles <- 'feature:all|element:all|saturation:-75'
  
  # Get British Columbia basemap
  # You will likely need to get an API key from google and enable it for the 
  # Maps Static API to get basemaps using ggmap. 
  # See help in ?ggmap::register_google and/or 
  # https://cloud.google.com/maps-platform/
  # If you save the key in your .Renviron file as a variable called `GMAP_KEY`
  # the following will work, otherwise just supply your key directly.
  
  ggmap::register_google(Sys.getenv("GMAP_KEY"))
  BCextent <- c(-139,48,-114,60)
  names(BCextent) <- c("left", "bottom", "right", "top")
  
  fourCorners <- expand.grid(
    as.data.frame(matrix(BCextent, ncol = 2, byrow = TRUE,
                         dimnames = list(NULL, c("Long", "Lat"))))
  )
  
  BCcenter <- c(mean(BCextent[c("left","right")]), 
                mean(BCextent[c("top","bottom")]))
  
  if (!nzchar("GMAP_KEY")) {
    ggMapBC <- get_googlemap(center = BCcenter, zoom = 5, scale = 1, 
                             maptype = 'roadmap', visible = fourCorners, 
                             style = styles)
  } else {
    ggMapBC <- get_map(location = BCcenter, zoom = 5, scale = 1, maptype = "terrain",
                       source = "stamen")
  }
  
  
  #tweak df for map plot
  results_map_df <- results_out %>% 
    mutate(category = recode(category, 
                             `N/A` = "Currently Not Enough Data for Trend Analysis"),
           category = factor(category, 
                             levels = c("Large Rate of Decline",
                                        "Moderate Rate of Decline",
                                        "Stable or Increasing",
                                        "Currently Not Enough Data for Trend Analysis"),
                             ordered = TRUE)) %>% 
    arrange(fct_rev(category)) %>% 
    bind_cols(st_as_sf(., crs = 4326, coords = c("Long", "Lat")) %>% 
                st_transform(3857) %>%
                st_coordinates() %>%
                as_tibble()) 
  
  #lines 169-172 above:
  #convert full df to an sf object, transform projection, extract coordinates, 
  #bind coordinates back to original df (tx Andy Teucher)
  
  #hard-code colours
  colrs <- c("Stable or Increasing" = "#deebf7",
             "Large Rate of Decline" = "#3182bd",
             "Moderate Rate of Decline" = "#9ecae1",
             "Currently Not Enough Data for Trend Analysis" = "grey80")
  
  legend_order <- names(colrs)
  
  #source function for aligning sf object with ggmap object
  devtools::source_gist("1467691edbc1fd1f7fbbabd05957cbb5", 
                        filename = "ggmap_sf.R")
  
  #plot
  summary_map <- ggmap_sf(ggMapBC, extent = "device") + 
    coord_map(xlim = c(-139, -114), ylim = c(47.8,60)) + 
    geom_sf(data = nrr_simp, fill = NA, inherit.aes = FALSE, size = 0.15) + 
    coord_sf(datum = NA) +
    geom_point(data = results_map_df, aes(x = X, y = Y, fill = category),
               shape = 21, size = 4, colour = "grey30") + 
    scale_fill_manual(values = colrs, breaks = legend_order) + 
    theme(legend.position = "bottom", legend.title = element_blank(),
          legend.direction = "vertical",
          legend.text = element_text(colour = "black", size = 11)) +
    guides(fill = guide_legend(ncol = 2))
  plot(summary_map)
  
  # #save list of well maps for gwl.Rmd
  #   save(summary_map, file="./tmp/map_data.RData")
  
  
  # Save Plots Objects------------------------------------------------------------
  
  #save plot objects to tmp folder for use in gwl.Rmd
  save(bc_bar_chart, regional_bar_chart, combined_bc_summary,
       regional_plots, summary_map, file = "tmp/figures.RData")
  
  
  ## Individual Observation Well Maps (PDF print version)-------------------------
  
  if (create_ggmaps) {
    
    #create list of well maps
    wellMaps <- vector("list", length(unique(results_viz$Well_Num)))
    names(wellMaps) <- unique(results_viz$Well_Num)
    for (w in names(wellMaps)) {
      well <- filter(results_viz, Well_Num == as.integer(w))
      wellMaps[[w]] <- tryCatch(get_googlemap(center = c(well$Long[1], well$Lat[1]), 
                                              zoom = 8, scale = 1,
                                              maptype = 'roadmap',
                                              style = styles), 
                                error = function(e) NULL)
    }
    
    #individual Obs Well ggmap plots 
    well_plots <- well_plots %>% 
      left_join(tibble(Well_Num = as.integer(names(wellMaps)), 
                       maps = wellMaps)) %>%
      mutate(map_plot = pmap(list(Long, Lat, colour, maps), 
                             ~ plot_point_with_inset(long = ..1, lat = ..2,
                                                     pointColour = ..3,
                                                     bigMap = ..4, 
                                                     overviewMap = ggMapBC,
                                                     overviewExtent = BCextent)))
    #save for use in .Rmd
    save(well_plots, file = "tmp/well_plots.RData")
    
  }
  
}

## Summary Stats for State & Category ------------------------------------------

summary_df_state <- results_viz %>% 
  group_by(state) %>% 
  summarize(total = n()) %>% 
  mutate(freq = round((total / sum(total))*100, digits = 0))

summary_df_category <- results_viz %>% 
  group_by(category) %>% 
  summarize(total = n()) %>% 
  mutate(freq = round((total / sum(total))*100, digits = 0))
}



