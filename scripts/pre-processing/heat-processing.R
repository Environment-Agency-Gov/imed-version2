#install.packages("ncdf4")
clear_all = TRUE
source("scripts/set-up.R")
gc()
#library(raster)



# Function to access time series grid data from CHESS SCAPE downloads ----------
# extracts one variable at a time

process_chess_scape_nc <- function(nc_filepath,
                                   var,
                                   keep_decades_only = TRUE,
                                   timeslice = FALSE,
                                   timeslice_years = NA) {
  # start clock
  tic("Build complete: made climate data frame from Chess Scape NC data.")
  
  nc <- nc_open(nc_filepath)
  # 7 variables: lon, lat, time_bnds, x_bnds, y_bnds, crsOSGB, tas
  # get data for lon, lat
  lon <- ncvar_get(nc, "lon")
  lat <- ncvar_get(nc, "lat")
  
  # convert matrixes into long lists (one wide)
  lon_long <- as.vector(lon)
  lat_long <- as.vector(lat)
  
  if(!timeslice) {
    # get time intervals
    # time_start unit: "hours since 1970-01-01T00:00:00Z" using 360 day calendar
    # convert hours since 1970 to dates
    # uses a 360 day calendar, so need special package and function PCICt
    hours <- ncvar_get(nc, "time_bnds")[1,] # time bands has start and end time, just take start
    seconds = as.numeric(hours) * 3600
    date = as.PCICt(x = seconds,
                    origin = "1970-01-01",
                    cal = "360")
    
    # expand coordinates with time band to get full extend
    spine <- expand.grid(lat = lat_long, date = date)
  } 
  
  # get main indicator
  var_name = deparse(substitute(var))
  message(paste0("Making array for: ", var_name))
  nc_array <- ncvar_get(nc, var_name)
  # make long list of tas values (same as above for coordinates)
  nc_array <- as.vector(nc_array)
  
  if(!timeslice) {
    # bring all together in a data frame
    message("Building core nc_df")
    nc_df <- data.frame(lon = lon_long, spine, nc_main_var = nc_array)
    rm(spine, lat, lat_long, lon, lon_long, nc, nc_array)
    gc()
  } else {
    nc_df <- data.frame(lon = lon_long, lat = lat_long, nc_main_var = nc_array)
  }
  
  # remove NA values
  nc_df <- nc_df %>% 
    filter(!is.na(nc_main_var))
  
  # convert K to degC if var is a temp
  if(grepl("tas", var_name)) {
    message("Converting temperatures to °C from °K.")
    nc_df <- nc_df %>% 
      mutate(nc_main_var := round(nc_main_var - 273.15, 2))  
  }
  
  # rename var to correct name
  nc_df <- nc_df %>% 
    rename({{ var }} := nc_main_var)
  
  if(!timeslice) {
    # calculate year, month and season
    message("Adding date details")
    date_df <- data.frame(date = date)
    
    date_df <- date_df %>% 
      mutate(year = as.numeric(format(date, "%Y"))) %>% 
      mutate(month = as.numeric(format(date, "%m"))) %>% 
      mutate(season = case_when(between(month,3,5) ~ "spring",
                                between(month,6,8) ~ "summer",
                                between(month,9,11) ~ "autumn",
                                (month == 12 | between(month,1,2)) ~ "winter"))
    
    # add year, month and season to main data
    nc_df <- left_join(nc_df, date_df, by = "date")
  } else {
    
    message("Adding timeslice date details")
    
    nc_df <- nc_df %>%
      arrange(lat, lon) %>% 
      group_by(lat, lon) %>% 
      mutate(month = row_number()) %>% 
      ungroup()
    
    nc_df <- nc_df %>% 
      mutate(year = timeslice_years) %>% 
      mutate(season = case_when(between(month,3,5) ~ "spring",
                              between(month,6,8) ~ "summer",
                              between(month,9,11) ~ "autumn",
                              (month == 12 | between(month,1,2)) ~ "winter"))
    
  }
  
  # select final fields - ignored for timeslice data
  if(keep_decades_only) {
    message("Filtering for decade years only (e.g. 2000, 2010, 2020, etc.)")
    nc_df <- nc_df %>% 
      filter(year %% 10 == 0)
  }
  
  # select final fields 
  nc_df <- nc_df %>% 
    select(lon,
           lat,
           #date,
           year,
           month,
           season,
           {{ var }})
  
  # stop timer and report
  toc()
  return(nc_df)

}


# Get timeslice data - smaller datasets with average monthly temperatures over 20 year period
tasmax_45_timeslice_2020_2040 <- process_chess_scape_nc(nc_filepath = "../gis-data/climate/chess-scape/downloads/chess-scape_rcp45_bias-corrected_01_tasmax_uk_1km_timeslice_20201201-20401130.nc",
                                                        var = tasmax,
                                                        keep_decades_only = FALSE,
                                                        timeslice = TRUE,
                                                        timeslice_years = "2020-2040")

tasmax_60_timeslice_2020_2040 <- process_chess_scape_nc(nc_filepath = "../gis-data/climate/chess-scape/downloads/chess-scape_rcp60_bias-corrected_01_tasmax_uk_1km_timeslice_20201201-20401130.nc",
                                                        var = tasmax,
                                                        keep_decades_only = FALSE,
                                                        timeslice = TRUE,
                                                        timeslice_years = "2020-2040")

tasmax_85_timeslice_2020_2040 <- process_chess_scape_nc(nc_filepath = "../gis-data/climate/chess-scape/downloads/chess-scape_rcp85_bias-corrected_01_tasmax_uk_1km_timeslice_20201201-20401130.nc",
                                                        var = tasmax,
                                                        keep_decades_only = FALSE,
                                                        timeslice = TRUE,
                                                        timeslice_years = "2020-2040")

tasmax_45_timeslice_2040_2060 <- process_chess_scape_nc(nc_filepath = "../gis-data/climate/chess-scape/downloads/chess-scape_rcp45_bias-corrected_01_tasmax_uk_1km_timeslice_20401201-20601130.nc",
                                                        var = tasmax,
                                                        keep_decades_only = FALSE,
                                                        timeslice = TRUE,
                                                        timeslice_years = "2040-2060")

tasmax_60_timeslice_2040_2060 <- process_chess_scape_nc(nc_filepath = "../gis-data/climate/chess-scape/downloads/chess-scape_rcp60_bias-corrected_01_tasmax_uk_1km_timeslice_20401201-20601130.nc",
                                                        var = tasmax,
                                                        keep_decades_only = FALSE,
                                                        timeslice = TRUE,
                                                        timeslice_years = "2040-2060")

tasmax_85_timeslice_2040_2060 <- process_chess_scape_nc(nc_filepath = "../gis-data/climate/chess-scape/downloads/chess-scape_rcp85_bias-corrected_01_tasmax_uk_1km_timeslice_20401201-20601130.nc",
                                                        var = tasmax,
                                                        keep_decades_only = FALSE,
                                                        timeslice = TRUE,
                                                        timeslice_years = "2040-2060")

haduk_grid1km_2022 <- process_chess_scape_nc(nc_filepath = "../gis-data/climate/met-office/tasmax_hadukgrid_uk_1km_mon_202201-202212.nc",
                                                        var = tasmax,
                                                        keep_decades_only = FALSE,
                                                        timeslice = TRUE,
                                                        timeslice_years = "2022")



make_maxtemp_timeslice_gis <- function(tasmax_timeslice,
                                       layer_name) {
  
  tasmax_timeslice <- tasmax_timeslice %>% 
    group_by(lon, lat) %>% 
    slice_max(order_by = tasmax,
              n = 1) %>% 
    ungroup()
  
  tasmax_timeslice_gis <- tasmax_timeslice %>% 
    st_as_sf(coords = c("lon", "lat"))
  
  tasmax_timeslice_gis <- tasmax_timeslice_gis %>% 
    st_set_crs(4326) %>% 
    st_transform(crs = 27700) 
  
  tasmax_timeslice_gis <- tasmax_timeslice_gis %>% 
    st_buffer(dist = 500,
              endCapStyle = "SQUARE")
  
  message(paste0("Saving: '", layer_name))
  st_write(dsn = "../gis-data/climate/chess-scape-timeslice.gpkg",
           obj = tasmax_timeslice_gis,
           layer = layer_name,
           delete_layer = TRUE,
           delete_dsn = FALSE,
           quiet = TRUE)
  
}

make_maxtemp_timeslice_gis(tasmax_timeslice = tasmax_45_timeslice_2020_2040,
                           layer_name = "tasmax-max-4.5-2020-40")
make_maxtemp_timeslice_gis(tasmax_timeslice = tasmax_45_timeslice_2040_2060,
                           layer_name = "tasmax-max-4.5-2040-60")

make_maxtemp_timeslice_gis(tasmax_timeslice = tasmax_60_timeslice_2020_2040,
                           layer_name = "tasmax-max-6.0-2020-40")
make_maxtemp_timeslice_gis(tasmax_timeslice = tasmax_60_timeslice_2040_2060,
                           layer_name = "tasmax-max-6.0-2040-60")

make_maxtemp_timeslice_gis(tasmax_timeslice = tasmax_85_timeslice_2020_2040,
                           layer_name = "tasmax-max-8.5-2020-40")
make_maxtemp_timeslice_gis(tasmax_timeslice = tasmax_85_timeslice_2040_2060,
                           layer_name = "tasmax-max-8.5-2040-60")

# align tasmax data with LSOA boundaries
align_timeslice_tas_with_lsoa <- function(layer_name,
                                          file_output_name = "test",
                                          add_to_gpkg = FALSE) {
  
  # read in a timeslice layer from the geopackage created above
  tasmax_gis <- st_read("../gis-data/climate/chess-scape/processing-outputs/chess-scape-timeslice.gpkg",
                        layer = layer_name,
                        quiet = TRUE)
  
  # get LSOA boundaries and just keep the code and geometry
  lsoa_boundaries <- get_lsoa_boundaries()
  lsoa_boundaries <- lsoa_boundaries %>% 
    select(LSOA11CD,
           geometry)
  
  # intersect LSOA boundaries with tasmax grid
  tic(paste0("Aligning '", layer_name, "' with LSOA boundaries"))
  tasmas_lsoa_intersect <- st_intersection(lsoa_boundaries, tasmax_gis)
  toc()
  
  # find area weighted average tasmax for each LSOA
  tasmax_by_lsoa <- tasmas_lsoa_intersect %>% 
    mutate(area = as.numeric(st_area(geometry))) %>% 
    st_drop_geometry() %>% 
    group_by(LSOA11CD) %>% 
    mutate(area_pct = area / sum(area)) %>% 
    group_by(LSOA11CD) %>% 
    summarise(tasmax_avg = weighted.mean(tasmax, w = area_pct)) %>% 
    ungroup()
  
  # make a full LSOA spine
  lsoa_spine <- st_drop_geometry(lsoa_boundaries)
  
  # join to lsoa intersected data to find any missing data points
  tasmax_by_lsoa <- left_join(lsoa_spine, tasmax_by_lsoa, by = "LSOA11CD")

  # inmpute missing values from neighbouring LSOAs
  tasmax_by_lsoa <- complete_missing_tas_lsoas(tasmax_by_lsoa, lsoa_boundaries)
    
  # save outputs
  full_file_output_name <- paste0("data/climate/", file_output_name, ".csv")
  tic(paste0("Saved csv file as: ", full_file_output_name))
  write.csv(tasmax_by_lsoa,
            full_file_output_name,
            row.names = FALSE,
            na = "")
  toc()
  
  if(add_to_gpkg) {
    tic(paste0("Saved GPKG layer as: ", file_output_name))
    tasmax_by_lsoa_gis <- left_join(lsoa_boundaries, tasmax_by_lsoa, by = "LSOA11CD")
    st_write(dsn = "../gis-data/climate/chess-scape/processing-outputs/chess-scape-timeslice.gpkg",
             obj = tasmax_by_lsoa_gis,
             layer = file_output_name,
             delete_layer = TRUE,
             delete_dsn = FALSE,
             quiet = TRUE)
    toc()
    }
  
  return(tasmax_by_lsoa)
  
}

complete_missing_tas_lsoas <- function(tasmax_by_lsoa, lsoa_boundaries) {
  
  # start timer
  tic("Correcting missing LSOA values")
  
  # find missing data lsoas
  lsoas_missing <- tasmax_by_lsoa %>% 
    filter(is.na(tasmax_avg)) %>% 
    transmute(LSOA11CD,
              missing = TRUE)
  
  # make into gis object
  lsoas_missing <- inner_join(lsoa_boundaries, lsoas_missing, by = "LSOA11CD")
  # buffer by 1m and then see which areas it intersect
  lsoas_missing <- lsoas_missing %>% 
    st_buffer(dist = 5)
  
  # identify adjacent neighbours
  lsoa_boundaries <- lsoa_boundaries %>% 
    rename(LSOA11CD_adjacent = LSOA11CD)
  
  lsoas_missing <- st_intersection(lsoas_missing, lsoa_boundaries)
  lsoas_missing <- lsoas_missing %>% 
    st_drop_geometry()
  
  # the isles of scilly has no adjacent LSOA, so set to lands end one as the adjacent
  lsoas_missing <- lsoas_missing %>% 
    mutate(LSOA11CD_adjacent = ifelse(LSOA11CD == "E01019077", "E01019005", LSOA11CD_adjacent))
  
  lsoa_missing_list <- unique(lsoas_missing$LSOA11CD)
  lsoas_missing <- lsoas_missing %>% 
    filter(!LSOA11CD_adjacent %in% lsoa_missing_list)
  
  # add in the data for adjacent LSOAs
  lsoas_missing <- left_join(lsoas_missing, tasmax_by_lsoa, by = c("LSOA11CD_adjacent" = "LSOA11CD"))
  
  lsoas_missing_values <- lsoas_missing %>% 
    group_by(LSOA11CD) %>% 
    summarise(tasmax_avg = round(mean(tasmax_avg, na.rm = TRUE), 4)) %>% 
    ungroup()
  
  # remove missing value rows from main data and append new data 
  tasmax_by_lsoa_final <- tasmax_by_lsoa %>% 
    filter(!is.na(tasmax_avg)) %>% 
    bind_rows(., lsoas_missing_values)
  
  # stop timer, report and return outputs
  toc()
  return(tasmax_by_lsoa_final)
  
}

tasmax_45_2020_2040_lsoa <- align_timeslice_tas_with_lsoa(layer_name = "tasmax-max-4.5-2020-40",
                                                          file_output_name = "tasmax-4.5-2020-2040-lsoa",
                                                          add_to_gpkg = TRUE)
tasmax_60_2020_2040_lsoa <- align_timeslice_tas_with_lsoa(layer_name = "tasmax-max-6.0-2020-40",
                                                          file_output_name = "tasmax-6.0-2020-2040-lsoa",
                                                          add_to_gpkg = FALSE)
tasmax_85_2020_2040_lsoa <- align_timeslice_tas_with_lsoa(layer_name = "tasmax-max-8.5-2020-40",
                                                          file_output_name = "tasmax-8.5-2020-2040-lsoa",
                                                          add_to_gpkg = TRUE)

tasmax_45_2040_2060_lsoa <- align_timeslice_tas_with_lsoa(layer_name = "tasmax-max-4.5-2040-60",
                                                          file_output_name = "tasmax-4.5-2040-2060-lsoa",
                                                          add_to_gpkg = TRUE)
tasmax_60_2040_2060_lsoa <- align_timeslice_tas_with_lsoa(layer_name = "tasmax-max-6.0-2040-60",
                                                          file_output_name = "tasmax-6.0-2040-2060-lsoa",
                                                          add_to_gpkg = FALSE)
tasmax_85_2040_2060_lsoa <- align_timeslice_tas_with_lsoa(layer_name = "tasmax-max-8.5-2040-60",
                                                          file_output_name = "tasmax-8.5-2040-2060-lsoa",
                                                          add_to_gpkg = TRUE)


hist(tasmax_45_2020_2040_lsoa$tasmax_avg)
hist(tasmax_60_2020_2040_lsoa$tasmax_avg)
hist(tasmax_85_2020_2040_lsoa$tasmax_avg)

hist(tasmax_45_2040_2040_lsoa$tasmax_avg)
hist(tasmax_60_2040_2060_lsoa$tasmax_avg)
hist(tasmax_85_2040_2060_lsoa$tasmax_avg)


# MET OFFICE DATA PROCESSING ----------------------------------------------

get_metoffice_haduk_data <- function(nc_filepath,
                                     var) {
  
  nc <- nc_open(nc_filepath)
  names(nc$var)
  
  # get data for lon, lat
  lon <- ncvar_get(nc, "longitude")
  lat <- ncvar_get(nc, "latitude")
  
  # convert matrixes into long lists (one wide)
  lon_long <- as.vector(lon)
  lat_long <- as.vector(lat)
  
  # HadUK uses standard calendar
  hours <- ncvar_get(nc, "time_bnds")[1,] # time bands has start and end time, just take start
  seconds = as.numeric(hours) * 3600
  date = as.PCICt(x = seconds,
                  origin = "1800-01-01",
                  cal = "standard")
  
  # expand coordinates with time band to get full extend
  spine <- expand.grid(lat = lat_long, date = date)
  
  # get main indicator
  var_name = deparse(substitute(var))
  message(paste0("Making array for: ", var_name))
  nc_array <- ncvar_get(nc, var_name)
  # make long list of tas values (same as above for coordinates)
  nc_array <- as.vector(nc_array)
  
  # build full data set
  nc_df <- data.frame(lon = lon_long, spine, nc_main_var = nc_array)
  
  # remove null values
  nc_df <- nc_df %>% 
    filter(!is.na(nc_main_var))
  
  # rename var to correct name
  nc_df <- nc_df %>% 
    rename({{ var }} := nc_main_var)
  
  # add date details
  date_df <- data.frame(date = date)
  
  date_df <- date_df %>% 
    mutate(year = as.numeric(format(date, "%Y"))) %>% 
    mutate(month = as.numeric(format(date, "%m"))) %>% 
    mutate(season = case_when(between(month,3,5) ~ "spring",
                              between(month,6,8) ~ "summer",
                              between(month,9,11) ~ "autumn",
                              (month == 12 | between(month,1,2)) ~ "winter"))
  
  # add year, month and season to main data
  nc_df <- left_join(nc_df, date_df, by = "date")
  
  
  
}

haduk_2022_tasmax <- get_metoffice_haduk_data(nc_filepath = "../gis-data/climate/met-office/tasmax_hadukgrid_uk_1km_mon_202201-202212.nc",
                                              var = tasmax)

haduk_2022_tas <- get_metoffice_haduk_data(nc_filepath = "../gis-data/climate/met-office/tas_hadukgrid_uk_1km_mon_202201-202212.nc",
                                              var = tas)
summary(haduk_2022_tas$tas)
summary(haduk_2022_tasmax$tasmax)

make_maxtemp_timeslice_gis(tasmax_timeslice = haduk_2022_tasmax,
                           layer_name = "haduk-tasmax-2020")


# other CHESS SCAPE data processing ---------------------------------------

# make wide table
process_big_chess_scape_datafiles <- function() {
  # get the seasonable tasmax data for RCP 4.5
  tasmax_45_seasonal <- process_chess_scape_nc(nc_filepath = "../gis-data/climate/chess-scape_rcp45_bias-corrected_01_tasmax_uk_1km_seasonal_19801201-20801130.nc",
                                               var = tasmax)
  saveRDS(tasmax_45_seasonal,
          "../gis-data/climate/tasmax_uk_1km_seasonal_1990_2080.rds")
  
  # get the monthly tasmax data for RCP 4.5
  ### NOT ENOUGH RAM FOR THIS. FALLS OVER
  tasmax_45_monthly <- process_chess_scape_nc(nc_filepath = "../gis-data/climate/chess-scape_rcp45_bias-corrected_15_tasmax_uk_1km_monthly_19801201-20801130.nc",
                                              var = tasmax)
  saveRDS(tasmax_45_monthly,
          "../gis-data/climate/tasmax_uk_1km_monthly_1990_2080.rds")
}


make_tasmax_wide <- function(taxmax_hc_seasonal,
                             rds_filename = "tasmax_uk_1km_seasonal_1990_2080_wide.rds") {
  
  taxmax_hc_seasonal_wide <- taxmax_hc_seasonal %>% 
    select(-date,
           -month) %>% 
    spread(key = season,
           value = tasmax)
  
  full_rds_filename <- paste0("../gis-data/climate/", rds_filename)
  message(paste0("Saving: ", full_rds_filename))
  saveRDS(taxmax_hc_seasonal_wide,
          full_rds_filename)
  
}


  
make_chess_scape_gis <- function(hc_df, data_year) {
  
  hc_df <- hc_df %>% 
    filter(year == data_year)
  
  hc_df_gis <- hc_df %>% 
    st_as_sf(coords = c("lon", "lat"))
  
  hc_df_gis <- hc_df_gis %>% 
    st_set_crs(4326) %>% 
    st_transform(crs = 27700) 
  
  hc_df_gis <- hc_df_gis %>% 
    st_buffer(dist = 500,
              endCapStyle = "SQUARE")
  
  dir.create("../gis-data/climate/tasmax_uk_1km_seasonal_2050_foe_bng/")
  st_write(dsn = "../gis-data/climate/tasmax_uk_1km_seasonal_2050_foe_bng/tasmax_uk_1km_seasonal_2050_foe_bng.shp",
           obj = hc_df_gis,
           delete_dsn = TRUE,
           delete_layer = TRUE)
  
}