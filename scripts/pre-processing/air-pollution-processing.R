# AIR POLLUTION PREPROCESSING ---------------------------------------------

# get individual air pollution data set
get_air_pollution_km2_point_data <- function(ap_filepath, make_sp = TRUE) {
  
  ap <- read.csv(ap_filepath,
                 stringsAsFactors = FALSE,
                 skip = 5,
                 na.strings = "MISSING")
  
  # removed the missing data - these are in the sea and so not needed.
  ap <- na.omit(ap)
  
  # convert data.frame in to an sf object
  if(make_sp) {
    ap <- turn_ap_into_sf(ap)  
  }
  
  return(ap)
  
}

# convert air pollution data into spatial object with centroids coordinates OSGB grid by default
turn_ap_into_sf <- function(ap_data, crs_system = 27700) {
  ap_point <- st_as_sf(ap_data,
                       coords = c("x", "y"))
  
  ap_point <- st_set_crs(ap_point,
                         value = crs_system)
  
}

# convert centroids into square 1km2 polygon grids
make_ap_grid_data <- function(ap_point) {
  
  # buffer from mid-point
  ap_grid <- st_buffer(ap_point, dist = 500, endCapStyle = "SQUARE")
  
  # calculate area
  ap_grid <- ap_grid %>% 
    mutate(area = as.numeric(st_area(geometry)))
  
}


# read in all air pollution data files in the air-pollution/defra folder,
# and convert into grid polygons (using functions above)
combine_all_1km2_pollution_data <- function() {
  
  files <- list.files("data/pollution/air-pollution/", pattern = ".csv", full.names = TRUE)
  
  #first data set
  pm25_filepath = "data/pollution/air-pollution/mappm252022g.csv"
  ap <- get_air_pollution_km2_point_data(pm25_filepath, make_sp = FALSE)
  # just keep grid and x/y data (i.e. spine) for join in loop
  ap <- ap[1:3]
  
  for(f in files) {
    message(paste0("Reading ", f))
    new <- read.csv(f,
                    stringsAsFactors = FALSE,
                    skip = 5,
                    na.strings = "MISSING")
    new <- na.omit(new)
    colnames(new)[1:3] <- c("gridcode", "x", "y")
    ap <- left_join(ap, new, by = c("gridcode", "x", "y"))
  }
  
  ap_point <- turn_ap_into_sf(ap)
  ap_grid <- make_ap_grid_data(ap_point)
}

# function that intersects LSOA boundaries with the grid polygon and determine the area make-up of 
# each LSOA in terms of which grids fall within that LSOA and to what extent
intersect_boundaries_with_ap <- function(ap_grid, boundary_data) {
  
  #start timer
  tic("Intersect of lsoas with 1km2 grid complete")
  ap_to_boundary <- st_intersection(boundary_data, ap_grid)
  toc()
  
  ap_to_boundary <- ap_to_boundary %>% 
    mutate(area_weight = as.numeric(st_area(geometry)))
  
}

# use the proportion of grid area to calculate a mean weighted average air pollution concentration
# for each LSOA
calculate_area_weighted_mean_conc <- function(ap_to_boundary, cols, include_max = FALSE) {
  
  # calculate mean concentration in an LSOA based on different areas of 1km grid and pollution levels within that LSOA
  ap_lsoa <- ap_to_boundary %>% 
    st_drop_geometry() %>% 
    group_by(LSOA11CD) %>% 
    mutate(area_weight_prop = area_weight / lsoa_area) %>% 
    group_by(LSOA11CD) %>% 
    summarise(across(all_of(cols), \(x) weighted.mean(x, w = area_weight_prop), .names = "{col}_mean")) %>% 
    ungroup()
  
  # calculate max concentration within an LSOA based on different pollution levels in 1km2 grid data
  if(include_max) {
    ap_lsoa_max <- ap_to_boundary %>% 
      st_drop_geometry() %>% 
      group_by(LSOA11CD) %>% 
      summarise(across(all_of(cols), \(x) max(x), .names = "{col}_max")) %>% 
      ungroup()  
    
    ap_lsoa <- inner_join(ap_lsoa, ap_lsoa_max, by = c("LSOA11CD", "LSOA11NM"))
  }
  
  return(ap_lsoa)
  
}

# main function
calc_mean_ap_by_lsoa <- function() {
  
  tic("Air pollution matched to LSOA boundaries")
  
  # read in all air pollution data and make into grid (from points)
  ap_grid <- combine_all_1km2_pollution_data()
  
  # intersect with lsoa boundaries
  lsoa_boundaries <- get_lsoa_boundaries()
  lsoa_boundaries <- lsoa_boundaries %>% 
    mutate(lsoa_area = as.numeric(st_area(geometry)))
  
  ap_to_lsoa_intersect <- intersect_boundaries_with_ap(ap_grid, lsoa_boundaries)
  
  # aggregate air pollution data to LSOA
  cols <- c("no22022",
            "no22023",
            "pm252022g",
            "pm252023g",
            "nox2023",
            "pm102022g",
            "pm102023g")
  
  ap_lsoa <- calculate_area_weighted_mean_conc(ap_to_lsoa_intersect, cols)
  
  # end timer and return data set
  toc()
  
  # return LSOA data set
  return(ap_lsoa)
  
}

# compare estimated mean background concentrations in each LSOA with WHO guidelines
assess_against_who_ap_guidelines <- function(ap_lsoa,
                                             who_pm25 = 5,
                                             who_pm10 = 15,
                                             who_no2 = 10) {
  
  ap_lsoa <- ap_lsoa %>% 
    mutate(no2_who_ratio = round(no22023_mean / who_no2, 4),
           pm25_who_ratio = round(pm252023g_mean / who_pm25, 4),
           pm10_who_ratio = round(pm102023g_mean / who_pm10, 4))
  
}


# process Defra background air pollution for NO2, PM2.5 and PM10
ap_lsoa <- calc_mean_ap_by_lsoa()
# assess against WHO guidelines
ap_lsoa <- assess_against_who_ap_guidelines(ap_lsoa,
                                            who_pm25 = 5,
                                            who_pm10 = 15,
                                            who_no2 = 10)

# use WHO guideline comparison as a way of normalising and combining to make one air pollution indicator
ap_lsoa <- make_combined_ap_score(ap_lsoa,
                                  no2_wgt = 1,
                                  pm25_wgt = 1)

write.csv(ap_lsoa,
          "data/pollution/air-pollution-lsoa.csv",
          row.names = FALSE,
          na = "")

# save in Near You folder for use in this (should we save this script in NEar You and run idependently from there, too?)
write.csv(ap_lsoa,
          "../near-you-data-ingest/data/health/air-pollution/air-pollution-lsoa-from-imed.csv",
          row.names = FALSE,
          na = "")
