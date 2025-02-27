clear_all = TRUE
source("scripts/set-up.R")

#' We had data for three areas of noise:
#'  - roads (strategic noise mapping)
#'  - railways (strategic noise mapping)
#'  - airports (https://noise-map.com/) shared files by Nawar Halabi: nawar.halabi'at'gmail.com

#' We should determine where noise level is above >55, which means including all areas above 55, 
#' then a second higher level >65

#' Use daytime noise for all, although air ports is a 07:00-19:00 12hr window,
#' whereas road and rail is a 16hour daytime window


process_roadrail_noise_data <- function(noise_source = "road",
                                        filter_65db = FALSE) {

  if(!noise_source %in% c("road", "rail")) {
    stop("Incorrect noise source. Should be one of 'road' or 'rail'")
  }
  if(noise_source == "road") {
    noise_data <- st_read("../gis-data/noise/RoadNoiseLAeq16hRound3-SHP/Road_Noise_LAeq16h_England_Round_3.shp")
  }
  if(noise_source == "rail") {
    noise_data <- st_read("../gis-data/noise/RailNoiseLAeq16hRound3-SHP/Rail_Noise_LAeq16h_England_Round_3.shp")
  }
  
  noise_data <- noise_data %>% 
    mutate(noiseclass_55db = ">=55 dB",
           noiseclass_65db = ifelse(noiseclass %in% c("65.0-69.9", "70.0-74.9", ">=75.0"), ">=65 dB", "<65 dB"))
  
  if(filter_65db) {
    noise_data <- noise_data %>% filter(noiseclass_65db == ">=65 dB")  
  }
  
  return(noise_data)

}

process_roadrail_noise_data_Lden <- function(noise_source = "road",
                                             filter_65db = FALSE) {
  
  if(!noise_source %in% c("road", "rail")) {
    stop("Incorrect noise source. Should be one of 'road' or 'rail'")
  }
  if(noise_source == "road") {
    noise_data <- st_read("../gis-data/noise/RoadNoiseLdenRound3-SHP/data/Road_Noise_Lden_England_Round_3.shp")
  }
  if(noise_source == "rail") {
    noise_data <- st_read("../gis-data/noise/RailNoiseLdenRound3-SHP/data/Rail_Noise_Lden_England_Round_3.shp")
  }
  
  noise_data <- noise_data %>% 
    mutate(noiseclass_55db = ">=55 dB",
           noiseclass_65db = ifelse(noiseclass %in% c("65.0-69.9", "70.0-74.9", ">=75.0"), ">=65 dB", "<65 dB"))
  
  if(filter_65db) {
    noise_data <- noise_data %>% filter(noiseclass_65db == ">=65 dB")  
  }
  
  return(noise_data)
  
}

# air port data
process_airport_noise <- function(noise_time = "day", filter_65db = FALSE) {
  
  if(noise_time == "day") {
    airport_noise <- st_read("../gis-data/noise/airports/gb_day_Leq.geojson")  
  }
  if(noise_time == "Lden") {
    airport_noise <- st_read("../gis-data/noise/airports/gb_Lden.geojson")  
  }
  
  airport_noise <- airport_noise %>% 
    st_transform(crs = 27700) %>% 
    rename(noiseclass = title) %>% 
    mutate(noiseclass = trimws(noiseclass))
  
  airport_noise <- airport_noise %>% 
    mutate(noiseclass_45db = ">=45 dB",
           noiseclass_55db = ifelse(noiseclass %in% c("55.00-60.00", "60.00-65.00", "65.00-70.00", "70.00-75.00", "75.00-80.00", "80.00-85.00", "85.00-90.00"),
                                    ">=55 dB",
                                    "<55 dB"),
           noiseclass_65db = ifelse(noiseclass %in% c("65.00-70.00", "70.00-75.00", "75.00-80.00", "80.00-85.00", "85.00-90.00"),
                                    ">=65 dB",
                                    "<65 dB"))
  
  airport_noise <- airport_noise %>% 
    mutate(area = as.numeric(st_area(geometry))) %>% 
    filter(area > 0)
  
  table(airport_noise$noiseclass,
        airport_noise$noiseclass_45db,
        useNA = "ifany")
  table(airport_noise$noiseclass,
        airport_noise$noiseclass_55db,
        useNA = "ifany")
  table(airport_noise$noiseclass,
        airport_noise$noiseclass_65db,
        useNA = "ifany")
  
  airport_noise <- st_make_valid(airport_noise)
  
  if(filter_65db) {
    airport_noise <- airport_noise %>% filter(noiseclass_65db == ">=65 dB")
  }
  
  return(airport_noise)
  
}
make_all_noise <- function(noise_time = "Lden",
                           save_geopackage = FALSE) {
  
  if(noise_time == "day") {
    # make noise level data sets for each noise source
    noise_rail_65db <- process_roadrail_noise_data(noise_source = "rail",
                                                   filter_65db = TRUE)
    noise_rail_55db <- process_roadrail_noise_data(noise_source = "rail")
    
    noise_road_65db <- process_roadrail_noise_data(noise_source = "road",
                                                   filter_65db = TRUE)
    noise_road_55db <- process_roadrail_noise_data(noise_source = "road")
    
    noise_airport_65db <- process_airport_noise(noise_time, filter_65db = TRUE)
    noise_airport_45db <- process_airport_noise(noise_time)  
  }
  
  if(noise_time == "Lden") {
    # make noise level data sets for each noise source
    noise_rail_65db <- process_roadrail_noise_data_Lden(noise_source = "rail",
                                                        filter_65db = TRUE)
    noise_rail_55db <- process_roadrail_noise_data_Lden(noise_source = "rail")
    
    noise_road_65db <- process_roadrail_noise_data_Lden(noise_source = "road",
                                                        filter_65db = TRUE)
    noise_road_55db <- process_roadrail_noise_data_Lden(noise_source = "road")
    
    noise_airport_65db <- process_airport_noise(noise_time, filter_65db = TRUE)
    noise_airport_45db <- process_airport_noise(noise_time)  
  }
  
  #qtm(noise_airport_45db)
  
  # make each layer a multipolygon
  noise_rail_65db <- st_as_sf(st_union(noise_rail_65db))
  message("unionised noise_rail_65db into a multipolygon")
  noise_rail_55db <- st_as_sf(st_union(noise_rail_55db))
  message("unionised noise_rail_55db into a multipolygon")
  noise_road_65db <- st_as_sf(st_union(noise_road_65db))
  message("unionised noise_road_65db into a multipolygon")
  noise_road_55db <- st_as_sf(st_union(noise_road_55db))
  message("unionised noise_road_55db into a multipolygon")
  noise_airport_65db <- st_as_sf(st_union(noise_airport_65db))
  message("unionised noise_airport_65db into a multipolygon")
  noise_airport_45db <- st_as_sf(st_union(noise_airport_45db))
  message("unionised noise_airport_45db into a multipolygon")
  
  # combined into one unionised layer (multipolygon) of all noise
  all_noise_65db <- bind_rows(noise_rail_65db,
                              noise_road_65db,
                              noise_airport_65db)
  all_noise_65db <- st_as_sf(st_union(all_noise_65db))
  message("unionised all 65db noise into a multipolygon")
  
  all_noise_55db <- bind_rows(noise_rail_55db,
                              noise_road_55db,
                              noise_airport_45db)
  
  all_noise_55db <- st_as_sf(st_union(all_noise_55db))
  message("unionised all 55db noise into a multipolygon")
  
  all_noise_55db <- all_noise_55db %>% 
    mutate(noise_sources = "road, rail, airports",
           noise_level = "above 55 dB; Airports 45 db")
  
  all_noise_65db <- all_noise_65db %>% 
    mutate(noise_sources = "road, rail, airports",
           noise_level = "above 65 dB")
  
  
  # save in a geopackage
  if(save_geopackage) {
    noise_gpkg = "../gis-data/noise/combined-noise-pollution.gpkg"
    
    save_as_gpkg(gpkg_out = noise_gpkg,
                 layer_data = all_noise_55db,
                 layer_name = "all-noise-above-55db-lden")
    save_as_gpkg(gpkg_out = noise_gpkg,
                 layer_data = all_noise_65db,
                 layer_name = "all-noise-above-65db-lden")
    save_as_gpkg(gpkg_out = noise_gpkg, layer_data = noise_rail_55db, layer_name = "rail-noise-above-55db")
    save_as_gpkg(gpkg_out = noise_gpkg, layer_data = noise_rail_65db, layer_name = "rail-noise-above-65db")
    save_as_gpkg(gpkg_out = noise_gpkg, layer_data = noise_road_55db, layer_name = "road-noise-above-55db")
    save_as_gpkg(gpkg_out = noise_gpkg, layer_data = noise_road_65db, layer_name = "road-noise-above-65db")
    save_as_gpkg(gpkg_out = noise_gpkg, layer_data = noise_airport_45db, layer_name = "airport-noise-above-55db")
    save_as_gpkg(gpkg_out = noise_gpkg, layer_data = noise_airport_65db, layer_name = "airport-noise-above-65db")
  }
  
  # make and save lsoa layer
  noise_lsoa <- make_noise_lsoa_dataset(noise_road_55db,
                                        noise_road_65db,
                                        noise_rail_55db,
                                        noise_rail_65db,
                                        noise_airport_45db,
                                        noise_airport_65db,
                                        all_noise_55db,
                                        all_noise_65db,
                                        noise_time)
  
}

#' Thats the raw data sorted.
#' Now need to summarise at LSOA level. I suggest the following:
#'   Area of LSOA exposed to >55 dB (all sources)
#'   Area of LSOA exposed to >65 dB (all sources)
#'   Area of LSOA exposed to >55 dB (split by road, rail, airport)
#'   Area of LSOA exposed to >65 dB (split by road, rail, airport)
#' And then calculate the following from using total LSOA area as demoninator
#'   % of LSOA (area) exposed to >55 dB (all sources)
#'   % of LSOA (area) exposed to >65 dB (all sources)
#'   % of LSOA (area) exposed to >55 dB (split by road, rail, airport)
#'   % of LSOA (area) exposed to >65 dB (split by road, rail, airport)
make_noise_lsoa_dataset <- function(noise_road_55db,
                                    noise_road_65db,
                                    noise_rail_55db,
                                    noise_rail_65db,
                                    noise_airport_45db,
                                    noise_airport_65db,
                                    all_noise_55db,
                                    all_noise_65db,
                                    noise_time = "Lden") {
  
  lsoa_boundaries <- get_lsoa_boundaries()
  
  noise_road_55db_lsoa <- st_intersection(lsoa_boundaries, noise_road_55db)
  noise_road_65db_lsoa <- st_intersection(lsoa_boundaries, noise_road_65db)
  noise_rail_55db_lsoa <- st_intersection(lsoa_boundaries, noise_rail_55db)
  noise_rail_65db_lsoa <- st_intersection(lsoa_boundaries, noise_rail_65db)
  noise_airport_45db_lsoa <- st_intersection(lsoa_boundaries, noise_airport_45db)
  noise_airport_65db_lsoa <- st_intersection(lsoa_boundaries, noise_airport_65db)
  all_noise_55db_lsoa <- st_intersection(lsoa_boundaries, all_noise_55db)
  all_noise_65db_lsoa <- st_intersection(lsoa_boundaries, all_noise_65db)
  
  
  group_noise_by_lsoa <- function(noise_lsoa_intersect, noise_type, noise_time) {
    
    noise_by_lsoa <- noise_lsoa_intersect %>% 
      mutate(noise_area_m2 = as.numeric(st_area(geometry))) %>% 
      st_drop_geometry() %>% 
      group_by(lsoa11 = LSOA11CD) %>% 
      summarise("{{ noise_type }}_area_m2" := sum(noise_area_m2)) %>% 
      ungroup()
    
  }
  
  noise_road_55db_lsoa_sum <- group_noise_by_lsoa(noise_road_55db_lsoa, road_55db)
  noise_road_65db_lsoa_sum <- group_noise_by_lsoa(noise_road_65db_lsoa, road_65db)
  noise_rail_55db_lsoa_sum <- group_noise_by_lsoa(noise_rail_55db_lsoa, rail_55db)
  noise_rail_65db_lsoa_sum <- group_noise_by_lsoa(noise_rail_65db_lsoa, rail_65db)
  noise_airport_45db_lsoa_sum <- group_noise_by_lsoa(noise_airport_45db_lsoa, airport_55db)
  noise_airport_65db_lsoa_sum <- group_noise_by_lsoa(noise_airport_65db_lsoa, airport_65db)
  all_noise_55db_lsoa_sum <- group_noise_by_lsoa(all_noise_55db_lsoa, all_noise_55db)
  all_noise_65db_lsoa_sum <- group_noise_by_lsoa(all_noise_65db_lsoa, all_noise_65db)
  
  lsoa_spine <- lsoa_boundaries %>% 
    transmute(lsoa11 = LSOA11CD,
              lsoa_total_area_m2 = as.numeric(st_area(geometry))) %>% 
    st_drop_geometry()
  
  noise_pollution_lsoa <- Reduce(function(x, y) left_join(x, y, by = "lsoa11"),
                                 list(lsoa_spine,
                                      noise_road_55db_lsoa_sum,
                                      noise_road_65db_lsoa_sum,
                                      noise_rail_55db_lsoa_sum,
                                      noise_rail_65db_lsoa_sum,
                                      noise_airport_45db_lsoa_sum,
                                      noise_airport_65db_lsoa_sum,
                                      all_noise_55db_lsoa_sum,
                                      all_noise_65db_lsoa_sum)) 
  
  noise_pollution_lsoa <- noise_pollution_lsoa %>%  
    mutate(road_55db_pct = road_55db_area_m2 / lsoa_total_area_m2,
           road_65db_pct = road_65db_area_m2 / lsoa_total_area_m2,
           rail_55db_pct = rail_55db_area_m2 / lsoa_total_area_m2,
           rail_65db_pct = rail_65db_area_m2 / lsoa_total_area_m2,
           airport_55db_pct = airport_55db_area_m2 / lsoa_total_area_m2,
           airport_65db_pct = airport_65db_area_m2 / lsoa_total_area_m2,
           noise_all_55kb_pct = all_noise_55db_area_m2 / lsoa_total_area_m2,
           noise_all_65kb_pct = all_noise_65db_area_m2 / lsoa_total_area_m2)
  
  noise_pollution_lsoa[is.na(noise_pollution_lsoa)] <- 0
  
  if(noise_time == "day") {
    save_filepath = "data/pollution/noise-pollution-day-Leq-lsoa-45db-airports.csv"
    message("Saving: ", save_filepath)
    write.csv(noise_pollution_lsoa,
              file = save_filepath,
              row.names = FALSE,
              na = "")  
  }
  if(noise_time == "Lden") {
    save_filepath = "data/pollution/noise-pollution-lden-lsoa-45db-airports.csv"
    message("Saving: ", save_filepath)
    write.csv(noise_pollution_lsoa,
              file = save_filepath,
              row.names = FALSE,
              na = "")  
  }
  
  return(noise_pollution_lsoa)
}


add_lsoa_layer_to_noise_gpkg <- function(noise_pollution_lsoa) {

  noise_pollution_lsoa_sf <- left_join(lsoa_boundaries, noise_pollution_lsoa, by = c("LSOA11CD" = "lsoa11"))
  noise_gpkg = "../gis-data/noise/combined-noise-pollution.gpkg"
  save_as_gpkg(gpkg_out = noise_gpkg,
               layer_data = noise_pollution_lsoa_sf,
               layer_name = "noise-lsoa")

}

noise_lsoa <- make_all_noise(noise_time = "Lden",
                             save_geopackage = FALSE)
