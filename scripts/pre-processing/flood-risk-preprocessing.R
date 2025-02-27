# ENVIRONMENT AGENCY DATA: RISK OF FLOODING FROM RIVERS AND SEAS ----------

make_risk_of_flooding_by_river_sea_lsoa <- function() {
  
  # get flood data (which has risk from v.low, low, medium, to high)
  flooding_river_sea <- st_read("../gis-data/water/fulldataset-riskoffloodingfromriversandsea_Shapefile-output/fulldataset-riskoffloodingfromriversandsea_Shapefile-output/ea_risk_of_flooding_from_rivers_and_sea.shp")
  # get lsoa boundaries
  lsoa_boundaries <- get_lsoa_boundaries()
  # then intersect to see which parts of each lsoa fall into v.low, low, medium, high
  flooding_river_sea_lsoa <- st_intersection(lsoa_boundaries, flooding_river_sea)
  # divide up lsoa (one row per lsoa) and calculate proportion of area in each category, plus total flood risk.
  flooding_river_sea_lsoa_summary <- flooding_river_sea_lsoa %>% 
    mutate(area = as.numeric(st_area(geometry))) %>% 
    st_drop_geometry() %>% 
    group_by(lsoa11 = LSOA11CD,
           prob_4band) %>% 
    summarise(area = sum(area, na.rm = TRUE)) %>% 
    ungroup()
  
  flooding_river_sea_lsoa_summary <- flooding_river_sea_lsoa_summary %>% 
    gather(key = indicator,
           value = val,
           -lsoa11,
           -prob_4band) %>% 
    mutate(risk = "risk") %>% 
    unite(flood_indicator, prob_4band, risk, indicator, sep = "_") %>% 
    mutate(flood_indicator = tolower(gsub(" ", "_", flood_indicator))) %>%
    spread(key = flood_indicator,
           value = val,
           fill = 0)
  
  flooding_river_sea_lsoa_summary <- flooding_river_sea_lsoa_summary %>% 
    mutate(medium_high_risk_area = medium_risk_area + high_risk_area) %>% 
    mutate(low_to_high_risk_area = medium_high_risk_area + low_risk_area) %>% 
    mutate(flood_risk_all_area = low_to_high_risk_area + very_low_risk_area)
  
  lsoa_areas_all <- lsoa_boundaries %>% 
    mutate(lsoa_area = as.numeric(st_area(geometry))) %>% 
    st_drop_geometry()
  
  lsoa_areas_all <- lsoa_areas_all %>% 
    select(lsoa11 = LSOA11CD,
           lsoa_area)
  
  flooding_river_sea_lsoa_summary <- left_join(lsoa_areas_all, flooding_river_sea_lsoa_summary, by = "lsoa11")
  
  flooding_river_sea_lsoa_summary[is.na(flooding_river_sea_lsoa_summary)] <- 0
  
  flooding_river_sea_lsoa_summary <- flooding_river_sea_lsoa_summary %>% 
    mutate(high_risk_area_pct = high_risk_area / lsoa_area,
           medium_high_risk_area_pct = medium_high_risk_area / lsoa_area,
           low_to_high_risk_area_pct = low_to_high_risk_area / lsoa_area,
           flood_risk_all_area_pct = flood_risk_all_area / lsoa_area)
  
  flooding_river_sea_lsoa_summary <- filter_for_england(flooding_river_sea_lsoa_summary, lsoa11)
  
  summary(flooding_river_sea_lsoa_summary$flood_risk_all_area_pct)
  
  write.csv(flooding_river_sea_lsoa_summary,
            "data/flooding/EA-flooding-risk-river-sea-lsoa.csv",
            row.names = FALSE)
  
  # make into a geopackage
  lsoa_boundaries <- lsoa_boundaries %>% 
    select(lsoa11 = LSOA11CD,
           geometry)
  
  flooding_river_sea_lsoa_gis <- left_join(lsoa_boundaries, flooding_river_sea_lsoa_summary, by = "lsoa11")
  
  save_as_gpkg(layer_data = flooding_river_sea_lsoa_gis,
               layer_name = "flooding-risk-lsoa")
  
  return(flooding_river_sea_lsoa_summary)
  
}


# ENVIRONMENT AGENCY DATA: RISK OF FLOODING FROM SURFACE WATERS -----------

risk_of_flooding_from_surface_water_lsoa_1_in_30 <- function() {
  
  # get flood data (which has risk from v.low, low, medium, to high)
  st_layers("../gis-data/water/uFMfSW_Banded_Vector_National_Extents.gdb")
  flooding_surface_water <- st_read("../gis-data/water/uFMfSW_Banded_Vector_National_Extents.gdb",
                                    layer = "uFMfSW_ENW_EXTENT_1in30_BV")
  # View(head(flooding_surface_water, 10))
  
  # get lsoa boundaries
  lsoa_boundaries <- get_lsoa_boundaries()
  lsoa_boundaries <- lsoa_boundaries %>% 
    select(LSOA11CD,
           geometry)
  # then intersect to see which parts of each lsoa fall into v.low, low, medium, high
  flooding_surface_water_lsoa <- st_intersection(lsoa_boundaries, flooding_surface_water)
  # divide up lsoa (one row per lsoa) and calculate proportion of area in each category, plus total flood risk.
  flooding_surface_water_lsoa_summary <- flooding_surface_water_lsoa %>% 
    mutate(area = as.numeric(st_area(geometry))) %>% 
    st_drop_geometry() %>% 
    group_by(lsoa11 = LSOA11CD) %>% 
    summarise(area = sum(area, na.rm = TRUE)) %>% 
    ungroup()
  
  flooding_surface_water_lsoa_summary <- flooding_surface_water_lsoa_summary %>% 
    rename(surface_flooding_area = area)
  
  lsoa_areas_all <- lsoa_boundaries %>% 
    mutate(lsoa_area = as.numeric(st_area(geometry))) %>% 
    st_drop_geometry()
  
  lsoa_areas_all <- lsoa_areas_all %>% 
    select(lsoa11 = LSOA11CD,
           lsoa_area)
  
  flooding_surface_water_lsoa_summary <- left_join(lsoa_areas_all, flooding_surface_water_lsoa_summary, by = "lsoa11")
  
  flooding_surface_water_lsoa_summary[is.na(flooding_surface_water_lsoa_summary)] <- 0
  
  flooding_surface_water_lsoa_summary <- flooding_surface_water_lsoa_summary %>% 
    mutate(surface_flooding_area_pct = surface_flooding_area / lsoa_area)
  
  flooding_surface_water_lsoa_summary <- filter_for_england(flooding_surface_water_lsoa_summary, lsoa11)
  
  summary(flooding_surface_water_lsoa_summary$surface_flooding_area_pct)
  
  write.csv(flooding_surface_water_lsoa_summary,
            "data/flooding/EA-flooding-risk-surface-water-lsoa.csv",
            row.names = FALSE)
  
  # make into a geopackage
  lsoa_boundaries <- lsoa_boundaries %>% 
    select(lsoa11 = LSOA11CD,
           geometry)
  
  flooding_surface_water_lsoa_gis <- inner_join(lsoa_boundaries, flooding_surface_water_lsoa_summary, by = "lsoa11")
  
  save_as_gpkg(layer_data = flooding_surface_water_lsoa_gis,
               layer_name = "flooding-risk-surface-lsoa")
  
  return(flooding_surface_water_lsoa_summary)
  
}


risk_of_flooding_from_surface_water_lsoa_1_in_100 <- function() {
  
  # get flood data for 1 in 1000, which is highest risk
  st_layers("../gis-data/water/uFMfSW_Banded_Vector_National_Extents.gdb")
  tic("Loaded Surface Water Flooding data")
  flooding_surface_water_1_ini_100 <- st_read("../gis-data/water/uFMfSW_1_in_100/uFMfSW_1_in_100.shp")
  toc()
  # View(head(flooding_surface_water_1_ini_100, 10))
  flooding_surface_water_1_ini_100 <- flooding_surface_water_1_ini_100 %>% 
    select(OBJECTID,
           geometry)
  
  # some bad geom
  flooding_surface_water_1_ini_100 <- st_make_valid(flooding_surface_water_1_ini_100)
  
  # get lsoa boundaries
  lsoa_boundaries <- get_lsoa_boundaries()
  lsoa_boundaries <- lsoa_boundaries %>% 
    select(lsoa11 = LSOA11CD,
           geometry) %>% 
    filter_for_england(lsoa11)
  # then intersect to see which parts of each lsoa fall into v.low, low, medium, high
  tic("Intersect LSOAs with flooding")
  flooding_surface_water_lsoa <- st_intersection(lsoa_boundaries, flooding_surface_water_1_ini_100)
  toc()
  
  # divide up lsoa (one row per lsoa) and calculate proportion of area in each category, plus total flood risk.
  flooding_surface_water_lsoa_summary <- flooding_surface_water_lsoa %>% 
    mutate(area = as.numeric(st_area(geometry))) %>% 
    st_drop_geometry() %>% 
    group_by(lsoa11) %>% 
    summarise(surface_flooding_area_1_in_100 = sum(area, na.rm = TRUE)) %>% 
    ungroup()
  
  # interim save
  write.csv(flooding_surface_water_lsoa_summary,
            "data/flooding/EA-flooding-risk-surface-water-1-in-100-lsoa.csv",
            row.names = FALSE)
  
  lsoa_areas_all <- lsoa_boundaries %>% 
    mutate(lsoa_area = as.numeric(st_area(geometry))) %>% 
    st_drop_geometry()
  
  lsoa_areas_all <- lsoa_areas_all %>% 
    select(lsoa11,
           lsoa_area)
  
  flooding_surface_water_lsoa_summary <- left_join(lsoa_areas_all, flooding_surface_water_lsoa_summary, by = "lsoa11")
  
  flooding_surface_water_lsoa_summary[is.na(flooding_surface_water_lsoa_summary)] <- 0
  
  flooding_surface_water_lsoa_summary <- flooding_surface_water_lsoa_summary %>% 
    mutate(surface_flooding_area_1_in_100_pct = surface_flooding_area_1_in_100 / lsoa_area)
  
  flooding_surface_water_lsoa_summary <- filter_for_england(flooding_surface_water_lsoa_summary, lsoa11)
  
  hist(flooding_surface_water_lsoa_summary$surface_flooding_area_1_in_100_pct)
  summary(flooding_surface_water_lsoa_summary$surface_flooding_area_1_in_100_pct)
  
  write.csv(flooding_surface_water_lsoa_summary,
            "data/flooding/EA-flooding-risk-surface-water-1-in-100-lsoa.csv",
            row.names = FALSE)
  
}


risk_of_flooding_from_surface_water_lsoa_1_in_1000 <- function() {
  
  # get flood data for 1 in 1000, which is highest risk
  st_layers("../gis-data/water/uFMfSW_Banded_Vector_National_Extents.gdb")
  tic("Loaded Surface Water Flooding data")
  flooding_surface_water <- st_read("../gis-data/water/uFMfSW_Banded_Vector_National_Extents.gdb",
                                    layer = "uFMfSW_ENW_EXTENT_1in1000_BV")
  toc()
  # View(head(flooding_surface_water, 10))
  
  # get lsoa boundaries
  lsoa_boundaries <- get_lsoa_boundaries()
  lsoa_boundaries <- lsoa_boundaries %>% 
    select(lsoa11 = LSOA11CD,
           geometry) %>% 
    filter_for_england(lsoa11)
  # then intersect to see which parts of each lsoa fall into v.low, low, medium, high
  tic("Intersect LSOAs with flooding")
  flooding_surface_water_lsoa <- st_intersection(lsoa_boundaries, flooding_surface_water)
  toc()
  # divide up lsoa (one row per lsoa) and calculate proportion of area in each category, plus total flood risk.
  flooding_surface_water_lsoa_summary <- flooding_surface_water_lsoa %>% 
    mutate(area = as.numeric(st_area(geometry))) %>% 
    st_drop_geometry() %>% 
    group_by(lsoa11) %>% 
    summarise(area = sum(area, na.rm = TRUE)) %>% 
    ungroup()
  
  # interim save
  write.csv(flooding_surface_water_lsoa_summary,
            "data/flooding/EA-flooding-risk-surface-water-1-in-1000-lsoa.csv",
            row.names = FALSE)
  
  flooding_surface_water_lsoa_summary <- flooding_surface_water_lsoa_summary %>% 
    rename(surface_flooding_area = area)
  
  lsoa_areas_all <- lsoa_boundaries %>% 
    mutate(lsoa_area = as.numeric(st_area(geometry))) %>% 
    st_drop_geometry()
  
  lsoa_areas_all <- lsoa_areas_all %>% 
    select(lsoa11 = LSOA11CD,
           lsoa_area)
  
  flooding_surface_water_lsoa_summary <- left_join(lsoa_areas_all, flooding_surface_water_lsoa_summary, by = "lsoa11")
  
  flooding_surface_water_lsoa_summary[is.na(flooding_surface_water_lsoa_summary)] <- 0
  
  flooding_surface_water_lsoa_summary <- flooding_surface_water_lsoa_summary %>% 
    mutate(surface_flooding_area_pct = surface_flooding_area / lsoa_area)
  
  flooding_surface_water_lsoa_summary <- filter_for_england(flooding_surface_water_lsoa_summary, lsoa11)
  
  summary(flooding_surface_water_lsoa_summary$surface_flooding_area_pct)
  
  write.csv(flooding_surface_water_lsoa_summary,
            "data/flooding/EA-flooding-risk-surface-water-1-in-1000-lsoa.csv",
            row.names = FALSE)
  
  # make into a geopackage
  lsoa_boundaries <- lsoa_boundaries %>% 
    select(lsoa11 = LSOA11CD,
           geometry)
  
  flooding_surface_water_lsoa_gis <- inner_join(lsoa_boundaries, flooding_surface_water_lsoa_summary, by = "lsoa11")
  
  save_as_gpkg(layer_data = flooding_surface_water_lsoa_gis,
               layer_name = "flooding-risk-surface-lsoa")
  
  return(flooding_surface_water_lsoa_summary)
  
}