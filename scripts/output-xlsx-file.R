make_imed_tab <- function(imed_lsoa) {
  
  imed_tab <- imed_lsoa %>% 
    select(lsoa11,
           #`MSOA code` = msoa11,
           #`MSOA name` = msoa11_name,
           #`LAD code` = oslaua,
           #`Local authority` = local_authority,
           #`Region code` = rgn,
           #`Region` = region_name,
           `IMED: score (1 = most env deprived)` = imed_score,
           `IMED: decile (1 = most env deprived)` = imed_decile,
           `Pollution domain: score (1 = most polluted)` = pollution_score_imed,
           `Pollution domain: decile (1 = most polluted)` = pollution_decile_imed,
           `Nature domain: score (1 = most nature deprived` = nature_score_imed,
           `Nature domain: decile (1 = most nature deprived` = nature_decile_imed,
           `Climate impact domain: score (1 = most impacted)` = climate_score_imed,
           `Climate impact domain: decile (1 = most impacted)` = climate_decile_imed)
  
}


make_nature_domain_tab <- function(imed_lsoa) {
  
  nature_tab <- imed_lsoa %>% 
    select(lsoa11,
           #`MSOA code` = msoa11,
           #`MSOA name` = msoa11_name,
           #`LAD code` = oslaua,
           #`Local authority` = local_authority,
           #`Region code` = rgn,
           #`Region` = region_name,
           `Nature domain: score (1 = most nature deprived` = nature_score_imed,
           `Nature domain: decile (1 = most nature deprived` = nature_decile_imed,
           `Tree canopy cover (%)` = tree_canopy_percent,
           `Tree canopy IMED indicator` = tree_canopy_imed,
           `Accessible greenspace (% with access)` = gs_measure,
           `Accessible greenspace IMED indicator` = gs_imed)
  
}


make_pollution_domain_tab <- function(imed_lsoa) {
  
  pollution_tab <- imed_lsoa %>% 
    select(lsoa11,
           #`MSOA code` = msoa11,
           #`MSOA name` = msoa11_name,
           #`LAD code` = oslaua,
           #`Local authority` = local_authority,
           #`Region code` = rgn,
           #`Region` = region_name,
           `Pollution domain: score (1 = most polluted)` = pollution_score_imed,
           `Pollution domain: decile (1 = most polluted)` = pollution_decile_imed,
           `NO2 (ug/m3)` = ap_no22022_mean,
           `PM2.5 (ug/m3)` = ap_pm252022g_mean,
           `Air pollution IMED indicator` = ap_imed_score,
           `Noise pollution above 55 db (45 db  aircraft) (% of LSOA area)` = noise_all_55kb_pct,
           `Noise pollution above 65 db (% of LSOA area)` = noise_all_65kb_pct,
           `Noise pollution IMED indicator` = noise_imed_score)
  
}

make_climate_domain_tab <- function(imed_lsoa) {
  
  climate_tab <- imed_lsoa %>% 
    select(lsoa11,
           #`MSOA code` = msoa11,
           #`MSOA name` = msoa11_name,
           #`LAD code` = oslaua,
           #`Local authority` = local_authority,
           #`Region code` = rgn,
           #`Region` = region_name,
           `Climate impact domain: score (1 = most impacted)` = climate_score_imed,
           `Climate impact domain: decile (1 = most impacted)` = climate_decile_imed,
           `Flooding risk from rivers and seas (% of LSOA area)` = flooding_rivers_seas_pct,
           `Flooding risk from surface water (% of LSOA area)` = flooding_surface_water_pct,
           `Flooding risk IMED indicator` = flood_risk_imed_score,
           `Max monthly temperature (CHESS-SCAPE 2020-2040)` = max_temp_average,
           `Heat exposure IMED indicator` = heat_exposure_imed)
  
}

save_imed_xlsx <- function(imed_lsoa) {
  
  tic("Saved IMED xlsx file, with domains")
  imed_tab <- make_imed_tab(imed_lsoa)
  pollution_domain_tab <- make_pollution_domain_tab(imed_lsoa)
  nature_domain_tab <- make_nature_domain_tab(imed_lsoa)
  climate_domain_tab <- make_climate_domain_tab(imed_lsoa)
  
  data.frame(n = 1:ncol(imed_tab), sapply(imed_tab, class))
  data.frame(n = 1:ncol(pollution_domain_tab), sapply(pollution_domain_tab, class))
  data.frame(n = 1:ncol(nature_domain_tab), sapply(nature_domain_tab, class))
  data.frame(n = 1:ncol(climate_domain_tab), sapply(climate_domain_tab, class))
  
  save_as_spreadsheet_multiformat(number_of_tabs = 4,
                                  tab1_data = imed_tab,
                                  tab1_name = "imed",
                                  tab2_data = pollution_domain_tab,
                                  tab2_name = "pollution",
                                  tab3_data = nature_domain_tab,
                                  tab3_name = "nature",
                                  tab4_data = climate_domain_tab,
                                  tab4_name = "climate",
                                  number_cols_1 = 0,
                                  percent_cols_1 = 0,
                                  number_cols_2 = 0,
                                  percent_cols_2 = 13:14,
                                  number_cols_3 = 0,
                                  percent_cols_3 = 10,
                                  number_cols_4 = 0,
                                  percent_cols_4 = 10:11,
                                  xlsx_path = "outputs/imed-version-1.xlsx",
                                  colwidths = 25,
                                  number_decimal = TRUE,
                                  percent_decimal = TRUE)
  
  toc()
  
}
