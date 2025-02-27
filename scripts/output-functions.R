
# ------------------------------------------------------------------------------
save_as_gpkg <- function(gpkg_out = "outputs/imed.gpkg",
                         layer_data,
                         layer_name) {
  
  st_write(obj = layer_data,
           dsn = gpkg_out,
           layer = layer_name,
           delete_dsn = FALSE,
           delete_layer = TRUE)
  
}


# -------------------------------------------------------------------------
# function to output results as a csv file

select_key_imed_vars <- function() {
  
  imed_lsoa_sf_vars <- imed_lsoa %>% 
    select(lsoa11,
           #ap_no22022_mean,
           #ap_pm252022g_mean,
           #ap_no2_who_normalised,
           #ap_pm25_who_normalised,
           #ap_imed_combined,
           ap_imed_score,
           #noise_all_55kb_pct,
           #noise_all_65kb_pct,
           noise_imed_score,
           #lsoa_hectares,
           #tree_canopy_hectares,
           tree_canopy_percent,
           tree_canopy_imed_score,
           #missing_prow,
           prow_footpath_length,
           prow_m_per_km2,
           prow_m_per_km2_topcoded_imed,
           greenspace_access_m2,
           greenspace_access_m2_per_pop,
           greenspace_access_m2_per_pop_log_imed,
           greenspace_prow_imed,
           #flooding_rivers_seas_extent,
           flooding_rivers_seas_pct,
           #flooding_surface_water_extent,
           flooding_surface_water_pct,
           flooding_rivers_seas_surface_pct,
           flood_risk_imed_score,
           max_temp_avg,
           max_temp_imed,
           pollution_score_imed,
           pollution_decile_imed,
           nature_score_imed,
           nature_decile_imed,
           climate_score_imed,
           climate_decile_imed,
           imed_score,
           imed_decile,
    )
  
}

# ------------------------------------------------------------------------------
# function to output results as a gpkg
make_imed_gpkg <- function(imed_lsoa,
                           gpkg_layer_name) {
  
  message("Saving IMED as geopackage...")
  tic("IMED geopackage saved")
  
  lsoa_boundaries <- get_lsoa_boundaries()
  lsoa_boundaries_eng <- filter_for_england(lsoa_boundaries, LSOA11CD)
  
  # select key sf fields
  lsoa_boundaries_eng <- lsoa_boundaries_eng %>% 
    select(lsoa11 = LSOA11CD,
           geometry)
  
  imed_lsoa_sf_vars <- imed_lsoa %>% 
    select(lsoa11,
           pollution_score_imed,
           pollution_decile_imed,
           nature_score_imed,
           nature_decile_imed,
           climate_score_imed,
           climate_decile_imed,
           imed_score,
           imed_decile,
           pollution_imed_rank_score,
           pollution_imed_rank_decile,
           nature_imed_rank_score,
           nature_imed_rank_decile,
           climate_imed_rank_score,
           climate_imed_rank_decile,
           imed_rank_score,
           imed_rank_decile,
           
    )
  
  imed_lsoa_sf <- left_join(lsoa_boundaries_eng, imed_lsoa_sf_vars, by = "lsoa11")
  
  # save
  save_as_gpkg(layer_data = imed_lsoa_sf,
               layer_name = gpkg_layer_name)
  
  toc()
  
}

# ------------------------------------------------------------------------------

# Make a map of IMED deciles
make_imed_map <- function(imed_lsoa) {
  
  lsoa_boundaries <- get_lsoa_boundaries(type = "super-g")
  
  lsoa_boundaries <- filter_for_england(lsoa_boundaries, LSOA11CD)
  
  imed_lsoa_gis <- left_join(lsoa_boundaries, imed_lsoa, by = c("LSOA11CD" = "lsoa11"))
  
  imed_lsoa_gis <- imed_lsoa_gis %>% 
    mutate(imed_decile = as.factor(imed_decile))
  
  tmap_mode("view")
  tm_shape(imed_lsoa_gis) + 
    tm_polygons("imed_decile",
                fill.scale = tm_scale_categorical(values = "rd_yl_gn"),
                col_alpha = 0,
                popup.vars = c("LSOA11CD",
                               "imed_score",
                               "imed_decile",
                               "pollution_score_imed",
                               "pollution_decile_imed",
                               "nature_score_imed",
                               "nature_decile_imed",
                               "climate_score_imed",
                               "climate_decile_imed",
                               "flood_risk_imed_score",
                               "heat_exposure_imed"))
  
}

# ------------------------------------------------------------------------------

# make histograms of each domain and main imed
# Nature
make_nature_histogram <- function(imed_lsoa) {
  
  nature_hist <- imed_lsoa %>% 
    transmute(lsoa11,
           `Tree canopy (%)` = tree_canopy_percent,
           #tree_canopy_percent_inv = rescale_zero_to_one(tree_canopy_percent, negative = TRUE),
           #tree_canopy_percent_log = rescale_zero_to_one(log(tree_canopy_percent), negative = TRUE),
           `Tree canopy IMED score` = tree_canopy_imed,
           #prow_imed,
           `Greenspace access (%)` = gs_measure,
           #greenspace_inv = rescale_zero_to_one(gs_measure, negative = TRUE),
           `Greenspace IMED score` = gs_imed,
           #greenspace_imed = gs_log_imed,
           #greenspace_prow_imed,
           #nature_score_test = rescale_zero_to_one(rescale_zero_to_one(tree_canopy_percent, negative = TRUE) + rescale_zero_to_one(gs_measure, negative = TRUE)),
           `Nature domain score` = nature_score_imed) %>% 
    gather(key = Indicator,
           value = score,
           -lsoa11) %>% 
    mutate(Indicator = factor(Indicator, levels = c("Tree canopy (%)",
                                                    #"tree_canopy_percent_log",
                                                    #"tree_canopy_percent_inv",
                                                 "Tree canopy IMED score",
                                                 #"prow_imed",
                                                 "Greenspace access (%)",
                                                 #"greenspace_inv",
                                                 "Greenspace IMED score",
                                                 #"greenspace_prow_imed",
                                                 #"nature_score_test",
                                                 "Nature domain score")))
  
  g_nature <- ggplot(data = nature_hist, aes(x = score, fill = Indicator)) + 
    geom_histogram(binwidth = 0.025) + 
    facet_wrap(Indicator ~ .,
               scales = "free_y",
               ncol = 1) + 
    #scale_fill_brewer(palette = "Dark2") + 
    scale_fill_manual(values = foe_palette_forests) + 
    theme_foe() + 
    labs(title = "IMED: nature domain and indicators",
         y = "LSOA count",
         x = "Indicator score or value")
  
  plot(g_nature)
  
  ggsave(filename = "outputs/histograms/nature-domain.jpg",
         plot = g_nature,
         device = jpeg,
         width = 14,
         height = 20,
         units = "cm",
         dpi = 200)
  
}

# greenspace and prow
make_greenspace_histogram <- function(imed_lsoa) {
  
  greenspace_hist <- imed_lsoa %>% 
    transmute(lsoa11,
              `Greenspace: Original measure` = gs_measure,
              `Greenspace: Rescaled` = rescale_zero_to_one(gs_measure, negative = TRUE),
              `Log (Greenspace)` = gs_log,
              `Log (Greenspace): Rescaled` = gs_log_imed) %>% 
    gather(key = Indicator,
           value = score,
           -lsoa11) %>% 
    mutate(Indicator = factor(Indicator, levels = c("Greenspace: Original measure",
                                                    "Greenspace: Rescaled",
                                                    "Log (Greenspace)",
                                                    "Log (Greenspace): Rescaled")))
  
  g_greenspace <- ggplot(data = greenspace_hist, aes(x = score, fill = Indicator)) + 
    geom_histogram(bins = 50) + 
    facet_wrap(Indicator ~ .,
               scales = "free",
               ncol = 1) + 
    #scale_fill_brewer(palette = "Dark2") + 
    scale_fill_manual(values = foe_palette_forests) + 
    theme_foe() + 
    labs(title = "IMED: Greenspace indicators",
         y = "LSOA count",
         x = "Indicator score or value")
  
  plot(g_greenspace)
  
  ggsave(filename = "outputs/histograms/greenspace-indicators.jpg",
         plot = g_greenspace,
         device = jpeg,
         width = 14,
         height = 14,
         units = "cm",
         dpi = 200)
  
}


# Pollution
make_pollution_histograms <- function(imed_lsoa) {
  
  pollution_hist <- imed_lsoa %>% 
    transmute(lsoa11,
              `NO2 concentration (µg/m3)` = ap_no22022_mean,
              `PM2.5 concentration (µg/m3)` = ap_pm252022g_mean,
              `Air pollution IMED score` = ap_imed_score,
              `Noise above 55/45dB (area %)` = noise_all_55kb_pct,
              `Noise IMED score` = noise_imed_score,
              `Pollution domain score` = pollution_score_imed,
    ) %>% 
    gather(key = Indicator,
           value = score,
           -lsoa11) %>% 
    mutate(Indicator = factor(Indicator, levels = c("NO2 concentration (µg/m3)",
                                                    "PM2.5 concentration (µg/m3)",
                                                    "Air pollution IMED score",
                                                    "Noise above 55/45dB (area %)",
                                                    "Noise IMED score",
                                                    "Pollution domain score")))
  
  g_pollution <- ggplot(data = pollution_hist, aes(x = score, fill = Indicator)) + 
    geom_histogram(binwidth = 0.025) + 
    facet_wrap(Indicator ~ .,
               scales = "free",
               ncol = 1) + 
    #scale_fill_brewer(palette = "Dark2") + 
    scale_fill_manual(values = foe_palette_main) + 
    theme_foe() + 
    labs(title = "IMED: pollution domain and indicators",
         y = "LSOA count",
         x = "Indicator score or value")
  
  plot(g_pollution)
  
  ggsave(filename = "outputs/histograms/pollution-domain.jpg",
         plot = g_pollution,
         device = jpeg,
         width = 14,
         height = 20,
         units = "cm",
         dpi = 200)
}


make_climate_histograms <- function(imed_lsoa) {
  
  climate_hist <- imed_lsoa %>% 
    transmute(lsoa11,
              #`Flooding from rivers and seas (%)` = flooding_rivers_seas_pct,
              #`Flooding from surface water (%)` = flooding_surface_water_pct,
              `Flooding: rivers/seas + surface (%)` = set_max(flooding_rivers_seas_surface_pct, m = 1),
              `Flooding risk IMED score` = flood_risk_imed_score,
              `Flooding risk IMED log score` = flood_risk_imed_log_score,
              `Maximum monthly summer temp` = max_temp_average,
              `Heat risk IMED score` = heat_exposure_imed,
              `Climate domain score` = climate_score_imed) %>% 
    gather(key = Indicator,
           value = score,
           -lsoa11) %>% 
    mutate(Indicator = factor(Indicator, levels = c(#"Flooding from rivers and seas (%)",
                                                    #"Flooding from surface water (%)",
                                                    "Flooding: rivers/seas + surface (%)",
                                                    "Flooding risk IMED score",
                                                    "Flooding risk IMED log score",
                                                    "Maximum monthly summer temp",
                                                    "Heat risk IMED score",
                                                    "Climate domain score")))
  
  g_climate <- ggplot(data = climate_hist, aes(x = score, fill = Indicator)) + 
    geom_histogram(binwidth = 0.025) + 
    facet_wrap(Indicator ~ .,
               scales = "free",
               ncol = 1) + 
    #scale_fill_brewer(palette = "Dark2") + 
    scale_fill_manual(values = foe_palette_main) + 
    theme_foe() +  
    labs(title = "IMED: climate domain and indicators",
         y = "LSOA count",
         x = "Indicator score or value")
  
  plot(g_climate)
  
  ggsave(filename = "outputs/histograms/climate-domain.jpg",
         plot = g_climate,
         device = jpeg,
         width = 14,
         height = 20,
         units = "cm",
         dpi = 200)
  
}



#   -----------------------------------------------------------------------

make_imed_distribution_plot <- function(imed_lsoa) {

  imed_hist_data <- imed_lsoa %>% 
    select(lsoa11,
           Pollution = pollution_score_imed,
           Nature = nature_score_imed,
           Climate = climate_score_imed,
           IMED = imed_score) %>% 
    gather(key = Domain,
           value = score,
           -lsoa11) %>% 
    mutate(Domain = factor(Domain, levels = c("Pollution",
                                              "Nature",
                                              "Climate",
                                              "IMED")))
  
  g_imed <- ggplot(data = imed_hist_data, aes(x = score, fill = Domain)) + 
    geom_histogram(bins = 100) + 
    facet_wrap(Domain ~ .,
               scales = "free",
               ncol = 1) + 
    #scale_fill_brewer(palette = "Dark2") + 
    scale_fill_manual(values = foe_palette_main) + 
    labs(title = "IMED: Main version with domain scores",
         y = "LSOA count",
         x = "IMED/domain score") + 
    theme_foe()
    #theme_bw()
  
  plot(g_imed)
  
  ggsave(filename = "outputs/histograms/imed-main.jpg",
         plot = g_imed,
         device = jpeg,
         width = 14,
         height = 20,
         units = "cm",
         dpi = 200)
}

make_imed_rank_score_distribution <- function(imed_lsoa) {
  
  imed_hist_data <- imed_lsoa %>% 
    select(lsoa11,
           Pollution = pollution_imed_rank_score,
           Nature = nature_imed_rank_score,
           Climate = climate_imed_rank_score,
           IMED = imed_rank_score) %>% 
    gather(key = Domain,
           value = score,
           -lsoa11) %>% 
    mutate(Domain = factor(Domain, levels = c("Pollution",
                                              "Nature",
                                              "Climate",
                                              "IMED")))
  
  g_imed <- ggplot(data = imed_hist_data, aes(x = score, fill = Domain)) + 
    geom_histogram(bins = 100) + 
    facet_wrap(Domain ~ .,
               scales = "free",
               ncol = 1) + 
    #scale_fill_brewer(palette = "Dark2") + 
    scale_fill_manual(values = foe_palette_main) + 
    theme_foe() + 
    labs(title = "IMED: Rank scored version with domain rank scores",
         y = "LSOA count",
         x = "IMED/domain score")
  
  plot(g_imed)
  
  ggsave(filename = "outputs/histograms/imed-rank-scored-version.jpg",
         plot = g_imed,
         device = jpeg,
         width = 14,
         height = 20,
         units = "cm",
         dpi = 200)
  
}

# ------------------------------------------------------------------------------

# Plot deciles against each other

compare_deciles <- function(imed_lsoa, decile_var) {
  imed_lsoa %>% 
    group_by(imed_decile,
             {{ decile_var }}) %>% 
    summarise(n = n()) %>% 
    ungroup() %>% 
    spread(key = {{ decile_var }},
           value = n,
           fill = 0)
}

# compare_deciles(imed_lsoa, pollution_decile_imed)
# compare_deciles(imed_lsoa, nature_decile_imed)
# compare_deciles(imed_lsoa, climate_decile_imed)

# ------------------------------------------------------------------------------

# urban and rural split
make_lsoa_rumorph <- function(onspd) {
  
  lsoa_rumorph <- onspd %>% 
    filter_for_england(lsoa11) %>% 
    group_by(lsoa11,
             ru11ind) %>% 
    summarise(n = n()) %>% 
    group_by(lsoa11) %>% 
    slice_max(order_by = n,
              n = 1,
              with_ties = FALSE) %>% 
    ungroup() %>% 
    mutate(rural_urban_label = case_when(ru11ind == "A1" ~ "Urban major conurbation",
                                   ru11ind == "B1" ~ "Urban minor conurbation",
                                   ru11ind == "C1" ~ "Urban city and town",
                                   ru11ind == "C2" ~ "Urban city and town in a sparse setting",
                                   ru11ind == "D1" ~ "Rural town and fringe",
                                   ru11ind == "D2" ~ "Rural town and fringe in a sparse setting",
                                   ru11ind == "E1" ~ "Rural village",
                                   ru11ind == "E2" ~ "Rural village in a sparse setting",
                                   ru11ind == "F1" ~ "Rural hamlet and isolated dwellings",
                                   ru11ind == "F2" ~ "Rural hamlet and isolated dwellings in a sparse setting")) %>% 
    mutate(rural_urban_simple = case_when(ru11ind == "A1" ~ "Urban",
                                          ru11ind == "B1" ~ "Urban",
                                          ru11ind == "C1" ~ "Urban",
                                          ru11ind == "C2" ~ "Urban",
                                          ru11ind == "D1" ~ "Rural",
                                          ru11ind == "D2" ~ "Rural",
                                          ru11ind == "E1" ~ "Rural",
                                          ru11ind == "E2" ~ "Rural",
                                          ru11ind == "F1" ~ "Rural",
                                          ru11ind == "F2" ~ "Rural"))
  
  imed_lsoa_rurmorph <- left_join(imed_lsoa, lsoa_rumorph, by = "lsoa11")
  imed_rurmorph <- imed_lsoa_rurmorph %>% 
    group_by(rural_urban_simple,
             imed_decile) %>% 
    summarise(n = n()) %>% 
    ungroup() %>% 
    spread(key = rural_urban_simple,
           value = n,
           fill = 0) %>% 
    mutate(`Urban %` = Urban / (Urban + Rural),
           `Rural %` = Rural / (Urban + Rural)) %>% 
    rename(`Rural (no. of LSOAs)` = Rural,
           `Urban (no. of LSOAs)` = Urban)
  
  #prop.table(table(imed_lsoa_rurmorph$rural_urban_simple))
  #table(imed_lsoa_rurmorph$rural_urban_simple)
  
  write.csv(imed_rurmorph,
            "outputs/imed-decile-by-rurality.csv",
            row.names = FALSE)
  
  return(imed_rurmorph)
  
}

# ------------------------------------------------------------------------------

# regional split

make_imed_decile_regional_summary <- function() {
  
  lsoa_to_rgn <- make_lsoa_to_region_lup()
  imed_lsoa_rgn <- left_join(imed_lsoa, lsoa_to_rgn, by = "lsoa11")
  imed_rgn <- imed_lsoa_rgn %>% 
    group_by(rgn,
             region_name,
             imed_decile) %>% 
    summarise(n = n()) %>% 
    ungroup() %>% 
    spread(key = imed_decile,
           value = n,
           fill = 0)

}


# -------------------------------------------------------------------------

# Local authority league tables

make_imed_decile_la_summary <- function() {
  
  lsoa_to_oslaua <- make_lsoa_to_oslaua_lup()
  imed_lsoa_oslaua <- left_join(imed_lsoa, lsoa_to_oslaua, by = "lsoa11")
  
  oslaua_lsoa_count <- imed_lsoa_oslaua %>% 
    group_by(oslaua) %>% 
    summarise(lsoa_n = n()) %>% 
    ungroup()
  
  imed_oslaua <- imed_lsoa_oslaua %>% 
    filter(imed_decile <= 3) %>% 
    group_by(oslaua,
             local_authority) %>% 
    summarise(imed_bottom3_deciles_n = n()) %>% 
    ungroup()
  
  imed_oslaua <- left_join(imed_oslaua, oslaua_lsoa_count, by = "oslaua") 
  
  imed_oslaua <- imed_oslaua %>% 
    transmute(oslaua,
              local_authority,
              lsoa_n,
              imed_bottom3_deciles_n,
              imed_bottom3_deciles_pct = imed_bottom3_deciles_n / lsoa_n)
  
}


# -------------------------------------------------------------------------

theme_foe <- function (base_size = 11, 
                       base_family = "Libre Franklin", 
                       base_line_size = base_size/22, 
                       base_rect_size = base_size/22,
                       colour_line = "#1E234D",
                       colour_font = "#1E234D") {

  half_line <- base_size/2
  t <- theme(line = element_line(colour = colour_line, 
                                 linewidth = base_line_size, 
                                 linetype = 1, 
                                 lineend = "butt"), 
             rect = element_rect(fill = "white", 
                                 colour = colour_line, 
                                 linewidth = base_rect_size, 
                                 linetype = 1), 
             text = element_text(family = base_family, face = "plain", 
                                 colour = colour_font, size = base_size, lineheight = 0.9, 
                                 hjust = 0.5, vjust = 0.5, angle = 0, margin = margin(), 
                                 debug = FALSE), 
             
             axis.line = element_line(colour = colour_line, 
                                      linewidth = base_line_size, 
                                      linetype = 1, 
                                      lineend = "butt"), 
             axis.line.x = NULL, 
             axis.line.y = NULL, 
             axis.text = element_text(size = rel(0.8), colour = colour_line), 
             axis.text.x = element_text(margin = margin(t = 0.8 * half_line/2), vjust = 1), 
             axis.text.x.top = element_text(margin = margin(b = 0.8 * half_line/2), vjust = 0), 
             axis.text.y = element_text(margin = margin(r = 0.8 * half_line/2), hjust = 1), 
             axis.text.y.right = element_text(margin = margin(l = 0.8 * half_line/2), hjust = 0),
             axis.ticks = element_line(colour = colour_line), 
             
             axis.ticks.length = unit(half_line/2, "pt"), axis.ticks.length.x = NULL, 
             axis.ticks.length.x.top = NULL, axis.ticks.length.x.bottom = NULL, 
             axis.ticks.length.y = NULL, axis.ticks.length.y.left = NULL, 
             axis.ticks.length.y.right = NULL, 
             axis.title.x = element_text(family = "Libre Franklin ExtraBold", margin = margin(t = half_line/2), vjust = 1), 
             axis.title.x.top = element_text(margin = margin(b = half_line/2), vjust = 0), 
             axis.title.y = element_text(family = "Libre Franklin ExtraBold", angle = 90, margin = margin(r = half_line/2), vjust = 1), 
             axis.title.y.right = element_text(angle = -90, margin = margin(l = half_line/2), vjust = 0), 
             
             legend.background = element_rect(colour = NA), 
             legend.spacing = unit(2 * half_line, "pt"), legend.spacing.x = NULL, 
             legend.spacing.y = NULL, 
             legend.margin = margin(half_line, half_line, half_line, half_line), 
             legend.key = element_rect(fill = "white", colour = NA), 
             legend.key.size = unit(1.2, "lines"), 
             legend.key.height = NULL, 
             legend.key.width = NULL, 
             legend.text = element_text(size = rel(0.8)), 
             legend.text.align = NULL, 
             legend.title = element_text(family = "Libre Franklin ExtraBold", hjust = 0), 
             legend.title.align = NULL, 
             legend.position = "right", 
             legend.direction = NULL, 
             legend.justification = "center", 
             legend.box = NULL, 
             legend.box.margin = margin(0, 0, 0, 0, "cm"), 
             legend.box.background = element_blank(), 
             legend.box.spacing = unit(2 * half_line, "pt"), 
             
             panel.background = element_rect(fill = "white", colour = "#1E234D"), 
             panel.border = element_rect(fill = NA, 
                                         colour = "#1E234D"), 
             panel.grid = element_line(colour = "white"), 
             panel.grid.minor = element_line(linewidth = rel(0.5)), 
             panel.spacing = unit(half_line, "pt"), panel.spacing.x = NULL, 
             panel.spacing.y = NULL, 
             panel.ontop = FALSE, 
             
             strip.background = element_rect(fill = "#1E234D", colour = "#1E234D"), 
             strip.clip = "inherit", 
             strip.text = element_text(family = "Libre Franklin ExtraBold", colour = "white", size = rel(0.8), margin = margin(0.8 * half_line, 0.8 * half_line, 0.8 * half_line, 0.8 * half_line)), 
             strip.text.x = NULL, 
             strip.text.y = element_text(angle = -90), 
             strip.text.y.left = element_text(angle = 90), 
             strip.placement = "inside", 
             strip.placement.x = NULL, 
             strip.placement.y = NULL, 
             strip.switch.pad.grid = unit(half_line/2, "pt"), 
             strip.switch.pad.wrap = unit(half_line/2, "pt"), 
             
             plot.background = element_rect(colour = "white"), 
             plot.title = element_text(family = "Libre Franklin ExtraBold",
                                       size = rel(1.2), hjust = 0, 
                                       vjust = 1, margin = margin(b = half_line)), 
             plot.title.position = "panel", 
             plot.subtitle = element_text(hjust = 0, vjust = 1, margin = margin(b = half_line)), 
             plot.caption = element_text(size = rel(0.8), hjust = 1, vjust = 1, margin = margin(t = half_line)), 
             plot.caption.position = "panel", 
             plot.tag = element_text(size = rel(1.2), hjust = 0.5, vjust = 0.5), 
             plot.tag.position = "topleft", 
             plot.margin = margin(half_line, half_line, half_line, half_line), 
             
             complete = TRUE)
  
}

foe_palette_main <- list("#61bdaa",
                         "#5a54a0",
                         "#ed6132",
                         "#00608a",
                         "#faef63",
                         "#098d7b",
                         "#66758d",
                         "#f49a3f",
                         "#1e234d",
                         "#094839",
                         "#f4f6fa",
                         "#fde6db")

foe_palette_oceans <- list("#b3cfdc",
                           "#80b0c5",
                           "#3380a1",
                           "#00608a",
                           "#1e234d")

foe_palette_forests <- list("#61bdaa",
                            "#098d7b",
                            "#096f63",
                            "#095144",
                            "#094839")
