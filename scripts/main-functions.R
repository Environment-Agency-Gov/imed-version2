# ------------------------------------------------------------------------------
# MAIN FUNCTION: make imed score
make_imed_lsoa <- function(ap_wgt = 1,
                           noise_wgt = 1,
                           tree_wgt = 1,
                           greenspace_wgt = 1,
                           #greenspace_combine_wgt = 0.5,
                           #prow_combine_wgt = 0.5,
                           #greenspace_prow_wgt = 1,
                           flood_wgt = 1,
                           heat_wgt = 1,
                           pollution_wgt = 1,
                           nature_wgt = 1,
                           climate_wgt = 1,
                           flood_log = FALSE,
                           add_other_geog = FALSE,
                           save_gpkg = FALSE,
                           gpkg_layer_name = "imed-lsoa") {
  
  # join all domains together
  imed_lsoa <- join_all_domain_data(ap_wgt,
                                    noise_wgt,
                                    tree_wgt,
                                    greenspace_wgt,
                                    #greenspace_combine_wgt,
                                    #prow_combine_wgt,
                                    #greenspace_prow_wgt,
                                    flood_wgt,
                                    heat_wgt,
                                    flood_log)
  
  # calculate combined score
  imed_lsoa <- calculate_combined_imed_score(imed_lsoa,
                                             pollution_wgt,
                                             nature_wgt,
                                             climate_wgt)
  
  # add geographical info
  if(add_other_geog) {
    imed_lsoa <- join_msoa_la_region_to_lsoa_data(imed_lsoa)
  }
  
  # save as gpkg
  if(save_gpkg) {
    make_imed_gpkg(imed_lsoa,
                   gpkg_layer_name)  
  }
  
  return(imed_lsoa)
  
  
}

# ------------------------------------------------------------------------------
# bring all domains together in one function
join_all_domain_data <- function(ap_wgt,
                                 noise_wgt,
                                 tree_wgt,
                                 greenspace_wgt,
                                 # greenspace_combine_wgt,
                                 # prow_combine_wgt,
                                 # greenspace_prow_wgt,
                                 flood_wgt,
                                 heat_wgt,
                                 flood_log) {
  
  # pollution
  pollution_imed <- make_pollution_domain(ap_wgt,
                                          noise_wgt)
  # nature
  nature_imed <- make_nature_domain(tree_wgt,
                                    greenspace_wgt)
                                    # greenspace_combine_wgt,
                                    # prow_combine_wgt,
                                    # greenspace_prow_wgt
                                    
  # flood risk
  climate_risk_imed <- make_climate_risk_domain(flood_wgt,
                                                heat_wgt,
                                                flood_log)
  
  # join all domains together
  imed_lsoa <- join_list_of_datasets(lsoa11_code_left_join,
                                     pollution_imed,
                                     nature_imed,
                                     climate_risk_imed)
  
  # TODO: One LSOA has no flood data - missing from source data. Set to 0 for now, but investigate why this is.
  imed_lsoa <- imed_lsoa %>% 
    mutate(climate_score_imed = is_null_zero(climate_score_imed))
  
  
  
  return(imed_lsoa)
  
}

# ------------------------------------------------------------------------------
# calculation of main index, with optional weighting
calculate_combined_imed_score <- function(imed_lsoa,
                                          pollution_wgt,
                                          nature_wgt,
                                          climate_wgt) {
  
  # initiate message and timer
  message("Calculating final IMED score...")
  tic("IMED score calculated")
  
  # calculate combined score with weighting
  imed_lsoa <- imed_lsoa %>% 
    mutate(imed_score = (pollution_score_imed * pollution_wgt) + 
                         (nature_score_imed * nature_wgt) + 
                         (climate_score_imed * climate_wgt)) %>% 
    mutate(imed_decile = ntile(desc(imed_score), n = 10)) %>% 
    mutate(imed_score = round(imed_score, 4))
  
  # alternative rank scoring based approach
  imed_lsoa <- imed_lsoa %>% 
    mutate(imed_rank_score = (pollution_imed_rank_score * pollution_wgt) + 
             (nature_imed_rank_score * nature_wgt) + 
             (climate_imed_rank_score * climate_wgt)) %>% 
    mutate(imed_rank_decile = ntile(desc(imed_rank_score), n = 10)) %>% 
    mutate(imed_rank_score = round(imed_rank_score, 4))
  
  # stop timer and return
  toc()
  return(imed_lsoa)
  
}



# ------------------------------------------------------------------------------
# Domain 1: Pollution
make_pollution_domain <- function(ap_wgt,
                                  noise_wgt) {
  
  # initial message and initiate timer:
  message("CALCULATING POLLUTION SCORE...")
  tic("POLLUTION SCORE calculated")
  
  # air pollution
  ap_imed <- make_air_pollution_imed_score(calc_from_raw_data = FALSE)
  # noise pollution
  noise_imed <- make_noise_imed()
  
  # join pollution data sets
  pollution_imed <- join_list_of_datasets(type_of_join = lsoa11_code_inner_join,
                                          ap_imed,
                                          noise_imed)
  
  # calculate pollution score
  pollution_imed <- pollution_imed %>% 
      mutate(pollution_score_imed = round((ap_imed_score * ap_wgt) + (noise_imed_score * noise_wgt), 4),
             pollution_score_imed = rescale_zero_to_one(pollution_score_imed)) %>% 
      mutate(pollution_decile_imed = ntile(desc(pollution_score_imed), n = 10))
  
  # alternative rank based score
  pollution_imed <- pollution_imed %>% 
    mutate(pollution_imed_rank_score = round((ap_imed_rank_score * ap_wgt) + (noise_imed_rank_score * noise_wgt), 4),
           pollution_imed_rank_score = rescale_zero_to_one(pollution_imed_rank_score)) %>% 
    mutate(pollution_imed_rank_decile = ntile(desc(pollution_imed_rank_score), n = 10))
  
  # stop timer and return
  toc()
  return(pollution_imed)
  
}

# ------------------------------------------------------------------------------
# Domain 2: Nature
make_nature_domain <- function(tree_wgt,
                               greenspace_wgt) {
                               #greenspace_combine_wgt = 0.5,
                               #prow_combine_wgt = 0.5,
                               #greenspace_prow_wgt
                               
  
  # initial message and initiate timer:
  message("CALCULATING NATURE SCORE...")
  tic("NATURE SCORE calculated")
  
  # canopy cover
  tree_imed <- make_tree_canopy_imed_score()
  # green space
  greenspace_imed <- make_greenspace_imed_indicator(use_500m_buffer = FALSE)
  # # prow
  # prow_imed <- make_prow_imed_indcator(use_lsoa_buffer = TRUE)
  # # combine prow and green space
  # greenspace_prow <- join_list_of_datasets(type_of_join = lsoa11_code_left_join,
  #                                          greenspace_imed,
  #                                          prow_imed)
  # greenspace_prow <- greenspace_prow %>% 
  #   mutate(missing_prow = ifelse(is.na(prow_imed), TRUE, FALSE)) %>% 
  #   transmute(lsoa11,
  #             missing_prow,
  #             #prow_footpath_length,
  #             prow_m_per_km2,
  #             prow_imed,
  #             prow_imed_decile,
  #             prow_imed_rank,
  #             prow_imed_rank_score,
  #             gs_m2_per_pop, #greenspace_access_m2_per_pop,
  #             gs_log, # greenspace_access_m2_per_pop_log,
  #             gs_log_imed, #greenspace_access_m2_per_pop_log_imed) %>% 
  #             gs_imed_decile,
  #             gs_imed_rank,
  #             gs_imed_rank_score) %>% 
  #   mutate(greenspace_prow_imed = ifelse(missing_prow,
  #                                        gs_log_imed,
  #                                        (prow_combine_wgt * prow_imed) + (greenspace_combine_wgt * gs_log_imed))) %>% 
  #   mutate(greenspace_prow_imed = rescale_zero_to_one(greenspace_prow_imed))
  #   
  # # make greenspace and prow combined decile, rank, and rank score
  # greenspace_prow <- greenspace_prow %>% 
  #   mutate(greenspace_prow_imed_rank = ifelse(missing_prow,
  #                                             rank(gs_imed_rank),
  #                                             rank((prow_combine_wgt * prow_imed_rank) + (greenspace_combine_wgt * gs_imed_rank)))) %>% 
  #   mutate(greenspace_prow_imed_rank_score = rescale_zero_to_one(greenspace_prow_imed_rank, negative = TRUE))
  # 
  # greenspace_prow <- greenspace_prow %>% 
  #   mutate(greenspace_prow_imed_decile = ifelse(missing_prow,
  #                                               ntile(gs_m2_per_pop, n = 10),
  #                                               ntile((prow_combine_wgt * ntile(prow_m_per_km2, n = 10)) + ntile(greenspace_combine_wgt * gs_m2_per_pop, n = 10), n = 10))) #%>% 
  #   # mutate(greenspace_prow_imed_decile_2 = ifelse(missing_prow,
  #   #                                               gs_imed_decile,
  #   #                                               ntile((prow_combine_wgt * prow_imed_decile) + (greenspace_combine_wgt * gs_imed_decile), n = 10)))
  # 
  
  # join all nature
  nature_domain <- join_list_of_datasets(type_of_join = lsoa11_code_left_join,
                                         tree_imed,
                                         greenspace_imed)
  
  # make combined score with weighting
  # do a separate index that excludes PROW where this is 0.
  nature_domain <- nature_domain %>% 
    mutate(nature_score_imed = (tree_canopy_imed * tree_wgt) + (gs_imed * greenspace_wgt)) %>% 
    mutate(nature_score_imed = round(rescale_zero_to_one(nature_score_imed), 4)) %>% 
    mutate(nature_decile_imed = ntile(desc(nature_score_imed), n = 10))

  
  # alternative ranking based score
  nature_domain <- nature_domain %>% 
    mutate(nature_imed_rank_score = (tree_canopy_imed_rank_score * tree_wgt) + (gs_imed_rank_score * greenspace_wgt)) %>% 
    mutate(nature_imed_rank_score = round(rescale_zero_to_one(nature_imed_rank_score), 4)) %>% 
    mutate(nature_imed_rank_decile = ntile(desc(nature_imed_rank_score), n = 10))

  # stop timer and return
  toc()
  return(nature_domain)
  
}

# ------------------------------------------------------------------------------
# Domain 3: Climate Risks
make_climate_risk_domain <- function(flood_wgt,
                                     heat_wgt,
                                     flood_log) {
  
  # initial message and initiate timer:
  message("CALCULATING CLIMATE SCORE...")
  tic("CLIMATE SCORE calculated")
  
  # flood risk
  flood_imed <- make_flood_imed_domain()
  
  # max temp
  heat_imed <- get_heat_exposure()
  
  # join all climate risk data
  climate_risk_imed <- join_list_of_datasets(lsoa11_code_left_join,
                                             flood_imed,
                                             heat_imed)
  
  # calculate climate score
  # OPTION use normal or log flood score
  if(flood_log) {
    climate_risk_imed <- climate_risk_imed %>% 
      mutate(climate_score_imed = 
               round(
                 (flood_risk_imed_log_score * flood_wgt) + (heat_exposure_imed * heat_wgt), 4))  
  } else {
    climate_risk_imed <- climate_risk_imed %>% 
      mutate(climate_score_imed = 
               round(
                 (flood_risk_imed_score * flood_wgt) + (heat_exposure_imed * heat_wgt), 4))
  }
  
  # finalise climate score and calculate climate domain deciles
  climate_risk_imed <- climate_risk_imed %>% 
    mutate(climate_score_imed = round(rescale_zero_to_one(climate_score_imed), 4)) %>% 
    mutate(climate_decile_imed = ntile(desc(climate_score_imed), n = 10))
  
  # calculate alternative climate rank score
  climate_risk_imed <- climate_risk_imed %>% 
    mutate(climate_imed_rank_score = 
             round(
               (flood_risk_imed_rank_score  * flood_wgt) + (heat_exposure_imed_rank_score * heat_wgt), 4)) %>% 
    mutate(climate_imed_rank_score = round(rescale_zero_to_one(climate_imed_rank_score), 4)) %>% 
    mutate(climate_imed_rank_decile = ntile(desc(climate_imed_rank_score), n = 10))
  
  # plot(climate_risk_imed$climate_score_imed,
  #      climate_risk_imed$climate_imed_rank_score)
  
  # table(climate_risk_imed$climate_decile_imed,
  #       climate_risk_imed$climate_imed_rank_decile)
  
  # stop timer and return
  toc()
  return(climate_risk_imed)
  
}


# ------------------------------------------------------------------------------