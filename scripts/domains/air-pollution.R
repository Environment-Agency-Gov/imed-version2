
# functions to process airpollution data and aggregate to LSOA ------------
make_air_pollution_imed_score <- function(calc_from_raw_data = TRUE) {
  
  # initial message and initiate timer:
  message("Calculating air pollution index...")
  tic("Air pollution index calculations complete:")
  
  if(calc_from_raw_data) {
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
    # ensure that data is returned from loop
    return(ap_lsoa)
    
  } else {
    # read in pre-prepared data
    ap_lsoa <- read.csv("data/pollution/air-pollution-lsoa.csv",
                        stringsAsFactors = FALSE)
   }
  
  ap_lsoa <- make_combined_ap_score(ap_lsoa,
                                    no2_wgt = 1,
                                    pm25_wgt = 1)
  
  # select subset of columns and rename so all suffixed with 'ap_'.
  ap_imed_lsoa <- select_ap_cols(ap_lsoa)
  
  # filter for England and Wales only
  ap_imed_lsoa <- filter_for_england(ap_imed_lsoa, lsoa11)
  
  # make rank and score
  ap_imed_lsoa <- make_ap_rank_and_score(ap_imed_lsoa)
  
  # make air pollution decile
  ap_imed_lsoa <- make_airpollution_decile_score(ap_imed_lsoa)
  
  # finalise data with source/raw data plus final imed indicators (i.e. remove intermediate values)
  ap_imed_lsoa <- finalise_airpollution_data(ap_imed_lsoa)
  
  # stop timer (and prompt message) and return final data
  toc()
  return(ap_imed_lsoa)
  
}

#  makes a score on a scale of 0 to 1, whereby 0.5 is WHO threshold.
make_combined_ap_score <- function(ap_lsoa,
                                      no2_wgt = 1,
                                      pm25_wgt = 1) {
  
  # NO2 split data by whether over the threshold or not, then rescale
  no2_below <- ap_lsoa %>% 
    filter(no2_who_normalised <= 0) %>% 
    select(LSOA11CD,
           no22022_mean,
           no2_who_ratio,
           no2_who_normalised) %>% 
    mutate(no2_score = rescale(no2_who_normalised, to = c(0, 0.5)))
  
  no2_above <- ap_lsoa %>% 
    filter(no2_who_normalised > 0) %>% 
    select(LSOA11CD,
           no22022_mean,
           no2_who_ratio,
           no2_who_normalised) %>% 
    mutate(no2_score = rescale(no2_who_normalised, to = c(0.5, 1)))
  
  # bring together
  ap_no2 <- bind_rows(no2_below,
                      no2_above)
  
  # PM2.5 split data by whether over the threshold or not, then rescale
  pm25_below <- ap_lsoa %>% 
    filter(pm25_who_normalised <= 0) %>% 
    select(LSOA11CD,
           pm252022g_mean,
           pm25_who_ratio,
           pm25_who_normalised) %>% 
    mutate(pm25_score = rescale(pm25_who_normalised, to = c(0, 0.5)))
  
  pm25_above <- ap_lsoa %>% 
    filter(pm25_who_normalised > 0) %>% 
    select(LSOA11CD,
           pm252022g_mean,
           pm25_who_ratio,
           pm25_who_normalised) %>% 
    mutate(pm25_score = rescale(pm25_who_normalised, to = c(0.5, 1)))
  
  # bring together
  ap_pm25 <- bind_rows(pm25_below,
                        pm25_above)
  
  # bring both sets of data together
  ap_lsoa_imed_2 <- inner_join(ap_no2, ap_pm25, by = "LSOA11CD")
  
  # finalise both indicators into one and keep just these indicators
  ap_lsoa_imed_2 <- ap_lsoa_imed_2 %>% 
    transmute(LSOA11CD,
              ap_score_combined = (no2_score * no2_wgt) + (pm25_score * pm25_wgt)) %>% 
    mutate(ap_imed_score = rescale_zero_to_one(ap_score_combined))
    
  # rejoin to main data
  ap_lsoa <- left_join(ap_lsoa, ap_lsoa_imed_2, by = "LSOA11CD")
  
}


select_ap_cols <- function(ap_lsoa) {
  
  ap_lsoa_final <- ap_lsoa %>% 
    select(lsoa11 = LSOA11CD,
           ap_no22022_mean = no22022_mean,
           ap_pm252022g_mean = pm252022g_mean,
           ap_no2_who_normalised = no2_who_normalised,
           ap_pm25_who_normalised = pm25_who_normalised,
           ap_score_combined,
           ap_imed_score)
}


make_airpollution_decile_score <- function(ap_lsoa) {
  
  ap_lsoa <- ap_lsoa %>% 
    mutate(ap_no2_decile = ntile(desc(ap_no22022_mean), n = 10),
           ap_pm25_decile = ntile(desc(ap_pm252022g_mean), n = 10)) %>% 
    mutate(ap_imed_decile = ntile((ap_no2_decile + ap_pm25_decile), n = 10))
  
}

make_ap_rank_and_score <- function(ap_lsoa) {
  
  # make air pollution imed rank
  ap_lsoa <- ap_lsoa %>% 
    mutate(ap_no2_rank = rank(desc(ap_no22022_mean)),
           ap_pm25_rank = rank(desc(ap_pm252022g_mean))) %>% 
    mutate(ap_imed_rank = rank(ap_no2_rank + ap_pm25_rank)) %>% 
    mutate(ap_imed_rank_score = rescale_zero_to_one(ap_imed_rank, negative = TRUE))
  
}

finalise_airpollution_data <- function(ap_lsoa) {
  
  ap_lsoa_final <- ap_lsoa %>% 
    select(lsoa11,
           ap_no22022_mean,
           ap_pm252022g_mean,
           ap_imed_score,
           ap_imed_decile,
           ap_imed_rank,
           ap_imed_rank_score)
  
}




get_preprocessed_airpollution_lsoa <- function() {
  
  ap_lsoa <- read.csv("data/pollution/air-pollution-lsoa.csv",
                      stringsAsFactors = FALSE)
  
  ap_lsoa <- ap_lsoa %>% 
    transmute(lsoa11 = LSOA11CD,
              airpollution_pm25_mean = pm252022g_mean,
              airpollution_no2_mean = no22022_mean,
              airpollution_pm25_who = ifelse(pm25_who_ratio > 1, "PM2.5 above WHO guidelines", "PM2.5 within WHO guidelines"),
              airpollution_no2_who = ifelse(no2_who_ratio > 1, "NO2 above WHO guidelines", "NO2 within WHO guidelines"),
              airpollution_pm25_who_ratio = pm25_who_ratio,
              airpollution_no2_who_ratio = no2_who_ratio)
  
}




