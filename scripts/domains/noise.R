make_noise_imed <- function() {
  
  # initial message and initiate timer:
  message("Calculating noise pollution index...")
  tic("Noise pollution index calculations complete")
  
  # get source data (pre-prepared by FoE using road, rail and airport data from different sources)
  noise_lsoa <- get_noise_pollution_lsoa(time = "Lden")
  # use to calculate score based on England average
  noise_lsoa <- calculate_noise_pollution_imed_score(noise_lsoa)
  
  # make decile
  noise_lsoa <- make_noise_decile_score(noise_lsoa)
  
  # make rank and rank score
  noise_lsoa <- make_noise_rank_and_score(noise_lsoa)
  
  # finalise data
  noise_lsoa <- finalise_noise_pollution(noise_lsoa)
  
  # stop timer (and prompt message) and return final data
  toc()
  return(noise_lsoa)
  
}

get_noise_pollution_lsoa <- function(time = "Lden") {
  
  # read in prepared data
  if(time == "day") {
    noise_pollution_lsoa <- read.csv("data/pollution/noise-pollution-lsoa.csv")  
  }
  if(time == "Lden") {
    noise_pollution_lsoa <- read.csv("data/pollution/noise-pollution-lden-lsoa-45db-airports.csv")  
  }
  # contains 16 noise pollution variables so just select the key one(s)
  noise_pollution_lsoa <- noise_pollution_lsoa %>% 
    select(lsoa11,
           noise_all_55kb_pct,
           noise_all_65kb_pct)
  
}


get_full_noise_pollution_lsoa <- function() {
  
  # read in prepared data
  noise_pollution_lsoa <- read.csv("data/pollution/noise-pollution-lsoa.csv")  
  # contains 16 noise pollution variables so just select the key one(s)
  noise_pollution_lsoa <- noise_pollution_lsoa %>% 
    transmute(lsoa11,
              lsoa_total_area_km2 = lsoa_total_area_m2 / 1000000,
              noise_all_55kb_pct,
              noise_all_55kb_km2 = all_noise_55db_area_m2 / 1000000,
              noise_all_65kb_pct,
              noise_all_65kb_km2 = all_noise_65db_area_m2 / 1000000)
  
}

calculate_noise_pollution_imed_score <- function(noise_lsoa, 
                                                 db65_factor = 2) {
  
  # use both the 65 and 55 decibels data but the 65 decibels data contributes a higher
  # significance to the overall LSOA score using this factor
  noise_lsoa <- noise_lsoa %>% 
    #mutate(noise_imed_score = ((noise_all_65kb_pct * db65_factor) + noise_all_55kb_pct) / (db65_factor + 1))
    mutate(noise_imed_score = round(noise_all_55kb_pct, 5))
  
  #hist(noise_lsoa$noise_imed_rank_score)
  
  # test <- noise_lsoa %>% 
  #   filter(noise_pollution_imed_score > 0)
  # hist(test$noise_pollution_imed_score)
  
}


make_noise_decile_score <- function(noise_lsoa) {
  
  noise_lsoa <- noise_lsoa %>% 
    mutate(noise_imed_decile = ntile(desc(noise_all_55kb_pct), n = 10))
  
}

make_noise_rank_and_score <- function(noise_lsoa) {
  
  # make noise imed rank and score
  noise_lsoa <- noise_lsoa %>% 
    mutate(noise_imed_rank = rank(desc(round(noise_all_55kb_pct, 5)))) %>% 
    mutate(noise_imed_rank_score = rescale_zero_to_one(noise_imed_rank, negative = TRUE))
  
}


finalise_noise_pollution <- function(noise_lsoa) {
  
  noise_lsoa_final <- noise_lsoa %>% 
    select(lsoa11,
           noise_all_55kb_pct,
           noise_all_65kb_pct,
           noise_imed_score,
           noise_imed_decile,
           noise_imed_rank,
           noise_imed_rank_score)
  
}
