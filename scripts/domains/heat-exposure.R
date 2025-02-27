get_heat_exposure <- function() {
  
  # start timer
  message("Calculating max temp score...")
  tic("Max temp score calculated")
  
  # get raw data
  tasmax_lsoa <- get_chess_scape_maxtemp()
  
  # make score from heat data
  heat_lsoa <- make_heat_exposure_score(tasmax_lsoa)
  
  # make heat exposure score
  heat_lsoa <- make_heat_exposure_score(heat_lsoa)
  
  # make decile from maxtemp
  heat_lsoa <- make_heat_exposure_rank(heat_lsoa)
  
  # make ranks and rank score from maxtemp
  heat_lsoa <- make_heat_exposure_decile(heat_lsoa)
  
  # finalise the list of heat exposure fields
  heat_lsoa <- finalise_heat_exposure(heat_lsoa)
  
  # end timer, report and return data
  toc()
  return(heat_lsoa)
  
}


get_chess_scape_maxtemp <- function() {
  
  tasmax_lsoa <- read.csv("data/climate/tasmax-8.5-2020-2040-lsoa.csv")
  
}

# make imed score
make_heat_exposure_score <- function(heat_lsoa) {
  
  heat_lsoa <- heat_lsoa %>% 
    mutate(heat_exposure_imed = rescale_zero_to_one(tasmax_avg))
  
}

make_heat_exposure_decile <- function(heat_lsoa) {
  
  heat_lsoa <- heat_lsoa %>% 
    mutate(heat_exposure_imed_decile = ntile(tasmax_avg, n = 10))
  
}

# make heat exposure imed rank and score
make_heat_exposure_rank <- function(heat_lsoa) {
 
  heat_lsoa <- heat_lsoa %>% 
    mutate(heat_exposure_imed_rank = rank(desc(round(tasmax_avg, 4)))) %>% 
    mutate(heat_exposure_imed_rank_score = rescale_zero_to_one(heat_exposure_imed_rank, negative = TRUE))
  
}

finalise_heat_exposure <- function(heat_lsoa) {
  
  heat_lsoa_final <- heat_lsoa %>% 
    select(lsoa11 = LSOA11CD,
           max_temp_average = tasmax_avg,
           heat_exposure_imed,
           heat_exposure_imed_decile,
           heat_exposure_imed_rank,
           heat_exposure_imed_rank_score)
  
}