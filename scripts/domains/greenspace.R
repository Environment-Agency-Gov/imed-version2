make_greenspace_imed_indicator <- function(use_500m_buffer = FALSE) {
  
  # initial message and initiate timer:
  message("Calculating accessible greenspace index...")
  tic("Accessible Greenspace index calculations complete")
  
  # Get greenspace data
  agi_lsoa <- get_greenspace_aug24(scenario = all_with_row_gs_access_pct)
  
  # Calculate greenspace IMED score
  agi_lsoa <- calculate_greenspace_imed_score(agi_lsoa)
  
  # make greenspace decile
  agi_lsoa <- calculate_greenspace_decile(agi_lsoa)
  
  # make greenspace imed rank and score
  agi_lsoa <- calculate_greenspace_rank(agi_lsoa)
  
  #finalise the data
  agi_lsoa_final <- finalise_greenspace_data(agi_lsoa)
  
  # stop timer (and prompt message) and return final data
  toc()
  return(agi_lsoa_final)
  
}

get_greenspace_aug24 <- function(scenario = all_with_row_gs_access_pct) {
  
  # read in combined greenspace
  gs_lsoa11 <- read.csv("data/nature/greenspace/greenspace-access-all-standards-lsoa11-august2024.csv")
  
  #names(gs_lsoa11)
  
  gs_lsoa11 <- gs_lsoa11 %>% 
    select(lsoa11,
           gs = {{ scenario }})
  
}


calculate_greenspace_imed_score <- function(agi_lsoa) {
  
  # finalise data
  agi_lsoa <- agi_lsoa %>% 
    mutate(gs_log = log(gs)) %>% 
    # some logged values come out as -Inf so these are set to minimum value,
    # first by setting "-Inf" to NA, then the minimum value
    mutate(gs_log = ifelse(gs_log == "-Inf", NA, gs_log)) %>% 
    mutate(gs_log = ifelse(is.na(gs_log), min(gs_log, na.rm = TRUE), gs_log))
  
  # normalise distribution - tried but this doesn't work
  # agi_lsoa <- agi_lsoa %>% 
  #   mutate(gs_norm = rescale_zero_to_one(pnorm(gs, mean = mean(agi_lsoa$gs), lower.tail = TRUE), negative = TRUE)) %>% 
  #   mutate(gs_norm = ifelse(gs_norm == "-Inf", 0, gs_norm))
  # 
  # hist(agi_lsoa$gs, breaks = 100)
  # hist(agi_lsoa$gs_norm, breaks = 100)
  # plot(agi_lsoa$gs_norm, agi_lsoa$gs)
  
  # convert into a score
  # the manually set minimum values thus becoming 1
  agi_lsoa <- agi_lsoa %>% 
    mutate(gs_imed = rescale_zero_to_one(gs, negative = TRUE),
           gs_log_imed = rescale_zero_to_one(gs_log, negative = TRUE))
  
}

# decile
calculate_greenspace_decile <- function(agi_lsoa) {
  
  agi_lsoa <- agi_lsoa %>% 
    mutate(gs_imed_decile = ntile(gs, n = 10))
  
}

# greenspace rank and rank score
calculate_greenspace_rank <- function(agi_lsoa) {
  
  agi_lsoa <- agi_lsoa %>% 
    mutate(gs_imed_rank = rank(gs, ties.method = "random"),
           gs_imed_rank_score = rescale_zero_to_one(gs_imed_rank, negative = TRUE))
  
  #hist(agi_lsoa$gs_imed_rank_score, breaks = 100)
  
}


finalise_greenspace_data <- function(agi_lsoa) {
  
  agi_lsoa_final <- agi_lsoa %>% 
    select(lsoa11,
           gs_measure = gs,
           gs_imed,
           gs_log,
           gs_log_imed,
           gs_imed_decile,
           gs_imed_rank,
           gs_imed_rank_score)
  
}

