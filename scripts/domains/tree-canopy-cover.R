
make_tree_canopy_imed_score <- function() {
  
  # initial message and initiate timer:
  message("Calculating tree canopy cover index...")
  tic("Tree canopy cover index calculations complete")
  
  # get source data (modelled by and provided by Terra Sulis)
  tree_canopy_lsoa <- get_tree_canopy_cover_lsoa()
  
  # use to calculate score based on England average
  tree_canopy_lsoa <- calculate_tree_canopy_imed_score(tree_canopy_lsoa)
  
  # get tree cover decile
  tree_canopy_lsoa <- make_tree_canopy_decile(tree_canopy_lsoa)
  
  # get tree cover rank and rank score
  tree_canopy_lsoa <- make_tree_canopy_rank(tree_canopy_lsoa)
  
  # finalise fields
  tree_canopy_lsoa <- finalise_tree_canopy(tree_canopy_lsoa)
  
  # stop timer (and prompt message) and return final data
  toc()
  return(tree_canopy_lsoa)
  
}


# get tree data
get_tree_canopy_cover_lsoa <- function() {
  
  # get tree data
  tree_canopy_lsoa <- read.xlsx("data/nature/trees/lsoa_2011_tree_canopy_area_results_021222.xlsx",
                                sheet = "lsoa_2011_ew_bfc",
                                na.strings = "(null)")
  
  tree_canopy_lsoa <- tree_canopy_lsoa %>% 
    # remove Welsh data as those are incomplete
    filter(!str_detect(lsoa11cd, 'W')) %>%
    # keep only fields of interest and rename
    transmute(lsoa11 = lsoa11cd,
              lsoa_hectares = tsr_unit_area_ha,
              tree_canopy_hectares = as.numeric(tsr_canopy_area_ha))%>%
    # get tree cover percentage in each lsoa
    mutate(tree_canopy_percent = tree_canopy_hectares/lsoa_hectares)
  
  # we have missing data for some LSOAs, so assume it is either the average? Or minimum?
  # we also have some infitessimall small areas, so assume these are 0.1%
  tree_canopy_lsoa <- tree_canopy_lsoa %>% 
    mutate(tree_canopy_percent = ifelse(tree_canopy_percent < 0.001, 0.001, tree_canopy_percent)) %>% 
    mutate(tree_canopy_percent = ifelse(is.na(tree_canopy_percent), min(tree_canopy_percent, na.rm = TRUE), tree_canopy_percent))
  
}

# calculate variation from England mean tree canopy cover
calculate_tree_canopy_imed_score <- function(tree_canopy_lsoa) {
  
  # calculate mean coverage in England
  mean_tree_cover <- tree_canopy_lsoa %>% 
    summarise(eng_avg_tree_cover = sum(tree_canopy_hectares, na.rm = TRUE) / sum(lsoa_hectares, na.rm = TRUE))
  # select as a numerical value
  mean_tree_cover = mean_tree_cover$eng_avg_tree_cover
  # use to calculate how each LSOA compares and thus make provisional tree canopy imed score
  # invert my minusing the value, so below 0 is good, above 0 is bad.
  #tree_canopy_lsoa <- tree_canopy_lsoa %>% 
  #  mutate(tree_canopy_imed = -(tree_canopy_percent - mean_tree_cover) / mean_tree_cover)
  
  # For now, keep simple and use (1 - tree canopy cover percent), i.e. inverse.
  tree_canopy_lsoa <- tree_canopy_lsoa %>% 
    mutate(tree_canopy_imed = ifelse(is.na(tree_canopy_percent), mean(tree_canopy_percent), tree_canopy_percent)) %>% 
    #mutate(tree_canopy_imed = 1 - tree_canopy_imed)
    mutate(tree_canopy_imed = rescale_zero_to_one(tree_canopy_imed, negative = TRUE))
  
  
}


make_tree_canopy_decile <- function(tree_canopy_lsoa) {
  
  tree_canopy_lsoa <- tree_canopy_lsoa %>% 
    mutate(tree_canopy_imed_decile = ntile(tree_canopy_percent, n = 10))
  
}

# make heat exposure imed rank and score
make_tree_canopy_rank <- function(tree_canopy_lsoa) {
  
  # make tree rank and score
  tree_canopy_lsoa <- tree_canopy_lsoa %>% 
    mutate(tree_canopy_imed_rank = rank(tree_canopy_percent)) %>% 
    mutate(tree_canopy_imed_rank_score = rescale_zero_to_one(tree_canopy_imed_rank, negative = TRUE))
  
}

finalise_tree_canopy <- function(tree_canopy_lsoa) {
  
  tree_canopy_lsoa_final <- tree_canopy_lsoa %>% 
    select(lsoa11,
           tree_canopy_percent,
           tree_canopy_imed,
           tree_canopy_imed_decile,
           tree_canopy_imed_rank,
           tree_canopy_imed_rank_score)
  
}