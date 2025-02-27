make_flood_imed_domain <- function(flood_defence_factor = 0.5) {

  # start timer
  message("Calculating flood risk score...")
  tic("Flood risk score calculated")
  
  # OLD - Uni of Manchester data
  #flood_lsoa_england <- get_uom_flood_risk_data_lsoa()
  
  # Environment data
  flood_lsoa_england <- get_ea_flooding_data()
  # make score based on two sets of flooding data
  flood_lsoa_england <- make_ea_flooding_imed_score(flood_lsoa_england)
  
  # make deciles
  flood_lsoa_england <- make_flooding_deciles(flood_lsoa_england)
  
  # make ranks and then normalise ranks based on flooding percent
  flood_lsoa_england <- make_flooding_ranks(flood_lsoa_england)
  
  # finalise data
  flood_lsoa_england <- finalise_flooding(flood_lsoa_england)
  
  # end timer, report and return data
  toc()
  return(flood_lsoa_england)
  
  
}

#' =============================================================================
#' ENvironment Agengy data: covering risk from:
#'   - rivers and seas
#'   - surface water flooding

get_ea_flooding_rivers_seas_lsoa <- function() {
  
  # get data previous made by Toby intersecting lsoa boundaries and EA GIS layers on flooding
  flooding_rivers_seas <- read.csv("data/flooding/EA-flooding-risk-river-sea-lsoa.csv",
                                   stringsAsFactors = FALSE)
  
  # get data previous made by Toby intersecting lsoa boundaries and EA GIS layers on flooding
  flooding_rivers_seas <- flooding_rivers_seas %>% 
    select(lsoa11,
           flooding_rivers_seas_extent = low_to_high_risk_area,
           flooding_rivers_seas_pct = low_to_high_risk_area_pct)
  
  # hist(flooding_rivers_seas$flooding_rivers_seas_pct)
  
}
 

get_ea_flooding_surface_water_lsoa <- function() {
  
  flooding_surface_waters <- read.csv("data/flooding/EA-flooding-risk-surface-water-1-in-100-lsoa.csv",
                                      stringsAsFactors = FALSE)
  
  # get data previous made by Toby intersecting lsoa boundaries and EA GIS layers on flooding
  # this layer was simplified by 10 meters to reduce data siz by two thirds.
  flooding_surface_waters <- flooding_surface_waters %>% 
    select(lsoa11,
           flooding_surface_water_extent = surface_flooding_area_1_in_100,
           flooding_surface_water_pct = surface_flooding_area_1_in_100_pct)
  
  # hist(flooding_surface_waters$flooding_surface_water_pct)
  
}

get_ea_flooding_data <- function() {
  
  # flooding from rivers and seas
  frs <- get_ea_flooding_rivers_seas_lsoa()
  # flooding from surface waters
  fsw <- get_ea_flooding_surface_water_lsoa()
  
  # join
  ea_flooding <- inner_join(frs, fsw, by = "lsoa11")
}

# make an IMED score based on two sets of EA data
make_ea_flooding_imed_score <- function(ea_flooding) {
  
  ea_flooding <- ea_flooding %>% 
    mutate(flooding_rivers_seas_surface_pct = flooding_rivers_seas_pct + flooding_surface_water_pct) %>% 
    mutate(flood_risk_imed_score = set_max(flooding_rivers_seas_surface_pct, m = 1)) %>% # this just top codes the data
    mutate(flood_risk_imed_log_score = log(ifelse(flooding_rivers_seas_surface_pct < 0.005, 0.005, flooding_rivers_seas_surface_pct))) %>% # log value to redistribute
    mutate(flood_risk_imed_log_score = rescale_zero_to_one(flood_risk_imed_log_score))
  
  # mutate(flood_risk_imed_score = round(rescale_zero_to_one(flooding_rivers_seas_surface_pct), 4)) # is it better just to add together and rescale?
  
  # hist(ea_flooding$flooding_rivers_seas_surface_pct, breaks = 100)
  # hist(ea_flooding$flood_risk_imed_log_score, breaks = 100)
  
}

# make deciles based on flooding percent
make_flooding_deciles <- function(ea_flooding) {
  
  # make flooding imed rank and score
  ea_flooding <- ea_flooding %>% 
    mutate(flood_risk_imed_decile = ntile(desc(flooding_rivers_seas_surface_pct), n = 10))
  
}

# make ranks and then normalise ranks based on flooding percent
make_flooding_ranks <- function(ea_flooding) {
  
  # make flooding imed rank and score
  ea_flooding <- ea_flooding %>% 
    mutate(flood_risk_imed_rank = rank(desc(round(flooding_rivers_seas_surface_pct, 6)))) %>% 
    mutate(flood_risk_imed_rank_score = rescale_zero_to_one(flood_risk_imed_rank, negative = TRUE))
  
}

# finalise data
finalise_flooding <- function(ea_flooding) {
  
  ea_flooding_final <- ea_flooding %>% 
    select(lsoa11,
           flooding_rivers_seas_pct,
           flooding_surface_water_pct,
           flooding_rivers_seas_surface_pct,
           flood_risk_imed_score,
           flood_risk_imed_log_score,
           flood_risk_imed_decile,
           flood_risk_imed_rank,
           flood_risk_imed_rank_score)
  
}


#' =============================================================================


# Functions that use University of manchester data ------------------------

get_uom_flood_risk_data_lsoa <- function() {
  
  # get University of Manchester flood vulnerability data
  # for England and Wales
  flood_lsoa_england <- read.xlsx("data/flooding/England LSOA flood data for mapping.xlsx") %>%
    rename(lsoa11 = LSOA.Code,
           lsoa11nm = LSOA.Name)
  
  #join England and Wales data
  old_names <- names(flood_lsoa_england)
  new_names <- gsub("%", "pct", old_names)
  new_names <- gsub("<", "lessthan_", new_names)
  new_names <- gsub(">", "morethan_", new_names)
  colnames(flood_lsoa_england) <- new_names
  flood_lsoa_england <- tidy_column_names(flood_lsoa_england)
  
  # process and rename fields
  flood_lsoa_england <- flood_lsoa_england %>% 
    transmute(lsoa11,
              flood_vulnerability_zscore = z_flood_vulnerability,
              population_old = pop_toti,
              flood_socially_vuln_top10pct = top_10pct_socially_vulnerable,
              flood_exposed = exposed,
              flood_defended = defended,
              flood_pop_exposed_n = number_of_population_exposed_approx,
              flood_pop_exposed_pct = pct_population_exposed / 100,
              flood_pop_exposed_under10pct = lessthan_10pct_population_exposed,
              flood_pop_exposed_10to50pct = morethan_10pct_but_lessthan_50pct_exposed,
              flood_pop_exposed_over50pct = morethan_50pct_exposed)
  
  summary(flood_lsoa_england$flood_pop_exposed_pct)
  
  # Update to latest population get lsoa pop stats
  pop_lsoa <- get_ons20_pop_lsoa()
  flood_lsoa_england <- left_join(flood_lsoa_england, pop_lsoa, by = "lsoa11")
  
  # recalculate number of people affected based on keeping percentages the same by calculating new pop 
  flood_lsoa_england <- flood_lsoa_england %>% 
    mutate(flood_pop_exposed_n = round(flood_pop_exposed_pct * population))
}

make_uom_flood_risk_imed_score <- function(flood_lsoa_england,
                                       flood_defence_factor = 1) {

  # select flood exposed percent and defended status for now
  flood_lsoa_england <- flood_lsoa_england %>% 
    select(lsoa11,
           flood_vulnerability_zscore,
           flood_exposed,
           flood_defended,
           flood_pop_exposed_pct) %>% 
    mutate(flood_pop_exposed_pct = ifelse(flood_exposed == "No", 0, flood_pop_exposed_pct)) %>% 
    mutate(flood_pop_exposed_pct = is_null_zero(flood_pop_exposed_pct))
  
  # Is flood risk the population exposeds?
  # If so, should we halve flood risk if defended?
  # Lets do this for the time being
  # TODO: Decide on how to measure flood risk and account for defenses
  # e.g. Those locations that are defended could be set 0 exposure?
  flood_lsoa_england <- flood_lsoa_england %>% 
    mutate(flood_risk = ifelse(flood_exposed == "Yes" & flood_defended == "Yes",
                               flood_pop_exposed_pct * flood_defence_factor,
                               flood_pop_exposed_pct))
  
  # Any flood risk is bad, so no need to adjust score - impossible to have a 
  # minus score and no guarantee that defenses prevent all flooding
  flood_lsoa_england <- flood_lsoa_england %>% 
    mutate(flood_risk_imed_score = rescale_zero_to_one(flood_risk, negative = FALSE)) 
  
  #hist(flood_lsoa_england$flood_risk_imed_score, breaks = 100)
  # plot(1 - flood_lsoa_england$flood_pop_exposed_pct,
  #      flood_lsoa_england$flood_risk_imed_score)
  
  return(flood_lsoa_england)
}

#' =============================================================================