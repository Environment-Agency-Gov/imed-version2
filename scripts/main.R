#' -----------------------------------------------------------------------------
#' *INDEX OF MULTIPLE ENVIRONMENTAL DEPRIVATION*
#' *VERSION 1: 2024*
#' Author: Toby Bridgeman (Friends of the Earth)
#' -----------------------------------------------------------------------------
#' 
#' *DATA SETS AND DOMAINS: Version 1*
#'Pollution
#'    •	Air pollution: Neighbourhoods with NO2 and PM2.5 levels above WHO guidelines: Friends of the Earth analysis of Defra’s modelled background air pollution data, 2022. uk-air.defra.gov.uk/data/pcm-data
#'    •	Noise pollution: Road and rail noise: Strategic noise mapping, Defra 2019. https://www.gov.uk/government/publications/strategic-noise-mapping-2019Pollution
#'    •	Noise pollution: Aircraft noise: Aircraft Noise Map (data provided on request) https://noise-map.com/home/.
#'  - Nature
#'    •	Greenspace: Access to green space in England: Scenario 2 (All green space with rights of way), Defra, Official Statistic in Development, 2024: https://www.gov.uk/government/statistics/access-to-green-space-in-england; https://www.gov.uk/government/statistics/access-to-green-space-in-england/access-to-green-space-in-england
#'    •	Tree canopy cover: Terra Sulis on behalf of Friends of the Earth, 2022. https://policy.friendsoftheearth.uk/insight/mapping-english-tree-cover-results-ranking-and-methodology
#'  - Climate risks
#'    •	Flood risk: Risk of Flooding from Rivers and Sea, Low to High Risk Extent. Environment Agency (2024) https://www.data.gov.uk/dataset/bad20199-6d39-4aad-8564-26a46778fd94/risk-of-flooding-from-rivers-and-sea; Risk of Flooding from Surface Water – 1 in 100 year event extent. Environment Agency (2015). https://environment.data.gov.uk/dataset/51a5c4e7-10d3-4f34-bb0e-558835ab8c85
#'    •	Heat exposure risk: Twenty-year mean-monthly (Jan-Dec) near-surface daily maximum air temperature 2020-40 for RCP 8.5. CHESS-SCAPE: Future projections of meteorological variables at 1 km resolution for the United Kingdom 1980-2080 derived from UK Climate Projections 2018. https://catalogue.ceda.ac.uk/uuid/8194b416cbee482b89e0dfbe17c5786c
#'  - Other
#'      LSOA boundary files: Lower layer Super Output Areas (December 2011) Boundaries EW BFC (V3). https://geoportal.statistics.gov.uk/datasets/ons::lower-layer-super-output-areas-december-2011-boundaries-ew-bfc-v3/about
#'  
#' 
#' Each data set has its own script.
#' The overall approach is higher scorer = higher deprivation.
#' Score will be expressed as a proportion where possible, with 0 being best and
#' 1 being worst.
#' So some indicators will need to be reciprocated or subtracted from 1 to meet 
#' the 0 = best, 1 = worst criteria. As follows
#' 
#' -------------------------------------------------------------------------------------------------------------------------------------
#' 
#' **Summary of data processing to derive IMED indicators**
#' #*POLLUTION*
#'    - Air pollution: 
#'        NO2 and PM2.5 annual average background concentrations each rescaled 
#'        to a value from 0 to 1, with 0.5 representing WHO guidelines, then both combined into 
#'        one air pollution indicator (0 = lowest levels of air pollution; 0.5 air pollution at
#'        WHO guidelines; 1 = highest air pollution from NO2 and PM2.5). 
#'    - Noise pollution:
#'          Proportion of an LSOA with noise levels from road and rail above 55 dB 
#'          and from aircraft above 45 dB (1 = 100% of LSOA impacted by noise pollution).
#'  #*Nature*
#'    - Tree cover:
#'        Tree canopy cover (% LSOA), inverted to between 0 and 1 (1 = minimum observed tree canopy cover).
#'    - Greenspace:
#'        Percentage of the population within 1 km of accessible greenspace 
#'        (Defra Scenario 2, which included at least 2 hectares of accessible greenspace 
#'        and rural rights of way). The percentage access was inverted to a value of 
#'        between 0 and 1 (1 = least amount of accessible greenspace, i.e. none).
#' #*Climate risks*
#'    - Flood risk:
#'        Proportion of LSOA at risk of flooding from rivers and seas, combined with 
#'        proportion of LSOA at risk of surface water flooding. This data was 
#'        normalised by using a log transformation and expressed on 0-1 scale 
#'        (0 = no part of LSOA at risk of flooding, 1 = all LSOA at risk of flooding).
#'    - Heat exposure risk:
#'        20 year modelled monthly average maximum temperature for 2020-2040 under 
#'        UKCIP18 RCP 8.5 (high emission/least emission mitigation) climate scenario, 
#'        rescaled to a value of 0-1 (0 = lowest average maximum monthly temp, 
#'        1 = highest average maximum monthly temp).
#'        
#' -------------------------------------------------------------------------------------------------------------------------------------
 
# SET UP PROGRAM

# selecting clear_all = T will remove all objects
# selecting clear_all = F will remove all objects except the ONSPD
clear_all = FALSE
source("scripts/set-up.R")
# Load ONSPD (approx 45-60secs)
load_onspd = FALSE
source("scripts/onspd.R")

# ------------------------------------------------------------------------------

# RUN PROGRAM

# main function to build the imed dataframe and save a geopackage output
# Set weightings or leave as default 1.
# Provide name for the geopackage layer which is saved as an output
imed_lsoa <- make_imed_lsoa(ap_wgt = 1,
                            noise_wgt = 1,
                            tree_wgt = 1,
                            greenspace_wgt = 1,
                            flood_wgt = 1,
                            heat_wgt = 1,
                            pollution_wgt = 1,
                            nature_wgt = 1,
                            climate_wgt = 1,
                            flood_log = TRUE,
                            add_other_geog = FALSE,
                            save_gpkg = TRUE,
                            gpkg_layer_name = "imed-lsoa")

# Make and save PNG distribution histograms
make_imed_distribution_plot(imed_lsoa)
make_imed_rank_score_distribution(imed_lsoa)
make_greenspace_histogram(imed_lsoa)
make_nature_histogram(imed_lsoa)
make_pollution_histograms(imed_lsoa)
make_climate_histograms(imed_lsoa)

# save xlsx with imed and domains by LSOA
save_imed_xlsx(imed_lsoa)

# make and save rural urban split of IMED deciles
imed_by_rurality <- make_lsoa_rumorph(onspd)

# make map to check results
make_imed_map(imed_lsoa)

# test stats
summary(imed_lsoa$imed_score)
hist(imed_lsoa$imed_score)
if(any(is.na(imed_lsoa$imed_score)) == TRUE) {
  message("ERROR: some NA values in final index")
}

# END ---------------------------------------------------------------------