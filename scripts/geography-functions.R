# A series of functions processing geographical data or making geographical lookups


# join main geographies to lsoa data set ----------------------------------

join_msoa_la_region_to_lsoa_data <- function(lsoa_data) {
  
  lsoa_to_msoa_lup <- make_lsoa_to_msoa_lup()
  lsoa_to_oslaua_lup <- make_lsoa_to_oslaua_lup()
  lsoa_to_region_lup <- make_lsoa_to_region_lup()
  
  imed_lsoa <- join_list_of_datasets(lsoa11_code_left_join,
                                     lsoa_data,
                                     lsoa_to_msoa_lup,
                                     lsoa_to_oslaua_lup,
                                     lsoa_to_region_lup)
  
  imed_lsoa <- imed_lsoa %>% 
    select(lsoa11,
           msoa11,
           msoa11_name,
           oslaua,
           local_authority,
           rgn,
           region_name,
           everything())
  
}




# ------------------------------------------------------------------------------
# get lsoa boundaries, full extent, clipped to coastline
get_lsoa_boundaries <- function(type = "full") {
  
  # Define the root path
  root_path <- "PATH_TO_LSOA"
  
  if(type == "full") {
    lsoa_boundaries <- st_read(file.path(root_path, "/gis-data/boundaries/lsoa/LSOA_2011_EW_BFC_V3/LSOA_2011_EW_BFC_V3.shp"),
                               quiet = TRUE)  
    lsoa_boundaries <- lsoa_boundaries %>% 
      select(LSOA11CD,
             geometry)
  }
  
  if(type == "super-g") {
    lsoa_boundaries <- st_read("../gis-data/boundaries/lsoa/LSOA_2011_EW_BSC_V4/LSOA_2011_EW_BSC_V4.shp")
    lsoa_boundaries <- lsoa_boundaries %>% 
      st_transform(crs = 27700) %>% 
      select(LSOA11CD,
             geometry)
  }
  
  return(lsoa_boundaries)
  
}



# ------------------------------------------------------------------------------
get_onspd <- function(keep.only.current = FALSE) {
  
  # set timer
  message("Reading in ONSPD...")
  tic("ONSPD loaded:")
  #set current filepath of ONSPD (to allow updates to ONSPD in data without having to update script)
  onspd.location <- list.files("../ons-geog-data/onspd/Data", 
                               pattern = "ONSPD.+csv", 
                               full.names = TRUE)
  # read in full onspd
  onspd <- read.csv(onspd.location,
                    stringsAsFactors = FALSE)
  
  # keep only current postcodes
  if(keep.only.current) {
    onspd <- onspd %>% 
      filter(is.na(doterm))
  }
  
  # select only geographical fields of interest
  onspd <- onspd %>% 
    select(pcds,
           dointr,
           doterm,
           oa11,
           lsoa11,
           msoa11,
           osward,
           pcon,
           oslaua,
           oscty,
           rgn,
           lat,
           long,
           oseast1m,
           osnrth1m)
  
  # end timer
  toc()
  
  # return ONSPD
  return(onspd)
}

# ------------------------------------------------------------------------------

get_oslaua_names <- function() {
  
  la_names_location <- list.files("../ons-geog-data/onspd/Documents/", pattern = "LA_UA.+csv", full.names = TRUE)
  la_names <- read.csv(la_names_location)
  la_names <- la_names[1:2]
  colnames(la_names)[1:2] <- c("oslaua", "local_authority")
  return(la_names)
  
}

make_lsoa_to_oslaua_lup <- function(add_la_names = TRUE) {
  
  # make England and Wales LSOA to MSOA lookup
  lsoa_to_oslaua <- onspd %>% 
    filter_for_england(lsoa11) %>% 
    group_by(lsoa11,
             oslaua) %>% 
    summarise(n = n()) %>% 
    group_by(lsoa11) %>% 
    slice_max(order_by = n,
              n = 1) %>% 
    ungroup() %>% 
    select(-n)
  
  #test <- lsoa_to_msoa %>% 
  #  group_by(lsoa11) %>% 
  #  summarise(n = n()) %>% 
  #  ungroup() %>% 
  #  filter(n > 1)
  
  # get msoa names and join
  if(add_la_names == TRUE) {
    la_names <- get_oslaua_names()
    lsoa_to_oslaua <- left_join(lsoa_to_oslaua, la_names, by = "oslaua")  
  }
  
  return(lsoa_to_oslaua)
  
}

# ------------------------------------------------------------------------------

get_region_names <- function() {
  
  region_names_location <- list.files("../ons-geog-data/onspd/Documents/", pattern = "Region.+csv", full.names = TRUE)
  region_names <- read.csv(region_names_location)
  region_names <- region_names[c(1,3)]
  colnames(region_names)[1:2] <- c("rgn", "region_name")
  return(region_names)
  
}

make_lsoa_to_region_lup <- function() {
  
  # make England and Wales LSOA to MSOA lookup
  lsoa_to_region <- onspd %>% 
    filter_for_england(lsoa11) %>% 
    group_by(lsoa11,
             rgn) %>% 
    summarise(n = n()) %>% 
    group_by(lsoa11) %>% 
    slice_max(order_by = n,
              n = 1) %>% 
    ungroup() %>% 
    select(-n)
  
  #test <- lsoa_to_msoa %>% 
  #  group_by(lsoa11) %>% 
  #  summarise(n = n()) %>% 
  #  ungroup() %>% 
  #  filter(n > 1)
  
  # get msoa names and join
  region_names <- get_region_names()
  lsoa_to_region <- left_join(lsoa_to_region, region_names, by = "rgn")
  
}

# ------------------------------------------------------------------------------

get_msoa_names <- function() {
  
  msoa_names <- read.csv("../environmental-data-for-change/data/general/MSOA-Names-1.20.csv",
                         stringsAsFactors = FALSE)
  
  msoa_names <- msoa_names %>% 
    select(msoa11 = msoa11cd,
           msoa11_name = msoa11hclnm)
}

make_lsoa_to_msoa_lup <- function() {
  
  # make England and Wales LSOA to MSOA lookup
  lsoa_to_msoa <- onspd %>% 
    distinct(lsoa11,
             msoa11) %>% 
    filter_for_england(lsoa11)
  
  #test <- lsoa_to_msoa %>% 
  #  group_by(lsoa11) %>% 
  #  summarise(n = n()) %>% 
  #  ungroup() %>% 
  #  filter(n > 1)
  
  # get msoa names and join
  msoa_names <- get_msoa_names()
  lsoa_to_msoa <- left_join(lsoa_to_msoa, msoa_names, by = "msoa11")
  
}

get_msoa_names <- function() {
  
  msoa_names <- read.csv("../environmental-data-for-change/data/general/MSOA-Names-1.20.csv",
                         stringsAsFactors = FALSE)
  
  msoa_names <- msoa_names %>% 
    select(msoa11 = msoa11cd,
           msoa11_name = msoa11hclnm)
}


#-------------------------------------------------------------------------------
