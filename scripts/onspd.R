get_onspd <- function(keep.only.current = TRUE, include.geom = TRUE) {
  
  # set timer
  message("reading in ONSPD...")
  tic("ONSPD loaded")
  #set current filepath of ONSPD (to allow updates to ONSPD in data without having to update script)
  onspd_location <- list.files("../ons-geog-data/onspd/Data", 
                               pattern = "ONSPD.+csv", 
                               full.names = TRUE)
  # read in full onspd
  onspd <- read.csv(onspd_location,
                    stringsAsFactors = FALSE)
  
  # keep only current postcodes
  if(keep.only.current) {
    onspd <- onspd %>% 
      filter(is.na(doterm))  
  }
  
  
  # select only geographical fields of interest
  if(include.geom) {
    onspd <- onspd %>% 
      select(pcds,
             oseast1m,
             osnrth1m,
             lat,
             long,
             oa11,
             lsoa11,
             msoa11,
             osward,
             pcon,
             oslaua,
             oscty,
             rgn,
             ru11ind)
  } else {
    onspd <- onspd %>% 
      select(pcds,
             oa11,
             lsoa11,
             msoa11,
             osward,
             pcon,
             oslaua,
             oscty,
             rgn,
             ru11ind)
  }
  
  
  
  # end timer
  toc()
  
  # return ONSPD
  return(onspd)
}

# run onspd if requested
if(load_onspd) {
  onspd <<- get_onspd()
}


make_simple_onspd <- function(onspd) {
  
  onspd_simple <- onspd %>% 
    select(pcds,
           lsoa11,
           oslaua,
           oseast1m,
           osnrth1m) %>% 
    na.omit(oseast1m)
  
  write.csv(onspd_simple,
            "../ons-geog-data/onspd/onspd-feb24-simple.csv",
            row.names = FALSE)
  
}

make_onspd_sf <- function(onspd) {
  
  onspd_sf <- onspd %>% 
    filter(!is.na(oseast1m)) %>% 
    select(pcds,
           oseast1m,
           osnrth1m) %>% 
    st_as_sf(coords = c("oseast1m", "osnrth1m"),
             crs = 27700)
  
}
