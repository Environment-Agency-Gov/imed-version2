if(clear_all) {
  rm(list = ls())  
} else {
  rm(list = ls()[!grepl("onspd", ls())])
}

load_packages <- function() {
  
  library(pacman)
  
  packages <- c("tidyverse",
                "openxlsx",
                "readODS",
                "sf",
                "tictoc",
                "tmap",
                "scales",
                "ncdf4",
                "PCICt",
                "RColorBrewer",
                "extrafont",
                "ggstatsplot")
  
  for (package in packages) {
    if (!require(package, character.only = TRUE)) {
      install.packages(package)
      library(package, character.only = TRUE)
    }
  }
  
  # library(tidyverse)
  # library(openxlsx)
  # library(readODS)
  # library(sf)
  # library(tictoc)
  # library(tmap)
  # tmap_mode("view")
  # library(scales)
  # 
  # library(ncdf4)
  # library(PCICt)
  # 
  # library(RColorBrewer)
  # library(extrafont)
  # 
  # library(ggstatsplot)
}

#load_packages()

load_scripts <- function() {
  
  # set up 
  source("scripts/common-functions.R")
  source("scripts/geography-functions.R")
  
  # Domains
  # Pollution
  source("scripts/domains/air-pollution.R")
  source("scripts/domains/noise.R")
  # Nature
  source("scripts/domains/tree-canopy-cover.R")
  source("scripts/domains/greenspace.R")
  # Climate risk
  source("scripts/domains/flood-risk.R")
  source("scripts/domains/heat-exposure.R")
  
  # IMED combine and scoring
  source("scripts/main-functions.R")
  
  # Outputs
  source("scripts/output-functions.R")
  source("scripts/save-workbook.R")
  source("scripts/output-xlsx-file.R")

}

#load_scripts()

options(dplyr.summarise.inform = FALSE)

# create outputs folder if doesn't exist (e,g, first time of running program)
if(!dir.exists("outputs")) {
  dir.create("outputs")
}

if(!dir.exists("outputs/histograms")) {
  dir.create("outputs/histograms")
}
