#' A series of common functions used across the program in different applications

# ------------------------------------------------------------------------------
# set to lower case and remove all punctuation and spaces from column names
tidy_column_names <- function(data, start_col = 2) {
  
  number_of_cols <- ncol(data)
  
  old_names <- names(data[start_col:number_of_cols])
  new_names <- tolower(old_names)
  new_names <- gsub("[[:punct:]]+", " ", new_names)
  new_names <- gsub("[ ]+", "_", new_names)
  new_names <- gsub("[_]+$", "", new_names)
  
  colnames(data)[start_col:number_of_cols] <- new_names
  
  return(data)  
}

make_nice_col_names <- function(data) {
  
  old_names <- names(data)
  new_names <- gsub("_", " ", old_names)
  new_names <- paste0(toupper(substring(new_names, 1, 1)), paste0(substring(new_names, 2)))
  new_names <- gsub("_pct", " (%)", new_names)
  new_names <- gsub("_n$|_number$", " (no.)", new_names)
  
  colnames(data) <- new_names
  
  return(data)  
  
}

# ------------------------------------------------------------------------------
# set any column values to 0 if they are null
is_null_zero <- function(x) ifelse(is.na(x), 0, x)

# ------------------------------------------------------------------------------
# function used to save geopackage layers
write_gpkg <- function(gpkg_name, layer, layer_name) {
  
  st_write(obj = layer,
           dsn = gpkg_name,
           layer = layer_name,
           delete_layer = TRUE)
  
}

# ------------------------------------------------------------------------------
# functions to join data sets for brevity.
join_list_of_datasets <- function(type_of_join, ...) {
  output <- Reduce(type_of_join,
                   list(...))
}

lsoa11_code_left_join <- function(x, y) left_join(x, y, by = "lsoa11")
lsoa11_code_inner_join <- function(x, y) inner_join(x, y, by = "lsoa11")

# ------------------------------------------------------------------------------
# get most recent population data by LSOA
get_ons20_pop_lsoa <- function() {
  
  #list.files("data/reference")
  #getSheetNames("data/reference/sape23dt4mid2020msoasyoaestimatesunformatted.xlsx")
  pop_lsoa <- read.xlsx("data/population/sape23dt13mid2020lsoabroadagesestimatesunformatted.xlsx",
                        sheet = "Mid-2020 Persons",
                        startRow = 4)
  pop_lsoa <- pop_lsoa %>% 
    select(lsoa11 = LSOA.Code,
           population = All.Ages)
  
}

# ------------------------------------------------------------------------------
filter_for_england <- function(data, ons_code) {
  data <- data %>% 
    filter(grepl("^E", {{ ons_code }}))
}


# ------------------------------------------------------------------------------

rescale_zero_to_one <- function(x, negative = FALSE) {
  
  # check if any NAs
  if(any(is.na(x))) {
    message(paste0("ERROR: ", x, " contains NA values"))
  }
  
  if(negative) {
    round(rescale(x, to = c(1,0)), 5)
  } else {
    round(rescale(x, to = c(0,1)), 5)
  }
  
  
}

# ------------------------------------------------------------------------------

name_list <- \(x) data.frame(names(x))

# ------------------------------------------------------------------------------

set_max <- function(x, m) ifelse(x > m, m, x)

# ------------------------------------------------------------------------------

save_csv <- function(df,
                     folder,
                     file_name,
                     version_no) {
  
  full_file_path <- paste0(folder, file_name, "-", version_no, ".csv")
  message(paste0("Saving: ", full_file_path))
  write.csv(df,
            full_file_path,
            row.names = FALSE,
            na = "")
  
}

# -------------------------------------------------------------------------
