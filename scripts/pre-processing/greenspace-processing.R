# NEW GREENSPACE DATA AUGUST 2024 -----------------------------------------

# new greenspace data August 2024
get_greenspace_data_lsoa <- function(scenario,
                                     name) {
  
  tic(paste0("greenspace data read in for ", scenario))
  list_ods_sheets("data/nature/greenspace/Access_to_greenspace_England_2024.ods")
  gs <- read_ods("data/nature/greenspace/Access_to_greenspace_England_2024.ods",
                 sheet = scenario,
                 skip = 5)
  toc()
  
  tic(paste0("GS data summarised at LSOA11 level for ", scenario))
  gs <- tidy_column_names(gs, start_col = 1)
  gs_lsoa <- gs %>% 
    group_by(lsoa21,
             rural_urban) %>% 
    summarise(across(all_of(c("household_count", "household_with_access_count")), \(x) sum(x, na.rm = TRUE))) %>% 
    ungroup() %>% 
    mutate(percentage_with_access = household_with_access_count / household_count)
  
  lsoa21_to_lsoa11_lup <- read.csv("data/geography/lookups/lsoa11-to-lsoa21-postcode-lup.csv",
                                   stringsAsFactors = FALSE)
  
  gs_lsoa11 <- left_join(gs_lsoa, lsoa21_to_lsoa11_lup, by = c("lsoa21" = "LSOA21CD"), relationship = "many-to-many")
  
  gs_lsoa11 <- gs_lsoa11 %>% 
    group_by(LSOA11CD) %>% 
    summarise(household_count = sum(round(household_count * postcode_pct)),
              household_with_access_count = sum(round(household_with_access_count * postcode_pct))) %>% 
    ungroup() %>% 
    mutate(percentage_with_access = household_with_access_count / household_count)
  
  # keep only england LSOA data
  gs_lsoa11 <- gs_lsoa11 %>% 
    filter(substring(LSOA11CD, 1, 1) == "E")
  
  toc()
  
  # finalise with name
  gs_lsoa11 <- gs_lsoa11 %>%  
    transmute(lsoa11 = LSOA11CD,
              "{{ name }}_gs_access_pct" := percentage_with_access)
  
}

combine_and_save_all_gs_aug24_data <- function() {
 
  gs_1 <- get_greenspace_data_lsoa(scenario = "Scenario1",
                                   name = all)
  gs_2 <- get_greenspace_data_lsoa(scenario = "Scenario2",
                                   name = all_with_row)
  gs_3 <- get_greenspace_data_lsoa(scenario = "Scenario3",
                                   name = doorstep)
  gs_4 <- get_greenspace_data_lsoa(scenario = "Scenario4",
                                   name = local)
  gs_5 <- get_greenspace_data_lsoa(scenario = "Scenario5",
                                   name = neighbourhood)
  gs_6 <- get_greenspace_data_lsoa(scenario = "Scenario6",
                                   name = combined_std)
  gs_7 <- get_greenspace_data_lsoa(scenario = "Scenario7",
                                   name = partialcombined_std)
  
  greenspace_lsoa11 <- join_list_of_datasets(lsoa11_code_inner_join,
                                             gs_1,
                                             gs_2,
                                             gs_3,
                                             gs_4,
                                             gs_5,
                                             gs_6,
                                             gs_7)
  
  message(paste0("Saving: ", "data/nature/greenspace/greenspace-access-all-standards-lsoa11-august2024.csv"))
  write.csv(greenspace_lsoa11,
            "data/nature/greenspace/greenspace-access-all-standards-lsoa11-august2024.csv",
            row.names = FALSE,
            na="")

  return(greenspace_lsoa11)
  
  }


greenspace_lsoa11 <- combine_and_save_all_gs_aug24_data()

library(ggplot2)

greenspace_lsoa11_long <- greenspace_lsoa11 %>% 
  gather(key = measure,
         value = access_pct,
         -lsoa11) %>% 
  mutate(measure = gsub("_access_pct", "", measure)) %>% 
  mutate(measure = gsub("[_]+", " ", measure)) %>% 
  mutate(measure = gsub("gs", "GS", measure))  %>% 
  mutate(measure = gsub("row", "PRoW", measure)) 
  
table(greenspace_lsoa11_long$measure)
  
g <- ggplot(data = greenspace_lsoa11_long, aes(x = access_pct, fill = measure)) + 
  geom_histogram(binwidth = 0.01) + 
  facet_wrap(. ~ measure,
             ncol = 1,
             scales = "free_y") + 
  theme_foe()

plot(g)
