## stat testing - data visualization ##

#---------------------------------------------#
####        Load Required Packages         ####
#---------------------------------------------#

library(tidyverse)
library(dplyr)

#-----------------------#
####    Read Data    ####
#-----------------------#

## water level stats data ##

wl_stats <- read.csv("data/processed_data/hydrology_data/gm_gl_wl_stats_2025_20260304.csv") %>% 
  select(year, stat, `Gilmore Meadow 1` = gilmore.meadow, 
         `Great Meadow 1` = great.meadow.1, `Great Meadow 2` = great.meadow.2, 
         `Great Meadow 3` = great.meadow.3, `Great Meadow 4` = great.meadow.4, 
         `Great Meadow 5` = great.meadow.5, `Great Meadow 6` = great.meadow.6) %>% 
  pivot_longer(cols = -c(year, stat), names_to = "site", values_to = "value") %>% 
  pivot_wider(names_from = stat, values_from = value) %>%
  arrange(site, year) %>% 
  mutate(wetland = if_else(grepl("Great Meadow", site), "Great Meadow", "Gilmore Meadow"))

#----------------------------#
####    Stats Testing     #### 
#----------------------------#

# dashboard statistical significance function: site-level -> averaged to wetland-year -> paired t-test across years
calculate_wetland_significance <- function(data, selected_years, selected_sites, alpha = 0.05) {
  if (length(selected_years) <= 3) {
    message("⚠️ Not enough years selected for significance testing (need >3).")
    return(NULL)
  }
  
  filtered_data <- data %>%
    filter(year %in% selected_years, site %in% selected_sites) %>%
    mutate(site_group = if_else(grepl("Great Meadow", site), "Great Meadow", "Gilmore Meadow"))
  
  wetlands_present <- unique(filtered_data$site_group)
  if (length(wetlands_present) < 2 || !all(c("Great Meadow", "Gilmore Meadow") %in% wetlands_present)) {
    message("⚠️ Both Great Meadow and Gilmore Meadow must be present for comparison.")
    return(NULL)
  }
  
  stat_cols <- filtered_data %>% select(where(is.numeric), -year) %>% names()
  
  yearly_means <- filtered_data %>%
    group_by(year, site_group) %>%
    summarise(across(all_of(stat_cols), \(x) mean(x, na.rm = TRUE)), .groups = "drop")
  
  map_dfr(stat_cols, function(var) {
    tryCatch({
      # reshape to wide format (one row per year)
      wide_data <- yearly_means %>%
        select(year, site_group, all_of(var)) %>%
        tidyr::pivot_wider(names_from = site_group, values_from = all_of(var)) %>%
        drop_na()  # ensure complete pairs
      # paired t-test
      test <- t.test(wide_data[["Great Meadow"]], wide_data[["Gilmore Meadow"]], paired = TRUE)
      data.frame(variable = var, p_value = test$p.value, significant = test$p.value < alpha)
    }, error = function(e) {
      data.frame(variable = var, p_value = NA, significant = FALSE)
    })
  })
}

results <- calculate_wetland_significance(
  wl_stats,
  2016:2025,
  unique(wl_stats$site)
)

View(results)
