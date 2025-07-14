## data visualization figure drafts ##

#---------------------------------------------#
####        Load Required Packages         ####
#---------------------------------------------#

library(tidyverse)
library(dplyr)
library(ggplot2)
library(broom)
library(purrr)

#-----------------------#
####    Read Data    ####
#-----------------------#

#Reading in CSVs as a tibble

## Veg data ##

VMMI_FOA_NETN <- read.csv("data/processed_data/FOA_NETN_VMMI_2011_2024.csv") %>%
  as_tibble()

species_list <- read.csv("data/processed_data/FOA_NETN_species_list_2011_2024.csv")


#----------------------------#
####    Plot Generation   #### 
#----------------------------#

# generic plot of VMMI values by site over time
ggplot(VMMI_FOA_NETN, aes(x = year, y = vmmi, color = site.name, group = site.name)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "VMMI Trends Over Time by Site",
       x = "Year",
       y = "VMMI",
       color = "Site Name") +
  theme_minimal()

# facet by wetland
ggplot(VMMI_FOA_NETN, aes(x = year, y = vmmi, color = site.name, group = site.name)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  facet_wrap(~ wetland) +  # Facet by wetland
  labs(title = "VMMI Trends Over Time by Site and Wetland",
       x = "Year",
       y = "VMMI",
       color = "Site") +
  theme_minimal()

# facet by wetland with threshold VMMI rankings
ggplot(VMMI_FOA_NETN, aes(x = year, y = vmmi, color = site.name, group = site.name)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 65.22746, linetype = "dashed", color = "darkgreen") +  # Good
  geom_hline(yintercept = 52.785, linetype = "dashed", color = "red") +          # Poor
  facet_wrap(~ wetland) +
  labs(title = "VMMI Trends Over Time by Site and Wetland",
       x = "Year",
       y = "VMMI",
       color = "Site") +
  theme_minimal()


# plot for average VMMI trends per wetland
# first calculate average VMMI per year per wetland
wetland_summary <- VMMI_FOA_NETN %>%
  group_by(wetland, year) %>%
  summarise(mean_vmmi = mean(vmmi, na.rm = TRUE))

# plot
ggplot(wetland_summary, aes(x = year, y = mean_vmmi, color = wetland, group = wetland)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(title = "Average VMMI Trends by Wetland",
       x = "Year",
       y = "Mean VMMI",
       color = "Wetland") +
  theme_minimal()


# plot box plots for summarizing VMMI per year across all sites 
ggplot(VMMI_FOA_NETN, aes(x = factor(year), y = vmmi)) +
  geom_boxplot() +
  facet_wrap(~ wetland) + 
  labs(title = "Distribution of VMMI by Year",
       x = "Year", 
       y = "VMMI") +
  theme_light()


# significance testing

site_models <- VMMI_FOA_NETN %>%
  group_by(site.name) %>%
  do(tidy(lm(vmmi ~ year, data = .)))

site_models %>% filter(term == "year")


# residuals
models_nested <- VMMI_FOA_NETN %>%
  group_by(site.name) %>%
  nest() %>%
  mutate(model = map(data, ~lm(vmmi ~ year, data = .)))

# Use `glance()` on each model to get model-level stats (e.g., R², p-value)
model_summaries <- models_nested %>%
  mutate(glance_out = map(model, glance)) %>%
  unnest(glance_out)


# graph invasive species by year

summary_table <- species_list %>%
  filter(invasive == "TRUE", ) %>%
  group_by(year) %>%
  summarise(
    count = n_distinct(latin.name),
    invasive_species = paste(unique(latin.name), collapse = ", ")
  )


# ggplot(summary_table, aes(x = factor(year), y = count)) +
#   geom_bar(stat = "identity", fill = "tomato") +
#   labs(
#     title = "Number of Invasive Species by Year",
#     x = "Year",
#     y = "Number of Invasive Species"
#   ) +
#   theme_minimal()



