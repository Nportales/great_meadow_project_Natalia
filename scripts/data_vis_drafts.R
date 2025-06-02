## data visualization figure drafts ##

#---------------------------------------------#
####        Load Required Packages         ####
#---------------------------------------------#

library(tidyverse)
library(dplyr)
library(ggplot2)

#-----------------------#
####    Read Data    ####
#-----------------------#

#Reading in CSVs as a tibble

## Veg data ##

VMMI_Glen_NETN <- read.csv("data/processed_data/VMMI_Glen_NETN_2011_2024.csv") %>%
  as_tibble()


#----------------------------#
####    Plot Generation   #### 
#----------------------------#

# generic plot of VMMI values by site over time
ggplot(VMMI_Glen_NETN, aes(x = year, y = vmmi, color = site.name, group = site.name)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "VMMI Trends Over Time by Site",
       x = "Year",
       y = "VMMI",
       color = "Site Name") +
  theme_minimal()

# facet by wetland
ggplot(VMMI_Glen_NETN, aes(x = year, y = vmmi, color = site.name, group = site.name)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  facet_wrap(~ wetland) +  # Facet by wetland
  labs(title = "VMMI Trends Over Time by Site and Wetland",
       x = "Year",
       y = "VMMI",
       color = "Site") +
  theme_minimal()



# plot for average VMMI trends per wetland
# first calculate average VMMI per year per wetland
wetland_summary <- VMMI_Glen_NETN %>%
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

