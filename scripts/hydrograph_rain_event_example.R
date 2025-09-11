# Load libraries
library(tidyverse)
library(lubridate)

# Read Great Meadow dataset
gm <- read.csv("data/processed_data/great_meadow_well_data_2024_20250715.csv") %>%
  rename(Date = date, Year = year, precip_cm = precip.cm) %>%
  mutate(
    Date = as.Date(Date),
    timestamp = as_datetime(timestamp),
    site = paste("Great Meadow", plot.num),
    water_depth = case_when(
      Year == 2016 & doy_h == 159.12 & plot.num == 3 & water.depth < -120 ~ NA_real_,
      Year == 2017 & doy_h == 215.02 & plot.num == 6 & water.depth < -115 ~ NA_real_,
      Year == 2021 & plot.num == 3 & doy == 224 & water.depth > 400 ~ NA_real_,
      Year == 2021 & plot.num == 3 & doy == 225 & water.depth > 400 ~ NA_real_,
      TRUE ~ water.depth
    )
  ) %>% 
  select(timestamp, 
         date = Date, 
         year = Year, 
         doy, 
         hr, 
         doy_h,
         precip_cm,
         lag_precip = lag.precip,
         water_depth = water.depth,
         site)

# Filter Great Meadow 1, to part of July 2021 with rain event
gm_july2021_half <- gm %>%
  filter(
    site == "Great Meadow 1",
    year == 2021,
    date >= as.Date("2021-07-05"),
    date <= as.Date("2021-07-15")
  )

# Compute min water level for precipitation baseline
minWL <- min(gm_july2021_half$water_depth, na.rm = TRUE)

# Plot with legend for both site + precip
ggplot(gm_july2021_half, aes(x = doy_h)) +
  # Water level (mapped to site for legend)
  geom_line(aes(y = water_depth, color = site), size = 0.8) +
  
  # Precipitation (scaled ×5 + minWL baseline, added as its own legend item)
  geom_line(aes(y = lag_precip * 5 + minWL, color = "Precipitation"), size = 0.8) +
  
  # Ground level
  geom_hline(yintercept = 0, color = "brown") +
  
  # Colors: include Great Meadow 1 and Precipitation
  scale_color_manual(
    values = c("Great Meadow 1" = "black", "Precipitation" = "blue"),
    breaks = c("Great Meadow 1", "Precipitation")
  ) +
  
  # Labels
  labs(
    title = NULL,
    y = "Water Level (cm)", 
    x = "Date",
    color = NULL
  ) +
  
  # X axis cropped part of July with rain event
  scale_x_continuous(
    breaks = c(186, 191, 196),   
    labels = c("Jul-05", "Jul-10", "Jul-15")
  ) +
  
  # Secondary axis for precip
  scale_y_continuous(
    sec.axis = sec_axis(~ .,
                        breaks = c(minWL, minWL + 10),
                        name = "Hourly Precip. (cm)",
                        labels = c("0", "2"))
  ) +
  
  # Theme
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.text.y.right = element_text(color = "blue"),
    axis.title.y.right = element_text(color = "blue"),
    legend.position = "bottom",
    legend.title = element_blank()
  )



#ggsave("outputs/hydrographs_rain_event/GM1_hydrograph_rain_event.png", width = 8, height = 5, dpi = 300)





####----------------------------------------------------------------------------



# Load libraries
library(tidyverse)
library(lubridate)

# --- Read Great Meadow data ---
gm <- read.csv("data/processed_data/great_meadow_well_data_2024_20250715.csv") %>%
  rename(Date = date, Year = year, precip_cm = precip.cm) %>%
  mutate(
    Date = as.Date(Date),
    timestamp = as_datetime(timestamp),
    site = paste("Great Meadow", plot.num),
    water_depth = case_when(
      Year == 2016 & doy_h == 159.12 & plot.num == 3 & water.depth < -120 ~ NA_real_,
      Year == 2017 & doy_h == 215.02 & plot.num == 6 & water.depth < -115 ~ NA_real_,
      Year == 2021 & plot.num == 3 & doy == 224 & water.depth > 400 ~ NA_real_,
      Year == 2021 & plot.num == 3 & doy == 225 & water.depth > 400 ~ NA_real_,
      TRUE ~ water.depth
    )
  )

# --- Read Gilmore Meadow data (similar structure assumed) ---
gl <- read.csv("data/raw_data/hydrology_data/gilmore_well_prec_data_2013-2024.csv") %>%
  rename(water.depth = GILM_WL) %>%
  mutate(
    site = "Gilmore Meadow",
    
    # Try to parse full datetime, fallback to date only + midnight
    timestamp_parsed = mdy_hm(timestamp),
    timestamp_parsed = if_else(
      is.na(timestamp_parsed),
      mdy(timestamp),
      timestamp_parsed
    ),
    timestamp_parsed = as.POSIXct(timestamp_parsed),
    timestamp = timestamp_parsed,
    
    # Standardize Date column too
    Date = mdy(Date)
  ) %>%
  select(timestamp, 
         Date,
         doy,
         Year,
         precip_cm,
         water.depth,
         lag.precip,
         hr,
         doy_h,
         site)

# --- Combine datasets ---
all_sites <- bind_rows(gm, gl)  %>% 
  select(timestamp, 
         date = Date, 
         year = Year, 
         doy, 
         hr, 
         doy_h,
         precip_cm,
         lag_precip = lag.precip,
         water_depth = water.depth,
         site)

# --- Filter both Great Meadow 1 and Gilmore Meadow for July 5–15, 2021 ---
july_subset <- all_sites %>%
  filter(
    site %in% c("Great Meadow 1", "Gilmore Meadow"),
    year == 2021,
    date >= as.Date("2021-07-05"),
    date <= as.Date("2021-07-15")
  )

# Compute min water level across both sites for precip baseline
minWL <- min(july_subset$water_depth, na.rm = TRUE)

# --- Plot ---
ggplot(july_subset, aes(x = doy_h)) +
  # Water level for both sites
  geom_line(aes(y = water_depth, color = site), size = 0.9) +
  
  # Precipitation (scaled ×5 + minWL baseline)
  geom_line(aes(y = lag_precip * 5 + minWL, color = "Precipitation"), size = 0.9) +
  
  # Ground level
  geom_hline(yintercept = 0, color = "brown") +
  
  # Colors
  scale_color_manual(
    values = c(
      "Great Meadow 1" = "black",
      "Gilmore Meadow" = "darkgray",
      "Precipitation" = "blue"
    ),
    breaks = c("Great Meadow 1", "Gilmore Meadow", "Precipitation")
  ) +
  
  # Labels
  labs(
    title = NULL,
    y = "Water Level (cm)", 
    x = "Date",
    color = NULL
  ) +
  
  # X axis cropped part of July with rain event
  scale_x_continuous(
    breaks = c(186, 191, 196),   
    labels = c("Jul-05", "Jul-10", "Jul-15")
  ) +
  
  # Secondary axis for precip
  scale_y_continuous(
    sec.axis = sec_axis(~ .,
                        breaks = c(minWL, minWL + 10),
                        name = "Hourly Precip. (cm)",
                        labels = c("0", "2"))
  ) +
  
  # Theme
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.text.y.right = element_text(color = "blue"),
    axis.title.y.right = element_text(color = "blue"),
    legend.position = "bottom",
    legend.title = element_blank()
  )


#ggsave("outputs/hydrographs_rain_event/GL_GM1_hydrograph_rain_event.png", width = 8, height = 5, dpi = 300)

