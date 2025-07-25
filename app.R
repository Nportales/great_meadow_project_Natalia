# Load packages
library(shiny)
library(tidyverse)
library(lubridate)
library(patchwork)
library(shinyWidgets)
library(DT)

# Read & prepare your processed data (from your current script)
gm <- read.csv("data/processed_data/great_meadow_well_data_2024_20250715.csv") %>%
  rename(Date = date, Year = year, precip_cm = precip.cm) %>%
  mutate(Date = as.Date(Date),
         timestamp = as_datetime(timestamp),
         site = paste("Great Meadow", plot.num),
         water.depth = case_when(
           Year == 2016 & doy_h == 159.12 & plot.num == 3 & water.depth < -120 ~ NA_real_,
           Year == 2017 & doy_h == 215.02 & plot.num == 6 & water.depth < -115 ~ NA_real_,
           Year == 2021 & plot.num == 3 & doy == 224 & water.depth > 400 ~ NA_real_,
           Year == 2021 & plot.num == 3 & doy == 225 & water.depth > 400 ~ NA_real_,
           TRUE ~ water.depth
         )) %>%
  mutate(siteyear = paste(site, Year, sep = "_"))

gl <- read.csv("data/raw_data/hydrology_data/gilmore_well_prec_data_2013-2024.csv") %>%
  rename(water.depth = GILM_WL) %>%
  select(-timestamp) %>%  # drop the all-NA column
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"),
         site = "Gilmore Meadow",
         siteyear = paste(site, Year, sep = "_"))

all_data <- bind_rows(gm, gl) %>% 
  filter(Year >= 2016 & Year <= 2023) 

wl_stats <- read.csv("data/processed_data/gm_gl_wl_stats.csv") %>% 
  select(year, stat, gilmore.meadow, great.meadow.1, great.meadow.2, 
         great.meadow.3, great.meadow.4, great.meadow.5, great.meadow.6) %>% 
  pivot_longer(cols = -c(year, stat), names_to = "site", values_to = "value") %>% 
  pivot_wider(names_from = stat, values_from = value) %>%
  arrange(site, year)

# UI
ui <- fluidPage(
  titlePanel("Wetland Hydrograph Viewer"),
  sidebarLayout(
    sidebarPanel(
      pickerInput("year", "Select Year(s):",
                  choices = sort(unique(all_data$Year)),
                  selected = 2023,
                  multiple = TRUE,
                  options = list(`actions-box` = TRUE,
                                 `deselect-all-text` = "Clear all",
                                 `select-all-text` = "Select all",
                                 `none-selected-text` = "Choose year(s)")),
      selectInput("gm_site", "Select Great Meadow Plot for Hydrograph:",
                  choices = sort(unique(all_data$site[grepl("Great Meadow", all_data$site)])),
                  selected = "Great Meadow 1"),
      
      selectInput("site", "Select Site(s) for Water Level Stats:",
                  choices = sort(unique(wl_stats$site)),
                  selected = "great.meadow.1",
                  multiple = TRUE)
    ),
    mainPanel(
      plotOutput("hydrograph", height = "600px"),
      br(),
      h4("Growing Season Water Level Statistics"),
      dataTableOutput("wl_stats")
    )
  )
)

# Server
server <- function(input, output, session) {
  output$hydrograph <- renderPlot({
    req(input$year, input$gm_site)
    
    plot_list <- lapply(input$year, function(yr) {
      plot_data_year <- all_data %>%
        filter(Year == yr, doy > 134, doy < 275)
      
      # Calculate min water level for consistent precipitation offset
      min_gm <- plot_data_year %>%
        filter(site == input$gm_site, !is.na(water.depth)) %>%
        pull(water.depth) %>% 
        min(na.rm = TRUE)
      
      min_gil <- plot_data_year %>%
        filter(site == "Gilmore Meadow", !is.na(water.depth)) %>%
        pull(water.depth) %>% 
        min(na.rm = TRUE)
      
      minWL <- min(min_gm, min_gil)
      
      # Create individual hydrograph plot
      ggplot() +
        geom_line(data = plot_data_year %>% filter(site == input$gm_site),
                  aes(x = doy_h, y = water.depth, color = input$gm_site), size = 0.7) +
        geom_line(data = plot_data_year %>% filter(site == "Gilmore Meadow"),
                  aes(x = doy_h, y = water.depth, color = "Gilmore Meadow"), size = 0.7) +
        geom_line(data = plot_data_year,
                  aes(x = doy_h, y = lag.precip * 5 + minWL),
                  color = "blue", size = 0.7) +
        geom_hline(yintercept = 0, color = "brown") +
        scale_color_manual(values = setNames(c("black", "darkgray"), 
                                             c(input$gm_site, "Gilmore Meadow"))) +
        labs(title = paste("Hydrograph for", yr),
             x = "Date", y = "Water Level (cm)") +
        scale_x_continuous(
          breaks = c(121, 152, 182, 213, 244, 274),
          labels = c('May-01', 'Jun-01', 'Jul-01', 'Aug-01', 'Sep-01', 'Oct-01')) +
        scale_y_continuous(
          name = "Water Level (cm)",
          sec.axis = sec_axis(~ (. - minWL) / 5,
                              name = "Hourly Precip (cm)",
                              breaks = seq(0, 8, by = 2))
        ) +
        theme_bw() +
        theme(legend.position = "bottom",
              legend.title = element_blank(),
              strip.text = element_text(size = 11),
              plot.title = element_text(hjust = 0.5))
    })
    
    # Combine plots vertically using patchwork
    wrap_plots(plot_list, ncol = 1)
  })
  
  output$wl_stats <- DT::renderDataTable({
    req(input$site, input$year)
    
    wl_stats %>%
      filter(site %in% input$site, year %in% input$year) %>% 
      rename(
        Year = year,
        Site = site,
        `Mean Water Level` = WL_mean,
        `SD Water Level` = WL_sd,
        `Minimum Water Level` = WL_min,
        `Maximum Water Level` = WL_max,
        `Maximum Water Level` = sd,
        `Maximum hourly increase` = max_inc,
        `Maximum hourly decrease` = max_dec,
        `Growing Season Change` = GS_change,
        `GS % surface water` = prop_GS_comp,
        `GS % over 30cm deep` = prop_over_0cm,
        `GS % within 30cm` = prop_bet_0_neg30cm,
        `GS % over 30cm deep` = prop_under_neg30cm
      )
  })
  
  
}



# Run app
shinyApp(ui, server)
