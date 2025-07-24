# Load packages
library(shiny)
library(tidyverse)
library(lubridate)

# Read & prepare your processed data (from your current script)
gm <- read_csv("data/processed_data/great_meadow_well_data_2024_20250715.csv") %>%
  rename(Date = date, Year = year, precip_cm = precip.cm) %>%
  mutate(timestamp = as_datetime(timestamp),
         site = paste("Great Meadow", plot.num),
         water.depth = case_when(
           Year == 2016 & doy_h == 159.12 & plot.num == 3 & water.depth < -120 ~ NA_real_,
           Year == 2017 & doy_h == 215.02 & plot.num == 6 & water.depth < -115 ~ NA_real_,
           Year == 2021 & plot.num == 3 & doy == 224 & water.depth > 400 ~ NA_real_,
           Year == 2021 & plot.num == 3 & doy == 225 & water.depth > 400 ~ NA_real_,
           TRUE ~ water.depth
         )) %>%
  mutate(siteyear = paste(site, Year, sep = "_"))

gl <- read_csv("data/raw_data/hydrology_data/gilmore_well_prec_data_2013-2024.csv") %>%
  rename(water.depth = GILM_WL) %>%
  select(-timestamp) %>%  # drop the all-NA column
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"),
         hr = as.character(hr),
         site = "Gilmore Meadow",
         siteyear = paste(site, Year, sep = "_"))

all_data <- bind_rows(gm, gl)
  

# UI
ui <- fluidPage(
  titlePanel("Wetland Hydrograph Viewer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("year", "Select Year:", choices = sort(unique(all_data$Year))),
      selectInput("gm_site", "Select Great Meadow Plot:", 
                  choices = sort(unique(all_data$site[grepl("Great Meadow", all_data$site)])),
                  selected = "Great Meadow 1")
    ),
    mainPanel(
      plotOutput("hydrograph", height = "600px")
    )
  )
)

# Server
server <- function(input, output, session) {
  output$hydrograph <- renderPlot({
    req(input$year, input$gm_site)
    
    # Filter data for selected year and growing season
    plot_data_year <- all_data %>%
      filter(Year == input$year, doy > 134, doy < 275)
    
    # Calculate min water level for selected Great Meadow site
    min_gm <- plot_data_year %>%
      filter(site == input$gm_site, !is.na(water.depth)) %>%
      pull(water.depth) %>% 
      min(na.rm = TRUE)
    
    # Calculate min water level for Gilmore Meadow
    min_gil <- plot_data_year %>%
      filter(site == "Gilmore Meadow", !is.na(water.depth)) %>%
      pull(water.depth) %>% 
      min(na.rm = TRUE)
    
    # Overall min water level for y-axis alignment
    minWL <- min(min_gm, min_gil)
    
    # Plot hydrograph
    p <- ggplot() +
      # Great Meadow selected site
      geom_line(data = plot_data_year %>% filter(site == input$gm_site),
                aes(x = doy_h, y = water.depth, color = input$gm_site), size = 1) +
      # Gilmore Meadow always included
      geom_line(data = plot_data_year %>% filter(site == "Gilmore Meadow"),
                aes(x = doy_h, y = water.depth, color = "Gilmore Meadow"), size = 1) +
      # Precipitation scaled and shifted relative to minWL
      geom_line(data = plot_data_year,
                aes(x = doy_h, y = lag.precip * 5 + minWL),
                color = "blue", size = 0.7) +
      scale_color_manual(values = setNames(
        c("black", "darkgray"),
        c(input$gm_site, "Gilmore Meadow")
      )) +
      geom_hline(yintercept = 0, color = "brown") +
      theme_bw() +
      theme(legend.title = element_blank(),
            legend.position = "bottom",
            strip.text = element_text(size = 11)) +
      labs(title = paste("Hydrograph for", input$year),
           x = "Date", y = "Water Level (cm)") +
      scale_x_continuous(
        breaks = c(121, 152, 182, 213, 244, 274),
        labels = c('May-01', 'Jun-01', 'Jul-01', 'Aug-01', 'Sep-01', 'Oct-01')) +
      scale_y_continuous(
        sec.axis = sec_axis(~., name = "Hourly Precip. (cm)", breaks = NULL)
      )
    
    p
  })
}



# Run app
shinyApp(ui, server)
