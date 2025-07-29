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

all_data <- bind_rows(gm, gl) %>% 
  filter(Year >= 2016 & Year <= 2023) 

wl_stats <- read.csv("data/processed_data/gm_gl_wl_stats.csv") %>% 
  select(year,
         stat, 
         `Gilmore Meadow` = gilmore.meadow, 
         `Great Meadow 1` = great.meadow.1, 
         `Great Meadow 2` = great.meadow.2, 
         `Great Meadow 3` = great.meadow.3, 
         `Great Meadow 4` = great.meadow.4, 
         `Great Meadow 5` = great.meadow.5, 
         `Great Meadow 6` = great.meadow.6) %>% 
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
      
      pickerInput("site", "Select Site(s) for Water Level Stats:",
                  choices = sort(unique(wl_stats$site)),
                  selected = "Great Meadow 1",
                  multiple = TRUE,
                  options = list(
                    `actions-box` = TRUE,
                    `deselect-all-text` = "Clear all",
                    `select-all-text` = "Select all",
                    `none-selected-text` = "Choose site(s)"
                  ))
    ),
    mainPanel(
      plotOutput("hydrograph", height = "600px", brush = brushOpts(id = "hydro_brush"))
    )
  ),
  
  fluidRow(
    column(width = 10, offset = 1, align = "center",
           h4("Selected Data Points"),
           tableOutput("brush_info"))
  ),
  
  fluidRow(
    column(width = 10, offset = 1, align = "center",
           h4("Growing Season Water Level Statistics"),
           dataTableOutput("wl_stats")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive data for plotting
  plot_data <- reactive({
    req(input$year, input$gm_site)
    
    all_data %>%
      filter(Year %in% input$year, doy > 134, doy < 275) %>%
      filter(site %in% c(input$gm_site, "Gilmore Meadow"))
  })
  
  output$hydrograph <- renderPlot({
    plot_data_filtered <- plot_data()
    req(nrow(plot_data_filtered) > 0)
    
    # Calculate precipitation offset for each year
    precip_data <- plot_data_filtered %>%
      group_by(Year) %>%
      summarise(
        min_water = min(c(
          water.depth[site == input$gm_site & !is.na(water.depth)],
          water.depth[site == "Gilmore Meadow" & !is.na(water.depth)]
        ), na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      left_join(plot_data_filtered, by = "Year") %>%
      mutate(precip_y = lag.precip * 5 + min_water)
    
    # Create the plot using facet_wrap instead of patchwork
    ggplot() +
      # Water level lines
      geom_line(data = plot_data_filtered %>% filter(site == input$gm_site),
                aes(x = doy_h, y = water.depth, color = input$gm_site), 
                size = 0.7) +
      geom_line(data = plot_data_filtered %>% filter(site == "Gilmore Meadow"),
                aes(x = doy_h, y = water.depth, color = "Gilmore Meadow"), 
                size = 0.7) +
      # Precipitation line
      geom_line(data = precip_data,
                aes(x = doy_h, y = precip_y),
                color = "blue", size = 0.7) +
      # Ground level reference
      geom_hline(yintercept = 0, color = "brown") +
      
      # Facet by year
      facet_wrap(~ Year, ncol = 1, scales = "free_y") +
      
      # Styling
      scale_color_manual(values = setNames(c("black", "darkgray"), 
                                           c(input$gm_site, "Gilmore Meadow"))) +
      labs(title = "Hydrographs by Year",
           x = "Day of Year", y = "Water Level (cm)") +
      scale_x_continuous(
        breaks = c(121, 152, 182, 213, 244, 274),
        labels = c('May-01', 'Jun-01', 'Jul-01', 'Aug-01', 'Sep-01', 'Oct-01')) +
      theme_bw() +
      theme(legend.position = "bottom",
            legend.title = element_blank(),
            strip.text = element_text(size = 11),
            plot.title = element_text(hjust = 0.5))
  })
  
  # Selected data output table - Now with separate columns for each site
  output$brush_info <- renderTable({
    req(input$hydro_brush)
    
    plot_data_filtered <- plot_data()
    req(nrow(plot_data_filtered) > 0)
    
    # Use brushedPoints function which works properly with faceted plots
    selected_data <- brushedPoints(
      plot_data_filtered,
      brush = input$hydro_brush,
      xvar = "doy_h",
      yvar = "water.depth"
    ) %>%
      select(site, Year, doy_h, timestamp, water.depth, precip_cm) %>%
      arrange(Year, doy_h) %>%
      # Round for better display
      mutate(
        doy_h = round(doy_h, 2),
        water.depth = round(water.depth, 2),
        precip_cm = round(precip_cm, 3),
        timestamp = format(timestamp, "%Y-%m-%d %H:%M:%S")
      )
    
    if(nrow(selected_data) == 0) {
      data.frame(Message = "No points selected - try brushing over the water level lines")
    } else {
      # Create a proper time grouping to match data points from both sites
      # Group by rounded doy_h to combine data from both sites at similar times
      pivoted_data <- selected_data %>%
        # Round doy_h slightly to group nearby times together
        mutate(doy_h_rounded = round(doy_h, 1)) %>%
        # Group by the rounded time
        group_by(Year, doy_h_rounded) %>%
        summarise(
          # Use the first timestamp and precipitation value in each group
          timestamp = first(timestamp),
          doy_h = first(doy_h),
          precip_cm = first(precip_cm),
          # Create separate columns for each site's water depth
          great_meadow_depth = ifelse(any(site == input$gm_site), 
                                      water.depth[site == input$gm_site][1], NA),
          gilmore_depth = ifelse(any(site == "Gilmore Meadow"), 
                                 water.depth[site == "Gilmore Meadow"][1], NA),
          .groups = 'drop'
        ) %>%
        # Remove the grouping column and rename for display
        select(-doy_h_rounded) %>%
        rename(
          `Timestamp` = timestamp,
          `Day of Year` = doy_h,
          `Precipitation (cm)` = precip_cm
        ) %>%
        # Create dynamic column names based on selected site
        rename_with(~ paste(input$gm_site, "Water Depth (cm)"), .cols = great_meadow_depth) %>%
        rename_with(~ "Gilmore Meadow Water Depth (cm)", .cols = gilmore_depth) %>%
        # Arrange by year and day
        arrange(Year, `Day of Year`)
      
      pivoted_data
    }
  })
  
  # WL stats output table
  output$wl_stats <- DT::renderDataTable({
    req(input$site, input$year)
    
    wl_stats %>%
      filter(site %in% input$site, year %in% input$year) %>% 
      mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
      select(
        Year = year,
        Site = site,
        `Mean Water Level` = WL_mean,
        `SD Water Level` = WL_sd,
        `Minimum Water Level` = WL_min,
        `Maximum Water Level` = WL_max,
        `Maximum Hourly Increase` = max_inc,
        `Maximum Hourly Decrease` = max_dec,
        `Growing Season Change` = GS_change,
        `GS % Complete Data` = prop_GS_comp,
        `GS % Surface Water` = prop_over_0cm,
        `GS % Within 30cm` = prop_bet_0_neg30cm,
        `GS % Over 30cm Deep` = prop_under_neg30cm
      )
  }, options = list(pageLength = 10, scrollX = TRUE))
}

# Run app
shinyApp(ui, server)

