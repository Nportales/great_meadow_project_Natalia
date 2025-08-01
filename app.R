library(shiny)
library(tidyverse)
library(lubridate)
library(patchwork)
library(shinyWidgets)
library(DT)
library(bslib)

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
  filter(Year >= 2016 & Year <= 2023) %>% 
  select(timestamp, 
         date = Date, 
         year = Year, 
         doy, 
         hr, 
         doy_h, 
         lag_precip = lag.precip,
         water_depth = water.depth,
         site)

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
ui <- page_fluid(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#1B365D",
    secondary = "#4C6D9A",    
    success = "#2E86C1",
    info = "#3498db",
    warning = "#f39c12",
    danger = "#e74c3c",
    base_font = font_google("Open Sans"),
    heading_font = font_google("Roboto", wght = c(400, 700))
  ),
  
  # Custom CSS for additional styling
  tags$head(
    tags$style(HTML("
      .content-section {
        margin: 30px 0;
        padding: 25px;
        border-radius: 15px;
        box-shadow: 0 4px 12px rgba(0,0,0,0.1);
        background: linear-gradient(135deg, #ffffff 0%, #f8f9fa 100%);
        border: 2px solid #1B365D;
      }
      
      .section-title {
        color: #1B365D;
        font-weight: 600;
        margin-bottom: 20px;
        padding-bottom: 10px;
        border-bottom: 2px solid #4C6D9A;
      }
      
      .sidebar-custom {
        background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);
        border-radius: 10px;
        padding: 20px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.08);
        border: 1px solid #dee2e6;
      }
      
      .main-title {
        background: linear-gradient(135deg, #1B365D 0%, #4C6D9A 100%);
        color: white;
        padding: 30px;
        margin: -15px -15px 30px -15px;
        text-align: center;
        border-radius: 0 0 20px 20px;
        box-shadow: 0 4px 12px rgba(0,0,0,0.15);
      }
      
      .brush-info-section {
        background: linear-gradient(135deg, #e8f4f8 0%, #f0f8ff 100%);
        border-radius: 12px;
        padding: 20px;
        margin: 20px 0;
        border: 1px solid #4C6D9A;
      }
      
      .stats-main {
        background: linear-gradient(135deg, #f8f9fa 0%, #ffffff 100%);
      }
      
      .dataTables_wrapper {
        font-size: 0.85rem !important;
      }

      .dataTables_wrapper table {
        font-size: 0.8rem !important;
      }

      .dataTables_wrapper .dataTables_info,
      .dataTables_wrapper .dataTables_paginate {
        font-size: 0.75rem !important;
      }
      
    "))
  ),
  
  # Main title with gradient background
  div(class = "main-title",
      h1("Wetland Hydrograph Visualizer", 
         style = "margin: 0; font-size: 2rem; text-shadow: 2px 2px 4px rgba(0,0,0,0.3)")
  ),
  
  # First section: Hydrographs 
  div(class = "content-section",
      layout_sidebar(
        sidebar = sidebar(
          class = "sidebar-custom",
          width = 300,
          h4("Hydrograph Controls", style = "color: #1B365D; margin-bottom: 20px;"),
          
          selectInput("gm_site", 
                      label = div(icon("map-marker"), "Select a Great Meadow Site:"),
                      choices = sort(unique(all_data$site[grepl("Great Meadow", all_data$site)])),
                      selected = "Great Meadow 1"),
          
          div(style = "margin-bottom: 15px;",
          pickerInput("year", 
                      label = div(icon("calendar"), "Select Year(s):"),
                      choices = sort(unique(all_data$year)),
                      selected = 2023,
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE,
                        `deselect-all-text` = "Clear all",
                        `select-all-text` = "Select all",
                        `none-selected-text` = "Choose year(s)",
                        `live-search` = TRUE,
                        style = "btn-outline-primary"
                      ))),
          
          br(),
          div(style = "padding: 15px; background-color: #e8f4f8; border-radius: 8px; border-left: 4px solid #3498db;",
              p(icon("info-circle"), " Use the brush tool to select data points on the hydrograph for a detailed view of the data.", 
                style = "margin: 0; font-size: 0.9rem; color: #2c3e50;")),
          
          div(style = "margin-top: 15px; text-align: center;",
              downloadButton("download_brush", "Download Selected Data", 
                             class = "btn-primary btn-sm", 
                             icon = icon("download")))
        ),
        
        card(
          full_screen = TRUE,
          card_header(
            class = "bg-primary text-white",
            "Hydrographs by Year"
          ),
          plotOutput("hydrograph", height = "600px", 
                     brush = brushOpts(id = "hydro_brush", fill = "#4C6D9A", opacity = 0.3))
        )
      )
  ),
  
  # Selected data points section with improved styling
  div(class = "brush-info-section",
      div(class = "row",
          div(class = "col-12",
              card(
                card_header(
                  class = "bg-success text-white",
                  "Selected Data from Hydrograph:"
                ),
                tableOutput("brush_info")
              )
          )
      )
  ),
  
  # Second section: Water Level Stats with improved styling
  div(class = "content-section stats-main",
      layout_sidebar(
        sidebar = sidebar(
          class = "sidebar-custom",
          width = 300,
          h4("Statistics Controls", style = "color: #1B365D; margin-bottom: 20px;"),
          
          pickerInput("stats_site", 
                      label = div(icon("map-marker"), "Select Site(s):"),
                      choices = sort(unique(wl_stats$site)),
                      selected = "Great Meadow 1",
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE,
                        `deselect-all-text` = "Clear all",
                        `select-all-text` = "Select all",
                        `none-selected-text` = "Choose site(s)",
                        `live-search` = TRUE,
                        style = "btn-outline-primary"
                      )),
          
          div(style = "margin-bottom: 15px;",
          pickerInput("stats_year", 
                      label = div(icon("calendar"), "Select Years:"),
                      choices = sort(unique(wl_stats$year)),
                      selected = c(2023, 2022),
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE,
                        `deselect-all-text` = "Clear all",
                        `select-all-text` = "Select all",
                        `none-selected-text` = "Choose year(s)",
                        `live-search` = TRUE,
                        style = "btn-outline-primary"
                      ))),
          
          br(),
          div(style = "padding: 15px; background-color: #e8f4f8; border-radius: 8px; border-left: 4px solid #3498db;",
              p(icon("info-circle"), " Growing Season statistics calculated from May through October.", 
                style = "margin: 0; font-size: 0.9rem; color: #2c3e50;")),
          
          div(style = "margin-top: 15px; text-align: center;",
              downloadButton("download_stats", "Download Table", 
                             class = "btn-primary btn-sm", 
                             icon = icon("download")))
        ),
        
        card(
          full_screen = TRUE,
          card_header(
            class = "bg-primary text-white",
            "Growing Season Water Level Statistics"
          ),
          div(style = "padding: 10px;",
              dataTableOutput("wl_stats"))
        )
      )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive data for plotting
  plot_data <- reactive({
    req(input$year, input$gm_site)
    
    all_data %>%
      filter(year %in% input$year, doy > 134, doy < 275) %>%
      filter(site %in% c(input$gm_site, "Gilmore Meadow"))
  })
  
  output$hydrograph <- renderPlot({
    plot_data_filtered <- plot_data()
    req(nrow(plot_data_filtered) > 0)
    
    # Calculate precipitation offset for each year
    precip_data <- plot_data_filtered %>%
      group_by(year) %>%
      summarise(
        min_water = min(c(
          water_depth[site == input$gm_site & !is.na(water_depth)],
          water_depth[site == "Gilmore Meadow" & !is.na(water_depth)]
        ), na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      left_join(plot_data_filtered, by = "year") %>%
      mutate(precip_y = lag_precip * 5 + min_water)
    
    global_min_water <- min(plot_data_filtered$water_depth, na.rm = TRUE)
    
    # Create the plot using facet_wrap instead of patchwork
    ggplot() +
      # Water level lines
      geom_line(data = plot_data_filtered %>% filter(site == input$gm_site),
                aes(x = doy_h, y = water_depth, color = input$gm_site), 
                size = 0.7) +
      geom_line(data = plot_data_filtered %>% filter(site == "Gilmore Meadow"),
                aes(x = doy_h, y = water_depth, color = "Gilmore Meadow"), 
                size = 0.7) +
      # Precipitation line
      geom_line(data = precip_data,
                aes(x = doy_h, y = precip_y, color = "Precipitation"), size = 0.7) +
      # Ground level reference
      geom_hline(yintercept = 0, color = "brown") +
      
      # Facet by year
      facet_wrap(~ year, ncol = 1, scales = "free_y") +
      
      # Styling
      scale_color_manual(values = c(
        setNames("black", input$gm_site),
        "Gilmore Meadow" = "darkgray",
        "Precipitation" = "blue"
      )) +
      labs(title = NULL,
           x = "Date", y = "Water Level (cm)") +
      scale_x_continuous(
        breaks = c(121, 152, 182, 213, 244, 274),
        labels = c('May-01', 'Jun-01', 'Jul-01', 'Aug-01', 'Sep-01', 'Oct-01')) +
      scale_y_continuous(
        name = "Water Level (cm)",
        sec.axis = sec_axis(~ (. - global_min_water) / 5,
                            name = "Hourly Precip. (cm)",
                            breaks = seq(0, 8, by = 2))) +
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
      yvar = "water_depth"
    ) %>%
      select(site, year, doy_h, timestamp, water_depth, lag_precip) %>%
      arrange(year, doy_h) %>%
      # Round for better display
      mutate(
        doy_h = round(doy_h, 2),
        water_depth = round(water_depth, 2),
        lag_precip = round(lag_precip, 3),
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
        group_by(year, doy_h_rounded) %>%
        summarise(
          # Use the first timestamp and precipitation value in each group
          timestamp = first(timestamp),
          doy_h = first(doy_h),
          lag_precip = first(lag_precip),
          # Create separate columns for each site's water depth
          great_meadow_depth = ifelse(any(site == input$gm_site), 
                                      water_depth[site == input$gm_site][1], NA),
          gilmore_depth = ifelse(any(site == "Gilmore Meadow"), 
                                 water_depth[site == "Gilmore Meadow"][1], NA),
          .groups = 'drop'
        ) %>%
        # Remove the grouping column and rename for display
        select(-doy_h_rounded) %>%
        rename(
          `Year` = year, 
          `Timestamp` = timestamp,
          `Day of Year` = doy_h,
          `Precipitation (cm)` = lag_precip
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
    req(input$stats_site, input$stats_year)
    
    wl_stats %>%
      filter(site %in% input$stats_site, year %in% input$stats_year) %>% 
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
        `GS % Surface Water` = prop_over_0cm,
        `GS % Within 30cm` = prop_bet_0_neg30cm,
        `GS % Over 30cm Deep` = prop_under_neg30cm,
        `GS % Complete Data` = prop_GS_comp
      )
  }, options = list(pageLength = 10, scrollX = TRUE))
  
  # Download handler for brushed data
  output$download_brush <- downloadHandler(
    filename = function() {
      paste("hydrograph_selected_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Get the brushed data (you'll need this reactive from your existing brush logic)
      brushed_data <- brushedPoints(plot_data(), input$hydro_brush,
                                    xvar = "doy_h", yvar = "water_depth")
      write.csv(brushed_data, file, row.names = FALSE)
    }
  )
  
  # Download handler for water level stats
  output$download_stats <- downloadHandler(
    filename = function() {
      paste("water_level_stats_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Use your filtered stats data
      filtered_stats <- wl_stats %>%
        filter(site %in% input$stats_site,
               year %in% input$stats_year)
      write.csv(filtered_stats, file, row.names = FALSE)
    }
  )
  
}

# Run app
shinyApp(ui, server)


