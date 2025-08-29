#### Biodiversity Data ####

#### iNat and ebird data -------------------------------------------------------

library(shiny)
library(bslib)
library(dplyr)
library(plotly)
library(purrr)
library(forcats)
library(RColorBrewer)
library(viridisLite)

# Read & prepare processed data

# iNat data
inat_data <- read.csv("data/raw_data/biodiversity_data/inat_greatmeadow_20250825.csv")

# eBird data
eBird_data <- read.csv("data/raw_data/biodiversity_data/ebird_greatmeadow_20250825.csv")

eBird_tax <- read.csv("data/raw_data/biodiversity_data/eBird_taxonomy_v2024.csv")

# merge eBird data
merged_data <- full_join(
  eBird_data, 
  eBird_tax, 
  by = c("TAXONOMIC.ORDER" = "TAXON_ORDER")
) %>% 
  filter(!is.na(SCIENTIFIC.NAME))


#UI
ui <- page_fillable(
  theme = bs_theme(
    bootswatch = "flatly",
    primary = "#2E8B57", 
    base_font = font_google("Open Sans")
  ),
  
  # Page Header
  div(
    class = "text-center mb-2",
    h2("Biodiversity of Great Meadow", class = "display-4 text-primary"),
    p("Summary of participatory science observations", class = "lead text-muted")
  ),
  
  #### iNaturalist Section ####
  div(class = "my-3",
      h3("iNaturalist", class = "text-center text-success mb-3"),
      
      # Single pie chart for species by taxa
      div(class = "text-center mb-3",
          plotlyOutput("spp_plot", height = "400px"),
          div(id = "spp_legend", class = "mt-2")
      ),
      
      # Stats with icons underneath
      layout_columns(
        col_widths = c(6, 6),
        
        card(
          class = "text-center",
          card_body(
            div(
              icon("binoculars", style = "font-size: 2rem; color: #28a745; margin-bottom: 10px;"),
              h3(textOutput("inat_observations"), class = "mb-1 text-primary"),
              div("Observations", class = "text-muted")
            )
          )
        ),
        
        card(
          class = "text-center", 
          card_body(
            div(
              icon("users", style = "font-size: 2rem; color: #17a2b8; margin-bottom: 10px;"),
              h3(textOutput("inat_people"), class = "mb-1 text-info"),
              div("Observers", class = "text-muted")
            )
          )
        )
      )
  ),
  
  #### eBird Section ####
  div(class = "my-3",
      h2("eBird", class = "text-center text-primary mb-3"),
      
      # Single pie chart for species by taxonomic group
      div(class = "text-center mb-3",
          plotlyOutput("ebird_spp_plot", height = "400px"),
          div(id = "ebird_spp_legend", class = "mt-2")
      ),
      
      # Stats with icons underneath
      layout_columns(
        col_widths = c(6, 6),
        
        card(
          class = "text-center",
          card_body(
            div(
              icon("clipboard-list", style = "font-size: 2rem; color: #28a745; margin-bottom: 10px;"),
              h3(textOutput("ebird_checklists"), class = "mb-1 text-primary"),
              div("Checklists", class = "text-muted")
            )
          )
        ),
        
        card(
          class = "text-center", 
          card_body(
            div(
              icon("users", style = "font-size: 2rem; color: #17a2b8; margin-bottom: 10px;"),
              h3(textOutput("ebird_observers"), class = "mb-1 text-info"),
              div("Observers", class = "text-muted")
            )
          )
        )
      )
  ),
  
  # Footer
  hr(),
  div(
    class = "text-center text-muted small mt-2",
    p("Data sourced from iNaturalist and eBird • Last updated: ", Sys.Date())
  )
)

server <- function(input, output, session) {
  
  # Function to generate a palette for any number of categories
  generate_palette <- function(n_colors, palette_type = "qualitative") {
    if(n_colors <= 8) {
      rev(viridis(n_colors, option = "D", end = 0.85))
    } else {
      # Use multiple viridis family palettes - all colorblind friendly
      base_colors <- c(
        rev(viridis(8, option = "D", end = 0.85)),  # Classic viridis (purple-teal)
        rev(viridis(6, option = "C", end = 0.9)),   # Plasma (purple-pink-orange)
        rev(rocket(6))                             
      )
      base_colors[1:min(n_colors, length(base_colors))]
    }
  }
  
  # Function to assign colors ensuring "Other" gets a neutral color
  get_chart_colors <- function(categories, palette_type = "qualitative") {
    n_cats <- length(categories)
    colors <- generate_palette(n_cats, palette_type)
    
    # If there's an "Other" category, assign it a neutral gray
    other_idx <- grep("Other", categories, ignore.case = TRUE)
    if(length(other_idx) > 0) {
      colors[other_idx] <- "#CCCCCC"  # Light gray for "Other"
    }
    
    return(colors)
  }
  
  # Rest of your existing helper functions...
  create_legend <- function(categories, colors) {
    legend_items <- map2_chr(categories, colors, function(cat, col) {
      paste0('<span style="display: inline-block; margin-right: 15px; margin-bottom: 5px;">',
             '<span style="display: inline-block; width: 12px; height: 12px; ',
             'background-color: ', col, '; margin-right: 5px; border-radius: 2px;"></span>',
             '<span style="font-size: 12px;">', cat, '</span></span>')
    })
    paste(legend_items, collapse = "")
  }
  
  # ---- Species pie chart for iNaturalist ----
  spp_summary <- reactive({
    inat_data %>%
      distinct(taxon_species_name, iconic_taxon_name) %>%
      filter(!is.na(iconic_taxon_name), iconic_taxon_name != "") %>%
      count(iconic_taxon_name) %>%
      arrange(desc(n)) %>%
      mutate(percentage = round(n/sum(n) * 100, 1))
  })
  
  output$spp_plot <- renderPlotly({
    data <- spp_summary()
    total_spp <- sum(data$n)
    chart_colors <- get_chart_colors(data$iconic_taxon_name)
    
    plot_ly(data, 
            labels = ~iconic_taxon_name, 
            values = ~n,
            type = "pie", 
            hole = 0.4,
            marker = list(colors = chart_colors,
                          line = list(color = "white", width = 2)),
            textinfo = "none",
            hovertemplate = "<b>%{label}</b><br>Species: %{value} (%{percent})<extra></extra>") %>%
      layout(
        showlegend = FALSE,
        margin = list(t = 40, b = 40, l = 40, r = 40),
        annotations = list(
          list(x = 0.5, y = 0.5, 
               text = paste0("<b>", formatC(total_spp, big.mark = ","), "</b><br>",
                             "<span style='font-size:14px'>Species</span>"),
               showarrow = FALSE,
               font = list(size = 18))
        )
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Create legend for iNat species
  observe({
    data <- spp_summary()
    chart_colors <- get_chart_colors(data$iconic_taxon_name)
    legend_html <- create_legend(data$iconic_taxon_name, chart_colors)
    insertUI(selector = "#spp_legend", 
             ui = div(HTML(legend_html), class = "text-center"),
             immediate = TRUE)
  })
  
  # ---- eBird species pie chart ----
  ebird_spp_summary <- reactive({
    df <- merged_data %>%
      distinct(SCIENTIFIC.NAME, SPECIES_GROUP) %>%
      count(SPECIES_GROUP) %>%
      mutate(SPECIES_GROUP = ifelse(n <= 2, "Other (1–2 species groups)", SPECIES_GROUP)) %>%
      group_by(SPECIES_GROUP) %>%
      summarise(n = sum(n), .groups = "drop") %>%
      mutate(percentage = round(n / sum(n) * 100, 1)) %>%
      arrange(desc(n))
    
    # Separate "Other" from the rest and reorder
    other_row <- df[df$SPECIES_GROUP == "Other (1–2 species groups)", ]
    main_rows <- df[df$SPECIES_GROUP != "Other (1–2 species groups)", ]
    
    # Combine with "Other" at the end
    final_df <- bind_rows(main_rows, other_row) %>%
      mutate(SPECIES_GROUP = factor(SPECIES_GROUP, levels = SPECIES_GROUP))
    
    return(final_df)
  })
  
  output$ebird_spp_plot <- renderPlotly({
    data <- ebird_spp_summary()
    total_spp <- sum(data$n)
    chart_colors <- get_chart_colors(data$SPECIES_GROUP)
    
    plot_ly(data, 
            labels = ~SPECIES_GROUP, 
            values = ~n,
            type = "pie", 
            hole = 0.4,
            marker = list(colors = chart_colors,
                          line = list(color = "white", width = 2)),
            textinfo = "none",
            hovertemplate = "<b>%{label}</b><br>Species: %{value} (%{percent})<extra></extra>") %>%
      layout(
        showlegend = FALSE,
        margin = list(t = 40, b = 40, l = 40, r = 40),
        annotations = list(
          list(x = 0.5, y = 0.5, 
               text = paste0("<b>", formatC(total_spp, big.mark = ","), "</b><br>",
                             "<span style='font-size:14px'>Species</span>"),
               showarrow = FALSE,
               font = list(size = 18))
        )
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Create legend for eBird species
  observe({
    data <- ebird_spp_summary()
    chart_colors <- get_chart_colors(data$SPECIES_GROUP)
    legend_html <- create_legend(data$SPECIES_GROUP, chart_colors)
    insertUI(selector = "#ebird_spp_legend", 
             ui = div(HTML(legend_html), class = "text-center"),
             immediate = TRUE)
  })
  
  # ---- iNaturalist stats ----
  inat_summary <- reactive({
    list(
      n_observations = n_distinct(inat_data$uuid),
      n_people = n_distinct(inat_data$user_id)
    )
  })
  
  output$inat_observations <- renderText({
    formatC(inat_summary()$n_observations, big.mark = ",")
  })
  
  output$inat_people <- renderText({
    formatC(inat_summary()$n_people, big.mark = ",")
  })
  
  ## ---- eBird summaries ----
  ebird_summary <- reactive({
    list(
      n_checklists = n_distinct(eBird_data$SAMPLING.EVENT.IDENTIFIER),
      n_observers = n_distinct(eBird_data$OBSERVER.ID)
    )
  })
  
  output$ebird_checklists <- renderText({
    formatC(ebird_summary()$n_checklists, big.mark = ",")
  })
  
  output$ebird_observers <- renderText({
    formatC(ebird_summary()$n_observers, big.mark = ",")
  })
}

shinyApp(ui, server)












#### Biodiversity Data ####

#### iNat and ebird data -------------------------------------------------------

library(shiny)
library(bslib)
library(dplyr)
library(plotly)
library(purrr)
library(forcats)
library(RColorBrewer)
library(viridisLite)

# Read & prepare processed data

# iNat data
inat_data <- read.csv("data/raw_data/biodiversity_data/inat_greatmeadow_20250825.csv")

# eBird data
eBird_data <- read.csv("data/raw_data/biodiversity_data/ebird_greatmeadow_20250825.csv")

eBird_tax <- read.csv("data/raw_data/biodiversity_data/eBird_taxonomy_v2024.csv")

# merge eBird data
merged_data <- full_join(
  eBird_data, 
  eBird_tax, 
  by = c("TAXONOMIC.ORDER" = "TAXON_ORDER")
) %>% 
  filter(!is.na(SCIENTIFIC.NAME))


#UI
ui <- page_fluid(
  theme = bs_theme(
    bootswatch = "flatly",
    primary = "#2E8B57", 
    base_font = font_google("Open Sans")
  ),
  
  # Page Header
  div(
    class = "text-center mb-3",
    h2("Biodiversity of Great Meadow", class = "display-4 text-primary"),
    p("Summary of participatory science observations", class = "lead text-muted")
  ),
  
  #### Side-by-side sections ####
  layout_columns(
    col_widths = c(6, 6),  # Equal width columns
    
    #### iNaturalist Section ####
    div(class = "p-3 border border-light rounded",
        h3("iNaturalist", class = "text-center text-success mb-3"),
        
        # Stats with icons ABOVE the pie chart
        div(class = "row g-2 mb-3",  # Added mb-3 for spacing below stats
            div(class = "col-6",
                card(
                  class = "text-center",
                  card_body(
                    div(
                      icon("binoculars", style = "font-size: 1.5rem; color: #28a745; margin-bottom: 8px;"),
                      h4(textOutput("inat_observations"), class = "mb-1 text-primary"),
                      div("Observations", class = "text-muted small")
                    )
                  )
                )
            ),
            div(class = "col-6",
                card(
                  class = "text-center", 
                  card_body(
                    div(
                      icon("users", style = "font-size: 1.5rem; color: #17a2b8; margin-bottom: 8px;"),
                      h4(textOutput("inat_people"), class = "mb-1 text-info"),
                      div("Observers", class = "text-muted small")
                    )
                  )
                )
            )
        ),
        
        # Pie chart for species by taxa BELOW stats
        div(class = "text-center",
            plotlyOutput("spp_plot", height = "350px"),
            div(id = "spp_legend", class = "mt-2")
        )
    ),
    
    #### eBird Section ####
    div(class = "p-3 border border-light rounded",
        h3("eBird", class = "text-center text-primary mb-3"),
        
        # Stats with icons ABOVE the pie chart
        div(class = "row g-2 mb-3",  # Added mb-3 for spacing below stats
            div(class = "col-6",
                card(
                  class = "text-center",
                  card_body(
                    div(
                      icon("clipboard-list", style = "font-size: 1.5rem; color: #28a745; margin-bottom: 8px;"),
                      h4(textOutput("ebird_checklists"), class = "mb-1 text-primary"),
                      div("Checklists", class = "text-muted small")
                    )
                  )
                )
            ),
            div(class = "col-6",
                card(
                  class = "text-center", 
                  card_body(
                    div(
                      icon("users", style = "font-size: 1.5rem; color: #17a2b8; margin-bottom: 8px;"),
                      h4(textOutput("ebird_observers"), class = "mb-1 text-info"),
                      div("Observers", class = "text-muted small")
                    )
                  )
                )
            )
        ),
        
        # Pie chart for species by taxonomic group BELOW stats
        div(class = "text-center",
            plotlyOutput("ebird_spp_plot", height = "350px"),
            div(id = "ebird_spp_legend", class = "mt-2")
        )
    )
  ),
  
  # Footer
  hr(),
  div(
    class = "text-center text-muted small mt-4 mb-3",  
    p("Data sourced from iNaturalist and eBird • Last updated: ", Sys.Date())
  )
)

#SERVER
server <- function(input, output, session) {
  
  # Function to generate a palette for any number of categories
  generate_palette <- function(n_colors, palette_type = "qualitative") {
    if(n_colors <= 8) {
      rev(viridis(n_colors, option = "D", end = 0.85))
    } else {
      # Use multiple viridis family palettes - all colorblind friendly
      base_colors <- c(
        rev(viridis(8, option = "D", end = 0.85)),  # Classic viridis (purple-teal)
        rev(viridis(6, option = "C", end = 0.9)),   # Plasma (purple-pink-orange)
        rev(rocket(6))                             
      )
      base_colors[1:min(n_colors, length(base_colors))]
    }
  }
  
  # Function to assign colors ensuring "Other" gets a neutral color
  get_chart_colors <- function(categories, palette_type = "qualitative") {
    n_cats <- length(categories)
    colors <- generate_palette(n_cats, palette_type)
    
    # If there's an "Other" category, assign it a neutral gray
    other_idx <- grep("Other", categories, ignore.case = TRUE)
    if(length(other_idx) > 0) {
      colors[other_idx] <- "#CCCCCC"  # Light gray for "Other"
    }
    
    return(colors)
  }
  
  # Helper function to create legend
  create_legend <- function(categories, colors) {
    legend_items <- map2_chr(categories, colors, function(cat, col) {
      paste0('<span style="display: inline-block; margin-right: 10px; margin-bottom: 3px;">',
             '<span style="display: inline-block; width: 10px; height: 10px; ',
             'background-color: ', col, '; margin-right: 4px; border-radius: 2px;"></span>',
             '<span style="font-size: 11px;">', cat, '</span></span>')
    })
    paste(legend_items, collapse = "")
  }
  
  # ---- Species pie chart for iNaturalist ----
  spp_summary <- reactive({
    inat_data %>%
      distinct(taxon_species_name, iconic_taxon_name) %>%
      filter(!is.na(iconic_taxon_name), iconic_taxon_name != "") %>%
      count(iconic_taxon_name) %>%
      arrange(desc(n)) %>%
      mutate(percentage = round(n/sum(n) * 100, 1))
  })
  
  output$spp_plot <- renderPlotly({
    data <- spp_summary()
    total_spp <- sum(data$n)
    chart_colors <- get_chart_colors(data$iconic_taxon_name)
    
    plot_ly(data, 
            labels = ~iconic_taxon_name, 
            values = ~n,
            type = "pie", 
            hole = 0.4,
            marker = list(colors = chart_colors,
                          line = list(color = "white", width = 2)),
            textinfo = "none",
            hovertemplate = "<b>%{label}</b><br>Species: %{value} (%{percent})<extra></extra>") %>%
      layout(
        showlegend = FALSE,
        margin = list(t = 10, b = 10, l = 10, r = 10),
        paper_bgcolor = "transparent",  # Remove white background
        plot_bgcolor = "transparent", 
        annotations = list(
          list(x = 0.5, y = 0.5, 
               text = paste0("<b>", formatC(total_spp, big.mark = ","), "</b><br>",
                             "<span style='font-size:12px'>Species</span>"),
               showarrow = FALSE,
               font = list(size = 16))
        )
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Create legend for iNat species
  observe({
    data <- spp_summary()
    chart_colors <- get_chart_colors(data$iconic_taxon_name)
    legend_html <- create_legend(data$iconic_taxon_name, chart_colors)
    insertUI(selector = "#spp_legend", 
             ui = div(HTML(legend_html), class = "text-center"),
             immediate = TRUE)
  })
  
  # ---- eBird species pie chart ----
  ebird_spp_summary <- reactive({
    df <- merged_data %>%
      distinct(SCIENTIFIC.NAME, SPECIES_GROUP) %>%
      count(SPECIES_GROUP) %>%
      mutate(SPECIES_GROUP = ifelse(n <= 2, "Other (1–2 species groups)", SPECIES_GROUP)) %>%
      group_by(SPECIES_GROUP) %>%
      summarise(n = sum(n), .groups = "drop") %>%
      mutate(percentage = round(n / sum(n) * 100, 1)) %>%
      arrange(desc(n))
    
    # Separate "Other" from the rest and reorder
    other_row <- df[df$SPECIES_GROUP == "Other (1–2 species groups)", ]
    main_rows <- df[df$SPECIES_GROUP != "Other (1–2 species groups)", ]
    
    # Combine with "Other" at the end
    final_df <- bind_rows(main_rows, other_row) %>%
      mutate(SPECIES_GROUP = factor(SPECIES_GROUP, levels = SPECIES_GROUP))
    
    return(final_df)
  })
  
  output$ebird_spp_plot <- renderPlotly({
    data <- ebird_spp_summary()
    total_spp <- sum(data$n)
    chart_colors <- get_chart_colors(data$SPECIES_GROUP)
    
    plot_ly(data, 
            labels = ~SPECIES_GROUP, 
            values = ~n,
            type = "pie", 
            hole = 0.4,
            marker = list(colors = chart_colors,
                          line = list(color = "white", width = 2)),
            textinfo = "none",
            hovertemplate = "<b>%{label}</b><br>Species: %{value} (%{percent})<extra></extra>") %>%
      layout(
        showlegend = FALSE,
        margin = list(t = 10, b = 10, l = 10, r = 10),
        paper_bgcolor = "transparent",  # Remove white background
        plot_bgcolor = "transparent", 
        annotations = list(
          list(x = 0.5, y = 0.5, 
               text = paste0("<b>", formatC(total_spp, big.mark = ","), "</b><br>",
                             "<span style='font-size:12px'>Species</span>"),
               showarrow = FALSE,
               font = list(size = 16))
        )
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Create legend for eBird species
  observe({
    data <- ebird_spp_summary()
    chart_colors <- get_chart_colors(data$SPECIES_GROUP)
    legend_html <- create_legend(data$SPECIES_GROUP, chart_colors)
    insertUI(selector = "#ebird_spp_legend", 
             ui = div(HTML(legend_html), class = "text-center"),
             immediate = TRUE)
  })
  
  # ---- iNaturalist stats ----
  inat_summary <- reactive({
    list(
      n_observations = n_distinct(inat_data$uuid),
      n_people = n_distinct(inat_data$user_id)
    )
  })
  
  output$inat_observations <- renderText({
    formatC(inat_summary()$n_observations, big.mark = ",")
  })
  
  output$inat_people <- renderText({
    formatC(inat_summary()$n_people, big.mark = ",")
  })
  
  ## ---- eBird summaries ----
  ebird_summary <- reactive({
    list(
      n_checklists = n_distinct(eBird_data$SAMPLING.EVENT.IDENTIFIER),
      n_observers = n_distinct(eBird_data$OBSERVER.ID)
    )
  })
  
  output$ebird_checklists <- renderText({
    formatC(ebird_summary()$n_checklists, big.mark = ",")
  })
  
  output$ebird_observers <- renderText({
    formatC(ebird_summary()$n_observers, big.mark = ",")
  })
}

shinyApp(ui, server)

