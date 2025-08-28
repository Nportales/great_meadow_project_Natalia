#8/28/25------------------------------------------------------------------------

#### Biodiversity Data ####

#### iNat and ebird data -------------------------------------------------------

library(shiny)
library(bslib)
library(dplyr)
library(plotly)
library(purrr)

# Read & prepare processed data

# iNat data
inat_data <- read.csv("data/raw_data/biodiversity_data/inat_greatmeadow_20250825.csv")

# eBird data
eBird_data <- read.csv("data/raw_data/biodiversity_data/ebird_greatmeadow_20250825.csv")

# UI
ui <- page_fillable(
  theme = bs_theme(
    bootswatch = "flatly",
    primary = "#2E8B57", 
    base_font = font_google("Open Sans")
  ),
  
  # Header
  div(
    class = "text-center mb-4",
    h1("iNaturalist Biodiversity Dashboard", 
       class = "display-4 text-primary"),
    p("Summary of observations from Great Meadow", 
      class = "lead text-muted")
  ),
  
  # Main content
  layout_columns(
    col_widths = c(4, 4, 4),
    
    # Observations
    div(
      class = "text-center",
      plotlyOutput("obs_plot", height = "300px"),
      div(id = "obs_legend", class = "mt-3")
    ),
    
    # Species
    div(
      class = "text-center",
      plotlyOutput("spp_plot", height = "300px"),
      div(id = "spp_legend", class = "mt-3")
    ),
    
    # People
    div(
      class = "text-center",
      plotlyOutput("ppl_plot", height = "300px"),
      div(id = "ppl_legend", class = "mt-3")
    )
  ),
  
  # Footer
  hr(),
  div(
    class = "text-center text-muted small mt-3",
    p("Data sourced from iNaturalist • Last updated: ", Sys.Date())
  )
)

# SERVER
server <- function(input, output, session) {
  
  # Define color palettes
  quality_colors <- c("research" = "#28a745", "needs_id" = "#ffc107", "casual" = "#6c757d")
  taxon_colors <- c("#FF6B6B", "#4ECDC4", "#45B7D1", "#96CEB4", "#FECA57", 
                    "#FF9FF3", "#54A0FF", "#5F27CD", "#00D2D3", "#FF9F43",
                    "#A8E6CF", "#FFB347", "#87CEEB", "#DDA0DD", "#F0E68C")
  
  # Helper function to create legend HTML
  create_legend <- function(categories, colors) {
    legend_items <- map2_chr(categories, colors, function(cat, col) {
      paste0('<span style="display: inline-block; margin-right: 15px; margin-bottom: 5px;">',
             '<span style="display: inline-block; width: 12px; height: 12px; ',
             'background-color: ', col, '; margin-right: 5px; border-radius: 2px;"></span>',
             '<span style="font-size: 12px;">', cat, '</span></span>')
    })
    paste(legend_items, collapse = "")
  }
  
  # ---- Observations ----
  obs_summary <- reactive({
    inat_data %>%
      distinct(uuid, quality_grade) %>%
      count(quality_grade) %>%
      mutate(
        percentage = round(n/sum(n) * 100, 1)
      )
  })
  
  output$obs_plot <- renderPlotly({
    data <- obs_summary()
    total_obs <- sum(data$n)
    
    plot_ly(data, 
            labels = ~quality_grade, 
            values = ~n,
            type = "pie", 
            hole = 0.6,
            marker = list(colors = quality_colors[data$quality_grade],
                          line = list(color = "white", width = 2)),
            textinfo = "none",
            hovertemplate = "<b>%{label}</b><br>Count: %{value} (%{percent})<extra></extra>") %>%
      layout(
        showlegend = FALSE,
        margin = list(t = 20, b = 20, l = 20, r = 20),
        annotations = list(
          list(x = 0.5, y = 0.5, 
               text = paste0("<b>", formatC(total_obs, big.mark = ","), "</b><br>",
                             "<span style='font-size:12px'>Observations</span>"),
               showarrow = FALSE,
               font = list(size = 16))
        )
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Create legend for observations
  observe({
    data <- obs_summary()
    legend_html <- create_legend(data$quality_grade, quality_colors[data$quality_grade])
    insertUI(selector = "#obs_legend", 
             ui = div(HTML(legend_html), class = "text-center"),
             immediate = TRUE)
  })
  
  # ---- Species ----
  spp_summary <- reactive({
    inat_data %>%
      distinct(taxon_species_name, iconic_taxon_name) %>%
      count(iconic_taxon_name) %>%
      arrange(desc(n)) %>%
      mutate(
        percentage = round(n/sum(n) * 100, 1)
      )
  })
  
  output$spp_plot <- renderPlotly({
    data <- spp_summary()
    total_spp <- sum(data$n)
    
    plot_ly(data, 
            labels = ~iconic_taxon_name, 
            values = ~n,
            type = "pie", 
            hole = 0.6,
            marker = list(colors = taxon_colors[1:nrow(data)],
                          line = list(color = "white", width = 2)),
            textinfo = "none",
            hovertemplate = "<b>%{label}</b><br>Count: %{value} (%{percent})<extra></extra>") %>%
      layout(
        showlegend = FALSE,
        margin = list(t = 20, b = 20, l = 20, r = 20),
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
  
  # Create legend for species
  observe({
    data <- spp_summary()
    legend_html <- create_legend(data$iconic_taxon_name, taxon_colors[1:nrow(data)])
    insertUI(selector = "#spp_legend", 
             ui = div(HTML(legend_html), class = "text-center"),
             immediate = TRUE)
  })
  
  # ---- People ----
  output$ppl_plot <- renderPlotly({
    n_people <- n_distinct(inat_data$user_id)
    
    plot_ly(labels = "Contributors", 
            values = n_people,
            type = "pie", 
            hole = 0.6,
            marker = list(colors = "#17a2b8",
                          line = list(color = "white", width = 2)),
            textinfo = "none",
            hovertemplate = "<b>Contributors</b><br>Count: %{value}<extra></extra>") %>%
      layout(
        showlegend = FALSE,
        margin = list(t = 20, b = 20, l = 20, r = 20),
        annotations = list(
          list(x = 0.5, y = 0.5, 
               text = paste0("<b>", formatC(n_people, big.mark = ","), "</b><br>",
                             "<span style='font-size:12px'>People</span>"),
               showarrow = FALSE,
               font = list(size = 16))
        )
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Create legend for people (simple single item)
  observe({
    legend_html <- create_legend("Contributors", "#17a2b8")
    insertUI(selector = "#ppl_legend", 
             ui = div(HTML(legend_html), class = "text-center"),
             immediate = TRUE)
  })
}

shinyApp(ui, server)












#### iNat and ebird data -------------------------------------------------------

library(shiny)
library(bslib)
library(dplyr)
library(plotly)
library(purrr)

# Read & prepare processed data

# iNat data
inat_data <- read.csv("data/raw_data/biodiversity_data/inat_greatmeadow_20250825.csv")

# eBird data
eBird_data <- read.csv("data/raw_data/biodiversity_data/ebird_greatmeadow_20250825.csv")


ui <- page_fillable(
  theme = bs_theme(
    bootswatch = "flatly",
    primary = "#2E8B57", 
    base_font = font_google("Open Sans")
  ),
  
  # Page Header
  div(
    class = "text-center mb-4",
    h2("Biodiversity of Great Meadow", class = "display-4 text-primary"),
    p("Summary of participatory science observations", class = "lead text-muted")
  ),
  
  #### iNaturalist Section ####
  div(class = "my-5",
      h3("iNaturalist", class = "text-center text-success mb-4"),
      layout_columns(
        col_widths = c(4,4,4),
        
        div(class = "text-center",
            plotlyOutput("obs_plot", height = "250px"),
            div(id = "obs_legend", class = "mt-3")
        ),
        
        div(class = "text-center",
            plotlyOutput("spp_plot", height = "250px"),
            div(id = "spp_legend", class = "mt-3")
        ),
        
        div(class = "text-center",
            plotlyOutput("ppl_plot", height = "250px"),
            div(id = "ppl_legend", class = "mt-3")
        )
      )
  ),
  
  #### eBird Section ####
  div(class = "my-5",
      h3("eBird", class = "text-center text-primary mb-4"),
      layout_columns(
        col_widths = c(4,4,4),
        
        div(class = "text-center",
            plotlyOutput("ebird_observations_plot", height = "250px")
        ),
        
        div(class = "text-center",
            plotlyOutput("ebird_spp_plot", height = "250px")
        ),
        
        div(class = "text-center",
            plotlyOutput("ebird_obs_plot", height = "250px")
        )
      )
  ),
  
  # Footer
  hr(),
  div(
    class = "text-center text-muted small mt-3",
    p("Data sourced from iNaturalist and eBird • Last updated: ", Sys.Date())
  )
)

server <- function(input, output, session) {
  
  # Define color palettes
  quality_colors <- c("research" = "#28a745", "needs_id" = "#ffc107", "casual" = "#6c757d")
  taxon_colors <- c("#FF6B6B", "#4ECDC4", "#45B7D1", "#96CEB4", "#FECA57", 
                    "#FF9FF3", "#54A0FF", "#5F27CD", "#00D2D3", "#FF9F43",
                    "#A8E6CF", "#FFB347", "#87CEEB", "#DDA0DD", "#F0E68C")
  
  # Helper function to create legend HTML
  create_legend <- function(categories, colors) {
    legend_items <- map2_chr(categories, colors, function(cat, col) {
      paste0('<span style="display: inline-block; margin-right: 15px; margin-bottom: 5px;">',
             '<span style="display: inline-block; width: 12px; height: 12px; ',
             'background-color: ', col, '; margin-right: 5px; border-radius: 2px;"></span>',
             '<span style="font-size: 12px;">', cat, '</span></span>')
    })
    paste(legend_items, collapse = "")
  }
  
  # ---- Observations ----
  obs_summary <- reactive({
    inat_data %>%
      distinct(uuid, quality_grade) %>%
      count(quality_grade) %>%
      mutate(
        percentage = round(n/sum(n) * 100, 1)
      )
  })
  
  output$obs_plot <- renderPlotly({
    data <- obs_summary()
    total_obs <- sum(data$n)
    
    plot_ly(data, 
            labels = ~quality_grade, 
            values = ~n,
            type = "pie", 
            hole = 0.6,
            marker = list(colors = quality_colors[data$quality_grade],
                          line = list(color = "white", width = 2)),
            textinfo = "none",
            hovertemplate = "<b>%{label}</b><br>Count: %{value} (%{percent})<extra></extra>") %>%
      layout(
        showlegend = FALSE,
        margin = list(t = 20, b = 20, l = 20, r = 20),
        annotations = list(
          list(x = 0.5, y = 0.5, 
               text = paste0("<b>", formatC(total_obs, big.mark = ","), "</b><br>",
                             "<span style='font-size:12px'>Observations</span>"),
               showarrow = FALSE,
               font = list(size = 16))
        )
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Create legend for observations
  observe({
    data <- obs_summary()
    legend_html <- create_legend(data$quality_grade, quality_colors[data$quality_grade])
    insertUI(selector = "#obs_legend", 
             ui = div(HTML(legend_html), class = "text-center"),
             immediate = TRUE)
  })
  
  # ---- Species ----
  spp_summary <- reactive({
    inat_data %>%
      distinct(taxon_species_name, iconic_taxon_name) %>%
      count(iconic_taxon_name) %>%
      arrange(desc(n)) %>%
      mutate(
        percentage = round(n/sum(n) * 100, 1)
      )
  })
  
  output$spp_plot <- renderPlotly({
    data <- spp_summary()
    total_spp <- sum(data$n)
    
    plot_ly(data, 
            labels = ~iconic_taxon_name, 
            values = ~n,
            type = "pie", 
            hole = 0.6,
            marker = list(colors = taxon_colors[1:nrow(data)],
                          line = list(color = "white", width = 2)),
            textinfo = "none",
            hovertemplate = "<b>%{label}</b><br>Count: %{value} (%{percent})<extra></extra>") %>%
      layout(
        showlegend = FALSE,
        margin = list(t = 20, b = 20, l = 20, r = 20),
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
  
  # Create legend for species
  observe({
    data <- spp_summary()
    legend_html <- create_legend(data$iconic_taxon_name, taxon_colors[1:nrow(data)])
    insertUI(selector = "#spp_legend", 
             ui = div(HTML(legend_html), class = "text-center"),
             immediate = TRUE)
  })
  
  # ---- People ----
  output$ppl_plot <- renderPlotly({
    n_people <- n_distinct(inat_data$user_id)
    
    plot_ly(labels = "Contributors", 
            values = n_people,
            type = "pie", 
            hole = 0.6,
            marker = list(colors = "#17a2b8",
                          line = list(color = "white", width = 2)),
            textinfo = "none",
            hovertemplate = "<b>Contributors</b><br>Count: %{value}<extra></extra>") %>%
      layout(
        showlegend = FALSE,
        margin = list(t = 20, b = 20, l = 20, r = 20),
        annotations = list(
          list(x = 0.5, y = 0.5, 
               text = paste0("<b>", formatC(n_people, big.mark = ","), "</b><br>",
                             "<span style='font-size:12px'>People</span>"),
               showarrow = FALSE,
               font = list(size = 16))
        )
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Create legend for people
  observe({
    legend_html <- create_legend("Contributors", "#17a2b8")
    insertUI(selector = "#ppl_legend", 
             ui = div(HTML(legend_html), class = "text-center"),
             immediate = TRUE)
  })
  
  ## ---- eBird summaries ----
  ebird_summary <- reactive({
    list(
      n_observations = n_distinct(eBird_data$GLOBAL.UNIQUE.IDENTIFIER),
      n_species = n_distinct(eBird_data$SCIENTIFIC.NAME),
      n_observers = n_distinct(eBird_data$OBSERVER.ID)
    )
  })
  
  # Checklists donut
  output$ebird_observations_plot <- renderPlotly({
    n_observations <- ebird_summary()$n_observations
    plot_ly(labels = "Observations", values = n_observations,
            type = "pie", hole = 0.6,
            marker = list(colors = "#28B463", line = list(color = "white", width = 2)),
            textinfo = "none") %>%
      layout(
        showlegend = FALSE,
        annotations = list(list(x = 0.5, y = 0.5,
                                text = paste0("<b>", formatC(n_observations, big.mark=","), "</b><br>",
                                              "<span style='font-size:12px'>Observations</span>"),
                                showarrow = FALSE,
                                font = list(size = 16)))
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Species donut
  output$ebird_spp_plot <- renderPlotly({
    n_species <- ebird_summary()$n_species
    plot_ly(labels = "Species", values = n_species,
            type = "pie", hole = 0.6,
            marker = list(colors = "#2E86AB", line = list(color = "white", width = 2)),
            textinfo = "none") %>%
      layout(
        showlegend = FALSE,
        annotations = list(list(x = 0.5, y = 0.5,
                                text = paste0("<b>", formatC(n_species, big.mark=","), "</b><br>",
                                              "<span style='font-size:12px'>Species</span>"),
                                showarrow = FALSE,
                                font = list(size = 16)))
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Observers donut
  output$ebird_obs_plot <- renderPlotly({
    n_obs <- ebird_summary()$n_observers
    plot_ly(labels = "Observers", values = n_obs,
            type = "pie", hole = 0.6,
            marker = list(colors = "#E67E22", line = list(color = "white", width = 2)),
            textinfo = "none") %>%
      layout(
        showlegend = FALSE,
        annotations = list(list(x = 0.5, y = 0.5,
                                text = paste0("<b>", formatC(n_obs, big.mark=","), "</b><br>",
                                              "<span style='font-size:12px'>Observers</span>"),
                                showarrow = FALSE,
                                font = list(size = 16)))
      ) %>%
      config(displayModeBar = FALSE)
  })
  
}

shinyApp(ui, server) 























#### another option - stats bar for eBird data




#### iNat and ebird data -------------------------------------------------------

library(shiny)
library(bslib)
library(dplyr)
library(plotly)
library(purrr)

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
)

#UI
ui <- page_fillable(
  theme = bs_theme(
    bootswatch = "flatly",
    primary = "#2E8B57", 
    base_font = font_google("Open Sans")
  ),
  
  # Page Header
  div(
    class = "text-center mb-4",
    h2("Biodiversity of Great Meadow", class = "display-4 text-primary"),
    p("Summary of participatory science observations", class = "lead text-muted")
  ),
  
  #### iNaturalist Section ####
  div(class = "my-5",
      h3("iNaturalist", class = "text-center text-success mb-4"),
      layout_columns(
        col_widths = c(4,4,4),
        
        div(class = "text-center",
            plotlyOutput("obs_plot", height = "250px"),
            div(id = "obs_legend", class = "mt-3")
        ),
        
        div(class = "text-center",
            plotlyOutput("spp_plot", height = "250px"),
            div(id = "spp_legend", class = "mt-3")
        ),
        
        div(class = "text-center",
            plotlyOutput("ppl_plot", height = "250px"),
            div(id = "ppl_legend", class = "mt-3")
        )
      )
  ),
  
  #### eBird Section ####
  div(class = "my-5",
      h2("eBird", class = "text-center text-primary mb-4"),
      
      # Stats bar
      div(class = "d-flex justify-content-center gap-5",
          
          div(class = "text-center",
              h3(textOutput("ebird_observations"), class = "mb-1 text-primary"),
              div("Observations", class = "text-muted")
          ),
          
          div(class = "text-center",
              h3(textOutput("ebird_species"), class = "mb-1 text-primary"),
              div("Species", class = "text-muted")
          ),
          
          div(class = "text-center",
              h3(textOutput("ebird_observers"), class = "mb-1 text-warning"),
              div("Observers", class = "text-muted")
          )
      )
  ),
  
  # Footer
  hr(),
  div(
    class = "text-center text-muted small mt-3",
    p("Data sourced from iNaturalist and eBird • Last updated: ", Sys.Date())
  )
)

server <- function(input, output, session) {
  
  # Define color palettes
  quality_colors <- c("research" = "#28a745", "needs_id" = "#ffc107", "casual" = "#6c757d")
  taxon_colors <- c("#FF6B6B", "#4ECDC4", "#45B7D1", "#96CEB4", "#FECA57", 
                    "#FF9FF3", "#54A0FF", "#5F27CD", "#00D2D3", "#FF9F43",
                    "#A8E6CF", "#FFB347", "#87CEEB", "#DDA0DD", "#F0E68C")
  
  # Helper function to create legend HTML
  create_legend <- function(categories, colors) {
    legend_items <- map2_chr(categories, colors, function(cat, col) {
      paste0('<span style="display: inline-block; margin-right: 15px; margin-bottom: 5px;">',
             '<span style="display: inline-block; width: 12px; height: 12px; ',
             'background-color: ', col, '; margin-right: 5px; border-radius: 2px;"></span>',
             '<span style="font-size: 12px;">', cat, '</span></span>')
    })
    paste(legend_items, collapse = "")
  }
  
  # ---- Observations ----
  obs_summary <- reactive({
    inat_data %>%
      distinct(uuid, quality_grade) %>%
      count(quality_grade) %>%
      mutate(
        percentage = round(n/sum(n) * 100, 1)
      )
  })
  
  output$obs_plot <- renderPlotly({
    data <- obs_summary()
    total_obs <- sum(data$n)
    
    plot_ly(data, 
            labels = ~quality_grade, 
            values = ~n,
            type = "pie", 
            hole = 0.6,
            marker = list(colors = quality_colors[data$quality_grade],
                          line = list(color = "white", width = 2)),
            textinfo = "none",
            hovertemplate = "<b>%{label}</b><br>Count: %{value} (%{percent})<extra></extra>") %>%
      layout(
        showlegend = FALSE,
        margin = list(t = 20, b = 20, l = 20, r = 20),
        annotations = list(
          list(x = 0.5, y = 0.5, 
               text = paste0("<b>", formatC(total_obs, big.mark = ","), "</b><br>",
                             "<span style='font-size:12px'>Observations</span>"),
               showarrow = FALSE,
               font = list(size = 16))
        )
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Create legend for observations
  observe({
    data <- obs_summary()
    legend_html <- create_legend(data$quality_grade, quality_colors[data$quality_grade])
    insertUI(selector = "#obs_legend", 
             ui = div(HTML(legend_html), class = "text-center"),
             immediate = TRUE)
  })
  
  # ---- Species ----
  spp_summary <- reactive({
    inat_data %>%
      distinct(taxon_species_name, iconic_taxon_name) %>%
      count(iconic_taxon_name) %>%
      arrange(desc(n)) %>%
      mutate(
        percentage = round(n/sum(n) * 100, 1)
      )
  })
  
  output$spp_plot <- renderPlotly({
    data <- spp_summary()
    total_spp <- sum(data$n)
    
    plot_ly(data, 
            labels = ~iconic_taxon_name, 
            values = ~n,
            type = "pie", 
            hole = 0.6,
            marker = list(colors = taxon_colors[1:nrow(data)],
                          line = list(color = "white", width = 2)),
            textinfo = "none",
            hovertemplate = "<b>%{label}</b><br>Count: %{value} (%{percent})<extra></extra>") %>%
      layout(
        showlegend = FALSE,
        margin = list(t = 20, b = 20, l = 20, r = 20),
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
  
  # Create legend for species
  observe({
    data <- spp_summary()
    legend_html <- create_legend(data$iconic_taxon_name, taxon_colors[1:nrow(data)])
    insertUI(selector = "#spp_legend", 
             ui = div(HTML(legend_html), class = "text-center"),
             immediate = TRUE)
  })
  
  # ---- People ----
  output$ppl_plot <- renderPlotly({
    n_people <- n_distinct(inat_data$user_id)
    
    plot_ly(labels = "Contributors", 
            values = n_people,
            type = "pie", 
            hole = 0.6,
            marker = list(colors = "#17a2b8",
                          line = list(color = "white", width = 2)),
            textinfo = "none",
            hovertemplate = "<b>Contributors</b><br>Count: %{value}<extra></extra>") %>%
      layout(
        showlegend = FALSE,
        margin = list(t = 20, b = 20, l = 20, r = 20),
        annotations = list(
          list(x = 0.5, y = 0.5, 
               text = paste0("<b>", formatC(n_people, big.mark = ","), "</b><br>",
                             "<span style='font-size:12px'>People</span>"),
               showarrow = FALSE,
               font = list(size = 16))
        )
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Create legend for people
  observe({
    legend_html <- create_legend("Contributors", "#17a2b8")
    insertUI(selector = "#ppl_legend", 
             ui = div(HTML(legend_html), class = "text-center"),
             immediate = TRUE)
  })
  
  ## ---- eBird summaries ----
  ebird_summary <- reactive({
    list(
      n_observations = n_distinct(eBird_data$GLOBAL.UNIQUE.IDENTIFIER),
      n_species = n_distinct(eBird_data$SCIENTIFIC.NAME),
      n_observers = n_distinct(eBird_data$OBSERVER.ID)
    )
  })
  
  output$ebird_observations <- renderText({
    formatC(ebird_summary()$n_observations, big.mark = ",")
  })
  
  output$ebird_species <- renderText({
    formatC(ebird_summary()$n_species, big.mark = ",")
  })
  
  output$ebird_observers <- renderText({
    formatC(ebird_summary()$n_observers, big.mark = ",")
  })
}

shinyApp(ui, server) 













#### iNat and ebird data -------------------------------------------------------

library(shiny)
library(bslib)
library(dplyr)
library(plotly)
library(purrr)

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
)

#UI
ui <- page_fillable(
  theme = bs_theme(
    bootswatch = "flatly",
    primary = "#2E8B57", 
    base_font = font_google("Open Sans")
  ),
  
  # Page Header
  div(
    class = "text-center mb-4",
    h2("Biodiversity of Great Meadow", class = "display-4 text-primary"),
    p("Summary of participatory science observations", class = "lead text-muted")
  ),
  
  #### iNaturalist Section ####
  div(class = "my-5",
      h3("iNaturalist", class = "text-center text-success mb-4"),
      
      # Single pie chart for species by taxa
      div(class = "text-center mb-4",
          plotlyOutput("spp_plot", height = "400px"),
          div(id = "spp_legend", class = "mt-3")
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
              div("Contributors", class = "text-muted")
            )
          )
        )
      )
  ),
  
  #### eBird Section ####
  div(class = "my-5",
      h2("eBird", class = "text-center text-primary mb-4"),
      
      # Stats bar
      div(class = "d-flex justify-content-center gap-5",
          
          div(class = "text-center",
              h3(textOutput("ebird_observations"), class = "mb-1 text-primary"),
              div("Observations", class = "text-muted")
          ),
          
          div(class = "text-center",
              h3(textOutput("ebird_species"), class = "mb-1 text-primary"),
              div("Species", class = "text-muted")
          ),
          
          div(class = "text-center",
              h3(textOutput("ebird_observers"), class = "mb-1 text-warning"),
              div("Observers", class = "text-muted")
          )
      )
  ),
  
  # Footer
  hr(),
  div(
    class = "text-center text-muted small mt-3",
    p("Data sourced from iNaturalist and eBird • Last updated: ", Sys.Date())
  )
)

server <- function(input, output, session) {
  
  # Define color palettes
  taxon_colors <- c("#FF6B6B", "#4ECDC4", "#45B7D1", "#96CEB4", "#FECA57", 
                    "#FF9FF3", "#54A0FF", "#5F27CD", "#00D2D3", "#FF9F43",
                    "#A8E6CF", "#FFB347", "#87CEEB", "#DDA0DD", "#F0E68C")
  
  # Helper function to create legend HTML
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
      count(iconic_taxon_name) %>%
      arrange(desc(n)) %>%
      mutate(
        percentage = round(n/sum(n) * 100, 1)
      )
  })
  
  output$spp_plot <- renderPlotly({
    data <- spp_summary()
    total_spp <- sum(data$n)
    
    plot_ly(data, 
            labels = ~iconic_taxon_name, 
            values = ~n,
            type = "pie", 
            hole = 0.4,
            marker = list(colors = taxon_colors[1:nrow(data)],
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
  
  # Create legend for species
  observe({
    data <- spp_summary()
    legend_html <- create_legend(data$iconic_taxon_name, taxon_colors[1:nrow(data)])
    insertUI(selector = "#spp_legend", 
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
      n_observations = n_distinct(eBird_data$GLOBAL.UNIQUE.IDENTIFIER),
      n_species = n_distinct(eBird_data$SCIENTIFIC.NAME),
      n_observers = n_distinct(eBird_data$OBSERVER.ID)
    )
  })
  
  output$ebird_observations <- renderText({
    formatC(ebird_summary()$n_observations, big.mark = ",")
  })
  
  output$ebird_species <- renderText({
    formatC(ebird_summary()$n_species, big.mark = ",")
  })
  
  output$ebird_observers <- renderText({
    formatC(ebird_summary()$n_observers, big.mark = ",")
  })
}

shinyApp(ui, server)




#-------------------------------------------------------------------------------





#### Biodiversity R shiny app notes ####


#### eBird data ####

eBird_data <- read.csv("data/raw_data/biodiversity_data/ebird_greatmeadow_20250825.csv")

colnames(eBird_data)
unique(eBird_data$OBSERVATION.TYPE)


#### iNat data ####


library(shiny)
library(dplyr)
library(plotly)


# Read & prepare processed data

inat_data <- read.csv("data/raw_data/biodiversity_data/inat_greatmeadow_20250825.csv")


# Example app
ui <- fluidPage(
  fluidRow(
    column(3, plotlyOutput("obs_plot")),
    column(3, plotlyOutput("spp_plot")),
    # column(3, plotlyOutput("id_plot")),
    column(3, plotlyOutput("ppl_plot"))
  )
)

server <- function(input, output, session) {
  
  # ---- Observations ----
  obs_summary <- inat_data %>%
    distinct(uuid, quality_grade) %>%        # unique obs by uuid
    count(quality_grade)
  
  output$obs_plot <- renderPlotly({
    plot_ly(obs_summary, labels = ~quality_grade, values = ~n,
            type = "pie", hole = 0.6) %>%
      layout(title = list(text = paste0(formatC(n_distinct(inat_data$uuid), big.mark = ","),
                                        "<br>OBSERVATIONS"),
                          x = 0.5, y = 0.5))
  })
  
  # ---- Species ----
  spp_summary <- inat_data %>%
    distinct(taxon_species_name, iconic_taxon_name) %>%  # unique species
    count(iconic_taxon_name)
  
  output$spp_plot <- renderPlotly({
    plot_ly(spp_summary, labels = ~iconic_taxon_name, values = ~n,
            type = "pie", hole = 0.6) %>%
      layout(title = list(text = paste0(formatC(n_distinct(inat_data$taxon_species_name), big.mark = ","),
                                        "<br>SPECIES"),
                          x = 0.5, y = 0.5))
  })
  
  # # ---- Identifications ----
  # # Assuming you have a column "ident_type"
  # id_summary <- df %>%
  #   count(ident_type)
  # 
  # output$id_plot <- renderPlotly({
  #   plot_ly(id_summary, labels = ~ident_type, values = ~n,
  #           type = "pie", hole = 0.6) %>%
  #     layout(title = list(text = paste0(formatC(sum(id_summary$n), big.mark = ","),
  #                                       "<br>IDENTIFICATIONS"),
  #                         x = 0.5, y = 0.5))
  # })
  
  # ---- People ----
  # Just total unique users for now
  n_people <- n_distinct(inat_data$user_id)
  
  output$ppl_plot <- renderPlotly({
    plot_ly(labels = "People", values = n_people,
            type = "pie", hole = 0.6) %>%
      layout(title = list(text = paste0(formatC(n_people, big.mark = ","),
                                        "<br>PEOPLE"),
                          x = 0.5, y = 0.5),
             showlegend = FALSE)
  })
}

shinyApp(ui, server)






#### DRAFT 2 -------------------------------------------------------------------



library(shiny)
library(bslib)
library(dplyr)
library(plotly)
library(purrr)

# Read & prepare processed data

inat_data <- read.csv("data/raw_data/biodiversity_data/inat_greatmeadow_20250825.csv")


ui <- page_fillable(
  theme = bs_theme(
    bootswatch = "flatly",
    primary = "#2E8B57", 
    base_font = font_google("Open Sans")
  ),
  
  # Header
  div(
    class = "text-center mb-4",
    h1("iNaturalist Biodiversity Dashboard", 
       class = "display-4 text-primary"),
    p("Summary of observations from Great Meadow", 
      class = "lead text-muted")
  ),
  
  # Main content
  layout_columns(
    col_widths = c(4, 4, 4),
    
    # Observations card
    card(
      card_header(
        icon("binoculars"), " Observations",
        class = "text-center bg-primary text-white"
      ),
      card_body(
        plotlyOutput("obs_plot", height = "250px"),
        div(id = "obs_legend", class = "mt-2"),
        class = "p-2"
      ),
      full_screen = TRUE
    ),
    
    # Species card
    card(
      card_header(
        icon("leaf"), " Species",
        class = "text-center bg-success text-white"
      ),
      card_body(
        plotlyOutput("spp_plot", height = "250px"),
        div(id = "spp_legend", class = "mt-2"),
        class = "p-2"
      ),
      full_screen = TRUE
    ),
    
    # People card
    card(
      card_header(
        icon("users"), " Contributors",
        class = "text-center bg-info text-white"
      ),
      card_body(
        plotlyOutput("ppl_plot", height = "250px"),
        div(id = "ppl_legend", class = "mt-2"),
        class = "p-2"
      ),
      full_screen = TRUE
    )
  ),
  
  # Footer
  hr(),
  div(
    class = "text-center text-muted small mt-3",
    p("Data sourced from iNaturalist • Last updated: ", Sys.Date())
  )
)

server <- function(input, output, session) {
  
  # Define color palettes
  quality_colors <- c("research" = "#28a745", "needs_id" = "#ffc107", "casual" = "#6c757d")
  taxon_colors <- c("#FF6B6B", "#4ECDC4", "#45B7D1", "#96CEB4", "#FECA57", 
                    "#FF9FF3", "#54A0FF", "#5F27CD", "#00D2D3", "#FF9F43")
  
  # Simplified helper function to create legend HTML
  create_legend <- function(categories, colors) {
    legend_items <- map2_chr(categories, colors, function(cat, col) {
      paste0('<span style="display: inline-block; margin-right: 15px; margin-bottom: 5px;">',
             '<span style="display: inline-block; width: 12px; height: 12px; ',
             'background-color: ', col, '; margin-right: 5px; border-radius: 2px;"></span>',
             '<span style="font-size: 12px;">', cat, '</span></span>')
    })
    paste(legend_items, collapse = "")
  }
  
  # ---- Observations ----
  obs_summary <- reactive({
    inat_data %>%
      distinct(uuid, quality_grade) %>%
      count(quality_grade) %>%
      mutate(
        percentage = round(n/sum(n) * 100, 1)
      )
  })
  
  output$obs_plot <- renderPlotly({
    data <- obs_summary()
    total_obs <- sum(data$n)
    
    plot_ly(data, 
            labels = ~quality_grade, 
            values = ~n,
            type = "pie", 
            hole = 0.6,
            marker = list(colors = quality_colors[data$quality_grade],
                          line = list(color = "white", width = 2)),
            textinfo = "none",
            hovertemplate = "<b>%{label}</b><br>Count: %{value} (%{percent})<extra></extra>") %>%
      layout(
        showlegend = FALSE,
        margin = list(t = 20, b = 20, l = 20, r = 20),
        annotations = list(
          list(x = 0.5, y = 0.5, 
               text = paste0("<b>", formatC(total_obs, big.mark = ","), "</b><br>",
                             "<span style='font-size:12px'>Observations</span>"),
               showarrow = FALSE,
               font = list(size = 16))
        )
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Create legend for observations
  observe({
    data <- obs_summary()
    legend_html <- create_legend(data$quality_grade, quality_colors[data$quality_grade])
    insertUI(selector = "#obs_legend", 
             ui = div(HTML(legend_html), class = "text-center"),
             immediate = TRUE)
  })
  
  # ---- Species ----
  spp_summary <- reactive({
    inat_data %>%
      distinct(taxon_species_name, iconic_taxon_name) %>%
      count(iconic_taxon_name) %>%
      arrange(desc(n)) %>%
      mutate(
        percentage = round(n/sum(n) * 100, 1)
      )
  })
  
  output$spp_plot <- renderPlotly({
    data <- spp_summary()
    total_spp <- sum(data$n)
    
    plot_ly(data, 
            labels = ~iconic_taxon_name, 
            values = ~n,
            type = "pie", 
            hole = 0.6,
            marker = list(colors = taxon_colors[1:nrow(data)],
                          line = list(color = "white", width = 2)),
            textinfo = "none",
            hovertemplate = "<b>%{label}</b><br>Count: %{value} (%{percent})<extra></extra>") %>%
      layout(
        showlegend = FALSE,
        margin = list(t = 20, b = 20, l = 20, r = 20),
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
  
  # Create legend for species
  observe({
    data <- spp_summary()
    legend_html <- create_legend(data$iconic_taxon_name, taxon_colors[1:nrow(data)])
    insertUI(selector = "#spp_legend", 
             ui = div(HTML(legend_html), class = "text-center"),
             immediate = TRUE)
  })
  
  # ---- People ----
  output$ppl_plot <- renderPlotly({
    n_people <- n_distinct(inat_data$user_id)
    
    plot_ly(labels = "Contributors", 
            values = n_people,
            type = "pie", 
            hole = 0.6,
            marker = list(colors = "#17a2b8",
                          line = list(color = "white", width = 2)),
            textinfo = "none",
            hovertemplate = "<b>Contributors</b><br>Count: %{value}<extra></extra>") %>%
      layout(
        showlegend = FALSE,
        margin = list(t = 20, b = 20, l = 20, r = 20),
        annotations = list(
          list(x = 0.5, y = 0.5, 
               text = paste0("<b>", formatC(n_people, big.mark = ","), "</b><br>",
                             "<span style='font-size:12px'>People</span>"),
               showarrow = FALSE,
               font = list(size = 16))
        )
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Create legend for people (simple single item)
  observe({
    legend_html <- create_legend("Contributors", "#17a2b8")
    insertUI(selector = "#ppl_legend", 
             ui = div(HTML(legend_html), class = "text-center"),
             immediate = TRUE)
  })
}

shinyApp(ui, server)


