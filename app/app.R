library(shiny)
library(tidyverse)
library(bslib)
library(bsicons)

# placeholder for data loading; more will be added
city_stations <- readRDS("data/city_stations.rds")

#build the city choices: "City, ST" labels in alphabetical order, GEOID is the value
city_list <- city_stations |>
  mutate(label = paste0(CITY, ", ", STATE)) |>
  arrange(label)

city_choices <- setNames(city_list$GEOID, city_list$label)

#set the date the data was pulled - UPDATE with latest pulls
data_date <- "2026-11-22"

#small result box for the Statistics tabs: a label and a number filled in by the server
result_box <- function(label, output_id) {
  card(
    card_body(
      p(class = "small text-muted mb-1", label),
      h4(class = "font-monospace mb-0", textOutput(output_id))
    )
  )
}

# starting to update app with basic layout and themes, will continue to add to this

ui <- navbarPage(
  title = tags$span(bs_icon("thermometer-half"), "U.S. Cities Climate Explorer"), #name of the app, with icon
  windowTitle = "U.S. Cities Climate Explorer", #name shown on the browser tab
  position = "static-top", #keeps it at the top of the window
  collapsible = TRUE, #will get smaller if the screen is smaller
  fluid = TRUE, #keeps it proportional to the size of the window
  theme = bs_theme(version = 5, bootswatch = "flatly", #theme
                   base_font = font_google("IBM Plex Sans"), #main text font
                   code_font = font_google("IBM Plex Mono")), #font for numbers
  
  #set up the different tabs
  tabPanel("About",
           h2("How has the climate changed where you live?"),
           p("A demonstration app for teachers and students. With this app, you can explore
                daily temperatures and precipitation data for cities around the U.S. since 1990,
                test whether trends are statistically meaningful, and compare NASA satellite-modeled
                data with NOAA weather station records."),
           #add cards on the "About" Panel
           card(
             card_header("Questions you can answer with this app"),
             card_body(
               layout_column_wrap(
                 width = "220px",
                 card(class = "bg-light", card_body(
                   strong("Is my city getting warmer?"),
                   p(class = "text-muted small mb-0", "Explore Tab, then Statistics > Trends")
                 )),
                 card(class = "bg-light", card_body(
                   strong("Does satellite data match ground station data?"),
                   p(class = "text-muted small mb-0", "Statistics > Satellite vs. Station")
                 )),
                 card(class = "bg-light", card_body(
                   strong("Are recent decades different?"),
                   p(class = "text-muted small mb-0", "Statistics > Decades and Regions")
                 )),
                 card(class = "bg-light", card_body(
                   strong("Was this summer unusual?"),
                   p(class = "text-muted small mb-0", "Statistics > Compare Two Time Periods")
                 ))
                 #need to add additional cards with data sources, names, etc
               )
             )
           ),
           layout_columns(
             col_widths = c(8, 4),
             
             card(
               card_header(strong("Data Sources")),
               card_body(
                 p(strong("NASA POWER"), "daily point data, modeled from satellite and reanalysis products (grid-cell estimates)"),
                 p(strong("NOAA GHCN-Daily"), "weather station observations, quality-flagged values removed"),
                 p(strong("U.S. Census Gazetteer"), "city names and coordinates"),
                 p(class = "text-muted small mb-0",
                   paste0("Coverage: 48 cities from across the United States, January 1990 to November 2026. Date pulled ",
                          data_date, "."))
               )
             ),
             
             card(class = "text-bg-dark", #dark background with light text
                  card_header(strong("How to Use the App")),
                  card_body(
                    p("1. Pick cities and a variable in Explore"),
                    p("2. Look for patterns in the map and charts"),
                    p("3. Test them in Statistics"),
                    p("4. Read the plain-language interpretation")
                  )
             )
           ),
           p(class = "text-muted small mb-0", "DATA 613 Group Project. Prashant Choudhary, Brook Estifanos, Rachel Stagner. References listed in the vignette")
  ),
  
  
  tabPanel("Explore",
           sidebarLayout(
             #left side: controls
             sidebarPanel(
               h4("Choose your Data"),
               p(class = "text-muted small mb-0", "Tab layout and controls: Role 3"),
               selectizeInput("cities", strong("Cities (up to 4)"),
                              choices = city_choices,
                              selected = "92242", #Washington, DC is the default
                              multiple = TRUE,
                              options = list(maxItems = 4,
                                             placeholder = "Choose up to 4 cities")),
               radioButtons("source", strong("Data source"),
                            choices = c("NASA POWER (satellite)", "NOAA weather station", "Both, overlaid")
               ),
               selectInput("variable", strong("Variable"),
                           choices = c("Mean temperature (°C)", "Max temperature (°C)",
                                       "Min temperature (°C)", "Precipitation (mm)") #get rid of precip
               ),
               p(strong("Years")),
               layout_columns(
                 numericInput("year_from", "From", value = 1990, min = 1990, max = 2026), #start of data
                 numericInput("year_to", "To", value = 2026, min = 1990, max = 2026) #change to whatever our end year is
               ),
               actionButton("button", "Submit")
               
             ),
             
             #right side: the outputs
             mainPanel(
               #map and time series side by side
               layout_columns(
                 col_widths = c(6, 6),
                 card(
                   card_header("Map: Prashant (Role 2)"),
                   #use leafletOutput("map") instead if the map uses leaflet
                   plotOutput("map", height = "300px")
                 ),
                 card(
                   card_header("Time Series: Brook (Role 1)"),
                   #time series for the selected cities and source
                   plotOutput("ts_plot", height = "300px")
                 )
               ),
               #summary table across the bottom
               card(
                 card_header("Summary: Prashant (Role 2)"),
                 #summary table for the selected cities
                 tableOutput("summary")
               )
             )
           )
  ),
  
  tabPanel("Statistics",
           #pill buttons across the top of the Statistics page, one per test
           tabsetPanel(
             type = "pills",
             
             # ---------------- TRENDS: Brook (Role 1) ----------------
             tabPanel("Trends",
                      #result boxes across the top
                      layout_columns(
                        class = "mt-3", #space below the pills
                        result_box("Slope (°C per decade)", "tr_slope"),
                        result_box("95% confidence interval", "tr_ci"),
                        result_box("p-value", "tr_p"),
                        result_box("R²", "tr_r2")
                      ),
                      card(card_header("Annual values with fitted trend: Brook (Role 1)"),
                           p(class = "small text-muted mb-0", "Linear regression of temperature over time per city."),
                           plotOutput("tr_fit", height = "300px")),
                      layout_columns(
                        col_widths = c(6, 6),
                        card(card_header("Residuals vs. fitted: Brook (Role 1)"),
                             plotOutput("tr_resid", height = "250px")),
                        card(card_header("Normal Q-Q plot of residuals: Brook (Role 1)"),
                             plotOutput("tr_qq", height = "250px"))
                      ),
                      card(card_header("What this means: Brook (Role 1)"),
                           textOutput("tr_text"))
             ),
             
             # ---------------- SATELLITE VS. STATION: Prashant (Role 2) ----------------
             tabPanel("Satellite vs. Station",
                      layout_columns(
                        class = "mt-3", #space below the pills
                        result_box("Mean bias (POWER minus station)", "sv_bias"),
                        result_box("Paired test p-value", "sv_p"),
                        result_box("Regression slope", "sv_slope"),
                        result_box("RMSE", "sv_rmse")
                      ),
                      layout_columns(
                        col_widths = c(7, 5),
                        card(card_header("Station vs. POWER, with 1:1 line: Prashant (Role 2)"),
                             p(class = "small text-muted mb-0", "Regression of station temperature on NASA POWER temperature."),
                             plotOutput("sv_scatter", height = "300px")),
                        card(card_header("Residuals by season: Prashant (Role 2)"),
                             plotOutput("sv_resid", height = "300px"))
                      ),
                      card(card_header("What this means: Prashant (Role 2)"),
                           textOutput("sv_text"))
             ),
             
             # ---------------- DECADES AND REGIONS: Rachel (Role 3) ----------------
             tabPanel("Decades and Regions",
                      layout_columns(
                        class = "mt-3", #space below the pills
                        result_box("F statistic", "dr_f"),
                        result_box("p-value", "dr_p"),
                        result_box("Groups compared", "dr_groups"),
                        result_box("Significant pairs", "dr_pairs")
                      ),
                      layout_columns(
                        col_widths = c(7, 5),
                        card(card_header("Group means with 95% intervals: Rachel (Role 3)"),
                             p(class = "small text-muted mb-0", "ANOVA comparing decades or regions."),
                             plotOutput("dr_means", height = "280px")),
                        card(card_header("Assumption checks: Rachel (Role 3)"),
                             uiOutput("dr_checks"))
                      ),
                      card(card_header("ANOVA table and pairwise comparisons: Rachel (Role 3)"),
                           tableOutput("dr_anova"),
                           tableOutput("dr_posthoc")),
                      card(card_header("What this means: Rachel (Role 3)"),
                           textOutput("dr_text"))
             ),
             
             # ---------------- COMPARE TWO PERIODS: Brook (Role 1) ----------------
             tabPanel("Compare Two Periods",
                      layout_columns(
                        class = "mt-3", #space below the pills
                        result_box("Period A mean", "cp_a_mean"),
                        result_box("Period B mean", "cp_b_mean"),
                        result_box("Difference (B minus A)", "cp_diff"),
                        result_box("p-value", "cp_p")
                      ),
                      card(card_header("Distribution of daily values in each period: Brook (Role 1)"),
                           p(class = "small text-muted mb-0", "Two-period comparison with a t-test or Wilcoxon test."),
                           plotOutput("cp_dist", height = "300px")),
                      card(card_header("What this means: Brook (Role 1)"),
                           textOutput("cp_text"))
             )
           )
  )
  
)

server <- function(input, output, session) {
  
  #Explore tab placeholders (replace with real outputs)
  output$map     <- renderPlot(plot.new()) #Prashant
  # Time series Brook
  output$ts_plot <- renderPlot({
    validate(need(length(input$cities) > 0, "Please choose at least one city."))
    
    selected_cities <- as.character(input$cities[seq_len(min(length(input$cities), 4))])
    yr_min <- min(as.integer(input$year_from), as.integer(input$year_to), na.rm = TRUE)
    yr_max <- max(as.integer(input$year_from), as.integer(input$year_to), na.rm = TRUE)
    
    nasa_col <- switch(
      input$variable,
      "Mean temperature (°C)" = "nasa_mean",
      "Max temperature (°C)"  = "nasa_max",
      "Min temperature (°C)"  = "nasa_min",
      "nasa_mean"
    )
    
    noaa_col <- switch(
      input$variable,
      "Mean temperature (°C)" = "noaa_mean",
      "Max temperature (°C)"  = "noaa_max",
      "Min temperature (°C)"  = "noaa_min",
      "noaa_mean"
    )
    
    filtered_df <- climate_joined |>
      mutate(
        GEOID = as.character(GEOID),
        date = as.Date(date),
        year = as.integer(year),
        city_label = paste0(city, ", ", state)
      ) |>
      filter(
        GEOID %in% selected_cities,
        year >= yr_min,
        year <= yr_max
      )
    
    nasa_rows <- filtered_df |>
      filter(!is.na(.data[[nasa_col]])) |>
      transmute(
        GEOID,
        city_label,
        date,
        year,
        source_label = "NASA POWER (satellite)",
        value = as.numeric(.data[[nasa_col]])
      )
    
    noaa_rows <- filtered_df |>
      filter(!is.na(.data[[noaa_col]])) |>
      transmute(
        GEOID,
        city_label,
        date,
        year,
        source_label = "NOAA weather station",
        value = as.numeric(.data[[noaa_col]])
      )
    
    plot_df <- switch(
      input$source,
      "NASA POWER (satellite)" = nasa_rows,
      "NOAA weather station"   = noaa_rows,
      "Both, overlaid"         = bind_rows(nasa_rows, noaa_rows),
      nasa_rows
    )
    
    validate(need(nrow(plot_df) > 0, "No non-missing temperature data for this selection."))
    
    if (identical(input$source, "Both, overlaid")) {
      ggplot(plot_df, aes(x = date, y = value, color = source_label, linetype = source_label)) +
        geom_line(linewidth = 0.45, alpha = 0.85, na.rm = TRUE) +
        facet_wrap(~ city_label,
          scales = "free_y") + 
          scale_color_manual(
            values = c("NASA POWER (satellite)" = "#D95F02", "NOAA weather station" = "#2166AC")) + 
        labs(
          title = paste0(input$variable, " — ", input$source),
          subtitle = paste0("Years: ", min(plot_df$year), "–", max(plot_df$year)),
          x = "Date",
          y = input$variable,
          color = "Source",
          linetype = "Source"
        ) +
        theme_minimal(base_size = 11) +
        theme(legend.position = "top")
    } else {
      ggplot(plot_df, aes(x = date, y = value, color = source_label)) +
        geom_line(linewidth = 0.45, alpha = 0.85, na.rm = TRUE) +
          scale_color_manual(
            values = c("NASA POWER (satellite)" = "#D95F02", "NOAA weather station" = "#2166AC")) +
        labs(
          title = paste0(input$variable, " — ", input$source),
          subtitle = paste0("Years: ", min(plot_df$year), "–", max(plot_df$year)),
          x = "Date",
          y = input$variable,
          color = "City"
        ) +
        theme_minimal(base_size = 11) +
        theme(legend.position = "top")
    }
  })
  output$summary <- renderTable(data.frame(Note = "Summary table coming soon")) #Prashant
  
  #Statistics result box placeholders (each owner replaces theirs with real results)
  result_ids <- c("tr_slope", "tr_ci", "tr_p", "tr_r2",       #Brook
                  "sv_bias", "sv_p", "sv_slope", "sv_rmse",   #Prashant
                  "dr_f", "dr_p", "dr_groups", "dr_pairs",    #Rachel
                  "cp_a_mean", "cp_b_mean", "cp_diff", "cp_p") #Brook
  for (id in result_ids) {
    output[[id]] <- renderText("--")
  }
  
  #Statistics chart placeholders (each owner replaces theirs with real plots)
  plot_ids <- c("tr_fit", "tr_resid", "tr_qq",  #Brook
                "sv_scatter", "sv_resid",        #Prashant
                "dr_means",                      #Rachel
                "cp_dist")                       #Brook
  for (id in plot_ids) {
    output[[id]] <- renderPlot(plot.new())
  }
  
  #"What this means" placeholders
  text_ids <- c("tr_text", "sv_text", "dr_text", "cp_text")
  for (id in text_ids) {
    output[[id]] <- renderText("Plain-language interpretation coming soon.")
  }
  
  #Decades and Regions placeholders (Rachel)
  output$dr_checks  <- renderUI(p(class = "text-muted", "Shapiro-Wilk, Levene, and group sizes coming soon."))
  output$dr_anova   <- renderTable(data.frame(Note = "ANOVA table coming soon"))
  output$dr_posthoc <- renderTable(data.frame(Note = "Pairwise comparisons coming soon"))
  
  #every Explore output should start with:
  #validate(need(length(input$cities) > 0, "Please choose at least one city."))
}

shinyApp(ui = ui, server = server)