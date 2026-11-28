library(shiny)
library(tidyverse)
library(bslib)
library(bsicons)
library(leaflet)

# placeholder for data loading; more will be added
city_stations <- readRDS("data/city_stations.rds")
climate_annual <- readRDS("data/climate_annual.rds")
climate_joined <- readRDS("data/climate_joined.rds")

#seasonal means for the Satellite vs. Station tab (Prashant)
climate_seasonal <- climate_joined |>
  mutate(
    month_num = lubridate::month(date),
    season = case_when(
      month_num %in% c(12, 1, 2) ~ "Winter",
      month_num %in% c(3, 4, 5)  ~ "Spring",
      month_num %in% c(6, 7, 8)  ~ "Summer",
      TRUE                       ~ "Fall"
    ),
    season = factor(season, levels = c("Winter", "Spring", "Summer", "Fall"))
  ) |>
  group_by(GEOID, city, state, year, season) |>
  summarise(
    noaa_mean = mean(noaa_mean, na.rm = TRUE),
    noaa_max  = mean(noaa_max,  na.rm = TRUE),
    noaa_min  = mean(noaa_min,  na.rm = TRUE),
    nasa_mean = mean(nasa_mean, na.rm = TRUE),
    nasa_max  = mean(nasa_max,  na.rm = TRUE),
    nasa_min  = mean(nasa_min,  na.rm = TRUE),
    .groups = "drop"
  )

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
  
  #----------------------Explore Tab------------- # 
  
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
               checkboxInput("smooth", "Show trend line", value = FALSE),
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
                   leafletOutput("map", height = "300px")
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
                      layout_sidebar(
                        class = "mt-3",
                        
                        # left side: controls for trend analysis
                        sidebar = sidebar(
                          title = "Choose your trend",
                          width = 280,
                          open = "always",
                          selectInput("tr_city", strong("City"),
                                      choices = city_choices,
                                      selected = "92242"),
                          radioButtons("tr_source", strong("Data source"),
                                       choices = c("NOAA weather station", "NASA POWER (satellite)")),
                          selectInput("tr_variable", strong("Variable"),
                                      choices = c("Mean temperature (°C)",
                                                  "Max temperature (°C)",
                                                  "Min temperature (°C)")),
                          layout_columns(
                            numericInput("tr_year_from", "From", value = 1990, min = 1990, max = 2026),
                            numericInput("tr_year_to", "To", value = 2026, min = 1990, max = 2026)
                          )
                        ),
                        
                        # right side: result boxes, diagnostic plots, and interpretation
                        layout_columns(
                          result_box("Slope (°C per decade)", "tr_slope"),
                          result_box("95% confidence interval", "tr_ci"),
                          result_box("p-value", "tr_p"),
                          result_box("R²", "tr_r2")
                        ),
                        card(card_header("Annual values with fitted trend: Brook (Role 1)"),
                             p(class = "small text-muted mb-0", "Linear regression of temperature over time for the selected city."),
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
                      )
             ),
             
             # ---------------- SATELLITE VS. STATION: Prashant (Role 2) ----------------
tabPanel("Satellite vs. Station",
         layout_sidebar(
           class = "mt-3",
           sidebar = sidebar(
             title = "Choose comparison",
             width = 280,
             open = "always",
             selectInput("sv_city", strong("City"),
                         choices = c("All 48 cities" = "all", city_choices),
                         selected = "all"),
             selectInput("sv_variable", strong("Variable"),
                         choices = c("Mean temperature (°C)" = "mean",
                                     "Max temperature (°C)"  = "max",
                                     "Min temperature (°C)"  = "min")),
             layout_columns(
               numericInput("sv_year_from", "From", value = 1990, min = 1990, max = 2026),
               numericInput("sv_year_to", "To", value = 2026, min = 1990, max = 2026)
             )
           ),
           layout_columns(
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
         )
),
             
             # ---------------- DECADES AND REGIONS: Rachel (Role 3) ----------------
             tabPanel("Decades and Regions",
                      layout_sidebar(
                        class = "mt-3", #space below the pills
                        
                        #left side: controls
                        sidebar = sidebar(
                          title = "Choose your test",
                          width = 280,
                          open = "always",
                          radioButtons("dr_group", strong("Compare by"),
                                       choices = c("Decade" = "decade", "Region (2020s)" = "region")),
                          radioButtons("dr_source", strong("Data source"),
                                       choices = c("NOAA station" = "noaa", "NASA POWER" = "nasa")),
                          selectInput("dr_variable", strong("Variable"),
                                      choices = c("Mean temperature" = "mean",
                                                  "Max temperature" = "max",
                                                  "Min temperature" = "min",
                                                  "Heat-wave days" = "heatwave_days",
                                                  "Cooling degree days" = "cdd",
                                                  "Heating degree days" = "hdd")),
                          radioButtons("dr_test", strong("Test"),
                                       choices = c("ANOVA + Tukey" = "anova",
                                                   "Welch + Games-Howell" = "welch",
                                                   "Kruskal-Wallis + Wilcoxon" = "kruskal")),
                          p(class = "small text-muted mb-0",
                            "Values are differences from each city's 1991 to 2020 average.")
                        ),
                        
                        #right side: results
                        layout_columns(
                          result_box("Test statistic", "dr_f"),
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
                      )
             ),
             
             # ---------------- COMPARE TWO PERIODS: Brook (Role 1) ----------------
             tabPanel("Compare Two Periods",
                      layout_sidebar(
                        class = "mt-3",
                        
                        # left side: controls for two-period comparison
                        sidebar = sidebar(
                          title = "Choose two periods",
                          width = 280,
                          open = "always",
                          selectInput("cp_city", strong("City"),
                                      choices = city_choices,
                                      selected = "92242"),
                          radioButtons("cp_source", strong("Data source"),
                                       choices = c("NOAA weather station", "NASA POWER (satellite)")),
                          selectInput("cp_variable", strong("Variable"),
                                      choices = c("Mean temperature (°C)",
                                                  "Max temperature (°C)",
                                                  "Min temperature (°C)")),
                          p(strong("Period A (earlier)")),
                          layout_columns(
                            numericInput("cp_a_from", "From", value = 1990, min = 1990, max = 2026),
                            numericInput("cp_a_to", "To", value = 2005, min = 1990, max = 2026)
                          ),
                          p(strong("Period B (later)")),
                          layout_columns(
                            numericInput("cp_b_from", "From", value = 2010, min = 1990, max = 2026),
                            numericInput("cp_b_to", "To", value = 2026, min = 1990, max = 2026)
                          )
                        ),
                        
                        # right side: result boxes, distribution plot, and interpretation
                        layout_columns(
                          result_box("Period A mean", "cp_a_mean"),
                          result_box("Period B mean", "cp_b_mean"),
                          result_box("Difference (B minus A)", "cp_diff"),
                          result_box("p-value", "cp_p")
                        ),
                        card(card_header("Distribution of annual values in each period: Brook (Role 1)"),
                             p(class = "small text-muted mb-0", "Two-period comparison using Welch's two-sample t-test."),
                             plotOutput("cp_dist", height = "300px")),
                        card(card_header("What this means: Brook (Role 1)"),
                             textOutput("cp_text"))
                      )
             )
           )
  )
  
)

server <- function(input, output, session) {
  
  # Helper to map Explore variable label to column suffix
  explore_var_col <- function(source_prefix, var_label) {
    suffix <- switch(var_label,
      "Mean temperature (°C)" = "mean",
      "Max temperature (°C)"  = "max",
      "Min temperature (°C)"  = "min",
      "Precipitation (mm)"    = "precip"
    )
    paste0(source_prefix, "_", suffix)
  }

  # ---------------- EXPLORE MAP & SUMMARY (Prashant) ----------------
  explore_city_stats <- reactive({
    validate(need(length(input$cities) > 0, "Please choose at least one city."))
    validate(need(input$year_from <= input$year_to, "'From' year must be <= 'To' year."))
    validate(need(
      !(input$variable == "Precipitation (mm)" && input$source != "NOAA weather station"),
      "Precipitation is only available from NOAA weather stations."
    ))
    
    df_yr <- climate_annual |>
      filter(year >= input$year_from, year <= input$year_to)
    
    noaa_col <- explore_var_col("noaa", input$variable)
    nasa_col <- if (input$variable == "Precipitation (mm)") NULL else explore_var_col("nasa", input$variable)
    
    # Aggregate all 48 cities for map coloring (complete years only)
    stats <- df_yr |>
      group_by(GEOID) |>
      summarise(
        noaa_val = mean(if_else(noaa_days >= 330, .data[[noaa_col]], NA_real_), na.rm = TRUE),
        nasa_val = if (!is.null(nasa_col)) mean(if_else(nasa_days >= 330, .data[[nasa_col]],
                                                        NA_real_), na.rm = TRUE) else NA_real_,
        .groups = "drop"
      ) |>
      mutate(
        display_val = case_when(
          input$source == "NASA POWER (satellite)" ~ nasa_val,
          input$source == "NOAA weather station"   ~ noaa_val,
          TRUE ~ (noaa_val + nasa_val) / 2
        )
      )
    
    city_stations |>
      left_join(stats, by = "GEOID") |>
      mutate(is_selected = GEOID %in% input$cities)
  })

  output$map <- renderLeaflet({
    df <- explore_city_stats()
    pal_name <- if (input$variable == "Precipitation (mm)") "Blues" else "YlOrRd"
    pal <- colorNumeric(palette = pal_name, domain = df$display_val, na.color = "#CCCCCC")

    popups <- sprintf(
      "<strong>%s, %s</strong> (%s)<br/>Station: %s (%.1f km)<br/>%s: <strong>%.2f</strong>",
      df$CITY, df$STATE, df$REGION, df$station_name, df$dist_km, input$variable, df$display_val
    )

    leaflet(df) |>
      addProviderTiles(providers$Esri.WorldGrayCanvas) |> #free light basemap, no API key needed
      setView(lng = -96, lat = 38, zoom = 3) |> #start centered on U.S.
      addCircleMarkers(
        data = filter(df, !is_selected),
        lng = ~LON, lat = ~LAT,
        radius = 5, stroke = TRUE, weight = 1, color = "#555555",
        fillColor = ~pal(display_val), fillOpacity = 0.6,
        popup = popups[!df$is_selected],
        label = ~paste0(CITY, ", ", STATE)
      ) |>
      addCircleMarkers(
        data = filter(df, is_selected),
        lng = ~LON, lat = ~LAT,
        radius = 9, stroke = TRUE, weight = 2.5, color = "#111111",
        fillColor = ~pal(display_val), fillOpacity = 0.95,
        popup = popups[df$is_selected],
        label = ~paste0(CITY, ", ", STATE, " (Selected)")
      ) |>
      addLegend(
        position = "bottomleft", pal = pal, values = ~display_val,
        title = input$variable, opacity = 0.85
      )
  })

  output$summary <- renderTable({
    validate(need(length(input$cities) > 0, "Please choose at least one city."))
    validate(need(input$year_from <= input$year_to, "'From' year must be <= 'To' year."))
    validate(need(
      !(input$variable == "Precipitation (mm)" && input$source != "NOAA weather station"),
      "Precipitation is only available from NOAA weather stations."
    ))

    df_sel <- climate_annual |>
      filter(GEOID %in% input$cities, year >= input$year_from, year <= input$year_to)

    calc_summary <- function(dat, col_name, src_label) {
      days_col <- paste0(sub("_.*", "", col_name), "_days") #noaa_days or nasa_days
      dat |>
        filter(!is.na(.data[[col_name]]), .data[[days_col]] >= 330) |> #complete years only
        group_by(City = paste0(city, ", ", state)) |>
        summarise(
          Source = src_label,
          Years  = n(),
          Mean   = round(mean(.data[[col_name]]), 2),
          SD     = round(sd(.data[[col_name]]), 2),
          Min    = round(min(.data[[col_name]]), 2),
          Max    = round(max(.data[[col_name]]), 2),
          `Trend (/decade)` = if (n() >= 2) {
            round(coef(lm(.data[[col_name]] ~ year))[2] * 10, 2)
          } else {
            NA_real_
          },
          .groups = "drop"
        )
    }

    if (input$source == "NOAA weather station") {
      calc_summary(df_sel, explore_var_col("noaa", input$variable), "NOAA Station")
    } else if (input$source == "NASA POWER (satellite)") {
      calc_summary(df_sel, explore_var_col("nasa", input$variable), "NASA POWER")
    } else {
      bind_rows(
        calc_summary(df_sel, explore_var_col("noaa", input$variable), "NOAA Station"),
        calc_summary(df_sel, explore_var_col("nasa", input$variable), "NASA POWER")
      ) |> arrange(City, Source)
    }
  })

  # ---------------- SATELLITE VS. STATION (Prashant) ----------------
  sv_data <- reactive({
    validate(need(input$sv_year_from <= input$sv_year_to, "'From' year must be <= 'To' year."))
    
    noaa_col <- paste0("noaa_", input$sv_variable)
    nasa_col <- paste0("nasa_", input$sv_variable)
    
    df <- climate_annual |>
      filter(year >= input$sv_year_from, year <= input$sv_year_to,
             noaa_days >= 330, nasa_days >= 330) #complete years only
    if (input$sv_city != "all") {
      df <- df |> filter(GEOID == input$sv_city)
    }
    
    df <- df |>
      transmute(
        GEOID, city, state, year,
        noaa = .data[[noaa_col]],
        nasa = .data[[nasa_col]],
        diff = nasa - noaa
      ) |>
      filter(!is.na(noaa), !is.na(nasa))
    
    validate(need(nrow(df) >= 3, "Not enough paired observations in this range."))
    df
  })

  sv_seasonal_data <- reactive({
    validate(need(input$sv_year_from <= input$sv_year_to, "'From' year must be <= 'To' year."))

    noaa_col <- paste0("noaa_", input$sv_variable)
    nasa_col <- paste0("nasa_", input$sv_variable)

    df <- climate_seasonal |>
      filter(year >= input$sv_year_from, year <= input$sv_year_to)
    if (input$sv_city != "all") {
      df <- df |> filter(GEOID == input$sv_city)
    }

    df |>
      transmute(
        season,
        diff = .data[[nasa_col]] - .data[[noaa_col]]
      ) |>
      filter(!is.na(diff))
  })

  sv_stats <- reactive({
    df <- sv_data()
    fit <- lm(noaa ~ nasa, data = df)
    tt  <- t.test(df$nasa, df$noaa, paired = TRUE)
    list(
      fit   = fit,
      bias  = mean(df$diff),
      p_val = tt$p.value,
      slope = unname(coef(fit)[2]),
      rmse  = sqrt(mean(df$diff^2))
    )
  })

  output$sv_bias  <- renderText(sprintf("%+.2f °C", sv_stats()$bias))
  output$sv_p     <- renderText({ p <- sv_stats()$p_val; if (p < 0.001) "< 0.001" else sprintf("%.3f", p) })
  output$sv_slope <- renderText(sprintf("%.3f", sv_stats()$slope))
  output$sv_rmse  <- renderText(sprintf("%.2f °C", sv_stats()$rmse))

  output$sv_scatter <- renderPlot({
    df <- sv_data()
    ggplot(df, aes(x = nasa, y = noaa)) +
      geom_point(alpha = 0.55, color = "#2166AC") +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.8) +
      geom_smooth(method = "lm", formula = y ~ x, se = TRUE, color = "#D95F02", linewidth = 0.9) +
      labs(
        x = "NASA POWER (°C)",
        y = "NOAA Station (°C)",
        caption = "Dashed line = 1:1 agreement; Orange line = linear regression fit"
      ) +
      theme_minimal(base_size = 13)
  })

  output$sv_resid <- renderPlot({
    df <- sv_seasonal_data()
    ggplot(df, aes(x = season, y = diff, fill = season)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
      geom_boxplot(alpha = 0.75, outlier.alpha = 0.35) +
      scale_fill_brewer(palette = "Set2") +
      labs(
        x = "Season",
        y = "POWER minus Station (°C)"
      ) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "none")
  })

  output$sv_text <- renderText({
    s <- sv_stats()
    seas <- sv_seasonal_data() |>
      group_by(season) |>
      summarise(mean_diff = mean(diff), .groups = "drop") |>
      slice_max(abs(mean_diff), n = 1)

    city_label <- if (input$sv_city == "all") "all 48 cities" else names(city_choices)[city_choices == input$sv_city]
    var_label  <- switch(input$sv_variable,
                         "mean" = "mean temperature",
                         "max"  = "max temperature",
                         "min"  = "min temperature")
    direction  <- if (s$bias > 0) "warmer" else "cooler"
    sig_phrase <- if (s$p_val < 0.05) "statistically significant" else "not statistically significant"

    sprintf(
      "For %s (%s, %d–%d), NASA POWER is on average %.2f °C %s than NOAA ground stations (paired t-test p %s, %s). The regression slope of %.3f and RMSE of %.2f °C show how closely satellite estimates track station readings, with the largest average seasonal gap in %s (%+.2f °C).",
      city_label, var_label, input$sv_year_from, input$sv_year_to,
      abs(s$bias), direction,
      if (s$p_val < 0.001) "< 0.001" else paste("=", sprintf("%.3f", s$p_val)),
      sig_phrase, s$slope, s$rmse, seas$season, seas$mean_diff
    )
  })
  
  #NASA precip isn't in our dta, so precipitation always uses NOAA
  observeEvent(input$variable, {
    if (input$variable == "Precipitation (mm)") {
      updateRadioButtons(session, "source", selected = "NOAA weather station")
    }
  })
  
  # ---------------- TIME SERIES (Brook) ----------------
  ts_data <- reactive({
    req(input$source, input$variable, input$year_from, input$year_to)
    prepare_explore_ts_data(
      annual        = climate_annual,
      city_ids      = input$cities,
      source_choice = input$source,
      var_label     = input$variable,
      year_from     = input$year_from,
      year_to       = input$year_to
    )
  })
  
  output$ts_plot <- renderPlot({
    validate(need(
      !(input$variable == "Precipitation (mm)" && input$source != "NOAA weather station"),
      "Precipitation is only available from NOAA weather stations. Choose \"NOAA weather station\" above."
    ))
    #Brook's existing call, unchanged
    p <- plot_explore_ts(
      ts_df     = ts_data(),
      var_label = input$variable
    )
    
    #color each city differently; facets already name the city
    if (input$source == "Both, overlaid") {
      #two sources: color by source, so NASA and NOAA are easy to tell apart
      p <- p + 
        aes(color = source) +
        scale_color_manual(values = c("NASA POWER (satellite)" = "#1B9E77",
                                      "NOAA weather station"   = "#D95F02"),
                           name = "Data source")
    } else {
      #one source: color each city differently; facets already name the city
      p <- p + 
        aes(color = city_label) +
        scale_color_brewer(palette = "Dark2") +
        guides(color = "none")
    }
    
    #add a dashed trend line when the box is checked
    if (isTRUE(input$smooth)) {
      if (input$source == "Both, overlaid") {
        #dashed trend in each source's color
        p <- p + geom_smooth(method = "lm", formula = y ~ x, se = FALSE,
                             linewidth = 0.8, linetype = "dashed")
      } else {
        p <- p + geom_smooth(method = "lm", formula = y ~ x, se = FALSE,
                             linewidth = 0.8, linetype = "dashed", color = "black")
      }
    }
    #prints the chart
    p
    
  })
  
  # ---------------- TRENDS (Brook) ----------------
  tr_data <- reactive({
    req(input$tr_city, input$tr_source, input$tr_variable, input$tr_year_from, input$tr_year_to)
    prepare_trends_data(
      annual        = climate_annual,
      city_id       = input$tr_city,
      source_choice = input$tr_source,
      var_label     = input$tr_variable,
      year_from     = input$tr_year_from,
      year_to       = input$tr_year_to
    )
  })
  
  tr_result <- reactive({
    fit_trends_model(tr_data())
  })
  
  format_tr_p <- function(p) {
    if (p < 0.001) "< 0.001" else sprintf("%.3f", p)
  }
  
  output$tr_slope <- renderText({
    sprintf("%+.2f", tr_result()$slope_dec)
  })
  
  output$tr_ci <- renderText({
    r <- tr_result()
    paste0("[", sprintf("%+.2f", r$ci_low), ", ", sprintf("%+.2f", r$ci_high), "]")
  })
  
  output$tr_p <- renderText({
    format_tr_p(tr_result()$p_value)
  })
  
  output$tr_r2 <- renderText({
    sprintf("%.2f", tr_result()$r_squared)
  })
  
  output$tr_fit <- renderPlot({
    plot_trends_fit(tr_result(), input$tr_variable)
  })
  
  output$tr_resid <- renderPlot({
    plot_trends_resid(tr_result())
  })
  
  output$tr_qq <- renderPlot({
    plot_trends_qq(tr_result())
  })
  
  output$tr_text <- renderText({
    describe_trends_result(tr_result(), input$tr_variable)
  })
  
  # ---------------- DECADES AND REGIONS (Rachel) ----------------
  dr_data <- reactive({
    prepare_group_data(climate_annual, input$dr_group,
                       paste0(input$dr_source, "_", input$dr_variable))
  })
  
  dr_result <- reactive(run_group_test(dr_data(), input$dr_test))
  
  #show p-values as "< 0.001" when they're tiny
  format_p <- function(p) if (p < 0.001) "< 0.001" else sprintf("%.3f", p)
  
  output$dr_f      <- renderText(sprintf("%.2f", dr_result()$statistic))
  output$dr_p      <- renderText(format_p(dr_result()$p_value))
  output$dr_groups <- renderText(dr_result()$n_groups)
  output$dr_pairs  <- renderText(paste(dr_result()$n_sig_pairs, "of", dr_result()$n_pairs))
  
  output$dr_means <- renderPlot(plot_group_means(dr_data()))
  
  output$dr_checks <- renderUI({
    r <- dr_result()
    tagList(
      p("Normality of residuals (Shapiro-Wilk): p =", format_p(r$shapiro_p),
        if (r$shapiro_p < 0.05) "(fails)" else "(passes)"),
      p("Equal spreads (Levene): p =", format_p(r$levene_p),
        if (r$levene_p < 0.05) "(fails)" else "(passes)"),
      p(class = "small text-muted mb-0",
        if (r$shapiro_p < 0.05) "Residuals aren't normal, so Kruskal-Wallis is the safest choice."
        else if (r$levene_p < 0.05) "Spreads differ, so Welch's ANOVA is the best choice."
        else "Both checks pass, so standard ANOVA is appropriate.")
    )
  })
  
  output$dr_anova <- renderTable({
    r <- dr_result()
    tibble(Test = r$stat_label, Statistic = round(r$statistic, 2), `p-value` = format_p(r$p_value))
  })
  
  output$dr_posthoc <- renderTable({
    dr_result()$posthoc |>
      mutate(difference = round(difference, 2),
             p_adj = sapply(p_adj, format_p)) |>
      rename(Comparison = comparison, `Difference (°C or days)` = difference, `Adjusted p` = p_adj)
  })
  
  output$dr_text <- renderText(describe_group_result(dr_result(), input$dr_group))
  
  # ---------------- COMPARE TWO PERIODS (Brook) ----------------
  cp_data <- reactive({
    req(
      input$cp_city, input$cp_source, input$cp_variable,
      input$cp_a_from, input$cp_a_to, input$cp_b_from, input$cp_b_to
    )
    prepare_cp_data(
      annual        = climate_annual,
      city_id       = input$cp_city,
      source_choice = input$cp_source,
      var_label     = input$cp_variable,
      a_from        = input$cp_a_from,
      a_to          = input$cp_a_to,
      b_from        = input$cp_b_from,
      b_to          = input$cp_b_to
    )
  })
  
  cp_result <- reactive({
    run_cp_test(cp_data())
  })
  
  format_cp_p <- function(p) {
    if (p < 0.001) "< 0.001" else sprintf("%.3f", p)
  }
  
  output$cp_a_mean <- renderText({
    sprintf("%.2f", cp_result()$mean_a)
  })
  
  output$cp_b_mean <- renderText({
    sprintf("%.2f", cp_result()$mean_b)
  })
  
  output$cp_diff <- renderText({
    sprintf("%+.2f", cp_result()$diff_ba)
  })
  
  output$cp_p <- renderText({
    format_cp_p(cp_result()$p_value)
  })
  
  output$cp_dist <- renderPlot({
    plot_cp_dist(cp_result(), input$cp_variable)
  })
  
  output$cp_text <- renderText({
    describe_cp_result(cp_result(), input$cp_variable)
  })
  #every Explore output should start with:
  #validate(need(length(input$cities) > 0, "Please choose at least one city."))
}

shinyApp(ui = ui, server = server)
