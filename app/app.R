library(shiny)
library(tidyverse)
library(bslib)
library(bsicons)

# placeholder for data loading; more will be added
city_stations <- readRDS("data/city_stations.rds")
climate_annual <- readRDS("data/climate_annual.rds")

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
                   paste0("Coverage: 48 cities from across the United States, January 1990 to October 2026. Date pulled ",
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
  
  #Explore tab placeholders (replace with real outputs)
  output$map     <- renderPlot(plot.new()) #Prashant
  output$summary <- renderTable(data.frame(Note = "Summary table coming soon")) #Prashant
  
  #Statistics result box placeholders (each owner replaces theirs with real results)
  result_ids <- c(
                  "sv_bias", "sv_p", "sv_slope", "sv_rmse" #Prashant
                  #Rachel's done
                 ) #Brook's Done
  for (id in result_ids) {
    output[[id]] <- renderText("--")
  }
  
  #Statistics chart placeholders (each owner replaces theirs with real plots)
  plot_ids <- c(  #Brook Done
                "sv_scatter", "sv_resid"       #Prashant
                #Rachel done
                )                     
  for (id in plot_ids) {
    output[[id]] <- renderPlot(plot.new())
  }
  
  #"What this means" placeholders
  
  text_ids <- c("sv_text")#Prashant
  for (id in text_ids) {
    output[[id]] <- renderText("Plain-language interpretation coming soon.")
  }
  
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