# =============================================================================
# Strait of Hormuz Energy Shock Explorer  --  companion Shiny app
# Author : Sekibenga Ivan Innocent
# Run    : shiny::runApp("app")   (from the project root)
# Data   : the same ten CSV files and the same cleaning code as Portfolio.Rmd
# =============================================================================

library(shiny)     # web application framework
library(bslib)     # modern Bootstrap theming
library(plotly)    # interactive charts
library(DT)        # interactive tables
library(dplyr)     # data wrangling
library(tidyr)     # reshaping
library(ggplot2)   # static plots (converted to interactive with plotly)
library(stringr)   # text helpers
library(forcats)   # factor ordering
library(scales)    # axis formats

# ---- Locate the shared helpers and data (works locally and when deployed) ----
root <- if (file.exists("R/helpers.R")) "." else if (file.exists("../R/helpers.R")) ".." else
  stop("Cannot find R/helpers.R. Run the app from the project root or copy R/ and data/ next to app.R.")
source(file.path(root, "R", "helpers.R"))                 # same cleaning code as the report
dat <- load_clean_data(file.path(root, "data"))           # all cleaned datasets
crude <- dat$crude; fuel <- dat$fuel; country <- dat$country; petrol <- dat$petrol
petrol_long <- dat$petrol_long; proscons <- dat$proscons; timeline <- dat$timeline; voy <- dat$voy

# ---- Pre-computed quantities used in several tabs ---------------------------
pre_base   <- mean(crude$Brent_USD[crude$Phase == "Pre-Conflict"])      # pre-war Brent average
brent_last <- crude$Brent_USD[which.max(crude$Date)]                    # latest Brent
brent_peak <- max(crude$Brent_USD)
last_pct   <- (brent_last / pre_base - 1) * 100                         # crude rise to last date
peak_pct   <- (brent_peak / pre_base - 1) * 100

# Pump-price pass-through and the elasticity used by the what-if calculator
petrol <- petrol %>% mutate(
  Pass_Through = Pct_Since_War_Calc / last_pct,
  Pass_Class = factor(case_when(Pass_Through >= 0.8 ~ "Near-full pass-through",
                                Pass_Through >= 0.3 ~ "Partial pass-through",
                                TRUE ~ "Shielded / administered"),
                      levels = c("Near-full pass-through", "Partial pass-through", "Shielded / administered")),
  Elasticity = log(Jul13_Price / Before_War_Price) / log(brent_last / pre_base))  # % pump change per % crude change

# Monthly history with the 12-month-forward change (for spike analysis)
fuel_lead <- fuel %>% mutate(Price_12m_Later = lead(Price, 12),
                             Change_12m_After_Pct = (Price_12m_Later / Price - 1) * 100)

# Standardised country measures for the adjustable Stress Index
z <- function(x) as.numeric(scale(x))
country <- country %>% mutate(Z_Import = z(Oil_Import_Pct), Z_GDP = z(-GDP_Impact_Pct),
                              Z_Stock = z(-Stock_Market_Change), Z_Infl = z(Inflation_Score), Z_Curr = z(Currency_Score))

# Latest-30-day trade metrics for the overview cards
voy_latest <- voy %>% group_by(Series) %>%
  summarise(Latest30 = mean(Pct_Baseline[Date > max(Date) - 30]), .groups = "drop")
last_flow_date <- max(voy$Date)

# Small UI helper: a metric card
metric_card <- function(title, value, sub, colour = "#0B2545") {
  div(class = "card shadow-sm h-100", style = paste0("border-left:6px solid ", colour, ";"),
      div(class = "card-body",
          div(class = "text-muted small", title),
          div(style = "font-size:1.7rem;font-weight:700;", value),
          div(class = "small", sub)))
}

# =============================================================================
# USER INTERFACE
# =============================================================================
ui <- navbarPage(
  title = "Hormuz Energy Shock Explorer",
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly", primary = "#0B2545"),

  # ---- Tab 1: Overview -------------------------------------------------------
  tabPanel("Overview",
    br(),
    h3("How a war moved oil, pumps, economies and ships"),
    p("An interactive companion to the portfolio report by Sekibenga Ivan Innocent. Use the tabs above to explore each dataset."),
    fluidRow(
      column(3, metric_card("Brent crude, peak vs pre-war", paste0("+", round(peak_pct, 1), "%"),
                            paste0("USD ", round(pre_base, 2), " to USD ", round(brent_peak, 2)), "#E4572E")),
      column(3, metric_card("Largest monthly oil jump, March 2026",
                            paste0("+", round(fuel$Pct_Chg[fuel$Date == max(fuel$Date)], 1), "%"),
                            "Among the biggest in 56 years", "#6A4C93")),
      column(3, metric_card("Median pump-price rise (14 countries)", paste0(round(median(petrol$Pct_Since_War_Calc), 1), "%"),
                            "Range 0% to 77%", "#F4B400")),
      column(3, metric_card("Crude export traffic, last 30 days",
                            paste0(round(voy_latest$Latest30[voy_latest$Series == "Crude oil exports"], 1), "% of normal"),
                            paste0("To ", format(last_flow_date, "%d %b %Y")), "#13A89E"))
    ),
    br(),
    fluidRow(column(12, plotlyOutput("ov_plot", height = "420px"))),
    p(class = "text-muted small",
      "Brent price is indexed to its pre-war average (= 100); crude-export traffic is indexed to its own pre-war normal (= 100). ",
      "Price and timeline data end 13 Jul 2026; the trade tracker runs to 19 Nov 2026.")
  ),

  # ---- Tab 2: Oil price explorer --------------------------------------------
  tabPanel("Oil Price Explorer",
    sidebarLayout(
      sidebarPanel(width = 3,
        sliderInput("oil_dates", "Date range", min = min(crude$Date), max = max(crude$Date),
                    value = c(min(crude$Date), max(crude$Date)), timeFormat = "%d %b"),
        checkboxGroupInput("oil_phases", "Conflict phases", choices = PHASE_LEVELS, selected = PHASE_LEVELS),
        checkboxGroupInput("oil_bench", "Benchmarks", choices = c("Brent", "WTI"), selected = c("Brent", "WTI")),
        checkboxInput("oil_events", "Show war-timeline events as vertical lines", FALSE),
        downloadButton("oil_dl", "Download filtered data (CSV)")
      ),
      mainPanel(width = 9,
        plotlyOutput("oil_plot", height = "420px"),
        h5("Summary by phase (filtered)"),
        DTOutput("oil_table")
      )
    )
  ),

  # ---- Tab 3: Historical shocks ---------------------------------------------
  tabPanel("Historical Shocks",
    sidebarLayout(
      sidebarPanel(width = 3,
        sliderInput("hist_years", "Years", min = min(fuel$Year), max = max(fuel$Year),
                    value = c(min(fuel$Year), max(fuel$Year)), sep = ""),
        sliderInput("hist_thr", "Highlight months with a rise of at least (%)", min = 10, max = 60, value = 25, step = 5),
        checkboxInput("hist_log", "Log scale", TRUE),
        uiOutput("hist_rarity")
      ),
      mainPanel(width = 9,
        plotlyOutput("hist_plot", height = "420px"),
        h5("Spike months and what happened 12 months later"),
        DTOutput("hist_table")
      )
    )
  ),

  # ---- Tab 4: Pump prices + what-if -----------------------------------------
  tabPanel("Pump Prices",
    tabsetPanel(
      tabPanel("Compare countries",
        br(),
        sidebarLayout(
          sidebarPanel(width = 3,
            checkboxGroupInput("pp_region", "Regions", choices = sort(unique(petrol$Region)), selected = sort(unique(petrol$Region))),
            radioButtons("pp_metric", "Metric",
                         choices = c("Change since war (%)" = "pct", "Price on 13 Jul (USD per litre)" = "usd",
                                     "Pass-through of crude rise" = "pt", "Extra cost of a 50-litre tank (USD)" = "tank")),
            selectizeInput("pp_countries", "Countries for the index chart", choices = sort(petrol$Country), multiple = TRUE,
                           selected = c("Pakistan", "India", "Sri Lanka", "USA", "UAE", "Germany"))
          ),
          mainPanel(width = 9,
            plotlyOutput("pp_bar", height = "380px"),
            h5("Price index (before war = 100)"),
            plotlyOutput("pp_lines", height = "340px")
          )
        )
      ),
      tabPanel("What-if calculator",
        br(),
        sidebarLayout(
          sidebarPanel(width = 3,
            selectInput("wi_country", "Country", choices = sort(petrol$Country), selected = "Pakistan"),
            sliderInput("wi_brent", "Brent price (USD per barrel)", min = 50, max = 180, value = round(brent_last), step = 1),
            helpText("The calculator applies each country's observed pass-through (how much its pump price moved when Brent moved from the pre-war average to the 13 July level) to the Brent price you choose. It is illustrative, not a forecast, and assumes the same pass-through applies when prices fall.")
          ),
          mainPanel(width = 9,
            fluidRow(column(4, uiOutput("wi_local")), column(4, uiOutput("wi_usd")), column(4, uiOutput("wi_tank"))),
            br(), plotlyOutput("wi_plot", height = "340px")
          )
        )
      )
    )
  ),

  # ---- Tab 5: Country risk ---------------------------------------------------
  tabPanel("Country Risk",
    tabsetPanel(
      tabPanel("Explore relationships",
        br(),
        sidebarLayout(
          sidebarPanel(width = 3,
            selectInput("cr_x", "X axis", selected = "Oil_Import_Pct",
                        choices = c("Oil import share (%)" = "Oil_Import_Pct", "Stock market change (%)" = "Stock_Market_Change",
                                    "Inflation risk score (1-5)" = "Inflation_Score", "Currency pressure score (1-5)" = "Currency_Score",
                                    "Population (millions)" = "Population_M")),
            selectInput("cr_y", "Y axis", selected = "GDP_Impact_Pct",
                        choices = c("GDP impact (pp)" = "GDP_Impact_Pct", "Stock market change (%)" = "Stock_Market_Change",
                                    "Inflation risk score (1-5)" = "Inflation_Score", "Currency pressure score (1-5)" = "Currency_Score")),
            checkboxGroupInput("cr_region", "Regions", choices = sort(unique(country$Region)), selected = sort(unique(country$Region))),
            checkboxInput("cr_noiran", "Exclude Iran (war party / outlier)", TRUE),
            checkboxInput("cr_line", "Show fitted straight line", TRUE),
            uiOutput("cr_stat")
          ),
          mainPanel(width = 9, plotlyOutput("cr_plot", height = "460px"))
        )
      ),
      tabPanel("Build your own Stress Index",
        br(),
        sidebarLayout(
          sidebarPanel(width = 3,
            helpText("Give each measure a weight (0 = ignore). The index is the weighted average of standardised measures; higher = more exposed."),
            sliderInput("w_import", "Oil import dependence", 0, 3, 1, step = 0.5),
            sliderInput("w_gdp", "GDP loss", 0, 3, 1, step = 0.5),
            sliderInput("w_stock", "Stock market fall", 0, 3, 1, step = 0.5),
            sliderInput("w_infl", "Inflation risk", 0, 3, 1, step = 0.5),
            sliderInput("w_curr", "Currency pressure", 0, 3, 1, step = 0.5)
          ),
          mainPanel(width = 9, plotlyOutput("cr_stress", height = "520px"))
        )
      ),
      tabPanel("Country table",
        br(), DTOutput("cr_table"))
    )
  ),

  # ---- Tab 6: Hormuz trade tracker ------------------------------------------
  tabPanel("Hormuz Trade Tracker",
    sidebarLayout(
      sidebarPanel(width = 3,
        checkboxGroupInput("vy_series", "Flows", choices = levels(voy$Series), selected = levels(voy$Series)),
        sliderInput("vy_dates", "Date range", min = min(voy$Date), max = max(voy$Date),
                    value = c(as.Date("2025-10-01"), max(voy$Date)), timeFormat = "%b %Y"),
        radioButtons("vy_metric", "Measure",
                     choices = c("7-day average, % of normal" = "ma", "Daily, % of normal" = "daily", "7-day average, raw index" = "raw")),
        checkboxInput("vy_brent", "Overlay Brent (pre-war = 100) on the % chart", FALSE)
      ),
      mainPanel(width = 9,
        plotlyOutput("vy_plot", height = "400px"),
        h5("Average traffic in the selected window, as % of normal"),
        DTOutput("vy_table"),
        h5("Cumulative 'days of normal trade' lost since 28 Feb 2026"),
        plotlyOutput("vy_lost", height = "300px")
      )
    )
  ),

  # ---- Tab 7: Data and methods ----------------------------------------------
  tabPanel("Data & Methods",
    sidebarLayout(
      sidebarPanel(width = 3,
        selectInput("dm_set", "Cleaned dataset",
                    choices = c("Daily crude" = "crude", "Monthly crude 1970-2026" = "fuel", "Country impact" = "country",
                                "Pump prices" = "petrol", "Pros and cons" = "proscons", "War timeline" = "timeline",
                                "Trade-tracker indexes" = "voy")),
        downloadButton("dm_dl", "Download as CSV")
      ),
      mainPanel(width = 9,
        h5("Methods in brief"),
        tags$ul(
          tags$li("All files are cleaned by the same functions as the PDF report (R/helpers.R)."),
          tags$li("Trade-tracker flows are shown as a percentage of 'normal' = their average daily index from 1 Jan 2025 to 27 Feb 2026."),
          tags$li("Pump-price pass-through = pump-price rise divided by Brent's rise from the pre-war average to 13 Jul 2026."),
          tags$li("Stress Index = weighted average of five standardised measures (oil import share, GDP loss, stock fall, inflation risk, currency pressure)."),
          tags$li("Price, pump, country and timeline data end around 13 Jul 2026; the trade tracker runs to 19 Nov 2026."),
          tags$li("Patterns shown are associations in small samples, not proof of cause and effect.")),
        DTOutput("dm_table")
      )
    )
  )
)

# =============================================================================
# SERVER
# =============================================================================
server <- function(input, output, session) {

  # ---- Overview chart: Brent versus crude-export traffic --------------------
  output$ov_plot <- renderPlotly({
    cmp <- bind_rows(
      crude %>% transmute(Date, Value = Brent_USD / pre_base * 100, Line = "Brent price (pre-war = 100)"),
      voy %>% filter(Series == "Crude oil exports", Date >= min(crude$Date), Date <= max(crude$Date)) %>%
        transmute(Date, Value = MA7_Pct, Line = "Crude export traffic, 7-day avg (normal = 100)"))
    p <- ggplot(cmp, aes(Date, Value, colour = Line)) +
      geom_vline(xintercept = as.numeric(WAR_START), linetype = "dotted") +
      geom_line(linewidth = 1.1) +
      scale_colour_manual(values = c("Brent price (pre-war = 100)" = PAL[["coral"]],
                                     "Crude export traffic, 7-day avg (normal = 100)" = PAL[["navy"]])) +
      labs(x = NULL, y = "Index", colour = NULL, title = "As ship traffic vanished, the oil price climbed") +
      theme_portfolio()
    ggplotly(p) %>% layout(legend = list(orientation = "h", y = -0.15))
  })

  # ---- Oil price explorer ----------------------------------------------------
  oil_data <- reactive({
    req(input$oil_dates, input$oil_phases)
    crude %>% filter(Date >= input$oil_dates[1], Date <= input$oil_dates[2], Phase %in% input$oil_phases)
  })

  output$oil_plot <- renderPlotly({
    d <- oil_data(); req(nrow(d) > 0, input$oil_bench)
    long <- d %>% select(Date, Phase, Brent = Brent_USD, WTI = WTI_USD) %>%
      pivot_longer(c(Brent, WTI), names_to = "Benchmark", values_to = "USD") %>%
      filter(Benchmark %in% input$oil_bench)
    p <- ggplot(long, aes(Date, USD, colour = Benchmark, group = Benchmark)) +
      geom_hline(yintercept = pre_base, linetype = "dashed", colour = PAL[["grey"]]) +
      geom_line(linewidth = 1) + geom_point(aes(shape = Phase), size = 1.6) +
      scale_colour_manual(values = c(Brent = PAL[["navy"]], WTI = PAL[["coral"]])) +
      scale_y_continuous(labels = label_dollar()) +
      labs(x = NULL, y = "USD per barrel", title = "Daily crude prices (dashed = pre-war Brent average)") +
      theme_portfolio()
    if (isTRUE(input$oil_events)) {                          # optional event lines
      ev <- timeline %>% filter(Date >= input$oil_dates[1], Date <= input$oil_dates[2])
      if (nrow(ev) > 0) p <- p + geom_vline(xintercept = as.numeric(ev$Date), colour = PAL[["grey"]], alpha = 0.35)
    }
    ggplotly(p) %>% layout(legend = list(orientation = "h", y = -0.2))
  })

  output$oil_table <- renderDT({
    d <- oil_data(); req(nrow(d) > 0)
    tab <- d %>% group_by(Phase) %>%
      summarise(Days = n(), Mean_Brent = mean(Brent_USD), Max_Brent = max(Brent_USD),
                Daily_Move_SD_Pct = sd(Brent_Ret_Pct, na.rm = TRUE),
                Mean_vs_PreWar_Pct = (mean(Brent_USD) / pre_base - 1) * 100, .groups = "drop")
    datatable(tab, rownames = FALSE, options = list(dom = "t")) %>% formatRound(3:6, 2)
  })

  output$oil_dl <- downloadHandler(
    filename = function() "oil_prices_filtered.csv",
    content = function(file) write.csv(oil_data(), file, row.names = FALSE))

  # ---- Historical shocks -----------------------------------------------------
  hist_data <- reactive({
    fuel_lead %>% filter(Year >= input$hist_years[1], Year <= input$hist_years[2])
  })

  spike_data <- reactive({
    hist_data() %>% filter(!is.na(Pct_Chg), Pct_Chg >= input$hist_thr)
  })

  output$hist_plot <- renderPlotly({
    d <- hist_data(); sp <- spike_data(); req(nrow(d) > 0)
    p <- ggplot(d, aes(Date, Price)) +
      geom_area(fill = PAL[["teal"]], alpha = 0.2) +
      geom_line(colour = PAL[["navy"]]) +
      geom_point(data = sp, colour = PAL[["coral"]], size = 2.8) +
      scale_y_continuous(labels = label_dollar()) +
      labs(x = NULL, y = "USD per barrel (nominal)", title = "Monthly crude oil price; red = months above your threshold") +
      theme_portfolio()
    if (isTRUE(input$hist_log)) p <- p + scale_y_log10(labels = label_dollar())
    ggplotly(p)
  })

  output$hist_table <- renderDT({
    sp <- spike_data(); req(nrow(sp) > 0)
    tab <- sp %>% transmute(Month = format(Date, "%b %Y"), Price, Rise_Pct = Pct_Chg, Price_12m_Later, Change_12m_After_Pct)
    datatable(tab, rownames = FALSE, options = list(pageLength = 8)) %>% formatRound(2:5, 1)
  })

  output$hist_rarity <- renderUI({
    allm <- fuel %>% filter(!is.na(Pct_Chg))
    n_hit <- sum(allm$Pct_Chg >= input$hist_thr)
    mar <- fuel$Pct_Chg[fuel$Date == max(fuel$Date)]
    div(class = "alert alert-info",
        strong(n_hit), " of ", nrow(allm), " months since 1970 rose by at least ", input$hist_thr, "%. ",
        "March 2026 rose by ", strong(paste0(round(mar, 1), "%")), ".")
  })

  # ---- Pump prices: compare --------------------------------------------------
  pp_data <- reactive({
    req(input$pp_region)
    petrol %>% filter(Region %in% input$pp_region)
  })

  output$pp_bar <- renderPlotly({
    d <- pp_data(); req(nrow(d) > 0)
    d <- d %>% mutate(Value = case_when(input$pp_metric == "pct" ~ Pct_Since_War_Calc,
                                        input$pp_metric == "usd" ~ Jul13_USD,
                                        input$pp_metric == "pt"  ~ Pass_Through,
                                        TRUE ~ Extra_50L_USD))
    lab <- c(pct = "Change since war (%)", usd = "USD per litre on 13 Jul", pt = "Pass-through ratio", tank = "Extra cost per 50 L tank (USD)")[[input$pp_metric]]
    p <- ggplot(d, aes(Value, fct_reorder(Country, Value), fill = Pass_Class)) +
      geom_col(width = 0.75) +
      scale_fill_manual(values = c("Near-full pass-through" = PAL[["coral"]], "Partial pass-through" = PAL[["gold"]],
                                   "Shielded / administered" = PAL[["teal"]])) +
      labs(x = lab, y = NULL, fill = NULL) + theme_portfolio()
    ggplotly(p) %>% layout(legend = list(orientation = "h", y = -0.2))
  })

  output$pp_lines <- renderPlotly({
    req(input$pp_countries)
    d <- petrol_long %>% filter(Country %in% input$pp_countries)
    p <- ggplot(d, aes(Date, Index, colour = Country, group = Country)) +
      geom_line(linewidth = 1) + geom_point(size = 2) +
      labs(x = NULL, y = "Index, before war = 100", colour = NULL) + theme_portfolio()
    ggplotly(p)
  })

  # ---- Pump prices: what-if calculator ----------------------------------------
  wi_row <- reactive(petrol %>% filter(Country == input$wi_country))

  wi_calc <- reactive({
    r <- wi_row()
    ratio <- (input$wi_brent / pre_base) ^ r$Elasticity         # price multiplier implied by chosen Brent
    list(local = r$Before_War_Price * ratio, usd = r$Before_War_USD * ratio,
         chg = (ratio - 1) * 100, tank = 50 * r$Before_War_USD * (ratio - 1),
         unit = r$Unit, e = r$Elasticity)
  })

  output$wi_local <- renderUI({
    r <- wi_calc()
    metric_card("Predicted pump price", paste0(formatC(r$local, format = "f", digits = 2), " ", r$unit),
                paste0(round(r$chg, 1), "% versus before the war"), "#E4572E")
  })
  output$wi_usd <- renderUI({
    r <- wi_calc()
    metric_card("In US dollars per litre", paste0("USD ", formatC(r$usd, format = "f", digits = 2)),
                paste0("Pass-through elasticity ", round(r$e, 2)), "#13A89E")
  })
  output$wi_tank <- renderUI({
    r <- wi_calc()
    metric_card("Extra cost of a 50-litre tank", paste0("USD ", formatC(r$tank, format = "f", digits = 1)),
                "versus before the war", "#F4B400")
  })

  output$wi_plot <- renderPlotly({
    r <- wi_row()
    grid <- tibble(Brent = seq(50, 180, by = 2)) %>%
      mutate(Price_USD = r$Before_War_USD * (Brent / pre_base) ^ r$Elasticity)
    now <- tibble(Brent = input$wi_brent, Price_USD = r$Before_War_USD * (input$wi_brent / pre_base) ^ r$Elasticity)
    p <- ggplot(grid, aes(Brent, Price_USD)) +
      geom_line(colour = PAL[["navy"]], linewidth = 1.1) +
      geom_point(data = now, colour = PAL[["coral"]], size = 4) +
      geom_vline(xintercept = pre_base, linetype = "dashed", colour = PAL[["grey"]]) +
      labs(x = "Brent, USD per barrel (dashed = pre-war average)", y = "Pump price, USD per litre",
           title = paste("Implied pump price in", input$wi_country)) + theme_portfolio()
    ggplotly(p)
  })

  # ---- Country risk: explore ---------------------------------------------------
  cr_data <- reactive({
    req(input$cr_region)
    d <- country %>% filter(Region %in% input$cr_region)
    if (isTRUE(input$cr_noiran)) d <- d %>% filter(Country != "Iran")
    d
  })

  output$cr_plot <- renderPlotly({
    d <- cr_data(); req(nrow(d) > 2)
    d$X <- d[[input$cr_x]]; d$Y <- d[[input$cr_y]]
    d$Tip <- paste0(d$Country, "<br>", input$cr_x, ": ", d$X, "<br>", input$cr_y, ": ", d$Y)
    p <- ggplot(d, aes(X, Y)) +
      geom_point(aes(colour = Region, size = Population_M, text = Tip), alpha = 0.85) +
      scale_colour_manual(values = REGION_COLS) + scale_size_area(max_size = 12, guide = "none") +
      labs(x = input$cr_x, y = input$cr_y) + theme_portfolio()
    if (isTRUE(input$cr_line)) p <- p + geom_smooth(method = "lm", se = TRUE, colour = PAL[["teal"]], fill = PAL[["teal"]], alpha = 0.2)
    ggplotly(p, tooltip = "text")
  })

  output$cr_stat <- renderUI({
    d <- cr_data(); req(nrow(d) > 3)
    ct <- suppressWarnings(cor.test(d[[input$cr_x]], d[[input$cr_y]], method = "spearman", exact = FALSE))
    div(class = "alert alert-info",
        "Spearman rank correlation: ", strong(round(unname(ct$estimate), 2)),
        " (p = ", signif(ct$p.value, 2), ", n = ", nrow(d), "). Small samples: treat as a pattern, not proof.")
  })

  # ---- Country risk: adjustable Stress Index -----------------------------------
  output$cr_stress <- renderPlotly({
    w <- c(input$w_import, input$w_gdp, input$w_stock, input$w_infl, input$w_curr)
    req(sum(w) > 0)
    d <- country %>% mutate(Stress = (w[1] * Z_Import + w[2] * Z_GDP + w[3] * Z_Stock + w[4] * Z_Infl + w[5] * Z_Curr) / sum(w))
    p <- ggplot(d, aes(Stress, fct_reorder(Country, Stress), fill = Vulnerability_f)) +
      geom_col(width = 0.75) + geom_vline(xintercept = 0, colour = PAL[["grey"]]) +
      scale_fill_manual(values = VULN_COLS) +
      labs(x = "Stress Index (higher = more exposed)", y = NULL, fill = "Supplied label") + theme_portfolio()
    ggplotly(p) %>% layout(legend = list(orientation = "h", y = -0.15))
  })

  output$cr_table <- renderDT({
    tab <- country %>% select(Country, Region, Oil_Import_Pct, GDP_Impact_Pct, Stock_Market_Change,
                              Inflation_Risk, Currency_Pressure, Vulnerability, Population_M, Policy_Response)
    datatable(tab, rownames = FALSE, filter = "top", options = list(pageLength = 10, scrollX = TRUE))
  })

  # ---- Hormuz trade tracker ------------------------------------------------------
  vy_data <- reactive({
    req(input$vy_series, input$vy_dates)
    voy %>% filter(Series %in% input$vy_series, Date >= input$vy_dates[1], Date <= input$vy_dates[2]) %>%
      mutate(Value = case_when(input$vy_metric == "ma" ~ MA7_Pct,
                               input$vy_metric == "daily" ~ Pct_Baseline,
                               TRUE ~ MA7))
  })

  output$vy_plot <- renderPlotly({
    d <- vy_data(); req(nrow(d) > 0)
    ylab <- c(ma = "% of normal (7-day average)", daily = "% of normal (daily)", raw = "Index (7-day average)")[[input$vy_metric]]
    p <- ggplot(d, aes(Date, Value, colour = Series, group = Series)) +
      geom_vline(xintercept = as.numeric(WAR_START), linetype = "dotted") +
      geom_line(linewidth = 1) +
      scale_colour_manual(values = SERIES_COLS) +
      labs(x = NULL, y = ylab, colour = NULL, title = "Traffic through the Strait of Hormuz (dotted line = war start)") +
      theme_portfolio()
    if (input$vy_metric != "raw") p <- p + geom_hline(yintercept = 100, linetype = "dashed", colour = PAL[["grey"]])
    if (isTRUE(input$vy_brent) && input$vy_metric != "raw") {       # optional Brent overlay
      b <- crude %>% filter(Date >= input$vy_dates[1], Date <= input$vy_dates[2]) %>%
        transmute(Date, Value = Brent_USD / pre_base * 100)
      if (nrow(b) > 0) p <- p + geom_line(data = b, aes(Date, Value), inherit.aes = FALSE, colour = PAL[["pink"]], linewidth = 1, linetype = "longdash")
    }
    ggplotly(p) %>% layout(legend = list(orientation = "h", y = -0.15))
  })

  output$vy_table <- renderDT({
    d <- vy_data(); req(nrow(d) > 0)
    tab <- voy %>% filter(Series %in% input$vy_series, Date >= input$vy_dates[1], Date <= input$vy_dates[2]) %>%
      group_by(Series, Phase) %>%
      summarise(Days = n(), Avg_Pct_of_Normal = mean(Pct_Baseline), Min_7day_Pct = min(MA7_Pct), .groups = "drop")
    datatable(tab, rownames = FALSE, options = list(pageLength = 8, dom = "tp")) %>% formatRound(4:5, 1)
  })

  output$vy_lost <- renderPlotly({
    req(input$vy_series)
    d <- voy %>% filter(Series %in% input$vy_series, Date >= WAR_START) %>%
      group_by(Series) %>% arrange(Date) %>%
      mutate(Lost_Days = cumsum(1 - Pct_Baseline / 100)) %>% ungroup()
    p <- ggplot(d, aes(Date, Lost_Days, colour = Series, group = Series)) +
      geom_line(linewidth = 1) + scale_colour_manual(values = SERIES_COLS) +
      labs(x = NULL, y = "Days of normal trade lost", colour = NULL) + theme_portfolio()
    ggplotly(p) %>% layout(legend = list(orientation = "h", y = -0.2))
  })

  # ---- Data and methods ----------------------------------------------------------
  dm_data <- reactive({
    switch(input$dm_set, crude = crude, fuel = fuel, country = country %>% select(-starts_with("Z_")),
           petrol = petrol, proscons = proscons, timeline = timeline, voy = voy)
  })
  output$dm_table <- renderDT(datatable(dm_data(), rownames = FALSE, filter = "top", options = list(pageLength = 10, scrollX = TRUE)))
  output$dm_dl <- downloadHandler(
    filename = function() paste0(input$dm_set, "_clean.csv"),
    content = function(file) write.csv(dm_data(), file, row.names = FALSE))
}

shinyApp(ui, server)
