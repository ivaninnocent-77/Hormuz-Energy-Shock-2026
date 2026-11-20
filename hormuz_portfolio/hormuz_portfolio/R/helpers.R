# =============================================================================
# helpers.R -- Shared data-cleaning functions, constants and plotting theme
# Project : Strait of Hormuz Energy Shock 2026
# Author  : Sekibenga Ivan Innocent
# Used by : Portfolio.Rmd (PDF report) and app/app.R (Shiny dashboard)
# Purpose : ONE source of truth for cleaning, so the report and the app can
#           never disagree about the numbers.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)    # data wrangling verbs
  library(tidyr)    # pivoting / reshaping
  library(readr)    # fast, typed CSV reading
  library(stringr)  # string detection helpers
  library(ggplot2)  # plotting theme
})

# ---- 1. Project constants ---------------------------------------------------

# The four conflict phases exactly as labelled in crude_oil_daily.csv
PHASE_LEVELS <- c("Pre-Conflict", "Active Conflict (Wave 1)",
                  "Ceasefire & Negotiations", "Re-Escalation & Blockade")

# US-Israeli strikes began on 28 Feb 2026; Hormuz was closed on 1 Mar 2026
WAR_START <- as.Date("2026-02-28")

# Last date covered by the price / timeline files (trade tracker runs later)
PRICE_END <- as.Date("2026-07-13")

# Colour palettes (consistent across report and app)
PAL <- c(navy = "#0B2545", teal = "#13A89E", gold = "#F4B400", coral = "#E4572E",
         purple = "#6A4C93", blue = "#1F77B4", green = "#2E933C", pink = "#D81B60",
         grey = "#6B7280")
PHASE_COLS <- c("Pre-Conflict" = "#2E933C", "Active Conflict (Wave 1)" = "#E4572E",
                "Ceasefire & Negotiations" = "#F4B400",
                "Re-Escalation & Blockade" = "#6A4C93",
                "After 13 Jul 2026 (flows only)" = "#6B7280")
SERIES_COLS <- c("Crude oil exports" = "#0B2545", "LNG exports" = "#13A89E",
                 "Fertiliser exports" = "#F4B400", "Agricultural imports" = "#E4572E")
REGION_COLS <- c("South Asia" = "#E4572E", "East Asia" = "#F4B400",
                 "Europe/Asia" = "#6A4C93", "Europe" = "#1F77B4",
                 "North America" = "#2E933C", "Middle East" = "#13A89E",
                 "Oceania" = "#D81B60", "Africa" = "#8D6E63")
VULN_COLS <- c("Low" = "#2E933C", "Moderate" = "#F4B400",
               "High" = "#F28C28", "Critical" = "#C62828")

# ---- 2. Small utilities -----------------------------------------------------

# Parse dates that arrive in TWO formats ("2025-01-01" and "1/1/2025")
parse_mixed_date <- function(x) {
  x   <- as.character(x)
  iso <- as.Date(x, format = "%Y-%m-%d")  # NA unless the text is ISO
  us  <- as.Date(x, format = "%m/%d/%Y")  # NA unless the text is US style
  dplyr::coalesce(iso, us)                # keep whichever parse succeeded
}

# Label every calendar date with its conflict phase (used for trade-tracker data)
assign_phase <- function(d) {
  out <- dplyr::case_when(
    d <  as.Date("2026-02-28") ~ "Pre-Conflict",
    d <  as.Date("2026-04-08") ~ "Active Conflict (Wave 1)",
    d <  as.Date("2026-06-03") ~ "Ceasefire & Negotiations",
    d <= PRICE_END             ~ "Re-Escalation & Blockade",
    TRUE                       ~ "After 13 Jul 2026 (flows only)")
  factor(out, levels = c(PHASE_LEVELS, "After 13 Jul 2026 (flows only)"))
}

# Consistent, publication-style ggplot theme
theme_portfolio <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(plot.title       = element_text(face = "bold", colour = PAL[["navy"]], size = base_size + 3),
          plot.subtitle    = element_text(colour = PAL[["grey"]]),
          plot.caption     = element_text(colour = PAL[["grey"]], size = base_size - 3, hjust = 0),
          panel.grid.minor = element_blank(),
          legend.position  = "bottom",
          strip.text       = element_text(face = "bold", colour = PAL[["navy"]]),
          axis.title       = element_text(colour = "#374151"))
}

# ---- 3. One cleaning function per dataset -----------------------------------

# 3a. Daily Brent / WTI prices ------------------------------------------------
clean_crude <- function(path) {
  read_csv(path, show_col_types = FALSE) %>%
    rename(Brent_Change_Pct_Source = Brent_Change_Pct,
           WTI_Change_Pct_Source   = WTI_Change_Pct) %>%   # keep originals for auditing
    mutate(Date  = parse_mixed_date(Date),                 # text -> Date
           Phase = factor(Phase, levels = PHASE_LEVELS)) %>%
    arrange(Date) %>%                                      # guarantee time order
    filter(!is.na(Brent_USD), !is.na(WTI_USD)) %>%         # drop blank placeholder rows
    mutate(Brent_Ret_Pct = (Brent_USD / dplyr::lag(Brent_USD) - 1) * 100, # own daily % move
           WTI_Ret_Pct   = (WTI_USD   / dplyr::lag(WTI_USD)   - 1) * 100,
           Spread_USD    = Brent_USD - WTI_USD)            # Brent-WTI gap
}

# 3b. Monthly crude price history 1970-2026 -----------------------------------
clean_fuel <- function(path) {
  read_csv(path, show_col_types = FALSE) %>%
    mutate(Date = parse_mixed_date(Date)) %>%
    arrange(Date) %>%
    rename(Price = Crude_Oil_Price) %>%
    mutate(Year    = as.integer(format(Date, "%Y")),
           Decade  = paste0((Year %/% 10) * 10, "s"),      # e.g. "1970s"
           Log_Ret = log(Price / dplyr::lag(Price)),       # monthly log return
           Pct_Chg = (Price / dplyr::lag(Price) - 1) * 100,
           Pct_Chg_12m = (Price / dplyr::lag(Price, 12) - 1) * 100)
}

# 3c. Country macro impact ----------------------------------------------------
clean_country <- function(path) {
  ord <- function(x, lv) factor(x, levels = lv, ordered = TRUE)  # ordered factor helper
  read_csv(path, show_col_types = FALSE) %>%
    mutate(across(where(is.character), str_trim)) %>%
    mutate(
      Inflation_Risk_f    = ord(Inflation_Risk,    c("Low", "Moderate", "High", "Very High", "Extreme")),
      Currency_Pressure_f = ord(Currency_Pressure, c("Low", "Moderate", "High", "Severe", "Extreme")),
      Vulnerability_f     = ord(Vulnerability,     c("Low", "Moderate", "High", "Critical")),
      Inflation_Score     = as.integer(Inflation_Risk_f),      # 1-5 numeric scale
      Currency_Score      = as.integer(Currency_Pressure_f),   # 1-5 numeric scale
      Vulnerability_Score = as.integer(Vulnerability_f),       # 1-4 numeric scale
      # Text-mine the free-text policy column into yes/no flags
      Policy_Subsidy_Support  = str_detect(Policy_Response, regex("subsid|capped|tax cuts|frozen", ignore_case = TRUE)) &
                                !str_detect(Policy_Response, regex("\\bended\\b|slashed", ignore_case = TRUE)),
      Policy_Reserves         = str_detect(Policy_Response, regex("tapped|drawdown|\\bSPR\\b", ignore_case = TRUE)),
      Policy_Demand_Restraint = str_detect(Policy_Response, regex("ration|suspended|halved|grounded|blackout|conservation", ignore_case = TRUE)),
      Policy_Supply_Diversify = str_detect(Policy_Response, regex("Russian|expedited|fast-tracking|redirected|restarts|surged|drilling", ignore_case = TRUE)),
      Policy_Macro_Finance    = str_detect(Policy_Response, regex("IMF|central bank", ignore_case = TRUE)),
      Policy_Security         = str_detect(Policy_Response, regex("Navy|naval|security|defen", ignore_case = TRUE)))
}

# 3d. Petrol pump prices ------------------------------------------------------
clean_petrol <- function(path) {
  read_csv(path, show_col_types = FALSE) %>%
    mutate(across(where(is.character), str_trim),
           Oil_Import_Dep = factor(Oil_Import_Dep, levels = c("Low", "Medium", "High"), ordered = TRUE),
           Pct_Since_War_Calc = (Jul13_Price / Before_War_Price - 1) * 100,  # recomputed in local currency
           Pct_Since_War_USD  = (Jul13_USD / Before_War_USD - 1) * 100,
           Extra_50L_USD      = 50 * (Jul13_USD - Before_War_USD))           # extra cost of a 50-litre tank
}

# Reshape the four price snapshots into a long table (one row per country-date)
petrol_to_long <- function(p) {
  stages      <- c("Before_War", "Mar7", "Apr6", "Jul13")
  stage_dates <- as.Date(c("2026-02-27", "2026-03-07", "2026-04-06", "2026-07-13"))
  local <- p %>%
    select(Country, ISO, Region, Oil_Import_Dep, all_of(paste0(stages, "_Price"))) %>%
    pivot_longer(all_of(paste0(stages, "_Price")), names_to = "Stage", values_to = "Price_Local") %>%
    mutate(Stage = str_remove(Stage, "_Price"))
  usd <- p %>%
    select(Country, all_of(paste0(stages, "_USD"))) %>%
    pivot_longer(-Country, names_to = "Stage", values_to = "Price_USD") %>%
    mutate(Stage = str_remove(Stage, "_USD"))
  local %>%
    left_join(usd, by = c("Country", "Stage")) %>%
    mutate(Date  = stage_dates[match(Stage, stages)],
           Stage = factor(Stage, levels = stages, labels = c("Before war", "7 Mar", "6 Apr", "13 Jul"))) %>%
    group_by(Country) %>%
    mutate(Index = Price_Local / first(Price_Local) * 100) %>%  # before-war = 100
    ungroup()
}

# 3e. Pros and cons -----------------------------------------------------------
clean_proscons <- function(path) {
  read_csv(path, show_col_types = FALSE) %>%
    select(-any_of("Icon")) %>%                               # empty decoration column
    mutate(across(where(is.character), str_trim)) %>%
    mutate(Impact_Level = dplyr::recode(Impact_Level, "Medium" = "Moderate"),  # harmonise labels
           Impact_Level = factor(Impact_Level, levels = c("Low", "Moderate", "High", "Critical"), ordered = TRUE),
           Impact_Score = as.integer(Impact_Level),
           Type         = factor(Type, levels = c("Pro", "Con")))
}

# 3f. War timeline ------------------------------------------------------------
clean_timeline <- function(path) {
  read_csv(path, show_col_types = FALSE) %>%
    mutate(Date = parse_mixed_date(Date),
           Category_Primary = str_extract(Category, "^[^/]+"),          # "Military/Energy" -> "Military"
           Category_Primary = factor(Category_Primary,
                                     levels = c("Political", "Diplomatic", "Military", "Energy", "Economic")),
           Phase = assign_phase(Date)) %>%
    arrange(Date)
}

# 3g. Strait of Hormuz trade-tracker indexes (four files, one schema) ---------
VOY_FILES <- c("Crude oil exports"    = "crude_oil_export_voy_intake_index.csv",
               "LNG exports"          = "lng_export_voy_intake_index.csv",
               "Fertiliser exports"   = "fertilizer_export_voy_intake_index.csv",
               "Agricultural imports" = "agricultural_product_import_voy_intake_index.csv")

clean_voy_one <- function(path, series) {
  d <- read_csv(path, show_col_types = FALSE) %>%
    mutate(date = parse_mixed_date(date)) %>%
    arrange(date) %>%
    distinct(date, .keep_all = TRUE)                           # guard against duplicate days
  # "Normal" level = average index from 1 Jan 2025 up to the day before the war
  base <- mean(d$voy_intake_index[d$date < WAR_START])
  d %>% transmute(Date = date, Series = series,
                  Index = voy_intake_index, MA7 = ma_7day, MA7_PY = prior_year_ma_7day,
                  Baseline = base,
                  Pct_Baseline = Index / base * 100,           # daily flow as % of normal
                  MA7_Pct      = MA7 / base * 100,             # smoothed flow as % of normal
                  Phase        = assign_phase(date),
                  Is_Zero_Day  = Index == 0)
}

clean_voy <- function(data_dir) {
  bind_rows(lapply(names(VOY_FILES), function(s) clean_voy_one(file.path(data_dir, VOY_FILES[[s]]), s))) %>%
    mutate(Series = factor(Series, levels = names(VOY_FILES)))
}

# ---- 4. Master loader -------------------------------------------------------
load_clean_data <- function(data_dir = "data") {
  petrol <- clean_petrol(file.path(data_dir, "petrol_prices_comparison.csv"))
  list(crude       = clean_crude(file.path(data_dir, "crude_oil_daily.csv")),
       fuel        = clean_fuel(file.path(data_dir, "fuel_prices_1970_2026.csv")),
       country     = clean_country(file.path(data_dir, "country_impact.csv")),
       petrol      = petrol,
       petrol_long = petrol_to_long(petrol),
       proscons    = clean_proscons(file.path(data_dir, "pros_cons_analysis.csv")),
       timeline    = clean_timeline(file.path(data_dir, "war_timeline.csv")),
       voy         = clean_voy(data_dir))
}
