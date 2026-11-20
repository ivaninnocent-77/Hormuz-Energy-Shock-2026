# Strait of Hormuz Energy Shock 2026 - Data Analyst Portfolio
Author: Sekibenga Ivan Innocent

## Contents
| Path | Purpose |
|---|---|
| `Portfolio.Rmd` | Full report (knits to PDF) |
| `app/app.R` | Interactive Shiny dashboard |
| `R/helpers.R` | Shared cleaning code, palettes, theme (used by both) |
| `data/` | The ten supplied CSV files (unchanged) |
| `business_questions.md` | 32 plain-language business questions |
| `install_packages.R` | One-step package installation |

## Run
1. `source("install_packages.R")` (and `tinytex::install_tinytex()` if you have no LaTeX).
2. Knit: open `Portfolio.Rmd` in RStudio and click **Knit to PDF**, or `rmarkdown::render("Portfolio.Rmd")`.
3. App: from the project root, `shiny::runApp("app")`.

Deploying the app: copy `R/` and `data/` inside the `app/` folder first (the app looks for them next to itself, then one level up).

## Notes
* Set the working directory to the project root before knitting.
* Fill in the placeholders in the final "About the Analyst" section of the report.
* Price/timeline data end 13 Jul 2026; trade-tracker data run to 19 Nov 2026.
