# Install every package needed by Portfolio.Rmd and app/app.R
pkgs <- c("tidyverse", "scales", "patchwork", "knitr", "kableExtra", "broom", "ggrepel",
          "rmarkdown", "tinytex", "shiny", "bslib", "plotly", "DT")
missing <- setdiff(pkgs, rownames(installed.packages()))
if (length(missing)) install.packages(missing)
# A LaTeX engine is needed to knit to PDF (skip if you already have one):
# tinytex::install_tinytex()
