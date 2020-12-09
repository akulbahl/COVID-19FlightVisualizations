# app initialization

packages = c("shiny",
            "shinythemes",
            "shinydashboard",
            "dashboardthemes",
            "shinyWidgets",
            "reshape2",
            "tidyverse",
            "knitr",
            "lubridate",
            "heatmaply",
            "ggthemes",
            "scales",
            "plotly")

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}

invisible(sapply(packages, install_if_missing))