# Peachtree Lottery Sales

This demo repository contains:

* `georgia_lottery.RDS` - Simulated data of Peachtree lottery sales in Goergia 2010-2025
* `report.qmd` - A [Quarto report](https://quarto.org/) generated from the data.
* `dashboard.qmd` - a [Quarto dashboard](https://quarto.org/docs/dashboards/) generated from the data
* `app.R` - A [Shiny app](https://shiny.posit.co/) generated from the data
* `querychat_app.R` - An [LLM powered Shiny app](https://posit-dev.github.io/querychat/) generated from the data (requires [configuring environment variables](https://posit-dev.github.io/querychat/r/reference/querychat-convenience.html#arg-client)).
* `plumber.R`, `lottery_model.rds`, and `deploy.R` - components of a model packaged as a web API with [Plumber](https://www.rplumber.io/index.html).