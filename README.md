# Peachtree Lottery Sales

This demo repository contains:

* Simulated data of Peachtree lottery sales (`georgia_lottery.RDS`)
* [A Quarto report](https://quarto.org/) (`report.qmd`)
* [A Quarto dashboard](https://quarto.org/docs/dashboards/) (`dashboard.qmd`)
* [A Shiny app](https://shiny.posit.co/) (`app.R`)
* [An LLM powered Shiny app](https://posit-dev.github.io/querychat/) (`querychat_app.R`)
* [A plumber model API](https://www.rplumber.io/index.html). (`plumber.R`, `lottery_model.rds`, and `deploy.R`)

Hosting the Querychat app will require configuring the `QUERYCHAT_CLIENT` environment variable, as well as an API key. See instructions [here](https://posit-dev.github.io/querychat/r/reference/querychat-convenience.html#arg-client).
