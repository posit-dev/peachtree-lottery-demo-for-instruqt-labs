# deploy.R
# Deploy the Georgia Lottery plumber API to Posit Connect
#
# Run this script once to publish (or re-publish) the API.
# Prerequisites:
#   1. An rsconnect account configured for your Posit Connect server.
#      Set one up with rsconnect::setAccountInfo() or via the
#      Positron / RStudio "Publishing" pane.
#   2. The plumber and rsconnect packages installed.
#
# Usage: source("deploy.R")  OR  Rscript deploy.R

library(rsconnect)

# ---- Configure these if not already set via rsconnect::accounts() ----------
# rsconnect::setAccountInfo(
#   name   = "<your-account-name>",
#   server = "<your-connect-server-url>",
#   token  = "<your-api-key>"
# )

# ---- Deploy ----------------------------------------------------------------
rsconnect::deployAPI(
  api        = ".",           # directory containing plumber.R
  appFiles   = c("plumber.R", "lottery_model.rds"),
  appTitle   = "Georgia Lottery Spend Predictor",
  forceUpdate = TRUE
)
