# plumber.R
# Georgia Lottery Spend Prediction API
#
# Predicts total annual lottery spend (in dollars) for a demographic cell
# based on their county, age group, income level, sex, and race/ethnicity.
#
# Deploy to Posit Connect with:
#   source("deploy.R")

library(plumber)

# Load the pre-fitted model once at startup
model <- readRDS("lottery_model.rds")

#* @apiTitle Georgia Lottery Spend Predictor
#* @apiDescription Predicts total annual lottery spending (USD) for a
#*   demographic group based on their county, age bracket, income level,
#*   sex, and race/ethnicity. Fitted on the georgia_lottery dataset using
#*   multiple linear regression (R² = 0.313).

#* Predict total lottery spend for a demographic group
#*
#* @param county       One of the 159 Georgia counties (e.g. "Fulton")
#* @param age_group    Age bracket: "20-29", "30-39", "40-49", "50-59",
#*                     "60-69", "70-79", or "80+"
#* @param income_level Income bracket: "Under $15K", "$15K-$30K",
#*                     "$30K-$50K", "$50K-$75K", "$75K-$100K",
#*                     "$100K-$150K", or "$150K+"
#* @param sex          "Male" or "Female"
#* @param race_ethnicity Race/ethnicity: "NH White", "NH Black", "Hispanic",
#*                       "NH Asian", "NH AIAN", or "NH Other/Multi"
#*
#* @get /predict
#* @post /predict
#* @serializer json
function(
  req, res,
  county         = "Fulton",
  age_group      = "40-49",
  income_level   = "$30K-$50K",
  sex            = "Male",
  race_ethnicity = "NH White"
) {

  # ---- Valid levels (must match training data) ----------------------------
  valid <- list(
    county = c(
      "Appling", "Atkinson", "Bacon", "Baker", "Baldwin", "Banks", "Barrow",
      "Bartow", "Ben Hill", "Berrien", "Bibb", "Bleckley", "Brantley",
      "Brooks", "Bryan", "Bulloch", "Burke", "Butts", "Calhoun", "Camden",
      "Candler", "Carroll", "Catoosa", "Charlton", "Chatham", "Chattahoochee",
      "Chattooga", "Cherokee", "Clarke", "Clay", "Clayton", "Clinch", "Cobb",
      "Coffee", "Colquitt", "Columbia", "Cook", "Coweta", "Crawford",
      "Crisp", "Dade", "Dawson", "Decatur", "DeKalb", "Dodge", "Dooly",
      "Dougherty", "Douglas", "Early", "Echols", "Effingham", "Elbert",
      "Emanuel", "Evans", "Fannin", "Fayette", "Floyd", "Forsyth",
      "Franklin", "Fulton", "Gilmer", "Glascock", "Glynn", "Gordon",
      "Grady", "Greene", "Gwinnett", "Habersham", "Hall", "Hancock",
      "Haralson", "Harris", "Hart", "Heard", "Henry", "Houston", "Irwin",
      "Jackson", "Jasper", "Jeff Davis", "Jefferson", "Jenkins", "Johnson",
      "Jones", "Lamar", "Lanier", "Laurens", "Lee", "Liberty", "Lincoln",
      "Long", "Lowndes", "Lumpkin", "Macon", "Madison", "Marion",
      "McDuffie", "McIntosh", "Meriwether", "Miller", "Mitchell", "Monroe",
      "Montgomery", "Morgan", "Murray", "Muscogee", "Newton", "Oconee",
      "Oglethorpe", "Paulding", "Peach", "Pickens", "Pierce", "Pike",
      "Polk", "Pulaski", "Putnam", "Quitman", "Rabun", "Randolph",
      "Richmond", "Rockdale", "Schley", "Screven", "Seminole", "Spalding",
      "Stephens", "Stewart", "Sumter", "Talbot", "Taliaferro", "Tattnall",
      "Taylor", "Telfair", "Terrell", "Thomas", "Tift", "Toombs", "Towns",
      "Treutlen", "Troup", "Turner", "Twiggs", "Union", "Upson", "Walker",
      "Walton", "Ware", "Warren", "Washington", "Wayne", "Webster",
      "Wheeler", "White", "Whitfield", "Wilcox", "Wilkes", "Wilkinson",
      "Worth"
    ),
    age_group = c("20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+"),
    income_level = c(
      "Under $15K", "$15K-$30K", "$30K-$50K",
      "$50K-$75K", "$75K-$100K", "$100K-$150K", "$150K+"
    ),
    sex = c("Male", "Female"),
    race_ethnicity = c(
      "NH White", "NH Black", "Hispanic",
      "NH Asian", "NH AIAN", "NH Other/Multi"
    )
  )

  inputs <- list(
    county         = county,
    age_group      = age_group,
    income_level   = income_level,
    sex            = sex,
    race_ethnicity = race_ethnicity
  )

  # ---- Input validation ---------------------------------------------------
  errors <- character(0)
  for (field in names(valid)) {
    if (!inputs[[field]] %in% valid[[field]]) {
      errors <- c(
        errors,
        paste0(
          "'", field, "' must be one of the valid values.",
          " Got: '", inputs[[field]], "'.",
          " See /valid-inputs for the full list."
        )
      )
    }
  }

  if (length(errors) > 0) {
    res$status <- 400
    return(list(
      error   = "Invalid input",
      details = errors,
      hint    = "Call GET /valid-inputs for a list of accepted values."
    ))
  }

  # ---- Build prediction data frame ----------------------------------------
  new_data <- data.frame(
    county         = factor(county,         levels = valid$county),
    age_group      = factor(age_group,      levels = valid$age_group),
    income_level   = factor(income_level,   levels = valid$income_level),
    sex            = factor(sex,            levels = valid$sex),
    race_ethnicity = factor(race_ethnicity, levels = valid$race_ethnicity),
    stringsAsFactors = FALSE
  )

  # ---- Predict ------------------------------------------------------------
  pred <- predict(model, newdata = new_data, interval = "prediction", level = 0.95)

  list(
    predicted_lottery_spend = round(as.numeric(pred[, "fit"]), 2),
    lower_95                = round(as.numeric(pred[, "lwr"]), 2),
    upper_95                = round(as.numeric(pred[, "upr"]), 2),
    inputs = inputs,
    model_info = list(
      response  = "lottery_spend (USD total per demographic cell per year)",
      r_squared = 0.313
    )
  )
}

#* List valid values for all input parameters
#* @get /valid-inputs
#* @serializer json
function() {
  list(
    county = c(
      "Appling", "Atkinson", "Bacon", "Baker", "Baldwin", "Banks", "Barrow",
      "Bartow", "Ben Hill", "Berrien", "Bibb", "Bleckley", "Brantley",
      "Brooks", "Bryan", "Bulloch", "Burke", "Butts", "Calhoun", "Camden",
      "Candler", "Carroll", "Catoosa", "Charlton", "Chatham", "Chattahoochee",
      "Chattooga", "Cherokee", "Clarke", "Clay", "Clayton", "Clinch", "Cobb",
      "Coffee", "Colquitt", "Columbia", "Cook", "Coweta", "Crawford",
      "Crisp", "Dade", "Dawson", "Decatur", "DeKalb", "Dodge", "Dooly",
      "Dougherty", "Douglas", "Early", "Echols", "Effingham", "Elbert",
      "Emanuel", "Evans", "Fannin", "Fayette", "Floyd", "Forsyth",
      "Franklin", "Fulton", "Gilmer", "Glascock", "Glynn", "Gordon",
      "Grady", "Greene", "Gwinnett", "Habersham", "Hall", "Hancock",
      "Haralson", "Harris", "Hart", "Heard", "Henry", "Houston", "Irwin",
      "Jackson", "Jasper", "Jeff Davis", "Jefferson", "Jenkins", "Johnson",
      "Jones", "Lamar", "Lanier", "Laurens", "Lee", "Liberty", "Lincoln",
      "Long", "Lowndes", "Lumpkin", "Macon", "Madison", "Marion",
      "McDuffie", "McIntosh", "Meriwether", "Miller", "Mitchell", "Monroe",
      "Montgomery", "Morgan", "Murray", "Muscogee", "Newton", "Oconee",
      "Oglethorpe", "Paulding", "Peach", "Pickens", "Pierce", "Pike",
      "Polk", "Pulaski", "Putnam", "Quitman", "Rabun", "Randolph",
      "Richmond", "Rockdale", "Schley", "Screven", "Seminole", "Spalding",
      "Stephens", "Stewart", "Sumter", "Talbot", "Taliaferro", "Tattnall",
      "Taylor", "Telfair", "Terrell", "Thomas", "Tift", "Toombs", "Towns",
      "Treutlen", "Troup", "Turner", "Twiggs", "Union", "Upson", "Walker",
      "Walton", "Ware", "Warren", "Washington", "Wayne", "Webster",
      "Wheeler", "White", "Whitfield", "Wilcox", "Wilkes", "Wilkinson",
      "Worth"
    ),
    age_group = c("20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+"),
    income_level = c(
      "Under $15K", "$15K-$30K", "$30K-$50K",
      "$50K-$75K", "$75K-$100K", "$100K-$150K", "$150K+"
    ),
    sex = c("Male", "Female"),
    race_ethnicity = c(
      "NH White", "NH Black", "Hispanic",
      "NH Asian", "NH AIAN", "NH Other/Multi"
    )
  )
}

#* Health check
#* @get /health
#* @serializer json
function() {
  list(status = "ok", model_loaded = !is.null(model))
}
