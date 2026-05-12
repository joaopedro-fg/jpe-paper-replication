library(tidyverse)
library(fredr)
library(dotenv)
library(here)

dotenv::load_dot_env(here(".env"))
fredr_set_key(Sys.getenv("FRED_API_KEY"))


fetch_series <- function(series_id) {
  fredr(
    series_id          = series_id,
    observation_start  = as.Date("1965-04-01"),  # 1965 Q2
    observation_end    = as.Date("1995-07-01"),  # 1995 Q3
    frequency          = "q",
    aggregation_method = "avg"
  ) |>
    select(date, series_id, value)
}

variables <- tribble(
  ~series_id,    ~var_name,              ~transformation,
  "GDPC1",       "real_gdp",             "log_level",
  "PCECC96",     "real_consumption",     "log_level",
  "GDPDEF",      "gdp_deflator",         "log_level",
  "GPDIC1",      "real_investment",      "log_level",
  "COMPRNFB",    "real_wage",            "log_level",
  "OPHNFB",      "labor_productivity",   "log_level",
  "FEDFUNDS",    "federal_funds_rate",   "log_level",
  "CP",          "real_profits",         "log_level",
  "M2SL",        "m2_growth",            "growth_rate"
)

raw_data <- variables %>%
  pull(series_id) %>%
  set_names() %>%
  map(fetch_series) %>%
  list_rbind()

transformed_data <- raw_data %>%
  left_join(variables, by = "series_id") %>%
  arrange(series_id, date) %>%
  group_by(series_id) %>%
  mutate(
    transformed_value = case_when(
      transformation == "log_level"   ~ log(value),
      transformation == "growth_rate" ~ (value - lag(value))/lag(value),
      TRUE ~ value
    )
  ) %>%
  ungroup()

macro_df <- transformed_data %>%
  select(date, var_name, transformed_value) %>%
  pivot_wider(
    names_from  = var_name,
    values_from = transformed_value
  ) %>%
  mutate(
    year    = lubridate::year(date),
    quarter = lubridate::quarter(date)
  ) %>%
  arrange(date) 

saveRDS(macro_df, here("data", "fed_data.rds"))