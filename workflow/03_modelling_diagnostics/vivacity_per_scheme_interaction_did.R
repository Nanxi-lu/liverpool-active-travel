# Per-scheme interaction difference-in-differences (log-linear, weather-adjusted)
# ---------------------------------------------------------------------------
# Extends the citywide 2022-2024 model so the city is the reference benchmark
# and each analysable scheme (12d, 12f) gets its own post x scheme term, with
# proper standard errors clustered by countline.
#
#   log(1 + count) =
#       post                         # city's average 2022->2024 change (benchmark)
#     + post:scheme_12d              # 12d's additional change vs the city
#     + post:scheme_12f              # 12f's additional change vs the city
#     + countline fixed effects
#     + matched month-day controls
#     + weekday controls
#     + standardized weather (temperature, log precipitation, wind, solar)
#   SEs clustered by countline.
#
# The pooled version of this specification reproduces
# reports/analysis_reports/vivacity_weather_adjusted_lcr_regression_2022_2024.csv
# (Active -33.4%, Cyclist -31.6%, Pedestrian -29.4% relative to the city).
#
# Schemes 12b and 12e have no quality-passing pre-installation data, and scheme
# 13 uses a 2023-2024 window, so this citywide 2022-2024 model covers 12d and
# 12f only; scheme 13 needs its own window.

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(lubridate); library(fixest)
})

base_dir <- "/Users/lu_nanxi/CASA/Dissertation_Data"
ar <- file.path(base_dir, "reports", "analysis_reports")

# ---------------------------------------------------------------- build frame
cohort <- read_csv(file.path(ar, "vivacity_baseline_adjusted_regression_cohort.csv"),
                   show_col_types = FALSE)
coh_ids <- as.character(cohort$countline_id)

df <- read_csv(file.path(ar, "vivacity_citywide_quality_gated_daily_cache.csv.gz"),
               show_col_types = FALSE) |>
  mutate(countline_id = as.character(countline_id), date = as_date(date)) |>
  filter(countline_id %in% coh_ids, year(date) %in% c(2022, 2024), quality_ok_day) |>
  mutate(md = format(date, "%m-%d")) |>
  filter(md >= "11-15", md <= "12-31") |>
  mutate(month_day = md, weekday = wday(date),
         cy = year(date), post = as.numeric(cy == 2024),
         scheme = ifelse(is.na(analysis_scheme_id) | analysis_scheme_id == "",
                         "city", analysis_scheme_id),
         s12d = as.numeric(scheme == "12d"),
         s12f = as.numeric(scheme == "12f"))

# keep month-days observed in BOTH years for each countline (calendar matching)
both <- df |> group_by(countline_id, month_day) |>
  summarise(n_years = n_distinct(cy), .groups = "drop") |>
  filter(n_years == 2) |> select(countline_id, month_day)
df <- inner_join(df, both, by = c("countline_id", "month_day"))

# ---------------------------------------------------------------- weather
weather <- read_csv(file.path(ar, "vivacity_era5_weather_daily.csv"), show_col_types = FALSE) |>
  mutate(date = as_date(date)) |>
  group_by(date) |>
  summarise(temp = mean(temperature_2m_mean), precip = mean(precipitation_sum),
            wind = mean(wind_speed_10m_max), solar = mean(shortwave_radiation_sum),
            .groups = "drop") |>
  mutate(logprecip = log1p(precip))
df <- left_join(df, weather, by = "date") |>
  mutate(across(c(temp, logprecip, wind, solar),
                ~ as.numeric(scale(.x)), .names = "z_{.col}"))

# ---------------------------------------------------------------- model
pct  <- function(b) 100 * (exp(b) - 1)
outcomes <- c(active_travel_total = "Active travel", cyclist = "Cyclist", pedestrian = "Pedestrian")

results <- lapply(names(outcomes), function(oc) {
  df$y <- log1p(df[[oc]])
  m <- feols(y ~ post + post:s12d + post:s12f +
               z_temp + z_logprecip + z_wind + z_solar |
               countline_id + month_day + weekday,
             data = df, cluster = ~countline_id)
  b <- coef(m); V <- vcov(m)
  combo <- function(label, comparison, terms) {
    L <- setNames(as.numeric(names(b) %in% terms), names(b))
    est <- sum(L * b); se <- sqrt(as.numeric(t(L) %*% V %*% L)); z <- est / se
    tibble(outcome = outcomes[[oc]], term = label, comparison = comparison,
           change_pct = round(pct(est), 1),
           lower95 = round(pct(est - 1.96 * se), 1),
           upper95 = round(pct(est + 1.96 * se), 1),
           p_value = round(2 * pnorm(-abs(z)), 4))
  }
  bind_rows(
    combo("City average change (benchmark)", "absolute", "post"),
    combo("12d total change", "absolute", c("post", "post:s12d")),
    combo("12f total change", "absolute", c("post", "post:s12f")),
    combo("12d vs city benchmark", "relative_to_city", "post:s12d"),
    combo("12f vs city benchmark", "relative_to_city", "post:s12f")
  )
}) |> bind_rows() |>
  mutate(pre_year = 2022, post_year = 2024, n_obs = nrow(df),
         model = "per-scheme interaction DiD, countline FE + matched month-day + weekday + weather; cluster SE by countline")

print(results, n = 100)
write_csv(results, file.path(ar, "vivacity_per_scheme_interaction_did_2022_2024.csv"))
cat("\nSaved per-scheme interaction results. N =", nrow(df),
    "| clusters =", n_distinct(df$countline_id), "\n")
