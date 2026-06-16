suppressPackageStartupMessages({
  library(data.table)
  library(jsonlite)
})

base_dir <- "/Users/lu_nanxi/CASA/Dissertation_Data"
report_dir <- file.path(base_dir, "reports", "analysis_reports")
share_dir <- file.path(
  base_dir,
  "output_visualised",
  "reports",
  "analysis_reports"
)
input_dir <- file.path(
  base_dir,
  "reports",
  "spreadsheets",
  "vivacity_five_scheme_official_hourly_inputs"
)
raw_dir <- file.path(
  base_dir,
  "Vivacity_LCR_official_hourly_2020_to_2025_05",
  "raw_csv"
)

checks <- list()
add_check <- function(section, check, passed, observed, expected) {
  checks[[length(checks) + 1L]] <<- data.table(
    section = section,
    check = check,
    passed = isTRUE(passed),
    observed = as.character(observed),
    expected = as.character(expected)
  )
}

raw_files <- list.files(
  raw_dir,
  pattern = "^classified-counts-.*\\.csv$",
  full.names = TRUE
)
add_check("Raw exports", "Official CSV file count", length(raw_files) == 90L,
  length(raw_files), 90L)

validation_path <- file.path(
  base_dir,
  "Vivacity_LCR_official_hourly_2020_to_2025_05",
  "qa",
  "vivacity_download_validation_summary.json"
)
validation <- fromJSON(validation_path)
add_check("Raw exports", "Validation reports no missing files",
  validation$missing_files == 0L, validation$missing_files, 0L)
add_check("Raw exports", "Actual rows equal expected rows",
  validation$total_data_rows == 63090288L,
  validation$total_data_rows, 63090288L)
add_check("Raw exports", "Validated countline inventory",
  validation$countlines == 711L,
  validation$countlines, 711L)

fixed <- fread(file.path(report_dir, "vivacity_fixed_year_average_comparison.csv"))
actual_pairs <- fixed[
  ,
  .(labels = paste(sort(unique(comparison_label)), collapse = ";")),
  by = analysis_scheme_id
][order(analysis_scheme_id)]
expected_pairs <- data.table(
  analysis_scheme_id = c("12b", "12d", "12e", "12f", "13"),
  labels = c(
    "2021 vs 2024;2022 vs 2025",
    "2021 vs 2024;2022 vs 2025",
    "2021 vs 2024;2022 vs 2025",
    "2021 vs 2024;2022 vs 2025",
    "2022 vs 2026;2023 vs 2025"
  )
)
add_check("Fixed-year comparison", "Requested year pairs",
  identical(actual_pairs, expected_pairs),
  paste(actual_pairs$labels, collapse = " | "),
  paste(expected_pairs$labels, collapse = " | "))

decomposition <- fread(file.path(
  report_dir,
  "vivacity_fixed_year_active_travel_decomposition.csv"
))
decomposition_error <- max(
  abs(c(
    decomposition$baseline_active_minus_components,
    decomposition$comparison_active_minus_components
  )),
  na.rm = TRUE
)
add_check("Fixed-year comparison", "Active travel decomposition",
  decomposition_error < 1e-9, decomposition_error, 0)

long_files <- c(
  "annual_scheme_outcomes_long.csv",
  "annual_scheme_route_outcomes_long.csv",
  "monthly_scheme_temporal_long.csv",
  "monthly_scheme_route_temporal_long.csv",
  "weekly_scheme_temporal_long.csv",
  "weekly_scheme_route_temporal_long.csv"
)
for (name in long_files) {
  dt <- fread(file.path(input_dir, name))
  keys <- setdiff(
    names(dt),
    c(
      "outcome",
      "outcome_label",
      "count_total",
      "average_per_observed_day",
      "count_per_observed_day"
    )
  )
  wide <- dcast(
    dt,
    as.formula(paste(paste(keys, collapse = " + "), "~ outcome")),
    value.var = "count_total",
    fun.aggregate = sum
  )
  difference <- max(
    abs(wide$active_travel_total - wide$pedestrian - wide$cyclist),
    na.rm = TRUE
  )
  add_check("Mode arithmetic", name, difference == 0, difference, 0)
}

eligibility <- fread(file.path(
  report_dir,
  "vivacity_other_lcr_2022_2024_sensor_eligibility.csv"
))
add_check("Citywide cohort", "Eligibility IDs are unique",
  uniqueN(eligibility$countline_id) == nrow(eligibility),
  uniqueN(eligibility$countline_id), nrow(eligibility))
add_check("Citywide cohort", "Eligibility metadata is complete",
  !anyNA(eligibility[, .(
    countline_id,
    countline_name,
    route_type,
    local_authority,
    lsoa21nm,
    hardware_name
  )]), sum(is.na(eligibility)), 0)
add_check("Citywide cohort", "All exported eligibility rows pass",
  all(eligibility$eligible_2022_2024), sum(eligibility$eligible_2022_2024),
  nrow(eligibility))

pairs <- fread(file.path(report_dir, "vivacity_imd_matched_sensor_pairs.csv"))
add_check("IMD pairs", "Countline IDs are not reused",
  uniqueN(pairs$countline_id) == nrow(pairs),
  uniqueN(pairs$countline_id), nrow(pairs))
add_check("IMD pairs", "Pair metadata is complete",
  !anyNA(pairs[, .(
    countline_id,
    pair_id,
    scheme_id,
    imd_role,
    treated_lsoa21nm,
    treated_imd_score,
    control_lsoa21nm,
    control_imd_score,
    countline_name,
    route_type,
    local_authority,
    lsoa21nm,
    hardware_name
  )]), sum(is.na(pairs)), 0)
add_check("IMD pairs", "Every pair has treated and control roles",
  all(pairs[, uniqueN(imd_role), by = pair_id]$V1 == 2L),
  paste(pairs[, uniqueN(imd_role), by = pair_id]$V1, collapse = ","), "2 per pair")

context_archive <- file.path(
  base_dir,
  "data_archives",
  "05_context_spatial_census_imd_data.zip"
)
context <- fread(cmd = sprintf(
  "unzip -p %s %s",
  shQuote(context_archive),
  shQuote("context_data/processed/matching/all_vivacity_countlines_lsoa2021_context.csv")
))[, .(countline_id = as.integer(countline_id), analysis_lsoa21cd)]
imd <- fread(cmd = sprintf(
  "unzip -p %s %s",
  shQuote(context_archive),
  shQuote("context_data/processed/imd/imd_2019_lcr_lsoa2021_collapsed.csv")
))[, .(analysis_lsoa21cd = LSOA21CD, source_imd_score = imd_score,
  source_imd_decile = imd_decile)]
source_scores <- merge(context, imd, by = "analysis_lsoa21cd")
pair_scores <- merge(pairs, source_scores, by = "countline_id", all.x = TRUE)
pair_scores[, claimed_imd_score := fifelse(
  imd_role == "Five scheme sensors",
  treated_imd_score,
  control_imd_score
)]
pair_scores[, claimed_imd_decile := fifelse(
  imd_role == "Five scheme sensors",
  treated_imd_decile,
  control_imd_decile
)]
score_error <- max(
  abs(pair_scores$claimed_imd_score - pair_scores$source_imd_score),
  na.rm = TRUE
)
decile_error <- sum(
  pair_scores$claimed_imd_decile != pair_scores$source_imd_decile,
  na.rm = TRUE
)
add_check("IMD pairs", "Published IMD scores match source",
  score_error == 0, score_error, 0)
add_check("IMD pairs", "Published IMD deciles match source",
  decile_error == 0, decile_error, 0)

context_change <- fread(file.path(
  report_dir,
  "vivacity_imd_matched_context_change_results.csv"
))
pair_change <- context_change[
  comparison_id %chin% c(
    "12d | Liverpool 052A",
    "12f | Wirral 006A"
  )
]
expected_combined <- pair_change[
  ,
  .(expected = 100 * (
    exp(mean(log1p(scheme_relative_to_control_percent / 100))) - 1
  )),
  by = outcome
]
published_combined <- context_change[
  comparison_id == "12d + 12f equal-weight",
  .(outcome, published = scheme_relative_to_control_percent)
]
combined_check <- merge(expected_combined, published_combined, by = "outcome")
combined_error <- max(abs(
  combined_check$expected - combined_check$published
))
add_check("IMD calculation", "Combined estimate is equal-context weighted",
  combined_error < 1e-9, combined_error, 0)

trajectory_selected <- fread(file.path(
  report_dir,
  "vivacity_preintervention_trajectory_selected_controls.csv"
))
add_check("Trajectory matching", "Six mode-specific controls selected",
  nrow(trajectory_selected) == 6L &&
    all(trajectory_selected[, .N, by = outcome]$N == 3L),
  nrow(trajectory_selected), 6L)
add_check("Trajectory matching", "Percentage-index log scale used",
  all(trajectory_selected$trajectory_scale ==
    "Pre-installation percentage index (mean = 100)") &&
    all(trajectory_selected$trajectory_log_definition ==
      "log(percentage index / 100)"),
  paste(unique(trajectory_selected$trajectory_log_definition), collapse = ";"),
  "log(percentage index / 100)")
add_check("Trajectory matching", "Road type excluded from selection",
  all(trajectory_selected$road_type_used_for_selection == FALSE),
  sum(trajectory_selected$road_type_used_for_selection), 0)
trajectory_gate_pass <- all(
  trajectory_selected$context_gate &
    trajectory_selected$hard_gate &
    trajectory_selected$common_pre_days >= 60L &
    trajectory_selected$pre_date_coverage >= 0.80 &
    trajectory_selected$minimum_month_coverage >= 0.60 &
    trajectory_selected$candidate_countline_coverage >= 0.75 &
    trajectory_selected$level_ratio >= 0.50 &
    trajectory_selected$level_ratio <= 2.00 &
    trajectory_selected$weekly_percentage_log_correlation >= 0
)
add_check("Trajectory matching", "All selected controls pass strict gates",
  trajectory_gate_pass, trajectory_gate_pass, TRUE)

trajectory_results <- fread(file.path(
  report_dir,
  "vivacity_preintervention_trajectory_results.csv"
))
trajectory_index_error <- max(abs(c(
  trajectory_results$scheme_pre_index - 100,
  trajectory_results$control_pre_index - 100
)))
add_check("Trajectory matching", "All comparison baselines indexed to 100",
  trajectory_index_error < 1e-9, trajectory_index_error, 0)

weather <- fread(file.path(report_dir, "vivacity_era5_weather_daily.csv"))
weather_missing <- sum(is.na(weather[, .(
  temperature_2m_mean,
  precipitation_sum,
  wind_speed_10m_max,
  shortwave_radiation_sum
)]))
add_check("Weather", "Unique weather site-date rows",
  !any(duplicated(weather, by = c("weather_site_id", "date"))),
  nrow(weather) - uniqueN(weather[, .(weather_site_id, date)]), 0)
add_check("Weather", "Weather values are complete",
  weather_missing == 0, weather_missing, 0)

report_path <- file.path(
  report_dir,
  "vivacity_before_after_peak_bar_graphs.html"
)
report_text <- paste(readLines(report_path, warn = FALSE), collapse = "\n")
required_panels <- c(
  "Scheme 12b: 2021 vs 2024",
  "Scheme 12b: 2022 vs 2025",
  "Scheme 13: 2022 vs 2026",
  "Scheme 13: 2023 vs 2025"
)
add_check("HTML report", "Requested fixed-year panels are present",
  all(vapply(required_panels, grepl, logical(1), x = report_text, fixed = TRUE)),
  sum(vapply(required_panels, grepl, logical(1), x = report_text, fixed = TRUE)),
  length(required_panels))
expected_month_day_label <- paste0(
  "12d: ",
  context_change[
    comparison_id == "12d | Liverpool 052A" &
      outcome == "active_travel_total",
    matched_month_days
  ],
  "; 12f: ",
  context_change[
    comparison_id == "12f | Wirral 006A" &
      outcome == "active_travel_total",
    matched_month_days
  ]
)
add_check("HTML report", "Combined IMD matched-day label is populated",
  grepl(expected_month_day_label, report_text, fixed = TRUE),
  grepl(expected_month_day_label, report_text, fixed = TRUE), TRUE)

result <- rbindlist(checks)
output_name <- "vivacity_analysis_integrity_audit.csv"
fwrite(result, file.path(report_dir, output_name))
fwrite(result, file.path(share_dir, output_name))

failed <- result[passed == FALSE]
if (nrow(failed) > 0L) {
  print(failed)
  stop(nrow(failed), " integrity checks failed.", call. = FALSE)
}

cat("All", nrow(result), "Vivacity integrity checks passed.\n")
