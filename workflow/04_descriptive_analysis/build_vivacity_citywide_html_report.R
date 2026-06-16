suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(jsonlite)
  library(lmtest)
  library(scales)
  library(sandwich)
})

base_dir <- "/Users/lu_nanxi/CASA/Dissertation_Data"
raw_dir <- file.path(
  base_dir,
  "Vivacity_LCR_official_hourly_2020_to_2025_05",
  "raw_csv"
)
input_dir <- file.path(
  base_dir,
  "reports",
  "spreadsheets",
  "vivacity_five_scheme_official_hourly_inputs"
)
output_dir <- file.path(base_dir, "reports", "analysis_reports")
output_visualised_dir <- file.path(
  base_dir,
  "output_visualised",
  "reports",
  "analysis_reports"
)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(output_visualised_dir, recursive = TRUE, showWarnings = FALSE)

assert_true <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
}

report_name <- "vivacity_before_after_peak_bar_graphs.html"
report_path <- file.path(output_dir, report_name)

treated_ids <- fread(file.path(input_dir, "treated_countline_coverage.csv"))
treated_ids[, countline_id := as.integer(countline_id)]
treated_ids[, first_reliable_date := as.IDate(first_reliable_date)]
treated_lookup <- unique(treated_ids[, .(
  countline_id,
  analysis_scheme_id,
  scheme_name,
  treated_road_group,
  route_group,
  first_reliable_date,
  confirmed_intervention_date
)])
assert_true(
  uniqueN(treated_lookup$countline_id) == nrow(treated_lookup),
  "Treated countline lookup contains duplicate countline IDs."
)
assert_true(
  !anyNA(treated_lookup[, .(
    countline_id,
    analysis_scheme_id,
    first_reliable_date,
    confirmed_intervention_date
  )]),
  "Treated countline lookup contains missing required identifiers or dates."
)

fixed_comparison <- fread(file.path(
  output_dir,
  "vivacity_fixed_year_average_comparison.csv"
))
setnames(
  fixed_comparison,
  c("intervention_date", "difference_comparison_minus_baseline"),
  c("confirmed_intervention_date", "difference")
)
fixed_comparison[, confirmed_intervention_date :=
  as.IDate(confirmed_intervention_date)]
fixed_comparison[, data_status := fifelse(
  !is.na(baseline_average_per_observed_day) &
    !is.na(comparison_average_per_observed_day),
  "Available",
  "Unavailable"
)]
expected_fixed_comparisons <- data.table(
  analysis_scheme_id = c("12b", "12d", "12e", "12f", "13"),
  expected_labels = c(
    "2021 vs 2024;2022 vs 2025",
    "2021 vs 2024;2022 vs 2025",
    "2021 vs 2024;2022 vs 2025",
    "2021 vs 2024;2022 vs 2025",
    "2022 vs 2026;2023 vs 2025"
  )
)
actual_fixed_comparisons <- fixed_comparison[
  ,
  .(expected_labels = paste(sort(unique(comparison_label)), collapse = ";")),
  by = analysis_scheme_id
]
assert_true(
  identical(
    expected_fixed_comparisons[order(analysis_scheme_id)],
    actual_fixed_comparisons[order(analysis_scheme_id)]
  ),
  "Fixed-year comparison table does not contain the requested year pairs."
)

context_archive <- file.path(base_dir, "data_archives", "05_context_spatial_census_imd_data.zip")
context_member <- "context_data/processed/matching/all_vivacity_countlines_lsoa2021_context.csv"
context <- fread(cmd = sprintf(
  "unzip -p %s %s",
  shQuote(context_archive),
  shQuote(context_member)
))
context <- context[, .(
  countline_id = as.integer(countline_id),
  context_countline_name = countline_name,
  route_type = fifelse(
    route_type_guess %chin% c("Road", "Path/cycle facility"),
    route_type_guess,
    "Other / crossing"
  ),
  local_authority = LAD22NM,
  lsoa21nm = analysis_lsoa21nm,
  longitude = as.numeric(countline_midpoint_lon),
  latitude = as.numeric(countline_midpoint_lat),
  hardware_name,
  hardware_status
)]
assert_true(
  uniqueN(context$countline_id) == nrow(context),
  "Archived context table contains duplicate countline IDs."
)

supplemental_inventory <- file.path(
  base_dir,
  "Vivacity_LCR_all_sensors_2021_to_2026_05",
  "supplemental_boundary_countline_46441",
  "vivacity_lcr_supplemental_boundary_inventory.csv"
)
if (file.exists(supplemental_inventory)) {
  supplemental <- fread(supplemental_inventory)
  supplemental <- supplemental[, .(
    countline_id = as.integer(countline_id),
    context_countline_name = countline_name,
    route_type = fifelse(
      grepl("path|cycle", tolower(countline_name)),
      "Path/cycle facility",
      fifelse(grepl("road|rd", tolower(countline_name)), "Road", "Other / crossing")
    ),
    local_authority = as.character(lad),
    lsoa21nm = as.character(nearest_lsoa21nm),
    longitude = as.numeric(longitude),
    latitude = as.numeric(latitude),
    hardware_name = as.character(hardware_name),
    hardware_status = as.character(hardware_status)
  )]
  context <- unique(rbindlist(list(context, supplemental), use.names = TRUE, fill = TRUE))
}
assert_true(
  uniqueN(context$countline_id) == nrow(context),
  "Combined context table contains duplicate countline IDs."
)

files <- list.files(raw_dir, pattern = "^classified-counts-.*\\.csv$", full.names = TRUE)
assert_true(length(files) == 90L, "Expected 90 validated Vivacity raw CSV exports.")
citywide_daily_cache_path <- file.path(
  output_dir,
  "vivacity_citywide_quality_gated_daily_cache.csv.gz"
)
analysis_daily_cache_path <- file.path(
  output_dir,
  "vivacity_analysis_eligible_daily_cache.csv.gz"
)
select_cols <- c(
  "UTC Datetime", "Local Datetime", "countlineId", "countlineName",
  "dataAvailabilityPercent", "dataError", "direction", "Pedestrian", "Cyclist"
)
cache_required_columns <- c(
  "countline_id",
  "date",
  "availability_hourly_periods_observed",
  "count_hourly_periods_observed",
  "pedestrian",
  "cyclist",
  "active_travel_total",
  "sensor_group",
  "quality_ok_day"
)
cache_is_current <- FALSE
if (file.exists(citywide_daily_cache_path)) {
  cache_command <- sprintf("gzip -dc %s", shQuote(citywide_daily_cache_path))
  cache_names <- names(fread(cmd = cache_command, nrows = 0L))
  cache_is_current <-
    all(cache_required_columns %chin% cache_names) &&
    file.info(citywide_daily_cache_path)$mtime >=
      max(file.info(files)$mtime)
}

if (cache_is_current) {
  message("Using current quality-gated citywide daily cache.")
  daily_valid <- fread(cmd = cache_command)
  daily_valid[, date := as.IDate(date)]
  daily_valid[, month_start := as.IDate(month_start)]
  daily_valid[, first_reliable_date := as.IDate(first_reliable_date)]
  missing_scheme_columns <- setdiff(
    c("scheme_name", "treated_road_group", "confirmed_intervention_date"),
    names(daily_valid)
  )
  if (length(missing_scheme_columns) > 0L) {
    daily_valid <- merge(
      daily_valid,
      treated_lookup[, c("countline_id", missing_scheme_columns), with = FALSE],
      by = "countline_id",
      all.x = TRUE
    )
  }
  daily_valid[, confirmed_intervention_date :=
    as.IDate(confirmed_intervention_date)]
} else {
  daily_parts <- vector("list", length(files))
  for (i in seq_along(files)) {
  message(sprintf("[%d/%d] %s", i, length(files), basename(files[i])))
  dt <- fread(files[i], select = select_cols, showProgress = FALSE)
  setnames(dt, c(
    "utc_datetime", "local_datetime", "countline_id", "countline_name",
    "availability", "data_error", "direction", "pedestrian", "cyclist"
  ))
  dt[, date := as.IDate(substr(local_datetime, 1, 10))]
  dt[, availability := suppressWarnings(as.numeric(
    fifelse(as.character(availability) == "NULL", NA_character_, as.character(availability))
  ))]
  dt[, data_error := tolower(as.character(data_error)) == "true"]
  dt[, pedestrian := suppressWarnings(as.numeric(pedestrian))]
  dt[, cyclist := suppressWarnings(as.numeric(cyclist))]
  assert_true(
    !any(duplicated(dt, by = c(
      "countline_id",
      "utc_datetime",
      "direction"
    ))),
    paste("Duplicate countline-hour-direction records in", basename(files[i]))
  )
  daily_parts[[i]] <- dt[
    ,
    .(
      countline_name = first(na.omit(countline_name)),
      hourly_periods_observed = uniqueN(utc_datetime),
      availability_hourly_periods_observed =
        uniqueN(utc_datetime[!is.na(availability)]),
      count_hourly_periods_observed =
        uniqueN(utc_datetime[!is.na(pedestrian) & !is.na(cyclist)]),
      data_availability_percent_min = suppressWarnings(min(availability, na.rm = TRUE)),
      data_error_any = any(data_error, na.rm = TRUE),
      has_counts = any(!is.na(pedestrian) & !is.na(cyclist)),
      pedestrian = sum(pedestrian, na.rm = TRUE),
      cyclist = sum(cyclist, na.rm = TRUE)
    ),
    by = .(countline_id, date)
  ]
  }

  daily <- rbindlist(daily_parts, use.names = TRUE, fill = TRUE)
  assert_true(
    !any(duplicated(daily, by = c("countline_id", "date"))),
    "Daily aggregation produced duplicate countline-date rows."
  )
  daily[is.infinite(data_availability_percent_min), data_availability_percent_min := NA_real_]
  daily[, countline_id := as.integer(countline_id)]
  daily[, active_travel_total := pedestrian + cyclist]
  daily <- merge(daily, context, by = "countline_id", all.x = TRUE)
  assert_true(
    !anyNA(daily$context_countline_name),
    "One or more raw countlines failed to join to the context inventory."
  )
  daily <- merge(
    daily,
    treated_lookup[, .(
      countline_id,
      analysis_scheme_id,
      scheme_name,
      treated_road_group,
      confirmed_intervention_date,
      first_reliable_date,
      treated_route_group = route_group
    )],
    by = "countline_id",
    all.x = TRUE
  )
  assert_true(
    all(
      daily[
        countline_id %in% treated_lookup$countline_id,
        !is.na(analysis_scheme_id) & !is.na(first_reliable_date)
      ]
    ),
    "One or more treated countlines failed to join to the treated lookup."
  )
  daily[!is.na(treated_route_group), route_type := treated_route_group]
  daily[is.na(route_type), route_type := fifelse(
    grepl("path|cycle", tolower(countline_name)),
    "Path/cycle facility",
    fifelse(grepl("road|rd", tolower(countline_name)), "Road", "Other / crossing")
  )]
  daily[is.na(local_authority) | local_authority == "", local_authority := "Unassigned boundary"]
  daily[, sensor_group := fifelse(
    countline_id %in% treated_lookup$countline_id,
    "Five scheme sensors",
    "Other LCR sensors"
  )]
  daily[, quality_ok_day :=
    hourly_periods_observed >= 23 &
    availability_hourly_periods_observed >= 23 &
    count_hourly_periods_observed >= 23 &
    !is.na(data_availability_percent_min) &
    data_availability_percent_min >= 80 &
    !data_error_any &
    has_counts &
    (sensor_group == "Other LCR sensors" |
      (!is.na(first_reliable_date) & date >= first_reliable_date))
  ]
  daily_valid <- daily[quality_ok_day == TRUE]
}
assert_true(
  all(daily_valid$active_travel_total ==
    daily_valid$pedestrian + daily_valid$cyclist),
  "Active-travel totals do not equal pedestrian plus cyclist counts."
)
daily_valid[, month_start := as.IDate(format(date, "%Y-%m-01"))]
daily_valid[, calendar_year := as.integer(format(date, "%Y"))]

outcomes <- c("pedestrian", "cyclist", "active_travel_total")
outcome_labels <- c(
  pedestrian = "Pedestrian",
  cyclist = "Cyclist",
  active_travel_total = "Active travel"
)

monthly_scheme_base <- daily_valid[
  sensor_group == "Five scheme sensors",
  .(
    countlines = uniqueN(countline_id),
    observed_countline_days = .N,
    pedestrian = sum(pedestrian),
    cyclist = sum(cyclist),
    active_travel_total = sum(active_travel_total)
  ),
  by = .(
    analysis_scheme_id,
    scheme_name,
    treated_road_group,
    confirmed_intervention_date,
    month_start
  )
]
monthly_scheme <- melt(
  monthly_scheme_base,
  id.vars = c(
    "analysis_scheme_id",
    "scheme_name",
    "treated_road_group",
    "confirmed_intervention_date",
    "month_start",
    "countlines",
    "observed_countline_days"
  ),
  measure.vars = outcomes,
  variable.name = "outcome",
  value.name = "count_total"
)
monthly_scheme[, outcome_label := outcome_labels[outcome]]
monthly_scheme[, count_per_observed_day :=
  count_total / observed_countline_days]

monthly_scheme_route_base <- daily_valid[
  sensor_group == "Five scheme sensors",
  .(
    countlines = uniqueN(countline_id),
    observed_countline_days = .N,
    pedestrian = sum(pedestrian),
    cyclist = sum(cyclist),
    active_travel_total = sum(active_travel_total)
  ),
  by = .(
    analysis_scheme_id,
    scheme_name,
    treated_road_group,
    confirmed_intervention_date,
    route_group = route_type,
    month_start
  )
]
monthly_scheme_route <- melt(
  monthly_scheme_route_base,
  id.vars = c(
    "analysis_scheme_id",
    "scheme_name",
    "treated_road_group",
    "confirmed_intervention_date",
    "route_group",
    "month_start",
    "countlines",
    "observed_countline_days"
  ),
  measure.vars = outcomes,
  variable.name = "outcome",
  value.name = "count_total"
)
monthly_scheme_route[, outcome_label := outcome_labels[outcome]]
monthly_scheme_route[, count_per_observed_day :=
  count_total / observed_countline_days]

imd_pair_specs <- data.table(
  pair_id = c(
    "12b | Knowsley 017B",
    "12d | Liverpool 052A",
    "12e | St. Helens 008C",
    "12e | St. Helens 014D",
    "12f | Wirral 006A",
    "13 | Halton 010B"
  ),
  scheme_id = c("12b", "12d", "12e", "12e", "12f", "13"),
  treated_lsoa21nm = c(
    "Knowsley 017B",
    "Liverpool 052A",
    "St. Helens 008C",
    "St. Helens 014D",
    "Wirral 006A",
    "Halton 010B"
  ),
  treated_imd_score = c(14.604, 26.670, 51.739, 75.142, 12.143, 64.805),
  treated_imd_decile = c(6L, 3L, 1L, 1L, 7L, 1L),
  control_street = c(
    "Liver St near Park Ln",
    "Clock Face Rd near Gartons Ln",
    "King St near Wellington St",
    "Stockbridge Ln near Waterpark Dr",
    "Harrison Dr",
    "West Derby Rd near Sheil Rd (both directions)"
  ),
  control_lsoa21nm = c(
    "Liverpool 061C",
    "St. Helens 020F",
    "Sefton 004H",
    "Knowsley 008F",
    "Wirral 003B",
    "Liverpool 024D"
  ),
  control_imd_score = c(15.028, 25.825, 55.692, 77.148, 9.320, 64.417),
  control_imd_decile = c(6L, 4L, 1L, 1L, 8L, 1L),
  pre_year = c(NA_integer_, 2022L, NA_integer_, NA_integer_, 2022L, 2023L),
  post_year = c(NA_integer_, 2024L, NA_integer_, NA_integer_, 2024L, 2024L),
  window_start = c(NA_character_, "11-15", NA_character_, NA_character_, "11-15", "09-27"),
  window_end = c(NA_character_, "12-31", NA_character_, NA_character_, "12-31", "12-31")
)
imd_pair_specs[, absolute_imd_score_difference :=
  abs(treated_imd_score - control_imd_score)]

make_imd_pair_lookup <- function(pair_id, scheme_id, treated_ids, control_ids) {
  rbindlist(list(
    data.table(
      pair_id = pair_id,
      scheme_id = scheme_id,
      imd_role = "Five scheme sensors",
      countline_id = treated_ids
    ),
    data.table(
      pair_id = pair_id,
      scheme_id = scheme_id,
      imd_role = "IMD-matched sensors",
      countline_id = control_ids
    )
  ))
}

imd_pair_lookup <- rbindlist(list(
  make_imd_pair_lookup(
    "12b | Knowsley 017B", "12b",
    c(46676L, 46686L, 46687L, 46688L),
    c(16364L, 16365L, 16388L, 16389L)
  ),
  make_imd_pair_lookup(
    "12d | Liverpool 052A", "12d",
    c(16373L, 16374L, 16393L, 16394L, 23779L),
    c(22898L, 22899L, 22900L)
  ),
  make_imd_pair_lookup(
    "12e | St. Helens 008C", "12e",
    c(51817L, 51818L),
    c(40740L, 40742L)
  ),
  make_imd_pair_lookup(
    "12e | St. Helens 014D", "12e",
    c(51794L, 51795L, 51796L),
    c(23811L, 23812L, 23813L)
  ),
  make_imd_pair_lookup(
    "12f | Wirral 006A", "12f",
    c(23799L, 23800L, 23801L),
    c(23802L, 23803L, 23804L)
  ),
  make_imd_pair_lookup(
    "13 | Halton 010B", "13",
    c(47768L, 47769L, 47772L, 47773L),
    c(15104L, 15106L, 15160L, 15161L, 15162L)
  )
))
assert_true(
  uniqueN(imd_pair_lookup$countline_id) == nrow(imd_pair_lookup),
  "An IMD countline is assigned to more than one matched pair."
)
assert_true(
  all(imd_pair_lookup$scheme_id %chin% imd_pair_specs$scheme_id),
  "IMD pair lookup contains an unknown scheme ID."
)

weather_start_date <- as.IDate("2020-01-01")
weather_end_date <- as.IDate("2025-05-31")
weather_daily_cache_path <- file.path(
  output_dir,
  "vivacity_era5_weather_daily.csv"
)

weather_sites <- merge(
  imd_pair_lookup,
  unique(context[, .(
    countline_id,
    longitude,
    latitude
  )]),
  by = "countline_id",
  all.x = TRUE
)[
  ,
  .(
    requested_longitude = mean(longitude, na.rm = TRUE),
    requested_latitude = mean(latitude, na.rm = TRUE),
    countlines = uniqueN(countline_id)
  ),
  by = .(pair_id, scheme_id, imd_role)
]
assert_true(
  !anyNA(weather_sites[, .(
    requested_longitude,
    requested_latitude
  )]),
  "One or more IMD weather sites has missing coordinates after the context join."
)
setorder(weather_sites, pair_id, imd_role)
weather_sites[, weather_site_id := sprintf("imd_context_%02d", .I)]

fetch_weather_sites <- function(sites, max_attempts = 5L) {
  query_url <- paste0(
    "https://archive-api.open-meteo.com/v1/archive?",
    "latitude=", paste(sprintf("%.6f", sites$requested_latitude), collapse = ","),
    "&longitude=", paste(sprintf("%.6f", sites$requested_longitude), collapse = ","),
    "&start_date=", weather_start_date,
    "&end_date=", weather_end_date,
    "&daily=temperature_2m_mean,precipitation_sum,",
    "wind_speed_10m_max,shortwave_radiation_sum",
    "&timezone=Europe%2FLondon",
    "&models=era5"
  )
  response <- NULL
  last_error <- NULL
  for (attempt in seq_len(max_attempts)) {
    response <- tryCatch(
      jsonlite::fromJSON(query_url, simplifyVector = FALSE),
      error = function(e) {
        last_error <<- conditionMessage(e)
        NULL
      }
    )
    if (!is.null(response)) break
    Sys.sleep(10 * attempt)
  }
  if (is.null(response)) {
    stop(
      "Open-Meteo multi-site weather download failed after ",
      max_attempts,
      " attempts: ",
      last_error
    )
  }
  if (length(sites$weather_site_id) == 1L && !is.null(response$daily)) {
    response <- list(response)
  }
  if (length(response) != nrow(sites)) {
    stop(
      "Open-Meteo returned ",
      length(response),
      " locations for ",
      nrow(sites),
      " requested weather sites."
    )
  }
  rbindlist(lapply(seq_len(nrow(sites)), function(i) {
    location <- response[[i]]
    weather <- as.data.table(lapply(location$daily, unlist))
    weather[, `:=`(
      weather_site_id = sites$weather_site_id[i],
      date = as.IDate(time),
      requested_longitude = sites$requested_longitude[i],
      requested_latitude = sites$requested_latitude[i],
      returned_grid_longitude = as.numeric(location$longitude),
      returned_grid_latitude = as.numeric(location$latitude),
      source_model = "ERA5 via Open-Meteo",
      timezone = "Europe/London"
    )]
    weather[, .(
      weather_site_id,
      date,
      temperature_2m_mean,
    precipitation_sum,
    wind_speed_10m_max,
    shortwave_radiation_sum,
      requested_longitude,
      requested_latitude,
      returned_grid_longitude,
      returned_grid_latitude,
      source_model,
      timezone
    )]
  }))
}

weather_cache_is_complete <- FALSE
if (file.exists(weather_daily_cache_path)) {
  weather_daily <- fread(weather_daily_cache_path)
  weather_daily[, date := as.IDate(date)]
  weather_cache_is_complete <-
    uniqueN(weather_daily$weather_site_id) == nrow(weather_sites) &&
    min(weather_daily$date) <= weather_start_date &&
    max(weather_daily$date) >= weather_end_date
}
if (!weather_cache_is_complete) {
  weather_daily <- fetch_weather_sites(weather_sites)
  fwrite(weather_daily, weather_daily_cache_path)
}
assert_true(
  !any(duplicated(weather_daily, by = c("weather_site_id", "date"))),
  "Weather cache contains duplicate site-date rows."
)
assert_true(
  !anyNA(weather_daily[, .(
    temperature_2m_mean,
    precipitation_sum,
    wind_speed_10m_max,
    shortwave_radiation_sum
  )]),
  "Weather cache contains missing daily weather values."
)

weather_site_lookup <- merge(
  imd_pair_lookup,
  weather_sites[, .(
    pair_id,
    scheme_id,
    imd_role,
    weather_site_id,
    requested_longitude,
    requested_latitude
  )],
  by = c("pair_id", "scheme_id", "imd_role"),
  all.x = TRUE
)

weather_lcr_daily <- weather_daily[
  ,
  .(
    temperature_2m_mean = mean(temperature_2m_mean, na.rm = TRUE),
    precipitation_sum = mean(precipitation_sum, na.rm = TRUE),
    wind_speed_10m_max = mean(wind_speed_10m_max, na.rm = TRUE),
    shortwave_radiation_sum = mean(shortwave_radiation_sum, na.rm = TRUE)
  ),
  by = date
]

add_weather_terms <- function(dt) {
  result <- copy(dt)
  result[, precipitation_log := log1p(pmax(precipitation_sum, 0))]
  source_columns <- c(
    "temperature_2m_mean",
    "precipitation_log",
    "wind_speed_10m_max",
    "shortwave_radiation_sum"
  )
  target_columns <- c(
    "temperature_z",
    "precipitation_z",
    "wind_z",
    "radiation_z"
  )
  for (i in seq_along(source_columns)) {
    values <- result[[source_columns[i]]]
    spread <- sd(values, na.rm = TRUE)
    if (!is.finite(spread) || spread == 0) {
      result[, (target_columns[i]) := 0]
    } else {
      result[, (target_columns[i]) :=
        (get(source_columns[i]) - mean(values, na.rm = TRUE)) / spread]
    }
  }
  result
}

other_sensor_eligibility <- daily_valid[
  sensor_group == "Other LCR sensors" & calendar_year %in% c(2022L, 2024L),
  .(
    valid_days_2022 = sum(calendar_year == 2022L),
    valid_days_2024 = sum(calendar_year == 2024L),
    first_valid_date = min(date),
    last_valid_date = max(date),
    countline_name = first(na.omit(countline_name)),
    route_type = first(na.omit(route_type)),
    local_authority = first(na.omit(local_authority)),
    lsoa21nm = first(na.omit(lsoa21nm)),
    hardware_name = first(na.omit(hardware_name))
  ),
  by = countline_id
]
other_sensor_eligibility[, eligible_2022_2024 :=
  valid_days_2022 > 0 & valid_days_2024 > 0]
eligible_other_ids <- other_sensor_eligibility[
  eligible_2022_2024 == TRUE,
  countline_id
]
eligible_other_count <- length(eligible_other_ids)
all_other_count <- uniqueN(
  daily_valid[sensor_group == "Other LCR sensors", countline_id]
)
excluded_other_count <- all_other_count - eligible_other_count
analysis_daily <- daily_valid[
  sensor_group == "Five scheme sensors" | countline_id %in% eligible_other_ids
]
fwrite(daily_valid, citywide_daily_cache_path)
fwrite(analysis_daily, analysis_daily_cache_path)

regression_source <- copy(analysis_daily[
  calendar_year %in% c(2022L, 2024L) &
    (
      sensor_group == "Other LCR sensors" |
        analysis_scheme_id %chin% c("12d", "12f")
    )
])
regression_source[, `:=`(
  month_day = format(date, "%m-%d"),
  weekday = factor(as.integer(format(date, "%u"))),
  post = as.integer(calendar_year == 2024L),
  treated = as.integer(
    sensor_group == "Five scheme sensors" &
      analysis_scheme_id %chin% c("12d", "12f")
  )
)]
regression_source <- regression_source[
  month_day >= "11-15" & month_day <= "12-31"
]
regression_pairs <- unique(regression_source[
  ,
  .(countline_id, treated, calendar_year, month_day)
])
regression_pair_wide <- dcast(
  regression_pairs,
  countline_id + treated + month_day ~ calendar_year,
  fun.aggregate = length,
  value.var = "calendar_year"
)
regression_pair_wide <- regression_pair_wide[`2022` > 0 & `2024` > 0]
regression_sensor_coverage <- regression_pair_wide[
  ,
  .(paired_month_days = .N),
  by = .(countline_id, treated)
][paired_month_days >= 20]
regression_daily <- merge(
  regression_source,
  regression_sensor_coverage[, .(countline_id, treated, paired_month_days)],
  by = c("countline_id", "treated")
)
regression_daily <- merge(
  regression_daily,
  regression_pair_wide[
    countline_id %in% regression_sensor_coverage$countline_id,
    .(countline_id, month_day)
  ],
  by = c("countline_id", "month_day")
)
regression_daily <- merge(
  regression_daily,
  weather_lcr_daily,
  by = "date",
  all.x = TRUE
)
assert_true(
  !anyNA(regression_daily[, .(
    countline_id,
    date,
    pedestrian,
    cyclist,
    active_travel_total,
    temperature_2m_mean,
    precipitation_sum,
    wind_speed_10m_max,
    shortwave_radiation_sum
  )]),
  "LCR regression data contains a failed count, date, or weather join."
)
regression_daily <- add_weather_terms(regression_daily)
regression_daily[, treated_post := post * treated]

fit_baseline_model <- function(outcome, include_weather = FALSE) {
  dat <- copy(regression_daily)
  dat[, log_outcome := log1p(pmax(get(outcome), 0))]
  model_terms <- c(
    "post",
    "treated_post",
    "factor(countline_id)",
    "factor(month_day)",
    "weekday"
  )
  if (include_weather) {
    model_terms <- c(
      model_terms,
      "temperature_z",
      "precipitation_z",
      "wind_z",
      "radiation_z"
    )
  }
  model <- lm(
    reformulate(model_terms, response = "log_outcome"),
    data = dat
  )
  robust_vcov <- sandwich::vcovCL(
    model,
    cluster = dat$countline_id,
    type = "HC1"
  )
  robust_test <- lmtest::coeftest(model, vcov. = robust_vcov)
  lcr_log <- coef(model)[["post"]]
  extra_log <- coef(model)[["treated_post"]]
  lcr_se <- sqrt(robust_vcov["post", "post"])
  extra_se <- sqrt(robust_vcov["treated_post", "treated_post"])
  scheme_log <- lcr_log + extra_log
  scheme_se <- sqrt(
    robust_vcov["post", "post"] +
      robust_vcov["treated_post", "treated_post"] +
      2 * robust_vcov["post", "treated_post"]
  )
  sensor_year <- dat[
    ,
    .(mean_log_outcome = mean(log_outcome)),
    by = .(countline_id, treated, calendar_year)
  ]
  sensor_change <- dcast(
    sensor_year,
    countline_id + treated ~ calendar_year,
    value.var = "mean_log_outcome"
  )
  sensor_change[, log_change := `2024` - `2022`]
  sensor_test <- t.test(
    sensor_change[treated == 1L, log_change],
    sensor_change[treated == 0L, log_change]
  )
  sensor_relative_log <- mean(
    sensor_change[treated == 1L, log_change]
  ) - mean(
    sensor_change[treated == 0L, log_change]
  )
  pct <- function(x) 100 * (exp(x) - 1)
  data.table(
    outcome = outcome,
    outcome_label = outcome_labels[[outcome]],
    lcr_adjusted_change_percent = pct(lcr_log),
    lcr_lower_95 = pct(lcr_log - 1.96 * lcr_se),
    lcr_upper_95 = pct(lcr_log + 1.96 * lcr_se),
    scheme_adjusted_change_percent = pct(scheme_log),
    scheme_lower_95 = pct(scheme_log - 1.96 * scheme_se),
    scheme_upper_95 = pct(scheme_log + 1.96 * scheme_se),
    scheme_relative_to_lcr_percent = pct(extra_log),
    relative_lower_95 = pct(extra_log - 1.96 * extra_se),
    relative_upper_95 = pct(extra_log + 1.96 * extra_se),
    relative_p_value = robust_test["treated_post", 4],
    sensor_level_relative_percent = pct(sensor_relative_log),
    sensor_level_lower_95 = pct(sensor_test$conf.int[1]),
    sensor_level_upper_95 = pct(sensor_test$conf.int[2]),
    sensor_level_p_value = sensor_test$p.value,
    treated_countlines = uniqueN(dat[treated == 1L, countline_id]),
    control_countlines = uniqueN(dat[treated == 0L, countline_id]),
    paired_daily_rows = nrow(dat),
    adjusted_r_squared = summary(model)$adj.r.squared,
    weather_adjusted = include_weather
  )
}
regression_results <- rbindlist(lapply(outcomes, fit_baseline_model))
regression_results[, conclusion := fifelse(
  scheme_relative_to_lcr_percent >= 0,
  "Scheme trajectory higher than LCR comparison trend",
  "Scheme trajectory lower than LCR comparison trend"
)]
weather_regression_results <- rbindlist(lapply(
  outcomes,
  function(outcome) fit_baseline_model(outcome, include_weather = TRUE)
))
weather_regression_comparison <- merge(
  regression_results[, .(
    outcome,
    outcome_label,
    unadjusted_relative_percent = scheme_relative_to_lcr_percent
  )],
  weather_regression_results[, .(
    outcome,
    weather_adjusted_relative_percent = scheme_relative_to_lcr_percent,
    weather_adjusted_lower_95 = relative_lower_95,
    weather_adjusted_upper_95 = relative_upper_95,
    weather_adjusted_p_value = relative_p_value
  )],
  by = "outcome",
  all = TRUE
)
weather_regression_comparison[, `:=`(
  outcome_label = outcome_labels[outcome],
  benchmark = "Eligible LCR sensors",
  adjustment_shift_percentage_points =
    weather_adjusted_relative_percent - unadjusted_relative_percent
)]
regression_cohort <- unique(regression_daily[
  ,
  .(
    countline_id,
    group = fifelse(treated == 1L, "Schemes 12d and 12f", "Eligible other LCR sensors"),
    analysis_scheme_id,
    paired_month_days
  )
])[order(group, analysis_scheme_id, countline_id)]

imd_daily <- merge(
  daily_valid,
  imd_pair_lookup,
  by = "countline_id",
  allow.cartesian = TRUE
)
imd_daily <- merge(
  imd_daily,
  imd_pair_specs,
  by = c("pair_id", "scheme_id"),
  all.x = TRUE
)
imd_daily <- merge(
  imd_daily,
  weather_sites[, .(
    pair_id,
    scheme_id,
    imd_role,
    weather_site_id
  )],
  by = c("pair_id", "scheme_id", "imd_role"),
  all.x = TRUE
)
imd_daily <- merge(
  imd_daily,
  weather_daily[, .(
    weather_site_id,
    date,
    temperature_2m_mean,
    precipitation_sum,
    wind_speed_10m_max,
    shortwave_radiation_sum
  )],
  by = c("weather_site_id", "date"),
  all.x = TRUE
)
assert_true(
  !anyNA(imd_daily[, .(
    pair_id,
    scheme_id,
    imd_role,
    treated_imd_score,
    control_imd_score,
    weather_site_id,
    temperature_2m_mean,
    precipitation_sum,
    wind_speed_10m_max,
    shortwave_radiation_sum
  )]),
  "IMD daily data contains a failed pair, score, or weather join."
)
imd_daily[, `:=`(
  month_day = format(date, "%m-%d"),
  weekday = factor(as.integer(format(date, "%u")))
)]

imd_composition_coverage <- imd_daily[
  ,
  .(roles_available = uniqueN(imd_role)),
  by = .(pair_id, date)
]
imd_composition_dates <- sort(imd_composition_coverage[
  roles_available == 2L,
  .(pairs_available = uniqueN(pair_id)),
  by = date
][pairs_available == uniqueN(imd_pair_specs$pair_id), date])
imd_composition_daily <- imd_daily[date %in% imd_composition_dates]
imd_composition_start <- min(imd_composition_dates)
imd_composition_end <- max(imd_composition_dates)
imd_composition_day_count <- length(imd_composition_dates)

summarise_imd_composition <- function(dt, group_columns) {
  base <- dt[
    ,
    .(
      countlines = uniqueN(countline_id),
      observed_countline_days = .N,
      pedestrian = sum(pedestrian),
      cyclist = sum(cyclist),
      active_travel_total = sum(active_travel_total)
    ),
    by = c(group_columns, "imd_role")
  ]
  long <- melt(
    base,
    id.vars = c(group_columns, "imd_role", "countlines", "observed_countline_days"),
    measure.vars = outcomes,
    variable.name = "outcome",
    value.name = "count_total"
  )
  long[, outcome_label := outcome_labels[outcome]]
  long[, average_per_observed_day := count_total / observed_countline_days]
  long[
    ,
    active_travel_average :=
      average_per_observed_day[outcome == "active_travel_total"],
    by = c(group_columns, "imd_role")
  ]
  long[, percentage_of_group_active_travel :=
    100 * average_per_observed_day / active_travel_average]
  long
}

imd_composition_by_pair <- summarise_imd_composition(
  imd_composition_daily,
  "pair_id"
)
imd_composition_by_pair <- merge(
  imd_composition_by_pair,
  unique(imd_pair_specs[, .(pair_id, scheme_id)]),
  by = "pair_id",
  all.x = TRUE
)
imd_composition_by_scheme <- summarise_imd_composition(
  imd_composition_daily,
  "scheme_id"
)
imd_composition_overall <- imd_composition_by_scheme[
  ,
  .(
    countlines = sum(countlines),
    observed_countline_days = sum(observed_countline_days),
    count_total = sum(count_total),
    average_per_observed_day = mean(average_per_observed_day),
    active_travel_average = mean(active_travel_average),
    percentage_of_group_active_travel =
      mean(percentage_of_group_active_travel)
  ),
  by = .(imd_role, outcome, outcome_label)
]
imd_composition_overall[, comparison_group := "Equal-weight five-scheme average"]
imd_composition_results <- rbindlist(list(
  imd_composition_overall[, `:=`(
    comparison_level = "Overall equal-scheme",
    pair_id = NA_character_,
    scheme_id = NA_character_
  )],
  imd_composition_by_scheme[, `:=`(
    comparison_level = "Scheme",
    comparison_group = scheme_id,
    pair_id = NA_character_
  )],
  imd_composition_by_pair[, `:=`(
    comparison_level = "Matched context",
    comparison_group = pair_id
  )]
), use.names = TRUE, fill = TRUE)
imd_composition_results[, `:=`(
  matched_calendar_days = imd_composition_day_count,
  matched_first_date = imd_composition_start,
  matched_last_date = imd_composition_end
)]

imd_level_diagnostics <- dcast(
  imd_composition_by_pair[outcome == "active_travel_total"],
  pair_id + scheme_id ~ imd_role,
  value.var = "average_per_observed_day"
)
setnames(
  imd_level_diagnostics,
  c("Five scheme sensors", "IMD-matched sensors"),
  c("scheme_active_travel_average", "control_active_travel_average")
)
imd_level_diagnostics[, scheme_to_control_level_ratio :=
  scheme_active_travel_average / control_active_travel_average]
imd_level_diagnostics[, level_match_diagnostic := fcase(
  scheme_to_control_level_ratio >= 0.67 &
    scheme_to_control_level_ratio <= 1.50,
  "Closer volume match",
  scheme_to_control_level_ratio >= 0.50 &
    scheme_to_control_level_ratio <= 2.00,
  "Moderate volume difference",
  default = "Large volume difference"
)]

prepare_imd_baseline_data <- function(pair_name) {
  spec <- imd_pair_specs[pair_id == pair_name][1]
  source <- copy(imd_daily[
    pair_id == pair_name &
      calendar_year %in% c(spec$pre_year, spec$post_year) &
      month_day >= spec$window_start &
      month_day <= spec$window_end
  ])
  source[, `:=`(
    post = as.integer(calendar_year == spec$post_year),
    treated = as.integer(imd_role == "Five scheme sensors")
  )]
  pair_days <- unique(source[
    ,
    .(countline_id, treated, calendar_year, month_day)
  ])
  pair_wide <- dcast(
    pair_days,
    countline_id + treated + month_day ~ calendar_year,
    fun.aggregate = length,
    value.var = "calendar_year"
  )
  pre_col <- as.character(spec$pre_year)
  post_col <- as.character(spec$post_year)
  pair_wide <- pair_wide[get(pre_col) > 0 & get(post_col) > 0]
  coverage <- pair_wide[
    ,
    .(paired_month_days = .N),
    by = .(countline_id, treated)
  ][paired_month_days >= 20L]
  common_month_days <- pair_wide[
    countline_id %in% coverage$countline_id,
    .(available_countlines = uniqueN(countline_id)),
    by = month_day
  ][available_countlines == nrow(coverage), month_day]
  model_data <- merge(
    source,
    coverage,
    by = c("countline_id", "treated")
  )[month_day %in% common_month_days]
  model_data[, treated_post := treated * post]
  list(
    spec = spec,
    data = model_data,
    coverage = coverage,
    common_month_days = common_month_days
  )
}

imd_baseline_pair_ids <- imd_pair_specs[!is.na(pre_year), pair_id]
imd_baseline_prepared <- setNames(
  lapply(imd_baseline_pair_ids, prepare_imd_baseline_data),
  imd_baseline_pair_ids
)

fit_imd_baseline_model <- function(
  dat,
  outcome,
  comparison_id,
  comparison_label,
  pre_year,
  post_year,
  model_scope = "Matched pair"
) {
  model_data <- copy(dat)
  model_data[, log_outcome := log1p(pmax(get(outcome), 0))]
  if (uniqueN(model_data$pair_id) > 1L) {
    model <- lm(
      log_outcome ~ post + treated_post +
        factor(countline_id) + factor(pair_id):factor(month_day) + weekday,
      data = model_data
    )
  } else {
    model <- lm(
      log_outcome ~ post + treated_post +
        factor(countline_id) + factor(month_day) + weekday,
      data = model_data
    )
  }
  robust_vcov <- sandwich::vcovCL(
    model,
    cluster = model_data$countline_id,
    type = "HC1"
  )
  robust_test <- lmtest::coeftest(model, vcov. = robust_vcov)
  control_log <- coef(model)[["post"]]
  extra_log <- coef(model)[["treated_post"]]
  control_se <- sqrt(robust_vcov["post", "post"])
  extra_se <- sqrt(robust_vcov["treated_post", "treated_post"])
  scheme_log <- control_log + extra_log
  scheme_se <- sqrt(
    robust_vcov["post", "post"] +
      robust_vcov["treated_post", "treated_post"] +
      2 * robust_vcov["post", "treated_post"]
  )
  sensor_period <- model_data[
    ,
    .(mean_log_outcome = mean(log_outcome)),
    by = .(countline_id, treated, post)
  ]
  sensor_change <- dcast(
    sensor_period,
    countline_id + treated ~ post,
    value.var = "mean_log_outcome"
  )
  sensor_change[, log_change := `1` - `0`]
  sensor_test <- tryCatch(
    t.test(
      sensor_change[treated == 1L, log_change],
      sensor_change[treated == 0L, log_change]
    ),
    error = function(e) NULL
  )
  sensor_relative_log <- mean(
    sensor_change[treated == 1L, log_change]
  ) - mean(
    sensor_change[treated == 0L, log_change]
  )
  pct <- function(x) 100 * (exp(x) - 1)
  data.table(
    comparison_id = comparison_id,
    comparison_label = comparison_label,
    model_scope = model_scope,
    outcome = outcome,
    outcome_label = outcome_labels[[outcome]],
    pre_year = pre_year,
    post_year = post_year,
    control_adjusted_change_percent = pct(control_log),
    control_lower_95 = pct(control_log - 1.96 * control_se),
    control_upper_95 = pct(control_log + 1.96 * control_se),
    scheme_adjusted_change_percent = pct(scheme_log),
    scheme_lower_95 = pct(scheme_log - 1.96 * scheme_se),
    scheme_upper_95 = pct(scheme_log + 1.96 * scheme_se),
    scheme_relative_to_control_percent = pct(extra_log),
    relative_lower_95 = pct(extra_log - 1.96 * extra_se),
    relative_upper_95 = pct(extra_log + 1.96 * extra_se),
    relative_p_value = robust_test["treated_post", 4],
    sensor_level_relative_percent = pct(sensor_relative_log),
    sensor_level_p_value = if (is.null(sensor_test)) NA_real_ else sensor_test$p.value,
    scheme_countlines = uniqueN(model_data[treated == 1L, countline_id]),
    control_countlines = uniqueN(model_data[treated == 0L, countline_id]),
    common_month_days = uniqueN(model_data$month_day),
    model_rows = nrow(model_data),
    adjusted_r_squared = summary(model)$adj.r.squared
  )
}

imd_pair_baseline_results <- rbindlist(lapply(
  imd_baseline_pair_ids,
  function(pair_name) {
    prepared <- imd_baseline_prepared[[pair_name]]
    rbindlist(lapply(
      outcomes,
      function(outcome) fit_imd_baseline_model(
        prepared$data,
        outcome,
        pair_name,
        paste0("Scheme ", prepared$spec$scheme_id),
        prepared$spec$pre_year,
        prepared$spec$post_year
      )
    ))
  }
))

imd_combined_12d_12f_data <- rbindlist(lapply(
  c("12d | Liverpool 052A", "12f | Wirral 006A"),
  function(pair_name) imd_baseline_prepared[[pair_name]]$data
), use.names = TRUE, fill = TRUE)
imd_combined_baseline_results <- rbindlist(lapply(
  outcomes,
  function(outcome) fit_imd_baseline_model(
    imd_combined_12d_12f_data,
    outcome,
    "12d + 12f combined",
    "Schemes 12d and 12f combined",
    2022L,
    2024L,
    "Combined matched pairs"
  )
))
imd_baseline_results <- rbindlist(list(
  imd_combined_baseline_results,
  imd_pair_baseline_results
), use.names = TRUE, fill = TRUE)
imd_baseline_results[, conclusion := fifelse(
  scheme_relative_to_control_percent >= 0,
  "Scheme trajectory higher than paired IMD baseline",
  "Scheme trajectory lower than paired IMD baseline"
)]

circular_block_bootstrap_means <- function(
  values,
  reps = 5000L,
  block_length = 7L,
  seed = 20260610L
) {
  n <- length(values)
  set.seed(seed)
  replicate(reps, {
    starts <- sample.int(n, ceiling(n / block_length), replace = TRUE)
    indices <- unlist(lapply(
      starts,
      function(start) {
        ((start - 1L + seq_len(block_length) - 1L) %% n) + 1L
      }
    ))[seq_len(n)]
    mean(values[indices])
  })
}

summarise_imd_context_change <- function(pair_name, outcome) {
  prepared <- imd_baseline_prepared[[pair_name]]
  model_data <- prepared$data
  site_daily <- model_data[
    ,
    .(site_mean = mean(get(outcome))),
    by = .(pair_id, imd_role, calendar_year, month_day)
  ]
  site_daily[, period := fifelse(
    calendar_year == prepared$spec$pre_year,
    "pre",
    "post"
  )]
  site_daily[, role_label := fifelse(
    imd_role == "Five scheme sensors",
    "scheme",
    "control"
  )]
  wide <- dcast(
    site_daily,
    pair_id + month_day ~ role_label + period,
    value.var = "site_mean"
  )
  wide[, `:=`(
    scheme_log_change = log1p(scheme_post) - log1p(scheme_pre),
    control_log_change = log1p(control_post) - log1p(control_pre)
  )]
  wide[, relative_log_change :=
    scheme_log_change - control_log_change]
  seed_offset <- match(pair_name, imd_baseline_pair_ids) * 100L +
    match(outcome, outcomes)
  scheme_boot <- circular_block_bootstrap_means(
    wide$scheme_log_change,
    seed = 20260610L + seed_offset
  )
  control_boot <- circular_block_bootstrap_means(
    wide$control_log_change,
    seed = 20261610L + seed_offset
  )
  relative_boot <- circular_block_bootstrap_means(
    wide$relative_log_change,
    seed = 20262610L + seed_offset
  )
  pct <- function(x) 100 * (exp(x) - 1)
  data.table(
    comparison_id = pair_name,
    comparison_label = paste0("Scheme ", prepared$spec$scheme_id),
    model_scope = "Matched-context daily change",
    outcome = outcome,
    outcome_label = outcome_labels[[outcome]],
    pre_year = prepared$spec$pre_year,
    post_year = prepared$spec$post_year,
    matched_month_days = nrow(wide),
    scheme_countlines = uniqueN(
      model_data[imd_role == "Five scheme sensors", countline_id]
    ),
    control_countlines = uniqueN(
      model_data[imd_role == "IMD-matched sensors", countline_id]
    ),
    control_change_percent = pct(mean(wide$control_log_change)),
    scheme_change_percent = pct(mean(wide$scheme_log_change)),
    scheme_relative_to_control_percent =
      pct(mean(wide$relative_log_change)),
    relative_lower_95 = pct(quantile(relative_boot, 0.025)),
    relative_upper_95 = pct(quantile(relative_boot, 0.975)),
    conditional_bootstrap_p = min(
      1,
      2 * (
        min(
          sum(relative_boot <= 0),
          sum(relative_boot >= 0)
        ) + 1
      ) / (length(relative_boot) + 1)
    ),
    scheme_bootstrap = list(scheme_boot),
    control_bootstrap = list(control_boot),
    relative_bootstrap = list(relative_boot)
  )
}

imd_context_change_pair_results <- rbindlist(lapply(
  imd_baseline_pair_ids,
  function(pair_name) rbindlist(lapply(
    outcomes,
    function(outcome) summarise_imd_context_change(pair_name, outcome)
  ))
))

imd_context_change_combined_results <- rbindlist(lapply(
  outcomes,
  function(outcome_name) {
    rows <- imd_context_change_pair_results[
      comparison_id %chin% c(
        "12d | Liverpool 052A",
        "12f | Wirral 006A"
      ) &
        outcome == outcome_name
    ]
    scheme_boot <- (
      rows$scheme_bootstrap[[1]] + rows$scheme_bootstrap[[2]]
    ) / 2
    control_boot <- (
      rows$control_bootstrap[[1]] + rows$control_bootstrap[[2]]
    ) / 2
    relative_boot <- (
      rows$relative_bootstrap[[1]] + rows$relative_bootstrap[[2]]
    ) / 2
    pct <- function(x) 100 * (exp(x) - 1)
    data.table(
      comparison_id = "12d + 12f equal-weight",
      comparison_label = "Schemes 12d and 12f combined",
      model_scope = "Equal-weight matched-context daily change",
      outcome = outcome_name,
      outcome_label = outcome_labels[[outcome_name]],
      pre_year = 2022L,
      post_year = 2024L,
      matched_month_days = NA_integer_,
      scheme_countlines = sum(rows$scheme_countlines),
      control_countlines = sum(rows$control_countlines),
      control_change_percent = pct(mean(log1p(
        rows$control_change_percent / 100
      ))),
      scheme_change_percent = pct(mean(log1p(
        rows$scheme_change_percent / 100
      ))),
      scheme_relative_to_control_percent = pct(mean(log1p(
        rows$scheme_relative_to_control_percent / 100
      ))),
      relative_lower_95 = pct(quantile(relative_boot, 0.025)),
      relative_upper_95 = pct(quantile(relative_boot, 0.975)),
      conditional_bootstrap_p = min(
        1,
        2 * (
          min(
            sum(relative_boot <= 0),
            sum(relative_boot >= 0)
          ) + 1
        ) / (length(relative_boot) + 1)
      ),
      scheme_bootstrap = list(scheme_boot),
      control_bootstrap = list(control_boot),
      relative_bootstrap = list(relative_boot)
    )
  }
))
imd_context_change_results <- rbindlist(list(
  imd_context_change_combined_results,
  imd_context_change_pair_results
), use.names = TRUE, fill = TRUE)
imd_context_change_export <- copy(imd_context_change_results)
imd_context_change_export[, c(
  "scheme_bootstrap",
  "control_bootstrap",
  "relative_bootstrap"
) := NULL]
imd_context_change_export[, interpretation := fcase(
  relative_lower_95 > 0,
  "Higher than paired baseline within the selected matched dates",
  relative_upper_95 < 0,
  "Lower than paired baseline within the selected matched dates",
  default = "Direction uncertain within the selected matched dates"
)]

fit_imd_context_weather_sensitivity <- function(
  pair_names,
  outcome,
  comparison_id,
  comparison_label
) {
  model_data <- rbindlist(lapply(
    pair_names,
    function(pair_name) imd_baseline_prepared[[pair_name]]$data
  ), use.names = TRUE, fill = TRUE)
  site_daily <- model_data[
    ,
    .(
      site_mean = mean(get(outcome)),
      temperature_2m_mean = mean(temperature_2m_mean),
      precipitation_sum = mean(precipitation_sum),
      wind_speed_10m_max = mean(wind_speed_10m_max),
      shortwave_radiation_sum = mean(shortwave_radiation_sum)
    ),
    by = .(
      pair_id,
      imd_role,
      date,
      calendar_year,
      month_day,
      weekday,
      post,
      treated,
      treated_post
    )
  ]
  site_daily <- add_weather_terms(site_daily)
  site_daily[, `:=`(
    log_outcome = log1p(pmax(site_mean, 0)),
    pair_role = interaction(pair_id, imd_role, drop = TRUE)
  )]
  if (length(pair_names) > 1L) {
    base_terms <- c(
      "post",
      "treated_post",
      "factor(pair_role)",
      "factor(pair_id):factor(month_day)",
      "weekday"
    )
  } else {
    base_terms <- c(
      "post",
      "treated_post",
      "factor(imd_role)",
      "factor(month_day)",
      "weekday"
    )
  }
  weather_terms <- c(
    base_terms,
    "temperature_z",
    "precipitation_z",
    "wind_z",
    "radiation_z"
  )
  unadjusted_model <- lm(
    reformulate(base_terms, response = "log_outcome"),
    data = site_daily
  )
  weather_model <- lm(
    reformulate(weather_terms, response = "log_outcome"),
    data = site_daily
  )
  pct <- function(x) 100 * (exp(x) - 1)
  unadjusted_effect <- pct(coef(unadjusted_model)[["treated_post"]])
  weather_adjusted_effect <- pct(coef(weather_model)[["treated_post"]])
  data.table(
    comparison_id = comparison_id,
    comparison_label = comparison_label,
    outcome = outcome,
    outcome_label = outcome_labels[[outcome]],
    unadjusted_relative_percent = unadjusted_effect,
    weather_adjusted_relative_percent = weather_adjusted_effect,
    adjustment_shift_percentage_points =
      weather_adjusted_effect - unadjusted_effect,
    site_days = nrow(site_daily),
    matched_dates = uniqueN(site_daily$date),
    weather_complete_percent = 100 * mean(complete.cases(
      site_daily[, .(
        temperature_2m_mean,
        precipitation_sum,
        wind_speed_10m_max,
        shortwave_radiation_sum
      )]
    )),
    unadjusted_adjusted_r_squared = summary(unadjusted_model)$adj.r.squared,
    weather_adjusted_r_squared = summary(weather_model)$adj.r.squared
  )
}

imd_weather_pair_results <- rbindlist(lapply(
  imd_baseline_pair_ids,
  function(pair_name) rbindlist(lapply(
    outcomes,
    function(outcome) fit_imd_context_weather_sensitivity(
      pair_name,
      outcome,
      pair_name,
      paste0("Scheme ", imd_baseline_prepared[[pair_name]]$spec$scheme_id)
    )
  ))
))
imd_weather_combined_results <- rbindlist(lapply(
  outcomes,
  function(outcome_name) {
    rows <- imd_weather_pair_results[
      comparison_id %chin% c(
        "12d | Liverpool 052A",
        "12f | Wirral 006A"
      ) &
        outcome == outcome_name
    ]
    assert_true(
      nrow(rows) == 2L,
      paste("Missing pair-level IMD weather result for", outcome_name)
    )
    pct <- function(x) 100 * (exp(x) - 1)
    unadjusted_effect <- pct(mean(log1p(
      rows$unadjusted_relative_percent / 100
    )))
    weather_adjusted_effect <- pct(mean(log1p(
      rows$weather_adjusted_relative_percent / 100
    )))
    data.table(
      comparison_id = "12d + 12f equal-weight",
      comparison_label = "Schemes 12d and 12f combined",
      outcome = outcome_name,
      outcome_label = outcome_labels[[outcome_name]],
      unadjusted_relative_percent = unadjusted_effect,
      weather_adjusted_relative_percent = weather_adjusted_effect,
      adjustment_shift_percentage_points =
        weather_adjusted_effect - unadjusted_effect,
      site_days = sum(rows$site_days),
      matched_dates = sum(rows$matched_dates),
      weather_complete_percent = min(rows$weather_complete_percent),
      unadjusted_adjusted_r_squared = NA_real_,
      weather_adjusted_r_squared = NA_real_
    )
  }
))
imd_weather_sensitivity_results <- rbindlist(list(
  imd_weather_combined_results,
  imd_weather_pair_results
), use.names = TRUE, fill = TRUE)

weather_sensitivity_results <- rbindlist(list(
  weather_regression_comparison[, .(
    benchmark,
    comparison_id = "12d + 12f versus LCR",
    comparison_label = "Schemes 12d and 12f versus eligible LCR sensors",
    outcome,
    outcome_label,
    unadjusted_relative_percent,
    weather_adjusted_relative_percent,
    adjustment_shift_percentage_points,
    observations = regression_results$paired_daily_rows[
      match(outcome, regression_results$outcome)
    ],
    weather_complete_percent = 100
  )],
  imd_weather_sensitivity_results[, .(
    benchmark = "IMD-matched streets",
    comparison_id,
    comparison_label,
    outcome,
    outcome_label,
    unadjusted_relative_percent,
    weather_adjusted_relative_percent,
    adjustment_shift_percentage_points,
    observations = site_days,
    weather_complete_percent
  )]
), use.names = TRUE, fill = TRUE)

imd_baseline_cohort <- rbindlist(lapply(
  names(imd_baseline_prepared),
  function(pair_name) {
    prepared <- imd_baseline_prepared[[pair_name]]
    unique(prepared$data[, .(
      pair_id,
      scheme_id,
      countline_id,
      imd_role,
      countline_name,
      route_type,
      hardware_name,
      pre_year = prepared$spec$pre_year,
      post_year = prepared$spec$post_year,
      common_month_days = length(prepared$common_month_days)
    )])
  }
), use.names = TRUE, fill = TRUE)

imd_pair_manifest <- merge(
  imd_pair_lookup,
  imd_pair_specs,
  by = c("pair_id", "scheme_id"),
  all.x = TRUE
)
imd_pair_manifest <- merge(
  imd_pair_manifest,
  unique(daily_valid[, .(
    countline_id,
    countline_name,
    route_type,
    local_authority,
    lsoa21nm,
    hardware_name
  )]),
  by = "countline_id",
  all.x = TRUE
)
assert_true(
  nrow(imd_pair_manifest) == nrow(imd_pair_lookup) &&
    !anyNA(imd_pair_manifest[, .(
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
    )]),
  "IMD pair manifest contains a failed or duplicating metadata join."
)

other_before_after_base <- analysis_daily[
  countline_id %in% eligible_other_ids & calendar_year %in% c(2022L, 2024L),
  .(
    sensors = uniqueN(countline_id),
    observed_countline_days = .N,
    pedestrian = sum(pedestrian),
    cyclist = sum(cyclist),
    active_travel_total = sum(active_travel_total)
  ),
  by = calendar_year
]
other_before_after <- melt(
  other_before_after_base,
  id.vars = c("calendar_year", "sensors", "observed_countline_days"),
  measure.vars = outcomes,
  variable.name = "outcome",
  value.name = "count_total"
)
other_before_after[, outcome_label := outcome_labels[outcome]]
other_before_after[, average_per_observed_day :=
  count_total / observed_countline_days]

scheme_date_coverage <- analysis_daily[
  sensor_group == "Five scheme sensors",
  .(scheme_groups_available = uniqueN(analysis_scheme_id)),
  by = date
]
matched_dates <- sort(scheme_date_coverage[
  scheme_groups_available == length(unique(treated_lookup$analysis_scheme_id)),
  date
])
assert_true(
  length(matched_dates) > 0L,
  "No exact dates contain usable observations for all five scheme groups."
)
common_start <- min(matched_dates)
common_end <- max(matched_dates)
matched_day_count <- length(matched_dates)
common_daily <- analysis_daily[date %in% matched_dates]

monthly_group_base <- common_daily[
  ,
  .(
    sensors = uniqueN(countline_id),
    observed_countline_days = .N,
    pedestrian = sum(pedestrian),
    cyclist = sum(cyclist),
    active_travel_total = sum(active_travel_total)
  ),
  by = .(month_start, sensor_group)
]
monthly_group <- melt(
  monthly_group_base,
  id.vars = c("month_start", "sensor_group", "sensors", "observed_countline_days"),
  measure.vars = outcomes,
  variable.name = "outcome",
  value.name = "count_total"
)
monthly_group[, outcome_label := outcome_labels[outcome]]
monthly_group[, count_per_observed_day := count_total / observed_countline_days]
monthly_other <- monthly_group[sensor_group == "Other LCR sensors"]

common_group_base <- common_daily[
  ,
  .(
    sensors = uniqueN(countline_id),
    observed_countline_days = .N,
    pedestrian = sum(pedestrian),
    cyclist = sum(cyclist),
    active_travel_total = sum(active_travel_total)
  ),
  by = sensor_group
]
common_group <- melt(
  common_group_base,
  id.vars = c("sensor_group", "sensors", "observed_countline_days"),
  measure.vars = outcomes,
  variable.name = "outcome",
  value.name = "count_total"
)
common_group[, outcome_label := outcome_labels[outcome]]
common_group[, average_per_observed_day := count_total / observed_countline_days]
common_group[
  ,
  active_travel_average := average_per_observed_day[outcome == "active_travel_total"],
  by = sensor_group
]
common_group[, percentage_of_group_active_travel :=
  100 * average_per_observed_day / active_travel_average]
common_group[, matched_calendar_days := matched_day_count]

sensor_window <- common_daily[
  ,
  .(
    observed_days = .N,
    pedestrian = sum(pedestrian) / .N,
    cyclist = sum(cyclist) / .N,
    active_travel_total = sum(active_travel_total) / .N
  ),
  by = .(sensor_group, countline_id)
]
sensor_window_long <- melt(
  sensor_window,
  id.vars = c("sensor_group", "countline_id", "observed_days"),
  measure.vars = outcomes,
  variable.name = "outcome",
  value.name = "sensor_average_per_day"
)
sensor_distribution <- sensor_window_long[
  ,
  .(
    sensors = uniqueN(countline_id),
    median_sensor_average = median(sensor_average_per_day),
    lower_quartile = quantile(sensor_average_per_day, 0.25),
    upper_quartile = quantile(sensor_average_per_day, 0.75)
  ),
  by = .(sensor_group, outcome)
]
sensor_distribution[, outcome_label := outcome_labels[outcome]]

common_comparison <- merge(
  common_group[sensor_group == "Five scheme sensors"],
  common_group[sensor_group == "Other LCR sensors"],
  by = c("outcome", "outcome_label"),
  suffixes = c("_scheme", "_other")
)
common_comparison[, difference_scheme_minus_other :=
  average_per_observed_day_scheme - average_per_observed_day_other]
common_comparison[, ratio_scheme_to_other :=
  average_per_observed_day_scheme / average_per_observed_day_other]
common_comparison[, `:=`(
  matched_calendar_days = matched_day_count,
  matched_first_date = common_start,
  matched_last_date = common_end
)]
common_comparison <- merge(
  common_comparison,
  sensor_distribution[
    sensor_group == "Five scheme sensors",
    .(outcome, median_scheme_sensor = median_sensor_average)
  ],
  by = "outcome",
  all.x = TRUE
)
common_comparison <- merge(
  common_comparison,
  sensor_distribution[
    sensor_group == "Other LCR sensors",
    .(outcome, median_other_sensor = median_sensor_average)
  ],
  by = "outcome",
  all.x = TRUE
)

route_common_base <- common_daily[
  sensor_group == "Five scheme sensors",
  .(
    sensors = uniqueN(countline_id),
    observed_countline_days = .N,
    pedestrian = sum(pedestrian),
    cyclist = sum(cyclist),
    active_travel_total = sum(active_travel_total)
  ),
  by = route_type
]
route_common <- melt(
  route_common_base,
  id.vars = c("route_type", "sensors", "observed_countline_days"),
  measure.vars = outcomes,
  variable.name = "outcome",
  value.name = "count_total"
)
route_common[, outcome_label := outcome_labels[outcome]]
route_common[, average_per_observed_day := count_total / observed_countline_days]

other_sensor_summary <- common_daily[
  sensor_group == "Other LCR sensors",
  .(
    valid_sensors = uniqueN(countline_id),
    valid_sensor_days = .N,
    first_valid_date = min(date),
    last_valid_date = max(date)
  )
]
all_sensor_summary <- common_daily[
  ,
  .(
    valid_sensors = uniqueN(countline_id),
    valid_sensor_days = .N,
    first_valid_date = min(date),
    last_valid_date = max(date)
  ),
  by = sensor_group
]
other_by_lad <- common_daily[
  sensor_group == "Other LCR sensors",
  .(
    sensors_with_valid_data = uniqueN(countline_id),
    observed_sensor_days = .N
  ),
  by = local_authority
][order(-sensors_with_valid_data)]
other_by_route <- common_daily[
  sensor_group == "Other LCR sensors",
  .(
    sensors_with_valid_data = uniqueN(countline_id),
    observed_sensor_days = .N
  ),
  by = route_type
][order(-sensors_with_valid_data)]

write_csv <- function(dt, filename) {
  fwrite(dt, file.path(output_dir, filename))
  fwrite(dt, file.path(output_visualised_dir, filename))
}
write_csv(monthly_group, "vivacity_citywide_monthly_sensor_group_comparison.csv")
write_csv(common_comparison, "vivacity_citywide_common_window_comparison.csv")
write_csv(other_by_lad, "vivacity_other_lcr_sensor_summary_by_local_authority.csv")
write_csv(route_common, "vivacity_five_scheme_common_window_route_comparison.csv")
write_csv(
  other_sensor_eligibility[
    eligible_2022_2024 == TRUE
  ][order(local_authority, countline_id)],
  "vivacity_other_lcr_2022_2024_sensor_eligibility.csv"
)
write_csv(
  other_before_after[order(outcome, calendar_year)],
  "vivacity_other_lcr_before_after_2022_2024.csv"
)
write_csv(
  regression_results[order(outcome)],
  "vivacity_baseline_adjusted_regression_2022_2024.csv"
)
write_csv(
  weather_regression_results[order(outcome)],
  "vivacity_weather_adjusted_lcr_regression_2022_2024.csv"
)
write_csv(
  regression_cohort,
  "vivacity_baseline_adjusted_regression_cohort.csv"
)
write_csv(
  data.table(
    matched_date = matched_dates,
    all_five_scheme_groups_available = TRUE
  ),
  "vivacity_exact_five_scheme_matched_dates.csv"
)
write_csv(
  imd_pair_manifest[order(pair_id, imd_role, countline_id)],
  "vivacity_imd_matched_sensor_pairs.csv"
)
write_csv(
  imd_composition_results[
    order(comparison_level, comparison_group, imd_role, outcome)
  ],
  "vivacity_imd_matched_active_travel_composition.csv"
)
write_csv(
  imd_baseline_results[order(model_scope, comparison_id, outcome)],
  "vivacity_imd_matched_countline_model_sensitivity.csv"
)
write_csv(
  imd_context_change_export[order(model_scope, comparison_id, outcome)],
  "vivacity_imd_matched_context_change_results.csv"
)
write_csv(
  imd_context_change_export[order(model_scope, comparison_id, outcome)],
  "vivacity_imd_matched_baseline_results.csv"
)
write_csv(
  imd_baseline_cohort[order(pair_id, imd_role, countline_id)],
  "vivacity_imd_matched_baseline_cohort.csv"
)
write_csv(
  imd_level_diagnostics[order(scheme_id, pair_id)],
  "vivacity_imd_matched_level_diagnostics.csv"
)
write_csv(
  data.table(
    matched_date = imd_composition_dates,
    all_six_scheme_context_pairs_available = TRUE
  ),
  "vivacity_imd_matched_exact_composition_dates.csv"
)
write_csv(
  weather_sites[order(pair_id, imd_role)],
  "vivacity_era5_weather_site_lookup.csv"
)
write_csv(
  weather_daily[order(weather_site_id, date)],
  "vivacity_era5_weather_daily.csv"
)
write_csv(
  imd_weather_sensitivity_results[
    order(comparison_id, outcome)
  ],
  "vivacity_imd_weather_adjusted_sensitivity.csv"
)
write_csv(
  weather_sensitivity_results[
    order(benchmark, comparison_id, outcome)
  ],
  "vivacity_weather_control_sensitivity_summary.csv"
)

trajectory_selected_path <- file.path(
  output_dir,
  "vivacity_preintervention_trajectory_selected_controls.csv"
)
trajectory_results_path <- file.path(
  output_dir,
  "vivacity_preintervention_trajectory_results.csv"
)
assert_true(
  file.exists(trajectory_selected_path) && file.exists(trajectory_results_path),
  paste(
    "Run match_vivacity_preintervention_trajectories.R before building",
    "the comprehensive HTML report."
  )
)
trajectory_selected <- fread(trajectory_selected_path)
trajectory_results <- fread(trajectory_results_path)
assert_true(
  nrow(trajectory_selected) == 6L &&
    all(trajectory_selected$selected_control) &&
    all(trajectory_selected$road_type_used_for_selection == FALSE),
  "Trajectory-selected controls are incomplete or unexpectedly use road type."
)
assert_true(
  all(trajectory_selected$trajectory_scale ==
    "Pre-installation percentage index (mean = 100)") &&
    all(trajectory_selected$trajectory_log_definition ==
      "log(percentage index / 100)"),
  "Trajectory matching is not using the required percentage-index log scale."
)

theme_report <- function() {
  theme_minimal(base_size = 10) +
    theme(
      plot.title = element_text(face = "bold", size = 12, colour = "#20313f"),
      plot.subtitle = element_text(size = 9, colour = "#5f6f7b"),
      legend.position = "bottom",
      legend.title = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.title.x = element_blank(),
      strip.text = element_text(face = "bold", colour = "#20313f")
    )
}

plot_to_inline_image <- function(plot, width = 9, height = 4.8, alt = "Chart") {
  tmp <- tempfile(fileext = ".png")
  grDevices::png(
    tmp,
    width = round(width * 140),
    height = round(height * 140),
    res = 140,
    bg = "white",
    type = "cairo"
  )
  print(plot)
  grDevices::dev.off()
  encoded <- base64enc::base64encode(tmp)
  unlink(tmp)
  paste0(
    '<img class="plot-image" alt="', alt,
    '" src="data:image/png;base64,', encoded, '">'
  )
}

scheme_order <- c("12b", "12d", "12e", "12f", "13")
scheme_plots <- vapply(scheme_order, function(scheme_id) {
  plot_data <- monthly_scheme[analysis_scheme_id == scheme_id]
  meta <- unique(plot_data[, .(
    scheme_name,
    treated_road_group,
    confirmed_intervention_date
  )])[1]
  plot_data[, outcome_label := factor(
    outcome_label,
    levels = c("Pedestrian", "Cyclist", "Active travel")
  )]
  p <- ggplot(
    plot_data,
    aes(month_start, count_per_observed_day, colour = outcome_label)
  ) +
    geom_line(linewidth = 0.8, na.rm = TRUE) +
    geom_point(size = 1.3, na.rm = TRUE) +
    geom_vline(
      xintercept = as.Date(meta$confirmed_intervention_date),
      colour = "#6b7280",
      linetype = "dashed",
      linewidth = 0.6
    ) +
    scale_colour_manual(values = c(
      "Pedestrian" = "#d97757",
      "Cyclist" = "#2a7f78",
      "Active travel" = "#255f85"
    )) +
    scale_x_date(date_breaks = "3 months", date_labels = "%b\n%Y") +
    scale_y_continuous(labels = label_number(accuracy = 1), expand = expansion(mult = c(0, 0.08))) +
    labs(
      title = paste0(
        "Scheme ", scheme_id, ": ", meta$scheme_name,
        " | installed ", meta$confirmed_intervention_date
      ),
      subtitle = meta$treated_road_group,
      y = "Average per observed countline-day"
    ) +
    theme_report()
  plot_to_inline_image(
    p,
    width = 7.7,
    height = 4.0,
    alt = paste0("Monthly temporal chart for scheme ", scheme_id)
  )
}, character(1))

other_trend_plot <- ggplot(
  monthly_other,
  aes(month_start, count_per_observed_day, colour = outcome_label)
) +
  geom_line(linewidth = 0.85) +
  scale_colour_manual(values = c(
    "Pedestrian" = "#d97757",
    "Cyclist" = "#2a7f78",
    "Active travel" = "#255f85"
  )) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b\n%Y") +
  scale_y_continuous(labels = label_number(accuracy = 1), expand = expansion(mult = c(0, 0.08))) +
  labs(
    title = "Monthly pattern across eligible other LCR sensors",
    subtitle = paste0(
      "Restricted to ", matched_day_count,
      " exact dates when all five scheme groups have usable observations"
    ),
    y = "Average per observed countline-day"
  ) +
  theme_report()

other_before_after_plot <- ggplot(
  other_before_after,
  aes(
    factor(outcome_label, levels = c("Pedestrian", "Cyclist", "Active travel")),
    average_per_observed_day,
    fill = factor(calendar_year)
  )
) +
  geom_col(position = position_dodge(width = 0.75), width = 0.66) +
  scale_fill_manual(
    values = c("2022" = "#2a7f78", "2024" = "#d97757"),
    name = "Year"
  ) +
  scale_y_continuous(
    labels = label_number(accuracy = 1),
    expand = expansion(mult = c(0, 0.1))
  ) +
  labs(
    title = "Other LCR sensors with valid observations in both years",
    subtitle = paste0(
      eligible_other_count,
      " eligible countlines; sensors without a 2022 and 2024 comparison are excluded"
    ),
    y = "Average per observed countline-day"
  ) +
  theme_report()

lad_plot <- ggplot(
  other_by_lad,
  aes(reorder(local_authority, sensors_with_valid_data), sensors_with_valid_data)
) +
  geom_col(fill = "#2a7f78", width = 0.7) +
  coord_flip() +
  geom_text(
    aes(label = comma(sensors_with_valid_data)),
    hjust = -0.15,
    size = 3,
    colour = "#20313f"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.16))) +
  labs(
    title = "Eligible other sensors by local authority",
    y = "Countlines"
  ) +
  theme_report()

common_bar_plot <- ggplot(
  common_group,
  aes(
    factor(outcome_label, levels = c("Pedestrian", "Cyclist", "Active travel")),
    percentage_of_group_active_travel,
    fill = sensor_group
  )
) +
  geom_col(position = position_dodge(width = 0.75), width = 0.66) +
  geom_text(
    aes(label = percent(percentage_of_group_active_travel / 100, accuracy = 0.1)),
    position = position_dodge(width = 0.75),
    vjust = -0.35,
    size = 3,
    colour = "#20313f"
  ) +
  scale_fill_manual(values = c(
    "Five scheme sensors" = "#d97757",
    "Other LCR sensors" = "#2a7f78"
  )) +
  scale_y_continuous(
    labels = label_percent(scale = 1, accuracy = 1),
    limits = c(0, 112),
    breaks = seq(0, 100, 20),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title = "Percentage-normalized active-travel composition",
    subtitle = paste0(
      "Each group's active-travel total is set to 100%; same ",
      matched_day_count, " matched dates"
    ),
    y = "Percentage of each group's active-travel total"
  ) +
  theme_report()

imd_composition_overall[, outcome_label := factor(
  outcome_label,
  levels = c("Pedestrian", "Cyclist", "Active travel")
)]
imd_composition_overall_plot <- ggplot(
  imd_composition_overall,
  aes(outcome_label, percentage_of_group_active_travel, fill = imd_role)
) +
  geom_col(position = position_dodge(width = 0.75), width = 0.66) +
  geom_text(
    aes(label = percent(percentage_of_group_active_travel / 100, accuracy = 0.1)),
    position = position_dodge(width = 0.75),
    vjust = -0.35,
    size = 3,
    colour = "#20313f"
  ) +
  scale_fill_manual(values = c(
    "Five scheme sensors" = "#d97757",
    "IMD-matched sensors" = "#c89b3c"
  )) +
  scale_y_continuous(
    labels = label_percent(scale = 1, accuracy = 1),
    limits = c(0, 112),
    breaks = seq(0, 100, 20),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title = "Active-travel composition: schemes and IMD-matched streets",
    subtitle = paste0(
      "Each scheme receives equal weight and each group's active travel is set to 100%; ",
      imd_composition_day_count, " exact matched dates"
    ),
    y = "Percentage of each group's active-travel total"
  ) +
  theme_report()

imd_composition_pair_plot_data <- copy(imd_composition_by_pair)
imd_composition_pair_plot_data[, outcome_label := factor(
  outcome_label,
  levels = c("Pedestrian", "Cyclist", "Active travel")
)]
imd_composition_pair_plot_data[, pair_label := factor(
  pair_id,
  levels = imd_pair_specs$pair_id
)]
imd_composition_pair_plot <- ggplot(
  imd_composition_pair_plot_data,
  aes(outcome_label, percentage_of_group_active_travel, fill = imd_role)
) +
  geom_col(position = position_dodge(width = 0.75), width = 0.66) +
  facet_wrap(~ pair_label, ncol = 2) +
  scale_fill_manual(values = c(
    "Five scheme sensors" = "#d97757",
    "IMD-matched sensors" = "#c89b3c"
  )) +
  scale_y_continuous(
    labels = label_percent(scale = 1, accuracy = 1),
    limits = c(0, 108),
    breaks = seq(0, 100, 25),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title = "Composition within each IMD-matched context",
    subtitle = "Active travel equals 100% separately for the scheme and matched street in every panel",
    y = "Share of active travel"
  ) +
  theme_report() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

imd_active_baseline_plot_data <- rbindlist(list(
  imd_context_change_results[outcome == "active_travel_total", .(
    comparison_label,
    model_scope,
    group = "Paired IMD baseline",
    adjusted_change_percent = control_change_percent
  )],
  imd_context_change_results[outcome == "active_travel_total", .(
    comparison_label,
    model_scope,
    group = "Scheme sensors",
    adjusted_change_percent = scheme_change_percent
  )]
))
imd_baseline_label_order <- c(
  "Schemes 12d and 12f combined",
  "Scheme 12d",
  "Scheme 12f",
  "Scheme 13"
)
imd_active_baseline_plot_data[, comparison_label := factor(
  comparison_label,
  levels = imd_baseline_label_order
)]
imd_active_baseline_plot <- ggplot(
  imd_active_baseline_plot_data,
  aes(comparison_label, adjusted_change_percent, fill = group)
) +
  geom_hline(yintercept = 0, colour = "#6b7280", linewidth = 0.5) +
  geom_col(position = position_dodge(width = 0.75), width = 0.64) +
  geom_text(
    aes(
      label = sprintf("%+.1f%%", adjusted_change_percent),
      vjust = fifelse(adjusted_change_percent >= 0, -0.45, 1.35)
    ),
    position = position_dodge(width = 0.75),
    size = 3,
    colour = "#20313f"
  ) +
  scale_fill_manual(values = c(
    "Paired IMD baseline" = "#c89b3c",
    "Scheme sensors" = "#d97757"
  )) +
  scale_y_continuous(
    labels = label_percent(scale = 1, accuracy = 1),
    expand = expansion(mult = c(0.18, 0.18))
  ) +
  labs(
    title = "Active-travel change against each paired IMD baseline",
    subtitle = "Equal-weight matched contexts using the same month-days before and after",
    y = "Matched-date change"
  ) +
  theme_report() +
  theme(axis.text.x = element_text(angle = 18, hjust = 1))

regression_plot_data <- rbindlist(list(
  regression_results[, .(
    outcome_label,
    group = "Eligible LCR comparison sensors",
    adjusted_change_percent = lcr_adjusted_change_percent,
    lower_95 = lcr_lower_95,
    upper_95 = lcr_upper_95
  )],
  regression_results[, .(
    outcome_label,
    group = "Schemes 12d and 12f",
    adjusted_change_percent = scheme_adjusted_change_percent,
    lower_95 = scheme_lower_95,
    upper_95 = scheme_upper_95
  )]
))
regression_plot_data[, outcome_label := factor(
  outcome_label,
  levels = c("Pedestrian", "Cyclist", "Active travel")
)]
regression_plot <- ggplot(
  regression_plot_data,
  aes(outcome_label, adjusted_change_percent, fill = group)
) +
  geom_hline(yintercept = 0, colour = "#6b7280", linewidth = 0.5) +
  geom_col(position = position_dodge(width = 0.75), width = 0.64) +
  geom_errorbar(
    aes(ymin = lower_95, ymax = upper_95),
    position = position_dodge(width = 0.75),
    width = 0.16,
    linewidth = 0.55
  ) +
  geom_text(
    aes(
      label = sprintf("%+.1f%%", adjusted_change_percent),
      vjust = fifelse(adjusted_change_percent >= 0, -0.45, 1.35)
    ),
    position = position_dodge(width = 0.75),
    size = 3,
    colour = "#20313f"
  ) +
  scale_fill_manual(values = c(
    "Eligible LCR comparison sensors" = "#2a7f78",
    "Schemes 12d and 12f" = "#d97757"
  )) +
  scale_y_continuous(
    labels = label_percent(scale = 1, accuracy = 1),
    expand = expansion(mult = c(0.16, 0.16))
  ) +
  labs(
    title = "Baseline-adjusted change relative to the LCR comparison trend",
    subtitle = "Paired 15 November–31 December dates in 2022 and 2024; 95% clustered confidence intervals",
    y = "Adjusted change from 2022 to 2024"
  ) +
  theme_report()

trajectory_plot_data <- melt(
  trajectory_results[
    scheme_id %chin% c("12d", "12f", "13")
  ],
  id.vars = c("scheme_id", "outcome"),
  measure.vars = c("scheme_post_index", "control_post_index"),
  variable.name = "group",
  value.name = "post_index"
)
trajectory_plot_data[, group_label := factor(
  group,
  levels = c("scheme_post_index", "control_post_index"),
  labels = c("Scheme", "Trajectory-matched control")
)]
trajectory_plot_data[, outcome_label := factor(
  fifelse(outcome == "pedestrian", "Pedestrian", "Cyclist"),
  levels = c("Pedestrian", "Cyclist")
)]
trajectory_percentage_plot <- ggplot(
  trajectory_plot_data,
  aes(scheme_id, post_index, fill = group_label)
) +
  geom_hline(yintercept = 100, colour = "#6b7280", linewidth = 0.5) +
  geom_col(position = position_dodge(width = 0.76), width = 0.66) +
  geom_text(
    aes(label = sprintf("%.1f", post_index)),
    position = position_dodge(width = 0.76),
    vjust = -0.35,
    size = 3,
    colour = "#20313f"
  ) +
  facet_wrap(~ outcome_label, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = c(
    "Scheme" = "#d97757",
    "Trajectory-matched control" = "#2a7f78"
  )) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.14))) +
  labs(
    title = "Post-period percentage index after pre-trajectory matching",
    subtitle = "Each scheme and control pre-period is set to 100; pedestrian and cyclist controls are selected separately",
    y = "Post-period index (pre-period = 100)"
  ) +
  theme_report()

weather_plot_source <- weather_sensitivity_results[
  comparison_id %chin% c(
    "12d + 12f versus LCR",
    "12d + 12f equal-weight"
  )
]
weather_plot_data <- melt(
  weather_plot_source,
  id.vars = c("benchmark", "comparison_id", "outcome", "outcome_label"),
  measure.vars = c(
    "unadjusted_relative_percent",
    "weather_adjusted_relative_percent"
  ),
  variable.name = "model",
  value.name = "relative_effect_percent"
)
weather_plot_data[, model_label := factor(
  model,
  levels = c(
    "unadjusted_relative_percent",
    "weather_adjusted_relative_percent"
  ),
  labels = c("Matched-date model", "Weather-adjusted model")
)]
weather_plot_data[, outcome_label := factor(
  outcome_label,
  levels = c("Pedestrian", "Cyclist", "Active travel")
)]
weather_sensitivity_plot <- ggplot(
  weather_plot_data,
  aes(outcome_label, relative_effect_percent, fill = model_label)
) +
  geom_hline(yintercept = 0, colour = "#6b7280", linewidth = 0.5) +
  geom_col(position = position_dodge(width = 0.75), width = 0.64) +
  geom_text(
    aes(
      label = sprintf("%+.1f%%", relative_effect_percent),
      vjust = fifelse(relative_effect_percent >= 0, -0.4, 1.3)
    ),
    position = position_dodge(width = 0.75),
    size = 3,
    colour = "#20313f"
  ) +
  facet_wrap(~ benchmark, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = c(
    "Matched-date model" = "#c89b3c",
    "Weather-adjusted model" = "#2a7f78"
  )) +
  scale_y_continuous(
    labels = label_percent(scale = 1, accuracy = 1),
    expand = expansion(mult = c(0.18, 0.18))
  ) +
  labs(
    title = "Does adding weather change the relative scheme estimate?",
    subtitle = "Daily temperature, precipitation, maximum wind speed and solar radiation; ERA5",
    y = "Scheme change relative to comparison baseline"
  ) +
  theme_report()

monthly_common <- monthly_group[
  month_start >= as.IDate(format(common_start, "%Y-%m-01")) &
    month_start <= as.IDate(format(common_end, "%Y-%m-01"))
]
monthly_common[, outcome_label := factor(
  outcome_label,
  levels = c("Pedestrian", "Cyclist", "Active travel")
)]
monthly_comparison_plot <- ggplot(
  monthly_common,
  aes(month_start, count_per_observed_day, colour = sensor_group)
) +
  geom_line(linewidth = 0.85) +
  geom_point(size = 1.2) +
  facet_wrap(~ outcome_label, scales = "free_y", ncol = 1) +
  scale_colour_manual(values = c(
    "Five scheme sensors" = "#d97757",
    "Other LCR sensors" = "#2a7f78"
  )) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b\n%Y") +
  scale_y_continuous(labels = label_number(accuracy = 1), expand = expansion(mult = c(0, 0.08))) +
  labs(
    title = "Monthly comparison on the exact five-scheme matched dates",
    y = "Average per observed countline-day"
  ) +
  theme_report()

route_plot <- ggplot(
  route_common[route_type %chin% c("Road", "Path/cycle facility")],
  aes(
    factor(outcome_label, levels = c("Pedestrian", "Cyclist", "Active travel")),
    average_per_observed_day,
    fill = route_type
  )
) +
  geom_col(position = position_dodge(width = 0.75), width = 0.66) +
  scale_fill_manual(values = c(
    "Road" = "#c89b3c",
    "Path/cycle facility" = "#255f85"
  )) +
  scale_y_continuous(labels = label_number(accuracy = 1), expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Five scheme sensors: road and path/cycle-facility outcomes",
    subtitle = paste0(
      "Restricted to the same ", matched_day_count,
      " dates used for the other-sensor comparison"
    ),
    y = "Average per observed countline-day"
  ) +
  theme_report()

other_trend_svg <- plot_to_inline_image(
  other_trend_plot, width = 10.4, height = 4.8,
  alt = "Monthly pattern across other Liverpool City Region sensors"
)
other_before_after_svg <- plot_to_inline_image(
  other_before_after_plot, width = 8.2, height = 4.5,
  alt = "2022 and 2024 comparison for eligible other Liverpool City Region sensors"
)
lad_svg <- plot_to_inline_image(
  lad_plot, width = 7.4, height = 4.4,
  alt = "Usable other sensors by local authority"
)
common_bar_svg <- plot_to_inline_image(
  common_bar_plot, width = 8.2, height = 4.5,
  alt = "Five scheme sensors compared with other Liverpool City Region sensors"
)
imd_composition_overall_svg <- plot_to_inline_image(
  imd_composition_overall_plot, width = 8.4, height = 4.6,
  alt = "Active travel composition for scheme and IMD matched sensors"
)
imd_composition_pair_svg <- plot_to_inline_image(
  imd_composition_pair_plot, width = 10.4, height = 9.0,
  alt = "Active travel composition within each IMD matched scheme context"
)
imd_active_baseline_svg <- plot_to_inline_image(
  imd_active_baseline_plot, width = 9.4, height = 5.2,
  alt = "Active travel change against paired IMD baselines"
)
regression_svg <- plot_to_inline_image(
  regression_plot, width = 9.2, height = 5.0,
  alt = "Baseline adjusted regression comparison for schemes and city controls"
)
trajectory_percentage_svg <- plot_to_inline_image(
  trajectory_percentage_plot, width = 9.2, height = 7.2,
  alt = "Percentage indexed pedestrian and cyclist trajectory matched comparisons"
)
weather_sensitivity_svg <- plot_to_inline_image(
  weather_sensitivity_plot, width = 9.2, height = 7.2,
  alt = "Matched date and weather adjusted scheme effect comparison"
)
monthly_comparison_svg <- plot_to_inline_image(
  monthly_comparison_plot, width = 8.2, height = 8.4,
  alt = "Monthly scheme versus other sensor comparison"
)
route_svg <- plot_to_inline_image(
  route_plot, width = 8.2, height = 4.5,
  alt = "Road and path cycle facility comparison for scheme sensors"
)

html_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x
}

fmt <- function(x, digits = 1) {
  ifelse(is.na(x), "No data", format(round(x, digits), nsmall = digits, trim = TRUE, big.mark = ","))
}

comparison_panels <- paste(vapply(
  scheme_order,
  function(scheme_id) {
    scheme_rows <- fixed_comparison[analysis_scheme_id == scheme_id]
    scheme_meta <- unique(scheme_rows[, .(
      scheme_name,
      treated_road_group,
      confirmed_intervention_date
    )])[1]
    comparison_order <- if (scheme_id == "13") {
      c("2022 vs 2026", "2023 vs 2025")
    } else {
      c("2021 vs 2024", "2022 vs 2025")
    }
    panels <- paste(vapply(comparison_order, function(label) {
      rows <- scheme_rows[comparison_label == label]
      max_value <- max(
        c(rows$baseline_average_per_observed_day, rows$comparison_average_per_observed_day),
        na.rm = TRUE
      )
      if (!is.finite(max_value) || max_value <= 0) max_value <- 1
      outcome_html <- paste(vapply(
        c("Pedestrian", "Cyclist", "Active travel"),
        function(outcome_name) {
          row <- rows[outcome_label == outcome_name][1]
          before <- row$baseline_average_per_observed_day
          after <- row$comparison_average_per_observed_day
          before_width <- ifelse(is.na(before), 0, 100 * before / max_value)
          after_width <- ifelse(is.na(after), 0, 100 * after / max_value)
          change_class <- ifelse(
            is.na(row$percent_change),
            "neutral",
            ifelse(row$percent_change >= 0, "positive", "negative")
          )
          change_text <- ifelse(
            is.na(row$percent_change),
            "No comparable baseline",
            sprintf("%+.1f%%", row$percent_change)
          )
          interpretation <- ifelse(
            row$data_status == "Available",
            paste0(
              "The ", row$comparison_year, " average is ",
              ifelse(row$difference >= 0, "higher", "lower"), " by ",
              fmt(abs(row$difference)), " per observed countline-day."
            ),
            paste0(
              "No usable ", row$baseline_year,
              " baseline remains after the confirmed first-reliable-date and availability rules."
            )
          )
          paste0(
            '<div class="outcome-block">',
            '<div class="outcome-title"><h4>', outcome_name, '</h4>',
            '<span class="change ', change_class, '">', change_text, '</span></div>',
            '<div class="bar-row ', ifelse(is.na(before), "empty", ""), '">',
            '<div class="bar-label"><span>Before</span><small>', row$baseline_year, '</small></div>',
            '<div class="bar-track"><div class="bar before" style="width:', before_width, '%"></div></div>',
            '<div class="bar-value">', fmt(before), '</div></div>',
            '<div class="bar-row ', ifelse(is.na(after), "empty", ""), '">',
            '<div class="bar-label"><span>After</span><small>', row$comparison_year, '</small></div>',
            '<div class="bar-track"><div class="bar after" style="width:', after_width, '%"></div></div>',
            '<div class="bar-value">', fmt(after), '</div></div>',
            '<p class="interpretation">', interpretation, '</p>',
            '</div>'
          )
        },
        character(1)
      ), collapse = "")
      paste0(
        '<article class="comparison-panel">',
        '<div class="panel-heading"><div><h3>Scheme ', scheme_id, ': ', label, '</h3>',
        '<p>', html_escape(scheme_meta$scheme_name), ' | ',
        html_escape(scheme_meta$treated_road_group), '</p></div>',
        '<span class="date-chip">Installed ', scheme_meta$confirmed_intervention_date, '</span></div>',
        outcome_html,
        '</article>'
      )
    }, character(1)), collapse = "")
    panels
  },
  character(1)
), collapse = "")

scheme_plot_html <- paste(vapply(seq_along(scheme_order), function(i) {
  paste0(
    '<figure class="chart-panel">',
    scheme_plots[[i]],
    '<figcaption>Dashed line: exact confirmed installation date. A line outside the observed series means reliable sensor data begin after installation.</figcaption>',
    '</figure>'
  )
}, character(1)), collapse = "")

lad_rows <- paste(vapply(seq_len(nrow(other_by_lad)), function(i) {
  row <- other_by_lad[i]
  paste0(
    "<tr><td>", html_escape(row$local_authority), "</td>",
    "<td>", comma(row$sensors_with_valid_data), "</td>",
    "<td>", comma(row$observed_sensor_days), "</td></tr>"
  )
}, character(1)), collapse = "")

route_rows <- paste(vapply(seq_len(nrow(other_by_route)), function(i) {
  row <- other_by_route[i]
  paste0(
    "<tr><td>", html_escape(row$route_type), "</td>",
    "<td>", comma(row$sensors_with_valid_data), "</td>",
    "<td>", comma(row$observed_sensor_days), "</td></tr>"
  )
}, character(1)), collapse = "")

other_before_after_wide <- dcast(
  other_before_after,
  outcome + outcome_label ~ calendar_year,
  value.var = "average_per_observed_day"
)
setnames(other_before_after_wide, c("2022", "2024"), c("average_2022", "average_2024"))
other_before_after_wide[, difference_2024_minus_2022 := average_2024 - average_2022]
other_before_after_wide[, percent_change :=
  100 * difference_2024_minus_2022 / average_2022]
other_before_after_rows <- paste(vapply(
  seq_len(nrow(other_before_after_wide)),
  function(i) {
    row <- other_before_after_wide[i]
    paste0(
      "<tr><td>", row$outcome_label, "</td>",
      "<td>", fmt(row$average_2022), "</td>",
      "<td>", fmt(row$average_2024), "</td>",
      "<td>", fmt(row$difference_2024_minus_2022), "</td>",
      "<td>", sprintf("%+.1f%%", row$percent_change), "</td></tr>"
    )
  },
  character(1)
), collapse = "")

comparison_rows <- paste(vapply(seq_len(nrow(common_comparison)), function(i) {
  row <- common_comparison[i]
  paste0(
    "<tr><td>", row$outcome_label, "</td>",
    "<td>", fmt(row$percentage_of_group_active_travel_scheme), "%</td>",
    "<td>", fmt(row$percentage_of_group_active_travel_other), "%</td>",
    "<td>", fmt(row$average_per_observed_day_scheme), "</td>",
    "<td>", fmt(row$average_per_observed_day_other), "</td>",
    "<td>", fmt(row$difference_scheme_minus_other), "</td>",
    "<td>", fmt(row$ratio_scheme_to_other, 2), "x</td>",
    "<td>", fmt(row$median_scheme_sensor), "</td>",
    "<td>", fmt(row$median_other_sensor), "</td></tr>"
  )
}, character(1)), collapse = "")

regression_rows <- paste(vapply(seq_len(nrow(regression_results)), function(i) {
  row <- regression_results[i]
  inference <- if (
    row$scheme_relative_to_lcr_percent < 0 &
      row$relative_p_value < 0.05 &
      row$sensor_level_p_value < 0.05
  ) {
    "Lower than LCR trend in both inference checks"
  } else if (row$scheme_relative_to_lcr_percent < 0) {
    "Lower point estimate; not conclusive in both checks"
  } else if (
    row$relative_p_value < 0.05 &
      row$sensor_level_p_value < 0.05
  ) {
    "Higher than LCR trend in both inference checks"
  } else {
    "Higher point estimate; not conclusive in both checks"
  }
  paste0(
    "<tr><td>", row$outcome_label, "</td>",
    "<td>", sprintf("%+.1f%%", row$lcr_adjusted_change_percent), "</td>",
    "<td>", sprintf("%+.1f%%", row$scheme_adjusted_change_percent), "</td>",
    "<td>", sprintf("%+.1f%%", row$scheme_relative_to_lcr_percent), "</td>",
    "<td>", sprintf(
      "%+.1f%% to %+.1f%%",
      row$relative_lower_95,
      row$relative_upper_95
    ), "</td>",
    "<td>", format.pval(row$relative_p_value, digits = 3, eps = 0.001), "</td>",
    "<td>", format.pval(row$sensor_level_p_value, digits = 3, eps = 0.001), "</td>",
    "<td>", inference, "</td></tr>"
  )
}, character(1)), collapse = "")

active_regression <- regression_results[outcome == "active_travel_total"][1]
cyclist_regression <- regression_results[outcome == "cyclist"][1]

imd_match_rows <- paste(vapply(seq_len(nrow(imd_pair_specs)), function(i) {
  row <- imd_pair_specs[i]
  diagnostic <- imd_level_diagnostics[pair_id == row$pair_id][1]
  control_ids <- imd_pair_manifest[
    pair_id == row$pair_id & imd_role == "IMD-matched sensors",
    paste(sort(countline_id), collapse = ", ")
  ]
  paste0(
    "<tr><td>", html_escape(row$pair_id), "</td>",
    "<td>", fmt(row$treated_imd_score, 3), " (decile ",
    row$treated_imd_decile, ")</td>",
    "<td>", html_escape(row$control_street), "<br><small>",
    html_escape(row$control_lsoa21nm), "</small></td>",
    "<td>", fmt(row$control_imd_score, 3), " (decile ",
    row$control_imd_decile, ")</td>",
    "<td>", fmt(row$absolute_imd_score_difference, 3), "</td>",
    "<td>", fmt(diagnostic$scheme_to_control_level_ratio, 2), "x<br><small>",
    diagnostic$level_match_diagnostic, "</small></td>",
    "<td>", control_ids, "</td></tr>"
  )
}, character(1)), collapse = "")

imd_overall_composition_rows <- paste(vapply(
  c("Pedestrian", "Cyclist", "Active travel"),
  function(outcome_name) {
    scheme_row <- imd_composition_overall[
      imd_role == "Five scheme sensors" & outcome_label == outcome_name
    ][1]
    control_row <- imd_composition_overall[
      imd_role == "IMD-matched sensors" & outcome_label == outcome_name
    ][1]
    paste0(
      "<tr><td>", outcome_name, "</td>",
      "<td>", fmt(scheme_row$percentage_of_group_active_travel), "%</td>",
      "<td>", fmt(control_row$percentage_of_group_active_travel), "%</td>",
      "<td>", fmt(scheme_row$average_per_observed_day), "</td>",
      "<td>", fmt(control_row$average_per_observed_day), "</td></tr>"
    )
  },
  character(1)
), collapse = "")

imd_pair_composition_rows <- paste(vapply(
  imd_pair_specs$pair_id,
  function(pair_name) {
    pair_data <- imd_composition_by_pair[pair_id == pair_name]
    scheme_ped <- pair_data[
      imd_role == "Five scheme sensors" & outcome == "pedestrian"
    ][1]
    scheme_cycle <- pair_data[
      imd_role == "Five scheme sensors" & outcome == "cyclist"
    ][1]
    control_ped <- pair_data[
      imd_role == "IMD-matched sensors" & outcome == "pedestrian"
    ][1]
    control_cycle <- pair_data[
      imd_role == "IMD-matched sensors" & outcome == "cyclist"
    ][1]
    paste0(
      "<tr><td>", html_escape(pair_name), "</td>",
      "<td>", fmt(scheme_ped$percentage_of_group_active_travel), "%</td>",
      "<td>", fmt(scheme_cycle$percentage_of_group_active_travel), "%</td>",
      "<td>", fmt(control_ped$percentage_of_group_active_travel), "%</td>",
      "<td>", fmt(control_cycle$percentage_of_group_active_travel), "%</td></tr>"
    )
  },
  character(1)
), collapse = "")

imd_active_baseline_rows <- paste(vapply(
  imd_baseline_label_order,
  function(label) {
    row <- imd_context_change_results[
      comparison_label == label & outcome == "active_travel_total"
    ][1]
    interpretation <- if (row$relative_upper_95 < 0) {
      "Lower within selected matched dates"
    } else if (row$relative_lower_95 > 0) {
      "Higher within selected matched dates"
    } else {
      "Direction uncertain within selected matched dates"
    }
    month_day_text <- if (
      row$comparison_id == "12d + 12f equal-weight"
    ) {
      paste0(
        "12d: ",
        imd_pair_baseline_results[
          comparison_label == "Scheme 12d" &
            outcome == "active_travel_total",
          common_month_days
        ],
        "; 12f: ",
        imd_pair_baseline_results[
          comparison_label == "Scheme 12f" &
            outcome == "active_travel_total",
          common_month_days
        ]
      )
    } else {
      as.character(row$matched_month_days)
    }
    paste0(
      "<tr><td>", html_escape(label), "</td>",
      "<td>", row$pre_year, " to ", row$post_year, "</td>",
      "<td>", month_day_text, "</td>",
      "<td>", row$scheme_countlines, " / ", row$control_countlines, "</td>",
      "<td>", sprintf("%+.1f%%", row$control_change_percent), "</td>",
      "<td>", sprintf("%+.1f%%", row$scheme_change_percent), "</td>",
      "<td>", sprintf("%+.1f%%", row$scheme_relative_to_control_percent), "</td>",
      "<td>", sprintf(
        "%+.1f%% to %+.1f%%",
        row$relative_lower_95,
        row$relative_upper_95
      ), "</td>",
      "<td>", interpretation, "</td></tr>"
    )
  },
  character(1)
), collapse = "")

imd_baseline_detail_rows <- paste(vapply(
  seq_len(nrow(imd_pair_baseline_results)),
  function(i) {
    row <- imd_pair_baseline_results[i]
    paste0(
      "<tr><td>", html_escape(row$comparison_label), "</td>",
      "<td>", row$outcome_label, "</td>",
      "<td>", sprintf("%+.1f%%", row$control_adjusted_change_percent), "</td>",
      "<td>", sprintf("%+.1f%%", row$scheme_adjusted_change_percent), "</td>",
      "<td>", sprintf("%+.1f%%", row$scheme_relative_to_control_percent), "</td>",
      "<td>", format.pval(row$relative_p_value, digits = 3, eps = 0.001),
      "</td></tr>"
    )
  },
  character(1)
), collapse = "")

imd_scheme_pedestrian_share <- imd_composition_overall[
  imd_role == "Five scheme sensors" & outcome == "pedestrian",
  percentage_of_group_active_travel
]
imd_scheme_cyclist_share <- imd_composition_overall[
  imd_role == "Five scheme sensors" & outcome == "cyclist",
  percentage_of_group_active_travel
]
imd_control_pedestrian_share <- imd_composition_overall[
  imd_role == "IMD-matched sensors" & outcome == "pedestrian",
  percentage_of_group_active_travel
]
imd_control_cyclist_share <- imd_composition_overall[
  imd_role == "IMD-matched sensors" & outcome == "cyclist",
  percentage_of_group_active_travel
]
imd_combined_active <- imd_combined_baseline_results[
  outcome == "active_travel_total"
][1]
imd_context_combined_active <- imd_context_change_combined_results[
  outcome == "active_travel_total"
][1]
weather_lcr_active <- weather_regression_comparison[
  outcome == "active_travel_total"
][1]
weather_imd_active <- imd_weather_combined_results[
  outcome == "active_travel_total"
][1]

trajectory_selected_rows <- paste(vapply(
  seq_len(nrow(trajectory_selected)),
  function(i) {
    row <- trajectory_selected[i]
    paste0(
      "<tr><td>", html_escape(row$scheme_id), "</td>",
      "<td>", tools::toTitleCase(row$outcome), "</td>",
      "<td>", html_escape(row$candidate_id), "</td>",
      "<td>", sprintf("%.3f", row$absolute_imd_score_difference), "</td>",
      "<td>", row$common_pre_days, " (",
      sprintf("%.1f%%", 100 * row$pre_date_coverage), ")</td>",
      "<td>", sprintf("%.2f", row$level_ratio), "</td>",
      "<td>", sprintf("%.3f", row$trend_log_change_distance), "</td>",
      "<td>", sprintf("%.3f", row$monthly_percentage_log_rmse), "</td>",
      "<td>", sprintf("%.3f", row$weekly_percentage_log_correlation), "</td>",
      "</tr>"
    )
  },
  character(1)
), collapse = "")

trajectory_result_rows <- paste(vapply(
  seq_len(nrow(trajectory_results)),
  function(i) {
    row <- trajectory_results[i]
    matched_days <- ifelse(
      is.na(row$matched_month_days),
      "Equal-weight 12d/12f",
      as.character(row$matched_month_days)
    )
    paste0(
      "<tr><td>", html_escape(row$scheme_id), "</td>",
      "<td>", tools::toTitleCase(row$outcome), "</td>",
      "<td>", row$pre_year, " to ", row$post_year, "</td>",
      "<td>", matched_days, "</td>",
      "<td>", sprintf("%.1f", row$scheme_post_index), "</td>",
      "<td>", sprintf("%.1f", row$control_post_index), "</td>",
      "<td>", sprintf(
        "%+.1f%%",
        row$scheme_relative_to_control_percent
      ), "</td></tr>"
    )
  },
  character(1)
), collapse = "")

trajectory_combined_pedestrian <- trajectory_results[
  scheme_id == "12d + 12f equal-weight" &
    outcome == "pedestrian"
][1]
trajectory_combined_cyclist <- trajectory_results[
  scheme_id == "12d + 12f equal-weight" &
    outcome == "cyclist"
][1]

weather_headline_results <- weather_sensitivity_results[
  comparison_id %chin% c(
    "12d + 12f versus LCR",
    "12d + 12f equal-weight"
  )
]
weather_sensitivity_rows <- paste(vapply(
  seq_len(nrow(weather_headline_results)),
  function(i) {
    row <- weather_headline_results[i]
    sign_changed <-
      sign(row$unadjusted_relative_percent) !=
      sign(row$weather_adjusted_relative_percent)
    impact <- if (sign_changed) {
      "Direction changes"
    } else if (abs(row$adjustment_shift_percentage_points) < 2) {
      "Minimal change"
    } else if (abs(row$adjustment_shift_percentage_points) < 5) {
      "Modest change"
    } else {
      "Material sensitivity"
    }
    paste0(
      "<tr><td>", html_escape(row$benchmark), "</td>",
      "<td>", row$outcome_label, "</td>",
      "<td>", sprintf("%+.1f%%", row$unadjusted_relative_percent), "</td>",
      "<td>", sprintf("%+.1f%%", row$weather_adjusted_relative_percent), "</td>",
      "<td>", sprintf(
        "%+.1f percentage points",
        row$adjustment_shift_percentage_points
      ), "</td>",
      "<td>", impact, "</td></tr>"
    )
  },
  character(1)
), collapse = "")

route_comparison_rows <- paste(vapply(seq_len(nrow(route_common)), function(i) {
  row <- route_common[i]
  paste0(
    "<tr><td>", html_escape(row$route_type), "</td>",
    "<td>", row$outcome_label, "</td>",
    "<td>", comma(row$sensors), "</td>",
    "<td>", comma(row$observed_countline_days), "</td>",
    "<td>", fmt(row$average_per_observed_day), "</td></tr>"
  )
}, character(1)), collapse = "")

scheme_stats <- all_sensor_summary[sensor_group == "Five scheme sensors"]
other_stats <- all_sensor_summary[sensor_group == "Other LCR sensors"]

css <- "
:root{--ink:#20313f;--muted:#5f6f7b;--line:#d5dee3;--soft:#f5f7f8;--navy:#255f85;--teal:#2a7f78;--coral:#d97757;--gold:#c89b3c;--pale-blue:#eaf2f7;--pale-teal:#e7f2f0;--pale-coral:#f9ece7;--pale-gold:#f6f0df;--good:#3e765f;--bad:#b45143}
*{box-sizing:border-box}html{scroll-behavior:smooth}body{font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',Arial,sans-serif;margin:0;color:var(--ink);line-height:1.48;background:#fff}main{max-width:1240px;margin:0 auto;padding:30px 24px 56px}header{border-bottom:4px solid var(--teal);padding-bottom:20px}h1{font-size:32px;line-height:1.12;margin:0 0 9px;letter-spacing:0}h2{font-size:23px;line-height:1.2;margin:0}h3{font-size:17px;margin:0 0 4px}h4{font-size:14px;margin:0}p{margin:0}.lead{max-width:1050px;color:#344754;font-size:15px}.eyebrow{font-size:12px;font-weight:700;text-transform:uppercase;color:var(--teal);margin-bottom:7px}.toc{display:flex;gap:8px;flex-wrap:wrap;margin:18px 0 0}.toc a{font-size:13px;color:var(--navy);text-decoration:none;border:1px solid #bdd0dc;padding:6px 9px;border-radius:4px;background:#fff}.section{padding:32px 0;border-bottom:1px solid var(--line)}.section-heading{display:flex;gap:14px;align-items:baseline;justify-content:space-between;margin-bottom:14px}.section-heading p{font-size:13px;color:var(--muted);max-width:680px}.note{padding:12px 14px;border-left:4px solid var(--navy);background:var(--pale-blue);color:#2c4555;margin:14px 0}.warning{border-left-color:var(--gold);background:var(--pale-gold)}.metric-grid{display:grid;grid-template-columns:repeat(4,minmax(0,1fr));gap:12px;margin:18px 0}.metric{border-top:3px solid var(--teal);background:var(--soft);padding:13px 14px;min-height:92px}.metric strong{display:block;font-size:24px;line-height:1.1;margin-top:7px}.metric span{font-size:12px;color:var(--muted)}.comparison-grid,.chart-grid{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:18px}.comparison-panel{border-top:3px solid var(--navy);padding:14px;background:#fff;box-shadow:0 0 0 1px var(--line)}.panel-heading{display:flex;justify-content:space-between;align-items:flex-start;gap:14px;margin-bottom:10px}.panel-heading p{font-size:12px;color:var(--muted)}.date-chip{font-size:11px;color:#344754;background:var(--pale-blue);border:1px solid #cbdbe5;padding:4px 7px;border-radius:4px;white-space:nowrap}.outcome-block{border-top:1px solid #e7ecef;padding:10px 0 4px}.outcome-title{display:flex;justify-content:space-between;gap:10px;align-items:center}.change{font-size:11px;padding:2px 6px;border-radius:3px;background:#edf1f4;color:var(--muted)}.change.positive{background:var(--pale-teal);color:var(--good)}.change.negative{background:var(--pale-coral);color:var(--bad)}.bar-row{display:grid;grid-template-columns:92px 1fr 78px;align-items:center;gap:9px;margin:6px 0}.bar-label{display:flex;justify-content:space-between;font-size:12px}.bar-label small{color:var(--muted)}.bar-track{height:19px;background:var(--soft);border:1px solid #e0e6e9;overflow:hidden}.bar{height:100%}.bar.before{background:var(--teal)}.bar.after{background:var(--coral)}.bar-value{text-align:right;font-size:12px;font-variant-numeric:tabular-nums}.empty .bar-value{color:#9aa6af}.interpretation{font-size:11px;color:var(--muted)}.chart-panel{margin:0;border-top:3px solid var(--teal);padding:10px 8px 6px;background:#fff;box-shadow:0 0 0 1px var(--line)}.plot-image{width:100%;height:auto;display:block}.chart-panel figcaption{font-size:11px;color:var(--muted);padding:4px 7px}.wide-chart{margin:16px 0;border-top:3px solid var(--navy);padding:10px;background:#fff;box-shadow:0 0 0 1px var(--line)}.split{display:grid;grid-template-columns:minmax(0,1.05fr) minmax(360px,.95fr);gap:20px;align-items:start}.table-wrap{overflow:auto;border:1px solid var(--line)}table{width:100%;border-collapse:collapse;font-size:12px}th{background:var(--navy);color:#fff;text-align:left;padding:8px;white-space:nowrap}td{padding:7px 8px;border-bottom:1px solid #e4eaed;font-variant-numeric:tabular-nums}tbody tr:nth-child(even){background:#f8fafb}.method-list{display:grid;grid-template-columns:220px 1fr;border-top:1px solid var(--line)}.method-list dt,.method-list dd{margin:0;padding:10px;border-bottom:1px solid var(--line)}.method-list dt{font-weight:700;background:var(--pale-blue)}.method-list dd{background:#fafbfc}.links{display:flex;gap:12px;flex-wrap:wrap;margin-top:14px}.links a{color:var(--navy);font-size:13px}.footer{padding-top:18px;color:var(--muted);font-size:12px}
@media(max-width:850px){main{padding:22px 14px 42px}.metric-grid{grid-template-columns:repeat(2,minmax(0,1fr))}.comparison-grid,.chart-grid,.split{grid-template-columns:1fr}.section-heading,.panel-heading{display:block}.date-chip{display:inline-block;margin-top:7px}.bar-row{grid-template-columns:78px 1fr 70px}.method-list{grid-template-columns:1fr}.method-list dd{border-left:1px solid var(--line)}}"

html <- paste0(
  '<!doctype html><html lang="en"><head><meta charset="utf-8">',
  '<meta name="viewport" content="width=device-width,initial-scale=1">',
  '<title>Vivacity five-scheme and city-region comparison</title>',
  '<style>', css, '</style></head><body><main>',
  '<header><div class="eyebrow">Liverpool City Region active-travel monitoring</div>',
  '<h1>Vivacity before/after averages and wider sensor context</h1>',
  '<p class="lead">This updated report combines the five scheme temporal profiles, fixed-year comparisons, the wider Liverpool City Region benchmark, IMD-based comparisons, separate pedestrian and cyclist pre-intervention trajectory matching, and weather-control sensitivity analysis. Other sensors without the required comparison observations are excluded from the relevant analyses.</p>',
  '<nav class="toc"><a href="#before-after">Before/after</a><a href="#scheme-trends">Five-scheme trends</a><a href="#other-sensors">Other LCR sensors</a><a href="#network-comparison">Scheme vs rest</a><a href="#baseline-regression">LCR baseline</a><a href="#imd-matched">IMD matched</a><a href="#trajectory-matched">Trajectory matched</a><a href="#weather-control">Weather control</a><a href="#route-types">Road vs path</a><a href="#methods">Methods</a></nav>',
  '</header>',

  '<section class="section" id="before-after"><div class="section-heading"><div><div class="eyebrow">Section 1</div><h2>Requested before-and-after year comparisons</h2></div><p>Annual totals divided by observed countline-days. Pedestrian and cyclist outcomes are shown separately; active travel is their sum.</p></div>',
  '<div class="note warning"><strong>Availability:</strong> no 2023-installed scheme has a usable 2021 baseline after applying the confirmed first-reliable dates and daily quality rule, so the 2021–2024 panels remain descriptive on the 2024 side only. The 2022–2025 comparison is available for schemes 12d and 12f. For scheme 13, 2022 has no usable baseline, 2023–2025 is available, and 2026 covers data only to 26 May.</div>',
  '<div class="comparison-grid">', comparison_panels, '</div></section>',

  '<section class="section" id="scheme-trends"><div class="section-heading"><div><div class="eyebrow">Section 2</div><h2>Monthly temporal profiles for the five schemes</h2></div><p>These are the HTML equivalents of the spreadsheet temporal visualisations, using the same official data and denominator.</p></div>',
  '<div class="chart-grid">', scheme_plot_html, '</div></section>',

  '<section class="section" id="other-sensors"><div class="section-heading"><div><div class="eyebrow">Section 3</div><h2>Comparable sensors in the rest of the city region</h2></div><p>“Rest of the city” means the remainder of the Liverpool City Region network, not Liverpool local authority alone. Only sensors with quality-passing observations in both 2022 and 2024 are retained.</p></div>',
  '<div class="metric-grid">',
  '<div class="metric"><span>Eligible other countlines</span><strong>', comma(eligible_other_count), '</strong></div>',
  '<div class="metric"><span>Other countlines excluded</span><strong>', comma(excluded_other_count), '</strong></div>',
  '<div class="metric"><span>Eligible matched sensor-days</span><strong>', comma(other_stats$valid_sensor_days), '</strong></div>',
  '<div class="metric"><span>Exact matched calendar days</span><strong>', comma(matched_day_count), '</strong></div>',
  '</div>',
  '<div class="note">The eligibility cohort contains ', comma(eligible_other_count), ' sensors with at least one quality-passing full day in 2022 and at least one in 2024. The other ', comma(excluded_other_count), ' sensors are removed from every LCR-wide table, chart, and comparison. No other LCR sensor has a usable 2021 observation, so a matching LCR-wide comparison beginning in 2021 cannot be produced.</div>',
  '<div class="split"><figure class="chart-panel">', other_before_after_svg, '</figure>',
  '<div class="table-wrap"><table><thead><tr><th>Outcome</th><th>2022 avg/day</th><th>2024 avg/day</th><th>Difference</th><th>Change</th></tr></thead><tbody>', other_before_after_rows, '</tbody></table></div></div>',
  '<div class="wide-chart">', other_trend_svg, '</div>',
  '<div class="split"><figure class="chart-panel">', lad_svg, '<figcaption>Counts refer only to the 2022–2024 eligible other-sensor cohort on the exact five-scheme matched dates.</figcaption></figure>',
  '<div><div class="table-wrap"><table><thead><tr><th>Local authority</th><th>Usable countlines</th><th>Sensor-days</th></tr></thead><tbody>', lad_rows, '</tbody></table></div>',
  '<div class="table-wrap" style="margin-top:14px"><table><thead><tr><th>Route type</th><th>Usable countlines</th><th>Sensor-days</th></tr></thead><tbody>', route_rows, '</tbody></table></div></div></div></section>',

  '<section class="section" id="network-comparison"><div class="section-heading"><div><div class="eyebrow">Section 4</div><h2>Five scheme sensors compared with eligible other LCR sensors</h2></div><p>The bar chart normalizes each group’s combined active-travel average to 100%, allowing pedestrian and cyclist composition to be compared without the larger other-sensor totals dominating the scale.</p></div>',
  '<div class="note warning"><strong>Interpret carefully:</strong> exact day matching removes differences caused by comparing different calendar dates, but this remains a contextual benchmark rather than a matched-location control or causal estimate. The five scheme locations were selected for active-travel investment and have a different mix of roads and paths from the wider sensor network. On these matched dates, ',
  comma(common_group[sensor_group == "Five scheme sensors", max(sensors)]),
  ' scheme countlines and ',
  comma(common_group[sensor_group == "Other LCR sensors", max(sensors)]),
  ' other countlines contribute; one of the 21 scheme countlines has no quality-passing observation on the matched dates.</div>',
  '<div class="split"><figure class="chart-panel">', common_bar_svg, '</figure><figure class="chart-panel">', monthly_comparison_svg, '</figure></div>',
  '<div class="table-wrap" style="margin-top:18px"><table><thead><tr><th>Outcome</th><th>Scheme share of active travel</th><th>Other share of active travel</th><th>Scheme sensors pooled avg/day</th><th>Other sensors pooled avg/day</th><th>Difference</th><th>Ratio</th><th>Median scheme sensor</th><th>Median other sensor</th></tr></thead><tbody>', comparison_rows, '</tbody></table></div>',
  '<p class="note"><strong>How to read the percentages:</strong> active travel equals pedestrian plus cyclist, so active travel is 100% for both groups. The pedestrian and cyclist percentages show the composition within each group, not the relative number of movements between groups. Raw pooled averages remain in the table. The pooled average weights each observed sensor-day equally after restricting both groups to the same calendar dates.</p></section>',

  '<section class="section" id="baseline-regression"><div class="section-heading"><div><div class="eyebrow">Section 5</div><h2>Did the analysable schemes increase more than the LCR comparison trend?</h2></div><p>This 2022–2024 baseline-adjusted model is limited to schemes 12d and 12f because they are the only schemes with usable 2022 pre-installation observations. It cannot estimate a five-scheme causal effect.</p></div>',
  '<div class="note warning"><strong>Headline finding:</strong> on paired 15 November–31 December dates, the 126 eligible LCR comparison sensors changed by an adjusted ', sprintf("%+.1f%%", active_regression$lcr_adjusted_change_percent), ' in combined active travel, while the seven balanced scheme countlines changed by approximately ', sprintf("%+.1f%%", active_regression$scheme_adjusted_change_percent), '. The scheme trajectory was therefore ', sprintf("%.1f%%", abs(active_regression$scheme_relative_to_lcr_percent)), ' lower relative to the LCR comparison trend. The clustered 95% interval is ', sprintf("%+.1f%% to %+.1f%%", active_regression$relative_lower_95, active_regression$relative_upper_95), ' with p = ', format.pval(active_regression$relative_p_value, digits = 3, eps = 0.001), '. An equal-weight sensor-level robustness test gives p = ', format.pval(active_regression$sensor_level_p_value, digits = 3, eps = 0.001), '. Combined active travel is therefore directionally lower, but statistically inconclusive.</div>',
  '<div class="wide-chart">', regression_svg, '</div>',
  '<div class="table-wrap"><table><thead><tr><th>Outcome</th><th>LCR adjusted change</th><th>Scheme adjusted change</th><th>Scheme relative to LCR</th><th>Clustered 95% CI</th><th>Clustered p</th><th>Sensor-level p</th><th>Interpretation</th></tr></thead><tbody>', regression_rows, '</tbody></table></div>',
  '<p class="note">The cyclist result is the most consistent evidence of a weaker scheme trajectory: the eligible LCR comparison sensors increased by ', sprintf("%+.1f%%", cyclist_regression$lcr_adjusted_change_percent), ', whereas the balanced scheme countlines changed by ', sprintf("%+.1f%%", cyclist_regression$scheme_adjusted_change_percent), '; the relative difference is ', sprintf("%+.1f%%", cyclist_regression$scheme_relative_to_lcr_percent), '. Both the clustered model (p = ', format.pval(cyclist_regression$relative_p_value, digits = 3, eps = 0.001), ') and the equal-weight sensor-level check (p = ', format.pval(cyclist_regression$sensor_level_p_value, digits = 3, eps = 0.001), ') retain this direction. Nevertheless, these are exploratory associations. Only seven treated countlines are available and scheme placement was non-random. Pre-intervention trajectory matching is assessed separately in Section 7 and weather in Section 8.</p></section>',

  '<section class="section" id="imd-matched"><div class="section-heading"><div><div class="eyebrow">Section 6</div><h2>Cross-comparison with IMD-matched street sensors</h2></div><p>Each of the six scheme LSOA contexts is paired with the route-compatible non-intervention street whose available sensor area has the nearest selected IMD score.</p></div>',
  '<div class="note"><strong>Matching structure:</strong> the five schemes occupy six LSOA contexts because scheme 12e spans St. Helens 008C and St. Helens 014D. The descriptive comparison uses ', comma(imd_composition_day_count), ' exact dates from ', imd_composition_start, ' to ', imd_composition_end, ', when every scheme context and its paired street have usable observations. ', comma(uniqueN(imd_composition_daily[imd_role == "Five scheme sensors", countline_id])), ' scheme countlines and ', comma(uniqueN(imd_composition_daily[imd_role == "IMD-matched sensors", countline_id])), ' matched-street countlines contribute.</div>',
  '<div class="table-wrap"><table><thead><tr><th>Scheme context</th><th>Scheme-area IMD</th><th>Paired street and LSOA</th><th>Paired-area IMD</th><th>Score difference</th><th>Post-period volume ratio</th><th>Control countline IDs</th></tr></thead><tbody>', imd_match_rows, '</tbody></table></div>',
  '<div class="note warning"><strong>Match diagnostic:</strong> similar IMD does not guarantee a similar street. The post-period active-travel volume ratio is shown as a diagnostic only and was not used to choose the controls. Large differences, especially for scheme 12b and its central-Liverpool match, indicate different land use or network function and make absolute-volume comparison inappropriate.</div>',
  '<h3 style="margin-top:22px">Percentage-normalized active-travel composition</h3>',
  '<p class="note">Combined active travel is set to 100% separately for each scheme and its paired IMD streets. The five scheme-specific percentages are then averaged so every scheme receives equal weight; schemes with more countlines or observed days no longer dominate the headline. On this basis, the scheme sensors comprise ', fmt(imd_scheme_pedestrian_share), '% pedestrian and ', fmt(imd_scheme_cyclist_share), '% cyclist movements; the paired IMD sensors comprise ', fmt(imd_control_pedestrian_share), '% pedestrian and ', fmt(imd_control_cyclist_share), '% cyclist movements. These percentages describe composition, not equal absolute volumes.</p>',
  '<div class="split"><figure class="chart-panel">', imd_composition_overall_svg, '</figure>',
  '<div class="table-wrap"><table><thead><tr><th>Outcome</th><th>Scheme share</th><th>IMD-paired share</th><th>Scheme avg/day</th><th>IMD-paired avg/day</th></tr></thead><tbody>', imd_overall_composition_rows, '</tbody></table></div></div>',
  '<div class="wide-chart">', imd_composition_pair_svg, '</div>',
  '<div class="table-wrap"><table><thead><tr><th>Scheme context</th><th>Scheme pedestrian</th><th>Scheme cyclist</th><th>Paired pedestrian</th><th>Paired cyclist</th></tr></thead><tbody>', imd_pair_composition_rows, '</tbody></table></div>',
  '<h3 style="margin-top:24px">Before/after change relative to the paired IMD baseline</h3>',
  '<div class="note warning"><strong>Revised main result:</strong> after first averaging countlines within each street context and then giving schemes 12d and 12f equal weight, the paired IMD streets changed by ', sprintf("%+.1f%%", imd_context_combined_active$control_change_percent), ' in active travel from 2022 to 2024, while the scheme contexts changed by ', sprintf("%+.1f%%", imd_context_combined_active$scheme_change_percent), '. The scheme trajectory was ', sprintf("%+.1f%%", imd_context_combined_active$scheme_relative_to_control_percent), ' relative to the paired baseline. A seven-day moving-block bootstrap over the selected matched dates gives a conditional interval of ', sprintf("%+.1f%% to %+.1f%%", imd_context_combined_active$relative_lower_95, imd_context_combined_active$relative_upper_95), '. This interval measures date-to-date uncertainty within these chosen pairs; it does not account for uncertainty in control selection or establish causation.</div>',
  '<div class="note"><strong>Model sensitivity:</strong> the earlier countline-weighted fixed-effects specification estimated a larger relative difference of ', sprintf("%+.1f%%", imd_combined_active$scheme_relative_to_control_percent), ' with a 95% interval of ', sprintf("%+.1f%% to %+.1f%%", imd_combined_active$relative_lower_95, imd_combined_active$relative_upper_95), '. Because the matched-context estimate is ', sprintf("%+.1f%%", imd_context_combined_active$scheme_relative_to_control_percent), ', the exact magnitude and conventional significance are sensitive to weighting and model choice. The stable conclusion is directional: the analysable scheme contexts did not outperform their selected IMD matches over these windows.</div>',
  '<div class="note"><strong>Availability:</strong> schemes 12d and 12f use matched 2022 and 2024 month-days. Scheme 13 uses matched 2023 and 2024 month-days because its reliable record begins on 27 September 2023, before its confirmed 1 March 2024 installation. Schemes 12b and 12e have no quality-passing pre-installation scheme observations, so their matched streets can support descriptive composition and post-installation trajectory comparisons only.</div>',
  '<div class="wide-chart">', imd_active_baseline_svg, '</div>',
  '<div class="table-wrap"><table><thead><tr><th>Comparison</th><th>Years</th><th>Matched month-days</th><th>Scheme/control countlines</th><th>IMD baseline change</th><th>Scheme change</th><th>Scheme relative to baseline</th><th>Conditional block-bootstrap interval</th><th>Interpretation</th></tr></thead><tbody>', imd_active_baseline_rows, '</tbody></table></div>',
  '<details style="margin-top:16px"><summary><strong>Countline-weighted fixed-effects sensitivity results</strong></summary><p class="note">These estimates retain countlines as the modelling units and cluster standard errors by countline. They are shown to demonstrate model sensitivity, not as the preferred headline.</p><div class="table-wrap" style="margin-top:10px"><table><thead><tr><th>Comparison</th><th>Outcome</th><th>IMD baseline change</th><th>Scheme change</th><th>Relative difference</th><th>Clustered p</th></tr></thead><tbody>', imd_baseline_detail_rows, '</tbody></table></div></details>',
  '<p class="note warning"><strong>Interpretation limit:</strong> IMD matching improves socioeconomic comparability, but it does not establish a valid counterfactual by itself. There are only two independent 2022–2024 matched scheme contexts, and one 2023–2024 context for scheme 13. Parallel pre-trends cannot be tested, the controls may have their own interventions, and land use, network form, traffic conditions, sensor configuration, and regression-to-the-mean remain uncontrolled. Weather is assessed separately as a model sensitivity, but that does not solve these remaining design limitations. Conditional bootstrap intervals should not be read as population-level causal confidence intervals.</p></section>',

  '<section class="section" id="trajectory-matched"><div class="section-heading"><div><div class="eyebrow">Section 7</div><h2>Pre-intervention percentage-trajectory matched controls</h2></div><p>Pedestrian and cyclist controls are selected separately using only observations before each confirmed installation date. Road/path type is not used.</p></div>',
  '<div class="note"><strong>Percentage and log scale:</strong> each treated and candidate pre-installation series is converted to an index with its own pre-period mean equal to 100. Trend models use <code>log(percentage index / 100)</code>, so matching evaluates proportional movement rather than raw count size. Absolute count levels are retained only as a broad comparability gate, requiring the candidate mean to be between one-half and twice the treated mean.</div>',
  '<div class="note"><strong>Selection requirements:</strong> candidates must remain within 10 IMD-score points and one IMD decile, share at least 80% of treated pre-period dates, retain at least 60% coverage in every pre-period month, have at least 75% average countline availability, show similar weekly log trends and monthly/weekday percentage profiles, and have a non-negative weekly percentage-log correlation. No candidate street is reused within the same outcome.</div>',
  '<div class="table-wrap"><table><thead><tr><th>Scheme</th><th>Outcome</th><th>Selected control</th><th>IMD-score difference</th><th>Shared pre-days</th><th>Count-level ratio</th><th>Weekly trend difference</th><th>Monthly percentage-shape difference</th><th>Weekly correlation</th></tr></thead><tbody>', trajectory_selected_rows, '</tbody></table></div>',
  '<div class="note warning"><strong>Availability:</strong> trajectory matching is possible only for schemes 12d, 12f, and 13. Schemes 12b and 12e have no quality-passing treated observations before their confirmed installation dates, so their pre-intervention percentage trends cannot be estimated.</div>',
  '<div class="wide-chart">', trajectory_percentage_svg, '</div>',
  '<div class="table-wrap"><table><thead><tr><th>Scheme</th><th>Outcome</th><th>Years</th><th>Matched month-days</th><th>Scheme post-index</th><th>Control post-index</th><th>Scheme relative to control</th></tr></thead><tbody>', trajectory_result_rows, '</tbody></table></div>',
  '<div class="note"><strong>Equal-weight 12d/12f result:</strong> after selecting different controls for each mode and setting every pre-period to 100, the combined pedestrian index is ', sprintf("%.1f", trajectory_combined_pedestrian$scheme_post_index), ' for the schemes and ', sprintf("%.1f", trajectory_combined_pedestrian$control_post_index), ' for their controls, a relative difference of ', sprintf("%+.1f%%", trajectory_combined_pedestrian$scheme_relative_to_control_percent), '. For cyclists, the scheme index is ', sprintf("%.1f", trajectory_combined_cyclist$scheme_post_index), ' and the control index is ', sprintf("%.1f", trajectory_combined_cyclist$control_post_index), ', a relative difference of ', sprintf("%+.1f%%", trajectory_combined_cyclist$scheme_relative_to_control_percent), '.</div>',
  '<p class="note warning"><strong>Interpretation limit:</strong> this improves pre-intervention comparability but remains a sensitivity analysis. The pre-period is short, especially for scheme 12d; scheme 13 has only one qualifying pedestrian control; and the selected streets still require external verification that no other intervention affected them.</p></section>',

  '<section class="section" id="weather-control"><div class="section-heading"><div><div class="eyebrow">Section 8</div><h2>Weather-control feasibility and sensitivity</h2></div><p>Daily ERA5 reanalysis is linked to each matched scheme/control context and to a Liverpool City Region area-average series.</p></div>',
  '<div class="note"><strong>Feasibility:</strong> weather control is feasible and reproducible for this project. The model uses daily mean temperature, precipitation, maximum 10 m wind speed, and shortwave solar radiation from 1 January 2020 to 31 May 2025. The source is ERA5 through the Open-Meteo Historical Weather API, using Europe/London dates. Data are complete for ', nrow(weather_sites), ' matched context locations.</div>',
  '<div class="note warning"><strong>How to interpret it:</strong> the existing exact-date matching already removes most broad weather differences because scheme and comparison sensors are observed on the same days. The weather-adjusted models therefore test residual differences between years and locations; they are sensitivity analyses, not a cure for non-random scheme placement or weak control matching.</div>',
  '<div class="note"><strong>Active-travel result:</strong> against the wider LCR benchmark, the relative estimate changes from ', sprintf("%+.1f%%", weather_lcr_active$unadjusted_relative_percent), ' to ', sprintf("%+.1f%%", weather_lcr_active$weather_adjusted_relative_percent), ' after weather adjustment, a shift of ', sprintf("%+.1f percentage points", weather_lcr_active$adjustment_shift_percentage_points), '. In the combined 12d/12f IMD-context regression sensitivity, it changes from ', sprintf("%+.1f%%", weather_imd_active$unadjusted_relative_percent), ' to ', sprintf("%+.1f%%", weather_imd_active$weather_adjusted_relative_percent), ', a shift of ', sprintf("%+.1f percentage points", weather_imd_active$adjustment_shift_percentage_points), '. The preferred equal-context matched-date estimate remains ', sprintf("%+.1f%%", imd_context_combined_active$scheme_relative_to_control_percent), '; weather adjustment is reported alongside it rather than replacing it.</div>',
  '<div class="wide-chart">', weather_sensitivity_svg, '</div>',
  '<div class="table-wrap"><table><thead><tr><th>Benchmark</th><th>Outcome</th><th>Matched-date estimate</th><th>Weather-adjusted estimate</th><th>Adjustment shift</th><th>Sensitivity</th></tr></thead><tbody>', weather_sensitivity_rows, '</tbody></table></div>',
  '<p class="note warning"><strong>Resolution limit:</strong> ERA5 is gridded reanalysis at approximately 0.25 degrees, about 25 km, so it represents city-region daily conditions rather than rain or wind measured directly at each street. It is suitable for controlling broad daily weather but cannot capture a short local shower, shade, surface condition, or microclimate at an individual sensor.</p></section>',

  '<section class="section" id="route-types"><div class="section-heading"><div><div class="eyebrow">Section 9</div><h2>Road and path/cycle-facility outcomes within the five schemes</h2></div><p>Separating route type prevents pedestrian-heavy paths and cyclist-heavy road countlines from being blended without explanation.</p></div>',
  '<div class="split"><figure class="chart-panel">', route_svg, '</figure>',
  '<div class="table-wrap"><table><thead><tr><th>Route type</th><th>Outcome</th><th>Sensors</th><th>Observed sensor-days</th><th>Average/day</th></tr></thead><tbody>', route_comparison_rows, '</tbody></table></div></div></section>',

  '<section class="section" id="methods"><div class="section-heading"><div><div class="eyebrow">Section 10</div><h2>Methods and definitions</h2></div></div>',
  '<dl class="method-list">',
  '<dt>Official source</dt><dd>The citywide comparison uses 90 Vivacity dashboard hourly exports covering 711 Liverpool City Region countlines from 1 January 2020 to 31 May 2025. The fixed-year scheme panels additionally use the validated scheme-only daily archive through 26 May 2026; the partial 2026 period is used only for the requested Scheme 13 comparison.</dd>',
  '<dt>Scheme countlines</dt><dd>', comma(uniqueN(treated_lookup$countline_id)), ' countlines are linked to schemes 12b, 12d, 12e, 12f, and 13; ', comma(scheme_stats$valid_sensors), ' have at least one quality-passing observation on the exact matched dates. Exact installation dates are 12b: 2023-03-01; 12d: 2023-03-01; 12e: 2023-03-01; 12f: 2023-06-01; 13: 2024-03-01.</dd>',
  '<dt>Quality rule</dt><dd>At least 23 distinct hourly periods must be present, with non-missing availability and both pedestrian and cyclist fields observed in at least 23 hourly periods. Daily minimum reported availability must be at least 80%, and no data error may be recorded. The 23-hour threshold retains daylight-saving transition days while excluding incomplete export-boundary fragments. Scheme-sensor dates must also be on or after the confirmed first reliable date.</dd>',
  '<dt>Outcome</dt><dd>Full-day pedestrian, cyclist, and active-travel counts. Active travel equals pedestrian plus cyclist. Despite the legacy filename, these figures are not AM/PM peak-hour counts.</dd>',
  '<dt>Denominator</dt><dd>Count totals divided by observed countline-days, so unavailable sensor-days are excluded rather than entered as zero.</dd>',
  '<dt>Percentage normalization</dt><dd>For the scheme-versus-other bar chart, each group’s pooled active-travel average is set to 100%. Pedestrian and cyclist values are divided by that group-specific active-travel average. This removes the level difference for visual comparison but does not imply equal absolute volumes.</dd>',
  '<dt>Baseline regression</dt><dd>A log-linear difference-in-differences model compares 2022 with 2024 using paired calendar days from 15 November to 31 December. The model includes a general 2024 LCR-comparison change, an additional scheme-by-2024 term, countline fixed effects, month-day controls, and weekday controls. Standard errors are clustered by countline. Sensors require at least 20 paired month-days. The final cohort contains ', regression_results[1, treated_countlines], ' treated countlines from schemes 12d and 12f and ', regression_results[1, control_countlines], ' eligible other LCR countlines. Because only seven treated countlines are available, an equal-weight sensor-level Welch test is also reported as a robustness check. Parallel pre-trends cannot be tested from the short baseline.</dd>',
  '<dt>IMD matching</dt><dd>Matching uses the continuous 2019 Index of Multiple Deprivation score assigned to each sensor’s LSOA 2021 area. The closest available route-compatible non-intervention street is selected for each of the six treated LSOA contexts. Continuous score distance is ranked before decile agreement because close scores can fall on opposite sides of a decile boundary. Scheme 12e is represented by two separate LSOA-context pairs rather than one averaged deprivation value.</dd>',
  '<dt>IMD composition comparison</dt><dd>Every scheme context and paired street must have at least one quality-passing observation on a retained calendar date. Across the resulting ', comma(imd_composition_day_count), ' dates, totals are divided by observed countline-days and active travel is set to 100% within each scheme and paired-control group. Scheme 12e’s two contexts are combined at scheme level. The five scheme-level percentages are then averaged with equal weight, preventing schemes with more sensors or more observed days from dominating the headline composition.</dd>',
  '<dt>Preferred IMD change estimator</dt><dd>For each analysable pair and matched month-day, countlines are first averaged into one scheme-context value and one paired-control-context value. Pre/post log changes are calculated within each context and differenced between scheme and control. Schemes 12d and 12f use 2022–2024 comparisons and receive equal weight in the combined estimate; scheme 13 uses 2023–2024. Seven-day circular moving-block resampling provides an interval for date-to-date uncertainty conditional on the selected pairs. It does not capture control-selection uncertainty and is not a causal population confidence interval.</dd>',
  '<dt>Countline-model sensitivity</dt><dd>A log-linear fixed-effects model is retained as a sensitivity analysis. It includes countline fixed effects, common month-day controls, weekday controls, and countline-clustered standard errors. Its larger estimated effect demonstrates that the magnitude and conventional inference are sensitive to weighting and model structure.</dd>',
  '<dt>Pre-intervention trajectory matching</dt><dd>Pedestrian and cyclist controls are selected separately. Within each mode, treated and candidate pre-installation daily series are divided by their own pre-period means and multiplied by 100. Weekly trend models use log(percentage index / 100). Matching also compares monthly and weekday percentage profiles, shared-date and monthly coverage, countline availability, broad count-level comparability, and IMD proximity. Road/path type is recorded but is not used for selection. Candidates require at least 80% shared dates, 60% coverage in every pre-period month, 75% countline availability, a count-level ratio from 0.5 to 2.0, an IMD-score difference no greater than 10, an IMD-decile difference no greater than one, and a non-negative weekly percentage-log correlation.</dd>',
  '<dt>Weather source</dt><dd>Daily weather is drawn from ERA5 reanalysis through the Open-Meteo Historical Weather API for 1 January 2020 to 31 May 2025. Twelve scheme/control context centroids are used. Variables are daily mean 2 m temperature in degrees Celsius, precipitation sum in millimetres, maximum 10 m wind speed in kilometres per hour, and shortwave solar radiation in megajoules per square metre. Dates use the Europe/London timezone. ERA5 has approximately 0.25-degree, about 25 km, spatial resolution and should be interpreted as city-region weather.</dd>',
  '<dt>Weather-adjusted sensitivity</dt><dd>The LCR and IMD-context log-linear models are re-estimated after adding standardized temperature, log-transformed precipitation, maximum wind speed, and shortwave solar radiation. The LCR model uses the daily mean across the twelve context grids; the IMD model uses each scheme or control context’s grid. The weather-adjusted coefficient is compared directly with the corresponding model without weather. It is not substituted for the preferred matched-context estimate because exact-date matching already controls common daily weather and because only two independent 2022–2024 scheme contexts are available.</dd>',
  '<dt>Other-sensor eligibility</dt><dd>An other LCR countline is included only if it has at least one quality-passing full day in both 2022 and 2024. This retains ', comma(eligible_other_count), ' of ', comma(all_other_count), ' other countlines and excludes ', comma(excluded_other_count), '. No other countline has usable 2021 data, so no LCR-wide comparison beginning in 2021 is reported.</dd>',
  '<dt>Exact day matching</dt><dd>For the scheme-versus-other contextual comparison, a calendar date is retained only when every one of the five scheme groups has at least one quality-passing observation. The eligible other LCR sensors are then filtered to precisely those ', comma(matched_day_count), ' dates, spanning ', common_start, ' to ', common_end, '.</dd>',
  '</dl>',
  '<div class="links"><a href="vivacity_analysis_integrity_audit.csv">Analysis integrity audit CSV</a><a href="vivacity_preintervention_trajectory_selected_controls.csv">Trajectory-selected controls CSV</a><a href="vivacity_preintervention_trajectory_candidate_scores.csv">All trajectory candidate scores CSV</a><a href="vivacity_preintervention_trajectory_results.csv">Trajectory comparison results CSV</a><a href="vivacity_preintervention_trajectory_unavailable_schemes.csv">Unavailable schemes CSV</a><a href="vivacity_baseline_adjusted_regression_2022_2024.csv">LCR baseline results CSV</a><a href="vivacity_weather_adjusted_lcr_regression_2022_2024.csv">Weather-adjusted LCR model CSV</a><a href="vivacity_weather_control_sensitivity_summary.csv">Weather sensitivity summary CSV</a><a href="vivacity_era5_weather_site_lookup.csv">Weather site lookup CSV</a><a href="vivacity_era5_weather_daily.csv">Daily ERA5 weather CSV</a><a href="vivacity_imd_weather_adjusted_sensitivity.csv">IMD weather sensitivity CSV</a><a href="vivacity_baseline_adjusted_regression_cohort.csv">LCR regression cohort CSV</a><a href="vivacity_imd_matched_sensor_pairs.csv">IMD matched pairs CSV</a><a href="vivacity_imd_matched_active_travel_composition.csv">Equal-scheme IMD composition CSV</a><a href="vivacity_imd_matched_context_change_results.csv">Preferred IMD context-change results CSV</a><a href="vivacity_imd_matched_countline_model_sensitivity.csv">Countline-model sensitivity CSV</a><a href="vivacity_imd_matched_level_diagnostics.csv">IMD match diagnostics CSV</a><a href="vivacity_imd_matched_baseline_cohort.csv">IMD baseline cohort CSV</a><a href="vivacity_imd_matched_exact_composition_dates.csv">IMD exact dates CSV</a><a href="vivacity_other_lcr_2022_2024_sensor_eligibility.csv">Other-sensor eligibility CSV</a><a href="vivacity_other_lcr_before_after_2022_2024.csv">Other-sensor before/after CSV</a><a href="vivacity_exact_five_scheme_matched_dates.csv">Exact five-scheme dates CSV</a><a href="vivacity_citywide_monthly_sensor_group_comparison.csv">Monthly comparison CSV</a><a href="vivacity_citywide_common_window_comparison.csv">Matched-day comparison CSV</a><a href="vivacity_other_lcr_sensor_summary_by_local_authority.csv">Local-authority summary CSV</a><a href="vivacity_five_scheme_common_window_route_comparison.csv">Road/path comparison CSV</a><a href="https://open-meteo.com/en/docs/historical-weather-api">Open-Meteo historical weather documentation</a><a href="https://cds.climate.copernicus.eu/datasets/reanalysis-era5-single-levels">Copernicus ERA5 dataset</a></div>',
  '</section>',
  '<p class="footer">Generated from the validated official dashboard exports. Updated 15 June 2026.</p>',
  '</main></body></html>'
)

writeLines(html, report_path, useBytes = TRUE)
file.copy(report_path, file.path(output_visualised_dir, report_name), overwrite = TRUE)

cat(sprintf(
  "Wrote %s\nEligible other sensors: %s of %s\nOther sensors on matched dates: %s\nExact matched dates: %s (%s to %s)\n",
  report_path,
  eligible_other_count,
  all_other_count,
  other_sensor_summary$valid_sensors,
  matched_day_count,
  common_start,
  common_end
))
