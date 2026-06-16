suppressPackageStartupMessages({
  library(data.table)
})

base_dir <- "/Users/lu_nanxi/CASA/Dissertation_Data"
report_dir <- file.path(base_dir, "reports", "analysis_reports")
share_dir <- file.path(
  base_dir,
  "output_visualised",
  "reports",
  "analysis_reports"
)
dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(share_dir, recursive = TRUE, showWarnings = FALSE)

assert_true <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
}

cache_path <- file.path(
  report_dir,
  "vivacity_citywide_quality_gated_daily_cache.csv.gz"
)
daily <- fread(cmd = sprintf("gzip -dc %s", shQuote(cache_path)))
daily[, date := as.IDate(date)]
daily[, calendar_year := as.integer(format(date, "%Y"))]
daily[, month_day := format(date, "%m-%d")]
daily[, month_id := format(date, "%Y-%m")]
daily[, weekday_number := as.integer(format(date, "%u"))]

context_archive <- file.path(
  base_dir,
  "data_archives",
  "05_context_spatial_census_imd_data.zip"
)
context <- fread(cmd = sprintf(
  "unzip -p %s %s",
  shQuote(context_archive),
  shQuote("context_data/processed/matching/all_vivacity_countlines_lsoa2021_context.csv")
))[, .(
  countline_id = as.integer(countline_id),
  analysis_lsoa21cd
)]
imd <- fread(cmd = sprintf(
  "unzip -p %s %s",
  shQuote(context_archive),
  shQuote("context_data/processed/imd/imd_2019_lcr_lsoa2021_collapsed.csv")
))[, .(
  analysis_lsoa21cd = LSOA21CD,
  imd_score,
  imd_decile
)]
countline_imd <- merge(context, imd, by = "analysis_lsoa21cd")
daily <- merge(daily, countline_imd, by = "countline_id", all.x = TRUE)
assert_true(
  !anyNA(daily[
    sensor_group == "Five scheme sensors",
    .(countline_id, lsoa21nm, imd_score, imd_decile)
  ]),
  "One or more treated countlines failed to join to IMD."
)
daily <- daily[
  sensor_group == "Five scheme sensors" |
    (!is.na(lsoa21nm) & !is.na(imd_score) & !is.na(imd_decile))
]

normalize_street <- function(values) {
  result <- tolower(trimws(values))
  result <- sub("[ ]+-[ ]+(eastbound|westbound).*", "", result)
  result <- sub(",?[ ]+near[ ]+.*", "", result)
  result <- sub(",[ ]*[nesw][ ]+of[ ]+.*", "", result)
  result <- gsub("[ ]+", " ", result)
  result
}
daily[, candidate_street := normalize_street(hardware_name)]

treated_specs <- data.table(
  scheme_id = c("12d", "12f", "13"),
  pair_id = c(
    "12d | Liverpool 052A",
    "12f | Wirral 006A",
    "13 | Halton 010B"
  ),
  treated_lsoa21nm = c(
    "Liverpool 052A",
    "Wirral 006A",
    "Halton 010B"
  ),
  installation_date = as.IDate(c(
    "2023-03-01",
    "2023-06-01",
    "2024-03-01"
  )),
  pre_start = as.IDate(c(
    "2022-11-15",
    "2022-11-15",
    "2023-09-27"
  )),
  pre_end = as.IDate(c(
    "2023-02-28",
    "2023-05-31",
    "2024-02-29"
  )),
  comparison_pre_year = c(2022L, 2022L, 2023L),
  comparison_post_year = c(2024L, 2024L, 2024L),
  comparison_window_start = c("11-15", "11-15", "09-27"),
  comparison_window_end = c("12-31", "12-31", "12-31")
)

treated_contexts <- daily[
  sensor_group == "Five scheme sensors" &
    analysis_scheme_id %chin% treated_specs$scheme_id,
  .(
    treated_countlines = uniqueN(countline_id),
    treated_imd_score = first(imd_score),
    treated_imd_decile = first(imd_decile),
    treated_routes = paste(sort(unique(route_type)), collapse = ";")
  ),
  by = .(
    scheme_id = analysis_scheme_id,
    treated_lsoa21nm = lsoa21nm
  )
]
treated_specs <- merge(
  treated_specs,
  treated_contexts,
  by = c("scheme_id", "treated_lsoa21nm"),
  all.x = TRUE
)
assert_true(
  !anyNA(treated_specs[, .(
    treated_countlines,
    treated_imd_score,
    treated_imd_decile,
    treated_routes
  )]),
  "Treated trajectory specifications are incomplete."
)

candidate_inventory <- daily[
  sensor_group == "Other LCR sensors" &
    !is.na(candidate_street) &
    candidate_street != "",
  .(
    candidate_countlines = uniqueN(countline_id),
    candidate_countline_ids = paste(sort(unique(countline_id)), collapse = ";"),
    candidate_hardware_names = paste(
      sort(unique(hardware_name)),
      collapse = "; "
    ),
    candidate_local_authority = first(local_authority),
    candidate_imd_score = first(imd_score),
    candidate_imd_decile = first(imd_decile),
    candidate_routes = paste(sort(unique(route_type)), collapse = ";"),
    candidate_first_date = min(date),
    candidate_last_date = max(date)
  ),
  by = .(
    candidate_lsoa21nm = lsoa21nm,
    candidate_street
  )
]
candidate_inventory[, candidate_id := paste(
  candidate_lsoa21nm,
  candidate_street,
  sep = " | "
)]

percentage_profile_log_rmse <- function(dt, group_column, index_columns) {
  profile <- dt[
    ,
    lapply(.SD, mean),
    by = group_column,
    .SDcols = index_columns
  ]
  if (nrow(profile) < 2L) return(Inf)
  left <- log(pmax(profile[[index_columns[1]]], 1e-6) / 100)
  right <- log(pmax(profile[[index_columns[2]]], 1e-6) / 100)
  sqrt(mean((left - right)^2))
}

trajectory_metrics <- function(spec, candidate, outcome) {
  treated_source <- daily[
    sensor_group == "Five scheme sensors" &
      analysis_scheme_id == spec$scheme_id &
      lsoa21nm == spec$treated_lsoa21nm &
      date >= spec$pre_start &
      date <= spec$pre_end
  ]
  candidate_source <- daily[
    sensor_group == "Other LCR sensors" &
      lsoa21nm == candidate$candidate_lsoa21nm &
      candidate_street == candidate$candidate_street &
      date >= spec$pre_start &
      date <= spec$pre_end
  ]

  treated_ids <- uniqueN(treated_source$countline_id)
  candidate_ids <- uniqueN(candidate_source$countline_id)
  treated_daily <- treated_source[
    ,
    .(
      treated_value = mean(get(outcome)),
      treated_countlines_observed = uniqueN(countline_id)
    ),
    by = .(date, month_id, weekday_number)
  ]
  candidate_daily <- candidate_source[
    ,
    .(
      candidate_value = mean(get(outcome)),
      candidate_countlines_observed = uniqueN(countline_id)
    ),
    by = .(date, month_id, weekday_number)
  ]
  common <- merge(
    treated_daily,
    candidate_daily,
    by = c("date", "month_id", "weekday_number")
  )
  treated_dates <- uniqueN(treated_daily$date)
  common_dates <- uniqueN(common$date)
  coverage_ratio <- if (treated_dates == 0L) 0 else common_dates / treated_dates
  candidate_countline_coverage <- if (
    common_dates == 0L || candidate_ids == 0L
  ) {
    0
  } else {
    mean(common$candidate_countlines_observed / candidate_ids)
  }
  treated_countline_coverage <- if (
    common_dates == 0L || treated_ids == 0L
  ) {
    0
  } else {
    mean(common$treated_countlines_observed / treated_ids)
  }

  treated_month_days <- treated_daily[, .(treated_days = .N), by = month_id]
  common_month_days <- common[, .(common_days = .N), by = month_id]
  monthly_coverage <- merge(
    treated_month_days,
    common_month_days,
    by = "month_id",
    all.x = TRUE
  )
  monthly_coverage[is.na(common_days), common_days := 0L]
  monthly_coverage[, coverage := common_days / treated_days]
  minimum_month_coverage <- if (nrow(monthly_coverage) == 0L) {
    0
  } else {
    min(monthly_coverage$coverage)
  }

  if (nrow(common) < 2L) {
    return(data.table(
      common_pre_days = common_dates,
      pre_date_coverage = coverage_ratio,
      minimum_month_coverage,
      treated_countline_coverage,
      candidate_countline_coverage,
      treated_mean = NA_real_,
      candidate_mean = NA_real_,
      level_ratio = NA_real_,
      level_log_distance = Inf,
      treated_window_log_change = NA_real_,
      candidate_window_log_change = NA_real_,
      trend_log_change_distance = Inf,
      monthly_percentage_log_rmse = Inf,
      weekday_percentage_log_rmse = Inf,
      weekly_percentage_log_correlation = NA_real_
    ))
  }

  treated_mean <- mean(common$treated_value)
  candidate_mean <- mean(common$candidate_value)
  common[, `:=`(
    treated_percentage_index =
      100 * (treated_value + 1) / (treated_mean + 1),
    candidate_percentage_index =
      100 * (candidate_value + 1) / (candidate_mean + 1),
    week_start = date - weekday_number + 1L
  )]
  weekly <- common[
    ,
    .(
      treated_percentage_index = mean(treated_percentage_index),
      candidate_percentage_index = mean(candidate_percentage_index)
    ),
    by = week_start
  ][order(week_start)]
  weekly[, week_index := seq_len(.N) - 1L]
  treated_model <- lm(
    log(pmax(treated_percentage_index, 1e-6) / 100) ~ week_index,
    data = weekly
  )
  candidate_model <- lm(
    log(pmax(candidate_percentage_index, 1e-6) / 100) ~ week_index,
    data = weekly
  )
  span_weeks <- max(weekly$week_index) - min(weekly$week_index)
  treated_window_log_change <- coef(treated_model)[["week_index"]] * span_weeks
  candidate_window_log_change <- coef(candidate_model)[["week_index"]] * span_weeks

  data.table(
    common_pre_days = common_dates,
    pre_date_coverage = coverage_ratio,
    minimum_month_coverage,
    treated_countline_coverage,
    candidate_countline_coverage,
    treated_mean,
    candidate_mean,
    level_ratio = (candidate_mean + 1) / (treated_mean + 1),
    level_log_distance = abs(log(
      (candidate_mean + 1) / (treated_mean + 1)
    )),
    treated_window_log_change,
    candidate_window_log_change,
    trend_log_change_distance = abs(
      candidate_window_log_change - treated_window_log_change
    ),
    monthly_percentage_log_rmse = percentage_profile_log_rmse(
      common,
      "month_id",
      c("treated_percentage_index", "candidate_percentage_index")
    ),
    weekday_percentage_log_rmse = percentage_profile_log_rmse(
      common,
      "weekday_number",
      c("treated_percentage_index", "candidate_percentage_index")
    ),
    weekly_percentage_log_correlation = suppressWarnings(cor(
      log(pmax(weekly$treated_percentage_index, 1e-6) / 100),
      log(pmax(weekly$candidate_percentage_index, 1e-6) / 100)
    ))
  )
}

score_one_scheme <- function(spec) {
  candidates <- copy(candidate_inventory)
  candidates[, `:=`(
    pair_id = spec$pair_id,
    scheme_id = spec$scheme_id,
    treated_lsoa21nm = spec$treated_lsoa21nm,
    installation_date = spec$installation_date,
    pre_start = spec$pre_start,
    pre_end = spec$pre_end,
    treated_imd_score = spec$treated_imd_score,
    treated_imd_decile = spec$treated_imd_decile,
    treated_routes = spec$treated_routes
  )]
  candidates[, `:=`(
    absolute_imd_score_difference =
      abs(candidate_imd_score - treated_imd_score),
    absolute_imd_decile_difference =
      abs(candidate_imd_decile - treated_imd_decile)
  )]
  candidates[, context_gate :=
    absolute_imd_score_difference <= 10 &
    absolute_imd_decile_difference <= 1]

  rbindlist(lapply(c("pedestrian", "cyclist"), function(outcome) {
    metrics <- rbindlist(lapply(seq_len(nrow(candidates)), function(i) {
      trajectory_metrics(spec, candidates[i], outcome)
    }))
    result <- cbind(copy(candidates), metrics)
    result[, outcome := outcome]
    result[, hard_gate :=
      context_gate &
      common_pre_days >= 60L &
      pre_date_coverage >= 0.80 &
      minimum_month_coverage >= 0.60 &
      candidate_countline_coverage >= 0.75 &
      level_ratio >= 0.50 &
      level_ratio <= 2.00 &
      trend_log_change_distance <= 0.75 &
      monthly_percentage_log_rmse <= 0.75 &
      weekday_percentage_log_rmse <= 0.75 &
      !is.na(weekly_percentage_log_correlation) &
      weekly_percentage_log_correlation >= 0]
    result[, weekly_correlation_penalty := fifelse(
      is.na(weekly_percentage_log_correlation),
      1,
      pmax(0, pmin(2, 1 - weekly_percentage_log_correlation))
    )]
    result[, availability_penalty :=
      (1 - pre_date_coverage) +
      (1 - minimum_month_coverage) +
      abs(candidate_countline_coverage - treated_countline_coverage)]
    result[, trajectory_score :=
      0.30 * level_log_distance +
      0.25 * trend_log_change_distance +
      0.20 * monthly_percentage_log_rmse +
      0.10 * weekday_percentage_log_rmse +
      0.10 * weekly_correlation_penalty +
      0.05 * availability_penalty]
    result[, context_score :=
      absolute_imd_score_difference / 10 +
      absolute_imd_decile_difference / 2]
    result[, total_match_score :=
      trajectory_score + 0.10 * context_score]
    setorder(
      result,
      -hard_gate,
      total_match_score,
      absolute_imd_score_difference,
      candidate_id
    )
    result[, rank_within_outcome := seq_len(.N)]
    result
  }), use.names = TRUE, fill = TRUE)
}

candidate_scores <- rbindlist(lapply(
  seq_len(nrow(treated_specs)),
  function(i) score_one_scheme(treated_specs[i])
), use.names = TRUE, fill = TRUE)

select_distinct_controls <- function(scores, outcome_name) {
  eligible <- scores[outcome == outcome_name & hard_gate == TRUE]
  eligibility_counts <- eligible[
    ,
    .(strict_candidates = .N),
    by = scheme_id
  ]
  message(
    outcome_name,
    " strict candidates: ",
    paste(
      treated_specs$scheme_id,
      eligibility_counts$strict_candidates[
        match(treated_specs$scheme_id, eligibility_counts$scheme_id)
      ],
      sep = "=",
      collapse = ", "
    )
  )
  assert_true(
    all(treated_specs$scheme_id %chin% eligible$scheme_id),
    paste("No strict trajectory match is available for", outcome_name)
  )
  candidate_lists <- lapply(
    treated_specs$scheme_id,
    function(id) eligible[scheme_id == id][order(total_match_score)]
  )
  names(candidate_lists) <- treated_specs$scheme_id
  combinations <- CJ(
    i12d = seq_len(nrow(candidate_lists[["12d"]])),
    i12f = seq_len(nrow(candidate_lists[["12f"]])),
    i13 = seq_len(nrow(candidate_lists[["13"]]))
  )
  combinations[, `:=`(
    c12d = candidate_lists[["12d"]]$candidate_id[i12d],
    c12f = candidate_lists[["12f"]]$candidate_id[i12f],
    c13 = candidate_lists[["13"]]$candidate_id[i13]
  )]
  combinations <- combinations[
    c12d != c12f & c12d != c13 & c12f != c13
  ]
  assert_true(
    nrow(combinations) > 0L,
    paste("No distinct-control assignment is available for", outcome_name)
  )
  combinations[, assignment_score :=
    candidate_lists[["12d"]]$total_match_score[i12d] +
    candidate_lists[["12f"]]$total_match_score[i12f] +
    candidate_lists[["13"]]$total_match_score[i13]]
  best <- combinations[order(assignment_score)][1]
  selected <- rbindlist(list(
    candidate_lists[["12d"]][best$i12d],
    candidate_lists[["12f"]][best$i12f],
    candidate_lists[["13"]][best$i13]
  ))
  selected[, selected_control := TRUE]
  selected
}

selected_controls <- rbindlist(lapply(
  c("pedestrian", "cyclist"),
  function(outcome) select_distinct_controls(candidate_scores, outcome)
))
assert_true(
  nrow(selected_controls) == 6L,
  "Expected three selected controls for each of two outcomes."
)
assert_true(
  all(selected_controls$hard_gate),
  "A selected trajectory control failed a hard matching requirement."
)
assert_true(
  all(selected_controls[
    ,
    uniqueN(candidate_id) == .N,
    by = outcome
  ]$V1),
  "A control street is reused within an outcome."
)

candidate_scores <- merge(
  candidate_scores,
  selected_controls[, .(
    scheme_id,
    outcome,
    candidate_id,
    selected_control
  )],
  by = c("scheme_id", "outcome", "candidate_id"),
  all.x = TRUE
)
candidate_scores[is.na(selected_control), selected_control := FALSE]

selected_results <- rbindlist(lapply(seq_len(nrow(selected_controls)), function(i) {
  selected <- selected_controls[i]
  selected_scheme <- selected[["scheme_id"]]
  selected_outcome <- selected[["outcome"]]
  selected_lsoa <- selected[["candidate_lsoa21nm"]]
  selected_street <- selected[["candidate_street"]]
  selected_candidate_id <- selected[["candidate_id"]]
  spec <- treated_specs[treated_specs$scheme_id == selected_scheme][1]
  treated_source <- daily[
    sensor_group == "Five scheme sensors" &
      analysis_scheme_id == spec$scheme_id &
      lsoa21nm == spec$treated_lsoa21nm &
      calendar_year %in% c(
        spec$comparison_pre_year,
        spec$comparison_post_year
      ) &
      month_day >= spec$comparison_window_start &
      month_day <= spec$comparison_window_end
  ]
  control_source <- daily[
    sensor_group == "Other LCR sensors" &
      lsoa21nm == selected_lsoa &
      candidate_street == selected_street &
      calendar_year %in% c(
        spec$comparison_pre_year,
        spec$comparison_post_year
      ) &
      month_day >= spec$comparison_window_start &
      month_day <= spec$comparison_window_end
  ]
  treated_context <- treated_source[
    ,
    .(scheme_value = mean(get(selected_outcome))),
    by = .(calendar_year, month_day)
  ]
  control_context <- control_source[
    ,
    .(control_value = mean(get(selected_outcome))),
    by = .(calendar_year, month_day)
  ]
  comparison <- merge(
    treated_context,
    control_context,
    by = c("calendar_year", "month_day")
  )
  comparison_wide <- dcast(
    comparison,
    month_day ~ calendar_year,
    value.var = c("scheme_value", "control_value")
  )
  pre <- as.character(spec$comparison_pre_year)
  post <- as.character(spec$comparison_post_year)
  required <- c(
    paste0("scheme_value_", pre),
    paste0("scheme_value_", post),
    paste0("control_value_", pre),
    paste0("control_value_", post)
  )
  comparison_wide <- comparison_wide[complete.cases(
    comparison_wide[, ..required]
  )]
  assert_true(
    nrow(comparison_wide) >= 20L,
    paste("Insufficient comparison dates for", selected_scheme, selected_outcome)
  )
  relative_log_change <- (
    log(
      100 *
        (comparison_wide[[paste0("scheme_value_", post)]] + 1) /
        (comparison_wide[[paste0("scheme_value_", pre)]] + 1) /
        100
    )
  ) - (
    log(
      100 *
        (comparison_wide[[paste0("control_value_", post)]] + 1) /
        (comparison_wide[[paste0("control_value_", pre)]] + 1) /
        100
    )
  )
  scheme_percentage_index <- 100 * exp(
    mean(
      log(
        100 *
          (comparison_wide[[paste0("scheme_value_", post)]] + 1) /
          (comparison_wide[[paste0("scheme_value_", pre)]] + 1) /
          100
      )
    )
  )
  control_percentage_index <- 100 * exp(
    mean(
      log(
        100 *
          (comparison_wide[[paste0("control_value_", post)]] + 1) /
          (comparison_wide[[paste0("control_value_", pre)]] + 1) /
          100
      )
    )
  )
  pct <- function(value) 100 * (exp(value) - 1)
  data.table(
    scheme_id = selected_scheme,
    outcome = selected_outcome,
    selected_control = selected_candidate_id,
    pre_year = spec$comparison_pre_year,
    post_year = spec$comparison_post_year,
    matched_month_days = nrow(comparison_wide),
    scheme_pre_index = 100,
    scheme_post_index = scheme_percentage_index,
    control_pre_index = 100,
    control_post_index = control_percentage_index,
    scheme_change_percent = scheme_percentage_index - 100,
    control_change_percent = control_percentage_index - 100,
    scheme_relative_to_control_percent = pct(mean(relative_log_change))
  )
}))
combined_results <- selected_results[
  scheme_id %chin% c("12d", "12f"),
  .(
    scheme_id = "12d + 12f equal-weight",
    selected_control =
      "Outcome-specific controls listed in selected-controls CSV",
    pre_year = 2022L,
    post_year = 2024L,
    matched_month_days = NA_integer_,
    scheme_pre_index = 100,
    scheme_post_index = 100 * exp(mean(log(scheme_post_index / 100))),
    control_pre_index = 100,
    control_post_index = 100 * exp(mean(log(control_post_index / 100))),
    scheme_change_percent =
      100 * exp(mean(log(scheme_post_index / 100))) - 100,
    control_change_percent =
      100 * exp(mean(log(control_post_index / 100))) - 100,
    scheme_relative_to_control_percent = 100 * (
      exp(mean(log(
        (scheme_post_index / 100) / (control_post_index / 100)
      ))) - 1
    )
  ),
  by = outcome
]
selected_results <- rbindlist(
  list(selected_results, combined_results),
  use.names = TRUE,
  fill = TRUE
)
assert_true(
  all(c("scheme_id", "outcome") %chin% names(selected_results)),
  paste(
    "Trajectory result columns are incomplete:",
    paste(names(selected_results), collapse = ", ")
  )
)

unavailable <- data.table(
  scheme_id = c("12b", "12e"),
  status = "Trajectory matching unavailable",
  reason = paste(
    "No quality-passing treated observations exist before the confirmed",
    "installation date, so pre-intervention level, trend, and seasonal",
    "similarity cannot be estimated."
  )
)

candidate_output_columns <- c(
  "scheme_id",
  "pair_id",
  "outcome",
  "candidate_id",
  "candidate_lsoa21nm",
  "candidate_street",
  "candidate_hardware_names",
  "candidate_countline_ids",
  "candidate_countlines",
  "candidate_local_authority",
  "treated_imd_score",
  "candidate_imd_score",
  "absolute_imd_score_difference",
  "treated_imd_decile",
  "candidate_imd_decile",
  "absolute_imd_decile_difference",
  "treated_routes",
  "candidate_routes",
  "context_gate",
  "common_pre_days",
  "pre_date_coverage",
  "minimum_month_coverage",
  "treated_countline_coverage",
  "candidate_countline_coverage",
  "treated_mean",
  "candidate_mean",
  "level_ratio",
  "treated_window_log_change",
  "candidate_window_log_change",
  "trend_log_change_distance",
  "monthly_percentage_log_rmse",
  "weekday_percentage_log_rmse",
  "weekly_percentage_log_correlation",
  "hard_gate",
  "trajectory_score",
  "total_match_score",
  "rank_within_outcome",
  "selected_control"
)
candidate_export <- candidate_scores[
  ,
  ..candidate_output_columns
]
candidate_export[, `:=`(
  trajectory_scale = "Pre-installation percentage index (mean = 100)",
  trajectory_log_definition = "log(percentage index / 100)",
  road_type_used_for_selection = FALSE
)]
setorder(
  candidate_export,
  scheme_id,
  outcome,
  -selected_control,
  rank_within_outcome
)

selected_output_columns <- setdiff(
  candidate_output_columns,
  "selected_control"
)
selected_export <- selected_controls[
  ,
  ..selected_output_columns
]
selected_export[, `:=`(
  trajectory_scale = "Pre-installation percentage index (mean = 100)",
  trajectory_log_definition = "log(percentage index / 100)",
  road_type_used_for_selection = FALSE
)]
setorder(selected_export, scheme_id, outcome)
selected_export[, selected_control := TRUE]

setorder(selected_results, scheme_id, outcome)
outputs <- list(
  vivacity_preintervention_trajectory_candidate_scores.csv = candidate_export,
  vivacity_preintervention_trajectory_selected_controls.csv = selected_export,
  vivacity_preintervention_trajectory_results.csv = selected_results,
  vivacity_preintervention_trajectory_unavailable_schemes.csv = unavailable
)
for (directory in c(report_dir, share_dir)) {
  for (name in names(outputs)) {
    fwrite(outputs[[name]], file.path(directory, name))
  }
}

cat(sprintf(
  paste0(
    "Scored %s scheme-outcome-candidate combinations.\n",
    "Selected %s strict trajectory controls.\n",
    "Wrote trajectory-matching outputs to %s and %s.\n"
  ),
  nrow(candidate_export),
  nrow(selected_export),
  report_dir,
  share_dir
))
