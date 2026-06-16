library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(scales)

base_dir <- "/Users/lu_nanxi/CASA/Dissertation_Data"
archive_path <- file.path(base_dir, "data_archives", "04_modelling_analysis_outputs.zip")
daily_member <- "Vivacity_full_day_cutoff_20260526/modelling_ready/tables/vivacity_descriptive_daily.csv"
output_dir <- file.path(base_dir, "reports", "analysis_reports")
output_visualised_dir <- file.path(
  base_dir,
  "output_visualised",
  "reports",
  "analysis_reports"
)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(output_visualised_dir, recursive = TRUE, showWarnings = FALSE)

daily_connection <- unz(archive_path, daily_member, open = "rb")
daily <- read_csv(
  daily_connection,
  col_select = c(
    dataset_role, analysis_scheme_id, scheme_name, treated_road_group,
    date, Pedestrian, Cyclist, active_travel_total, intervention_date,
    descriptive_row
  ),
  show_col_types = FALSE
) |>
  mutate(
    date = as_date(date),
    intervention_date = as_date(intervention_date),
    calendar_year = year(date),
    week_start = floor_date(date, unit = "week", week_start = 1)
  )
close(daily_connection)

target_outcomes <- c("Pedestrian", "Cyclist", "active_travel_total")

year_pairs <- tibble(
  analysis_scheme_id = c("12b", "12b", "12d", "12d", "12e", "12e", "12f", "12f", "13", "13"),
  comparison_label = c(
    "2021 vs 2024", "2022 vs 2025",
    "2021 vs 2024", "2022 vs 2025",
    "2021 vs 2024", "2022 vs 2025",
    "2021 vs 2024", "2022 vs 2025",
    "2022 vs 2026", "2023 vs 2025"
  ),
  baseline_year = c(2021, 2022, 2021, 2022, 2021, 2022, 2021, 2022, 2022, 2023),
  comparison_year = c(2024, 2025, 2024, 2025, 2024, 2025, 2024, 2025, 2026, 2025),
  comparison_note = c(
    rep("Scheme installed in 2023: fixed calendar-year comparison requested by user.", 8),
    "Scheme installed in 2024: fixed calendar-year comparison requested by user. 2026 is partial to the available data cut-off.",
    "Scheme installed in 2024: fixed calendar-year comparison requested by user."
  )
)

annual <- daily |>
  filter(dataset_role == "treated", descriptive_row) |>
  pivot_longer(
    cols = all_of(target_outcomes),
    names_to = "outcome",
    values_to = "count"
  ) |>
  group_by(
    analysis_scheme_id, scheme_name, treated_road_group,
    dataset_role, intervention_date, calendar_year, outcome
  ) |>
  summarise(
    observed_weeks = n_distinct(week_start),
    observed_countline_days = n(),
    count_total = sum(count, na.rm = TRUE),
    average_per_observed_day = if_else(
      observed_countline_days > 0,
      count_total / observed_countline_days,
      NA_real_
    ),
    .groups = "drop"
  ) |>
  mutate(
    role_label = "Treated",
    outcome_label = recode(
      outcome,
      Pedestrian = "Pedestrian",
      Cyclist = "Cyclist",
      active_travel_total = "Active travel"
    )
  )

scheme_metadata <- daily |>
  filter(dataset_role == "treated") |>
  group_by(analysis_scheme_id) |>
  summarise(
    scheme_name = first(na.omit(scheme_name)),
    treated_road_group = first(na.omit(treated_road_group)),
    dataset_role = first(na.omit(dataset_role)),
    role_label = "Treated",
    intervention_date = first(na.omit(intervention_date)),
    .groups = "drop"
  )

comparison_grid <- year_pairs |>
  tidyr::crossing(
    tibble(
      outcome = target_outcomes,
      outcome_label = c("Pedestrian", "Cyclist", "Active travel")
    )
  ) |>
  left_join(scheme_metadata, by = "analysis_scheme_id")

comparison <- comparison_grid |>
  left_join(
    annual |>
      select(-scheme_name, -treated_road_group, -dataset_role, -role_label, -intervention_date, -outcome_label) |>
      rename(
        baseline_year = calendar_year,
        baseline_observed_weeks = observed_weeks,
        baseline_observed_countline_days = observed_countline_days,
        baseline_count_total = count_total,
        baseline_average_per_observed_day = average_per_observed_day
      ),
    by = c("analysis_scheme_id", "baseline_year", "outcome")
  ) |>
  left_join(
    annual |>
      select(
        analysis_scheme_id, comparison_year = calendar_year, outcome,
        comparison_observed_weeks = observed_weeks,
        comparison_observed_countline_days = observed_countline_days,
        comparison_count_total = count_total,
        comparison_average_per_observed_day = average_per_observed_day
      ),
    by = c("analysis_scheme_id", "comparison_year", "outcome")
  ) |>
  mutate(
    difference_comparison_minus_baseline =
      comparison_average_per_observed_day - baseline_average_per_observed_day,
    percent_change = if_else(
      !is.na(baseline_average_per_observed_day) & baseline_average_per_observed_day > 0,
      (comparison_average_per_observed_day / baseline_average_per_observed_day - 1) * 100,
      NA_real_
    ),
    interpretation = case_when(
      is.na(baseline_average_per_observed_day) & is.na(comparison_average_per_observed_day) ~
        "No usable treated observations in either requested year under the current quality-filtered daily output.",
      is.na(baseline_average_per_observed_day) ~
        "No usable treated baseline-year observations under the current quality-filtered daily output.",
      is.na(comparison_average_per_observed_day) ~
        "No usable treated comparison-year observations under the current quality-filtered daily output.",
      difference_comparison_minus_baseline > 0 ~
        paste0(
          "Comparison-year average is higher by ",
          round(difference_comparison_minus_baseline, 1),
          " per observed countline-day (",
          round(percent_change, 1),
          "%)."
        ),
      difference_comparison_minus_baseline < 0 ~
        paste0(
          "Comparison-year average is lower by ",
          abs(round(difference_comparison_minus_baseline, 1)),
          " per observed countline-day (",
          round(percent_change, 1),
          "%)."
        ),
      TRUE ~ "No difference between the two requested years."
    )
  ) |>
  arrange(analysis_scheme_id, comparison_label, factor(outcome, levels = target_outcomes))

decomposition <- comparison |>
  select(
    analysis_scheme_id, scheme_name, treated_road_group,
    comparison_label, baseline_year, comparison_year,
    outcome, baseline_average_per_observed_day, comparison_average_per_observed_day
  ) |>
  pivot_wider(
    names_from = outcome,
    values_from = c(baseline_average_per_observed_day, comparison_average_per_observed_day)
  ) |>
  mutate(
    baseline_pedestrian_plus_cyclist =
      baseline_average_per_observed_day_Pedestrian + baseline_average_per_observed_day_Cyclist,
    comparison_pedestrian_plus_cyclist =
      comparison_average_per_observed_day_Pedestrian + comparison_average_per_observed_day_Cyclist,
    baseline_active_minus_components =
      baseline_average_per_observed_day_active_travel_total - baseline_pedestrian_plus_cyclist,
    comparison_active_minus_components =
      comparison_average_per_observed_day_active_travel_total - comparison_pedestrian_plus_cyclist,
    note = "Fixed-year averages are calculated from annual totals divided by observed countline-days; active travel should match pedestrian + cyclist apart from rounding or missing-year cases."
  ) |>
  arrange(analysis_scheme_id, comparison_label)

write_csv(comparison, file.path(output_dir, "vivacity_fixed_year_average_comparison.csv"))
write_csv(decomposition, file.path(output_dir, "vivacity_fixed_year_active_travel_decomposition.csv"))

# Replace the old direct-comparison CSV with the new fixed-year comparison so
# existing report links open the current requested comparison.
write_csv(comparison, file.path(output_dir, "vivacity_direct_before_after_peak_comparison.csv"))

plot_data <- comparison |>
  filter(!is.na(baseline_average_per_observed_day) | !is.na(comparison_average_per_observed_day)) |>
  mutate(
    outcome_label = recode(
      outcome,
      Pedestrian = "Pedestrian",
      Cyclist = "Cyclist",
      active_travel_total = "Active travel"
    )
  ) |>
  pivot_longer(
    cols = c(baseline_average_per_observed_day, comparison_average_per_observed_day),
    names_to = "period_type",
    values_to = "average_per_observed_day"
  ) |>
  mutate(
    period_label = case_when(
      period_type == "baseline_average_per_observed_day" ~ paste0("Baseline: ", baseline_year),
      TRUE ~ paste0("Comparison: ", comparison_year)
    ),
    scheme_panel = paste0(analysis_scheme_id, " - ", treated_road_group),
    period_label = factor(period_label, levels = unique(period_label))
  )

p <- ggplot(
  plot_data,
  aes(x = outcome_label, y = average_per_observed_day, fill = period_label)
) +
  geom_col(position = position_dodge(width = 0.75), width = 0.68, na.rm = TRUE) +
  facet_grid(scheme_panel ~ comparison_label, scales = "free_y") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = c("#3F7CAC", "#D98C3A", "#6BA292", "#A65D78", "#7A6BB7", "#C6A15B")) +
  labs(
    title = "Fixed-year active-travel average comparison",
    subtitle = "Averages are count totals divided by observed countline-days. These are fixed calendar-year comparisons, not peak weeks.",
    x = NULL,
    y = "Average count per observed countline-day",
    fill = "Year"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    strip.text.y = element_text(angle = 0, hjust = 0),
    panel.grid.minor = element_blank()
  )

ggsave(
  file.path(output_dir, "vivacity_fixed_year_average_comparison.svg"),
  p,
  width = 13,
  height = 10,
  dpi = 180
)

# Keep the previous graph filename current for existing links.
ggsave(
  file.path(output_dir, "vivacity_grouped_bar_before_after_peak_all_schemes.svg"),
  p,
  width = 13,
  height = 10,
  dpi = 180
)

fmt <- function(x, digits = 3) {
  ifelse(is.na(x), "", format(round(x, digits), nsmall = digits, trim = TRUE))
}

comparison_display <- comparison |>
  mutate(
    baseline_average_per_observed_day = fmt(baseline_average_per_observed_day),
    comparison_average_per_observed_day = fmt(comparison_average_per_observed_day),
    difference_comparison_minus_baseline = fmt(difference_comparison_minus_baseline),
    percent_change = fmt(percent_change, 2)
  )

row_html <- apply(comparison_display, 1, function(row) {
  paste0(
    "<tr>",
    "<td>", row["analysis_scheme_id"], "</td>",
    "<td>", row["treated_road_group"], "</td>",
    "<td>", row["comparison_label"], "</td>",
    "<td>", row["outcome_label"], "</td>",
    "<td>", row["baseline_year"], "</td>",
    "<td>", row["baseline_observed_countline_days"], "</td>",
    "<td>", row["baseline_average_per_observed_day"], "</td>",
    "<td>", row["comparison_year"], "</td>",
    "<td>", row["comparison_observed_countline_days"], "</td>",
    "<td>", row["comparison_average_per_observed_day"], "</td>",
    "<td>", row["difference_comparison_minus_baseline"], "</td>",
    "<td>", row["percent_change"], "</td>",
    "<td>", row["interpretation"], "</td>",
    "</tr>"
  )
}) |> paste(collapse = "\n")

html <- paste0(
  "<!doctype html><html><head><meta charset=\"utf-8\"><title>Vivacity fixed-year average comparison</title>",
  "<style>body{font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',Arial,sans-serif;margin:28px;line-height:1.45;color:#1f2933}",
  "table{border-collapse:collapse;width:100%;font-size:13px}th,td{border:1px solid #d7dde5;padding:6px 8px;vertical-align:top}th{background:#eef2f7}",
  ".small{color:#64748b}.note{background:#eef8f6;border-left:4px solid #2f6f73;padding:10px 12px;margin:12px 0}</style></head><body>",
  "<h1>Vivacity fixed-year average comparison</h1>",
  "<p class=\"small\">Generated from quality-filtered daily treated rows in the modelling archive. This replaces the previous independent peak comparison for the direct before/after view.</p>",
  "<div class=\"note\"><strong>Definition:</strong> for schemes installed in 2023, the requested comparisons are 2021 vs 2024 and 2022 vs 2025. For the scheme installed in 2024, the requested comparisons are 2022 vs 2026 and 2023 vs 2025. Values are annual count totals divided by annual observed countline-days. The 2026 comparison is partial to the available data cut-off.</div>",
  "<p><strong>Important:</strong> this is no longer a peak-week comparison. It is a fixed calendar-year average comparison, so pedestrian + cyclist is directly comparable with active travel within each year.</p>",
  "<p><a href=\"vivacity_fixed_year_average_comparison.csv\">CSV: fixed-year comparison</a> | <a href=\"vivacity_fixed_year_active_travel_decomposition.csv\">CSV: active-travel decomposition check</a> | <a href=\"vivacity_before_after_peak_bar_graphs.html\">Open bar graph page</a></p>",
  "<h2>Comparison table</h2>",
  "<table><thead><tr>",
  "<th>scheme</th><th>location</th><th>comparison</th><th>outcome</th>",
  "<th>baseline year</th><th>baseline observed countline-days</th><th>baseline avg</th>",
  "<th>comparison year</th><th>comparison observed countline-days</th><th>comparison avg</th>",
  "<th>difference</th><th>percent change</th><th>interpretation</th>",
  "</tr></thead><tbody>", row_html, "</tbody></table>",
  "</body></html>"
)

writeLines(html, file.path(output_dir, "vivacity_fixed_year_average_comparison.html"))
writeLines(html, file.path(output_dir, "vivacity_direct_before_after_peak_comparison.html"))

bar_html <- paste0(
  "<!doctype html><html><head><meta charset=\"utf-8\"><title>Vivacity fixed-year average bar graphs</title>",
  "<style>body{font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',Arial,sans-serif;margin:28px;color:#1f2933;line-height:1.45}.small{color:#64748b}img{max-width:100%;height:auto;border:1px solid #d7dde5}</style></head><body>",
  "<h1>Vivacity fixed-year average bar graphs</h1>",
  "<p class=\"small\">Grouped bar graph generated from <code>vivacity_fixed_year_average_comparison.csv</code>. This is a fixed calendar-year average comparison, not an independent peak comparison.</p>",
  "<p>For schemes installed in 2023: 2021 vs 2024 and 2022 vs 2025. For the scheme installed in 2024: 2022 vs 2026 and 2023 vs 2025. 2026 is partial to the available data cut-off.</p>",
  "<img src=\"vivacity_fixed_year_average_comparison.svg\" alt=\"Fixed-year average comparison bar graphs\">",
  "<p>Related table: <a href=\"vivacity_fixed_year_average_comparison.html\">fixed-year comparison table</a>.</p>",
  "</body></html>"
)

writeLines(bar_html, file.path(output_dir, "vivacity_fixed_year_average_bar_graphs.html"))

fixed_year_outputs <- c(
  "vivacity_fixed_year_average_comparison.csv",
  "vivacity_fixed_year_active_travel_decomposition.csv",
  "vivacity_direct_before_after_peak_comparison.csv",
  "vivacity_fixed_year_average_comparison.svg",
  "vivacity_grouped_bar_before_after_peak_all_schemes.svg",
  "vivacity_fixed_year_average_comparison.html",
  "vivacity_direct_before_after_peak_comparison.html",
  "vivacity_fixed_year_average_bar_graphs.html"
)
file.copy(
  file.path(output_dir, fixed_year_outputs),
  file.path(output_visualised_dir, fixed_year_outputs),
  overwrite = TRUE
)

message("Wrote fixed-year comparison outputs to: ", output_dir)
