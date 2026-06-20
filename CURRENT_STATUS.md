# Current Project Status

**Aligned on:** 20 June 2026

This file is the authoritative progress register for the Vivacity dissertation
workflow. If another document conflicts with it, this status and the current
integrated analysis report take precedence.

## Current Analytical Position

The project has moved beyond simple before/after description and an older
weekly interrupted-time-series prototype. The active framework is:

1. quality-gated fixed-year description;
2. log-linear difference-in-differences against eligible LCR countlines;
3. per-scheme post × scheme interactions for 12d and 12f;
4. an equal-context IMD-matched before/after comparison as the preferred
   headline estimator;
5. outcome-specific pre-intervention trajectory matching as a sensitivity
   analysis; and
6. weather-adjusted sensitivity analysis.

The analysis remains exploratory because intervention placement was not random,
the treated sample is small, some control matches are weak, and parallel trends
cannot be established robustly from the common baseline.

## Current Cohorts

- 21 scheme-linked countlines across five schemes.
- 7 treated countlines enter the common 2022–2024 LCR model:
  4 for scheme 12d and 3 for scheme 12f.
- 126 other LCR countlines enter the balanced regression cohort.
- Schemes 12b and 12e have no quality-passing pre-installation treated
  baseline.
- Scheme 13 uses a separate 2023–2024 window in the matched-context analysis.

## Current Headline Results

### Fixed-year description

- Scheme 12d active travel: approximately +20% from 2022 to 2025.
- Scheme 12f active travel: approximately +51% from 2022 to 2025.
- Scheme 13 active travel: approximately -18% from 2023 to 2025.
- These are descriptive changes, not comparison-adjusted scheme effects.

### Wider LCR difference-in-differences benchmark

For schemes 12d and 12f pooled over paired 15 November–31 December dates:

- active travel: approximately -33% relative to the eligible LCR trend;
- cyclist: approximately -31% relative to the eligible LCR trend; and
- pedestrian: approximately -29% relative to the eligible LCR trend.

The active-travel and pedestrian intervals include zero. The pooled cycling
result is more consistently negative, but the citywide benchmark is unmatched.

### Per-scheme city benchmark

- Scheme 12d active travel: about -15% relative to the city benchmark,
  statistically uncertain.
- Scheme 12f active travel: about -50% relative to the city benchmark,
  statistically uncertain.
- Scheme 12f cycling: about -42% relative to the city benchmark and
  statistically flagged in the current countline-clustered model.

These estimates are sensitive to the very small treated sample and should not
be read as independent population-level scheme effects.

### Preferred IMD-matched context estimate

For schemes 12d and 12f with equal context weight:

- active travel: -9.9% relative to the selected matched streets;
- cyclist: -32.9%; and
- pedestrian: -3.1%.

The active-travel conditional moving-block interval is approximately -16.4% to
-2.4%, but it conditions on the selected controls and does not account for
control-selection uncertainty.

### Weather

Adding ERA5 daily temperature, precipitation, wind, and solar radiation shifts
the main relative estimates by less than one percentage point. Broad daily
weather therefore does not explain the direction of the current findings.

## Current Interpretation

The stable finding is not that the schemes caused a reduction. It is that the
analysable scheme countlines did not outperform their selected comparison
baselines over the available windows, especially for cycling. The magnitude
depends materially on the benchmark and weighting structure.

Equity remains contextual rather than causal. The data can describe the
deprivation settings represented by the schemes and comparison streets, but
the current sample cannot estimate a robust deprivation × intervention effect.

## Active Documents

| Artifact | Role | Status |
|---|---|---|
| `reports/analysis_reports/vivacity_current_analysis.html` | Integrated Vivacity report | Current |
| `reports/chapter_drafts/overall_draft_1.md` | Integrated dissertation draft | Current |
| `reports/chapter_drafts/overall_draft_1.html` | Rendered draft for browsing | Current |
| `reports/analysis_reports/liverpool_council_active_travel_rail_analytical_report.docx` | Council-facing synthesis | Current |
| `reports/rendered_html/rail_station_time_series_trends.html` | Separate rail context | Current |
| `output_visualised/index.html` | Curated publication index | Current |

## Retired Material

The following categories are no longer active and should not be cited:

- the May weekly interrupted-time-series model and causal-diagnostic notebooks;
- separate generated methodology, results, and discussion DOCXs;
- early descriptive-analysis DOCXs;
- the 1 June progress presentation, which still requested an exact-date rerun;
- first-stage and legacy Vivacity HTML notebooks; and
- duplicate fixed-year HTML wrappers superseded by the integrated report.

## Remaining Methodological Work

1. Cluster or resample at the physical site/scheme-context level where
   possible, not only by countline.
2. Add leave-one-site-out sensitivity checks.
3. Verify selected comparison streets for concurrent interventions.
4. Treat any future parallel-trend/event-study diagnostics as current only
   after they are rebuilt from the present quality-gated cohort.
