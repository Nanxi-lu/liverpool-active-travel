---
title: "Vivacity Active Travel Dissertation: Integrated Draft"
date: "20 June 2026"
format:
  html:
    embed-resources: true
---

## Working Title

Do walking and cycling trajectories at selected Liverpool City Region active
travel schemes differ from wider and context-matched comparison areas, and
what can the available monitoring data support about equity?

## 1. Introduction

Active travel infrastructure may improve safety, continuity and convenience
for walking and cycling, but its effects are difficult to isolate from
seasonality, weather, post-pandemic travel recovery, land-use change and
pre-existing differences between places. These attribution problems are
especially important in equity-focused evaluation because neighbourhoods
differ in car ownership, transport need, route quality and exposure to wider
investment.

This dissertation uses Vivacity automated countline data to examine pedestrian
and cyclist trajectories around five Liverpool City Region active travel
schemes. Sensor records are linked to LSOA 2021 geography, IMD 2019 and Census
context. Scheme-linked countlines are compared with eligible wider-network
countlines and selected comparison streets.

The study asks:

> Do walking and cycling trajectories at scheme-linked sensors differ from
> wider and context-matched comparison areas, and are the data sufficient for
> an equity-focused interpretation?

The design is an exploratory matched-comparison difference-in-differences
study. It is exploratory because the schemes were not randomly assigned,
monitoring was not designed prospectively for evaluation, the treated sample
is small, and comparison streets require external verification.

The objectives are:

1. construct reliable daily pedestrian and cyclist observations;
2. distinguish sensor absence from genuine zero movement;
3. compare scheme trajectories with wider and context-matched baselines;
4. test sensitivity to control selection, weighting and weather; and
5. assess what the evidence can support about neighbourhood equity.

## 2. Literature Review Position

The literature review should justify three propositions. First, active travel
infrastructure can affect behaviour, but effects vary by context and outcome.
Second, built-environment interventions require careful counterfactual design
because before/after changes may reflect wider trends. Third, equity concerns
both the distribution of investment and whether different communities benefit.

The dissertation therefore uses quasi-experimental comparison logic but avoids
claiming that the available monitoring data produce a definitive causal impact
estimate.

## 3. Research Design

### 3.1 Schemes and analytical roles

| Scheme | Corridor | Confirmed date | Current role |
|---|---|---:|---|
| 12b | Cronton Road / Sandy Lane | 2023-03-01 | Descriptive only |
| 12d | Aigburth Road / Otterspool Promenade | 2023-03-01 | 2022–2024 comparison |
| 12e | Park Road / Path off Park Road | 2023-03-01 | Descriptive only |
| 12f | Leasowe / Wallasey corridor | 2023-06-01 | 2022–2024 comparison |
| 13 | Astmoor / Warrington Road | 2024-03-01 | Separate 2023–2024 comparison |

Schemes 12b and 12e have no quality-passing treated observations before their
confirmed dates. They can support post-installation description and
composition analysis but not a before/after comparison.

### 3.2 Data sources

The primary source is 90 official Vivacity dashboard exports covering 711
Liverpool City Region countlines from 1 January 2020 to 31 May 2025. A
validated scheme-only archive extends selected fixed-year panels to 26 May
2026.

Contextual sources are:

- LSOA 2021 boundaries;
- IMD 2019 deprivation scores and deciles;
- Census 2021 population, car availability and commuting context;
- ERA5 daily weather accessed through Open-Meteo; and
- ORR station usage, used separately as regional movement context.

### 3.3 Unit of analysis

The active comparison models use daily countline observations. Outcomes are:

```text
pedestrian
cyclist
active_travel_total = pedestrian + cyclist
```

Counts are analysed as `log(1 + count)`. Countline fixed effects absorb stable
location characteristics such as a site's underlying busyness. Month-day and
weekday controls align the compared calendar structure.

## 4. Data Preparation

### 4.1 Daily aggregation and quality rule

Hourly directional records are aggregated to one row per countline and date.
A countline-day is retained when:

- at least 23 hourly periods are present;
- availability and pedestrian/cyclist fields are observed in at least 23
  periods;
- minimum reported daily availability is at least 80%;
- no Vivacity data-error flag is present; and
- for scheme sensors, the date is on or after the confirmed first reliable
  date.

Unavailable days are excluded rather than entered as zero.

### 4.2 First reliable date

Long early periods containing zero counts and missing availability are treated
as structural sensor absence. The first reliable date is based on a sustained
run of quality-passing positive observations. This prevents installation or
commissioning gaps from becoming artificial treatment effects.

### 4.3 Context and comparison construction

Countlines are spatially joined to LSOA 2021 and assigned IMD and Census
context. The wider LCR benchmark retains other countlines with quality-passing
observations in both 2022 and 2024.

For the context-matched analysis, each treated LSOA context is paired with a
route-compatible comparison street selected using continuous IMD similarity.
Similar IMD does not guarantee a valid counterfactual, so absolute-volume
diagnostics, trajectory matching and cautious interpretation are retained.

## 5. Model Specification

### 5.1 Wider LCR difference-in-differences

For schemes 12d and 12f, the common comparison uses paired calendar days from
15 November to 31 December in 2022 and 2024:

```text
log(1 + outcome) =
  β0
  + β1 post
  + β2 treated × post
  + countline fixed effects
  + month-day controls
  + weekday controls
  + error
```

The `post` coefficient estimates the general change among eligible LCR
countlines. The `treated × post` coefficient estimates the additional change
at scheme countlines relative to that trend. Standard errors are clustered by
countline.

The cohort contains seven treated countlines—four from 12d and three from
12f—and 126 comparison countlines. The small treated sample limits statistical
power and makes conventional inference sensitive to the clustering level.

### 5.2 Per-scheme interaction model

The pooled treatment indicator is replaced with separate interactions:

```text
log(1 + outcome) =
  post
  + post × scheme_12d
  + post × scheme_12f
  + countline fixed effects
  + month-day and weekday controls
  + weather
```

This reveals whether the pooled result is shared across schemes or driven by
one location.

### 5.3 Preferred IMD-matched context estimator

Within each scheme/control pair, countlines are first averaged to one daily
context value. Pre/post log changes are calculated for the scheme and control
contexts and then differenced. Schemes 12d and 12f receive equal weight.

Seven-day circular moving-block resampling describes date-to-date uncertainty
conditional on the selected pairs. It does not capture control-selection
uncertainty and is not a population-level causal confidence interval.

### 5.4 Sensitivity analyses

Two further checks are retained:

- pedestrian- and cyclist-specific controls selected using pre-installation
  percentage trajectories; and
- weather-adjusted models using temperature, precipitation, wind and solar
  radiation.

## 6. Results

### 6.1 Descriptive fixed-year changes

The fixed-year panels show observed change at scheme sensors:

| Scheme | Comparison | Active-travel change |
|---|---|---:|
| 12d | 2022 to 2025 | approximately +20% |
| 12f | 2022 to 2025 | approximately +51% |
| 13 | 2023 to 2025 | approximately -18% |

These figures do not control for wider change. Positive absolute growth can
coexist with a negative difference-in-differences estimate if comparison areas
grow faster.

### 6.2 Wider LCR benchmark

Across schemes 12d and 12f, eligible LCR countlines changed by approximately
+2.5% in active travel over the matched 2022–2024 window, while scheme
countlines changed by approximately -31.3%. The relative estimate is about
-33.0%. Its clustered interval includes zero, so the active-travel direction is
lower but statistically inconclusive.

Cycling shows the most consistent negative relative result: eligible LCR
countlines increased by about +12.7%, while scheme countlines changed by about
-22.2%, a relative difference near -31.0%.

### 6.3 Per-scheme heterogeneity

The pooled citywide estimate masks different scheme trajectories:

- scheme 12d active travel is about -14.5% relative to the city benchmark and
  statistically uncertain;
- scheme 12f active travel is about -50.2% relative to the city benchmark and
  statistically uncertain; and
- scheme 12f cycling is about -41.7% relative to the city benchmark in the
  current countline-clustered model.

These per-scheme results should be treated cautiously because the two schemes
contain only four and three treated countlines respectively.

### 6.4 IMD-matched contexts

The preferred equal-context estimate for schemes 12d and 12f is:

| Outcome | Scheme relative to matched streets |
|---|---:|
| Active travel | -9.9% |
| Cyclist | -32.9% |
| Pedestrian | -3.1% |

The active-travel estimate is materially smaller than the citywide estimate.
This difference demonstrates that effect magnitude depends on the comparison
design and weighting structure. The stable conclusion is directional: the
analysable scheme contexts did not outperform their selected matches.

### 6.5 Weather and trajectory sensitivity

Weather adjustment changes the headline relative estimates by less than one
percentage point. Broad daily weather does not explain their direction.

Outcome-specific trajectory matching also produces a stronger negative cycling
comparison than pedestrian comparison. These results remain conditional on the
selected control streets.

## 7. Discussion

The dissertation's strongest contribution is a transparent monitoring and
comparison workflow rather than a definitive verdict on scheme effectiveness.
It shows why active travel evaluation depends on reliable pre-intervention
coverage, explicit treatment dates, suitable controls, disaggregated outcomes
and a clear distinction between absolute and comparison-adjusted change.

The wider LCR, IMD-matched and trajectory-matched estimates should be presented
as complementary sensitivity analyses. The variation between approximately
-33% and -10% for active travel is substantive evidence of model dependence,
not a nuisance to conceal.

Equity interpretation remains descriptive. Scheme and comparison countlines
can be located within deprivation contexts, but the sample is too small and
unbalanced for a credible deprivation × intervention interaction.

## 8. Limitations and Next Work

- Schemes 12b and 12e lack usable pre-installation treated baselines.
- Only seven treated countlines enter the common 2022–2024 model.
- Multiple countlines may belong to the same physical sensor or corridor, so
  countline clustering may overstate the number of independent treated units.
- Comparison streets require external checks for concurrent interventions.
- Parallel trends cannot be established robustly from the common short
  baseline.
- Leave-one-site-out and site-level small-sample inference should be added.
- Countline sensors measure flows at fixed points and cannot identify trip
  purpose, individual users or wider route substitution.

## 9. Conclusion

The Vivacity data provide detailed local evidence, but the current monitoring
structure does not support a strong causal or deprivation-stratified impact
claim. Descriptive changes are mixed, and scheme countlines generally do not
outperform comparison baselines over the available windows, particularly for
cycling. The magnitude is sensitive to benchmark choice.

The dissertation should therefore present a careful exploratory
difference-in-differences study and emphasise the practical monitoring
conditions required for stronger future evaluation.
