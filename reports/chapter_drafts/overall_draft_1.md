## Working Title

Do deprived and affluent neighbourhoods exhibit different time-series trajectories in walking and cycling uptake after active travel interventions, compared with matched non-intervention areas?

## 1. Introduction

Active travel has become a central policy concern in transport planning because walking and cycling can contribute to healthier, lower-carbon, and more spatially efficient urban mobility. In practice, however, the effects of active travel schemes are difficult to evaluate. New infrastructure is rarely introduced under experimental conditions, monitoring equipment is often installed after scheme planning has already begun, and observed changes in walking and cycling may reflect weather, seasonality, wider travel recovery, local land-use change, or pre-existing differences between places rather than the scheme itself. These challenges are particularly important for equity-focused evaluation. If active travel interventions are intended to support more inclusive mobility, then it is not enough to ask whether walking and cycling increased overall. It is also necessary to ask whether the observed trajectories differ across neighbourhood contexts, especially between more deprived and more affluent areas.

This dissertation examines walking and cycling trajectories around selected Liverpool City Region active travel schemes using Vivacity automated countline sensor data. Vivacity sensors provide classified counts of pedestrians, cyclists and motorised traffic at specific countlines. This gives the dissertation a high-frequency, street-level evidence base that is more detailed than annual or area-level transport indicators. The analysis links these sensor records to small-area deprivation and Census context using LSOA 2021 geography, and compares treated countlines associated with active travel schemes against matched non-intervention countlines where suitable controls are available.

The research question is:

> Do deprived and affluent neighbourhoods exhibit different time-series trajectories in walking and cycling uptake after active travel interventions, compared with matched non-intervention areas?

The study is designed as an exploratory matched-control interrupted time-series analysis. It is exploratory because the available monitoring data were not produced through a planned experimental evaluation design. The key methodological task is therefore not only to estimate whether treated sites changed after intervention, but also to test whether the available data are strong enough to support causal interpretation. The dissertation prioritises transparent data cleaning, explicit inclusion rules, contextual matching, pre-trend diagnostics, and cautious interpretation.

The dissertation has four main objectives:

1. To clean and prepare Vivacity countline data into reliable daily and weekly analysis tables.
2. To link treated and candidate control countlines to LSOA 2021 deprivation and Census context.
3. To compare walking and cycling trajectories at treated and matched non-intervention countlines.
4. To assess whether the available evidence supports an equity-focused interpretation of active travel uptake across deprived and affluent neighbourhood contexts.

The intended contribution is both empirical and methodological. Empirically, the study describes observed pedestrian and cyclist trajectories around selected Liverpool City Region schemes. Methodologically, it demonstrates the conditions under which opportunistic sensor data can, and cannot, support active travel evaluation. The strongest contribution is therefore not a simple claim that the schemes succeeded or failed, but a structured assessment of how sensor coverage, intervention-date coding, control-site quality and neighbourhood context shape what can be inferred.

## 2. Brief Literature Review Position

The literature review will be developed separately, but its role in the dissertation is clear. It should support three linked arguments. First, active travel infrastructure can influence walking and cycling by changing perceived safety, convenience, route continuity and the attractiveness of non-car travel, but effects are often context-dependent. Second, causal attribution is difficult in built-environment evaluation because interventions are not randomly assigned and because travel behaviour is shaped by seasonality, wider urban change, and existing infrastructure networks. Third, equity matters because deprived and affluent neighbourhoods may differ in car ownership, transport need, safety conditions, access to bicycles, route quality, and exposure to wider investment patterns.

The literature review should therefore justify the use of a quasi-experimental, matched-control time-series approach while also explaining why the analysis remains cautious. It should not promise a strong causal model that the data cannot support. Instead, it should frame the dissertation as an empirical test of relative trajectories and a critical assessment of monitoring capacity for equity-focused active travel evaluation.

## 3. Methodology

### 3.1 Research Design

The dissertation uses an exploratory matched-control interrupted time-series design. Treated countlines are Vivacity countlines associated with selected active travel schemes. Control countlines are non-intervention Vivacity countlines selected through contextual matching and then filtered through a data-quality gate. The design compares whether treated countlines changed differently from controls after the confirmed scheme intervention date.

The workflow uses the confirmed exact scheme dates as intervention dates. These dates are encoded in the current analysis and are used by the integrity gate, modelling-ready tables and current Vivacity outputs.

The design is not presented as a definitive causal impact evaluation. It remains exploratory because the control pool is limited, some sensors may still lack reliable pre-intervention coverage even after exact-date recalculation, and control sites require manual verification to confirm that they were not affected by other transport or public-realm interventions. The analysis therefore estimates relative trajectory differences rather than definitive scheme effects.

### 3.2 Study Area and Scheme Selection

The study focuses on selected Liverpool City Region active travel schemes for which Vivacity countline data are available. Five treated scheme groups are included in the project workflow:

| Scheme | Location / corridor | Current analytical role |
|---|---|---|
| 12b | Cronton Road / Sandy Lane | Descriptive only because no quality-passing pre-installation observations remain |
| 12d | Aigburth Road / Otterspool Promenade | Exploratory treated-control model candidate |
| 12e | Park Road / Path off Park Road | Descriptive only because no quality-passing pre-installation observations remain |
| 12f | Leasowe / Wallasey corridor | Exploratory treated-control model candidate |
| 13 | Astmoor / Warrington Road | Exploratory treated-control model candidate |

Schemes 12d, 12f and 13 provide usable pre/post observations for the current comparative analyses. Schemes 12b and 12e are retained for descriptive interpretation because their reliable treated records begin after their confirmed intervention dates.

### 3.3 Data Sources

The primary dataset is Vivacity automated countline data. These data contain classified movement counts by date/time, countline, direction and mode. The main mode classes used in the dissertation are:

| Mode class | Analytical use |
|---|---|
| Pedestrian | Main walking outcome |
| Cyclist | Main cycling outcome |
| Car | Motorised context / traffic outcome |
| Motorbike | Motorised total |
| Bus | Motorised total |
| OGV1 | Motorised total |
| OGV2 | Motorised total |
| LGV | Motorised total |

Contextual data are linked at LSOA 2021 level. These include IMD 2019 deciles, income and employment deprivation deciles, Census 2021 population density, household car availability, walking and cycling commute shares, car-driver commute share, work-from-home share and household deprivation indicators. LSOA 2021 is used as the common spatial unit because Vivacity countline locations can be spatially joined to LSOA polygons, and Census 2021 variables are available at this geography. IMD 2019 is bridged from LSOA 2011 to LSOA 2021 using an exact-fit lookup.

Supplementary context datasets include the Liverpool City Region LTP3 cycling indicator and Office of Rail and Road station usage data. These are not the main causal data sources. The LTP3 cycling index can contextualise broader annual cycling trends, while ORR station data can support separate descriptive rail context. Neither replaces the street-level Vivacity treated-control comparison.

### 3.4 Unit of Analysis

The raw unit is the Vivacity countline observation. For cleaning and descriptive analysis, observations are aggregated to daily countline-level records. For modelling, daily records are aggregated to weekly countline-level outcomes to reduce short-term noise and allow occasional missing days to be handled through observed-day weighting.

The main modelling unit is therefore:

> countline-week, with outcomes expressed per observed countline-day.

This avoids treating weeks with fewer valid days as equivalent to complete weeks. Model weights are based on the number of observed days contributing to each weekly countline record.

### 3.5 Outcome Specification

The dissertation should present outcomes in a disaggregated order. Pedestrian and cyclist outcomes should be reported separately first, followed by active travel total as a summary measure. Road and path/cycle-facility countlines should also be separated where the sample size allows.

| Outcome | Definition | Interpretation |
|---|---|---|
| `pedestrian` | Daily pedestrian count | Walking activity at the countline |
| `cyclist` | Daily cyclist count | Cycling activity at the countline |
| `active_travel_total` | `pedestrian + cyclist` | Combined walking and cycling; summary only |
| `motorised_total` | `car + motorbike + bus + OGV1 + OGV2 + LGV` | Motorised traffic context |
| `total_count` | `active_travel_total + motorised_total` | Total counted movement |
| `active_travel_share` | `active_travel_total / total_count` | Share of movement that is walking/cycling |
| `cycle_share` | `cyclist / total_count` | Cycling share of total counted movement |
| `pedestrian_per_observed_day` | Weekly pedestrian count divided by observed days | Model outcome for walking |
| `cyclist_per_observed_day` | Weekly cyclist count divided by observed days | Model outcome for cycling |
| `active_per_observed_day` | Weekly active travel count divided by observed days | Summary model outcome |

Route type is a second analytical dimension. Countlines are classified as road, path/cycle facility, crossing or unknown/other using exported metadata and countline names. This distinction matters because a scheme may change use on a dedicated path without producing the same trajectory on a road countline, or apparent change may reflect redistribution between carriageway and path rather than a net increase in walking or cycling.

The preferred reporting hierarchy is:

1. Pedestrian outcomes by scheme and treated/control role.
2. Cyclist outcomes by scheme and treated/control role.
3. Road versus path/cycle-facility outcomes.
4. Active travel total as a combined summary.

## 4. Methods

### 4.1 Data Preparation Workflow

The project is organised as a reproducible workflow under `/Users/lu_nanxi/CASA/Dissertation_Data`. Raw and processed data are stored in local zip archives, and the executable workflow is divided into five stages:

| Stage | Folder | Purpose |
|---|---|---|
| 1 | `workflow/01_data_preparation/` | Clean treated/control data, apply full-day cutoff, build integrity gate |
| 2 | `workflow/02_context_matching/` | Join sensors to LSOA/IMD/Census context and select matched controls |
| 3 | `workflow/03_modelling_diagnostics/` | Run exploratory models, pre-trend checks and event-study diagnostics |
| 4 | `workflow/04_descriptive_analysis/` | Produce descriptive trends, route-type summaries and sensitivity tables |
| 5 | `workflow/05_document_generation/` | Generate methods, results, discussion and diagnostic report drafts |

The workflow contains current outputs using the confirmed intervention dates, together with archived earlier outputs retained only as provenance.

### 4.2 Daily Aggregation

Raw Vivacity exports contain classified counts by timestamp, direction and countline. These records are aggregated to daily countline-level totals. Directional observations are summed within each countline and date. The daily table retains countline metadata, scheme identifiers, route type, data availability, data error flags and mode-specific counts.

The daily derived variables are:

```text
active_travel_total = pedestrian + cyclist
motorised_total = car + motorbike + bus + OGV1 + OGV2 + LGV
total_count = active_travel_total + motorised_total
active_travel_share = active_travel_total / total_count
cycle_share = cyclist / total_count
```

This daily aggregation produces the base table for reliability checking, descriptive analysis and later weekly aggregation.

### 4.3 Full-Day Cutoff

The workflow applies a full-day cutoff at 26 May 2026. This avoids including incomplete recent observations. All daily treated and control datasets are filtered to complete days up to this cutoff. The cutoff is important because incomplete final-day data could artificially lower counts and distort trends.

### 4.4 First Reliable Date

A central preprocessing problem is that early zero counts may not be real zero movement. Some early rows contain zeros or missing availability because the sensor was not yet active or not producing usable data. Treating these rows as true zero pedestrian or cyclist flows would create artificial before/after changes.

To address this, the workflow identifies a first reliable date for each countline. A reliable day must satisfy the following conditions:

| Criterion | Rule |
|---|---|
| Date validity | date is present |
| Availability | `dataAvailabilityPercent_min >= 80` |
| Error status | no data error |
| Observed movement | `active_travel_total > 0` |
| Reliability window | at least 14 reliable days within the following 21-day window |

Rows before the first reliable date are treated as outside the usable observation window rather than as true low-activity days. This is one of the most important cleaning decisions in the dissertation because it separates sensor absence from genuine behavioural absence.

### 4.5 Quality Flags

Each daily row is assigned quality flags before analysis. The main flags are:

| Flag | Meaning |
|---|---|
| `availability_missing_bool` | Daily availability is missing |
| `low_availability_bool` | Daily availability is below the 80% threshold |
| `data_error_bool` | Vivacity data error is recorded |
| `quality_ok_day` | Date is valid, confirmed intervention date is present, availability is sufficient and no error is recorded |
| `quality_reliable_day` | `quality_ok_day` plus positive active travel flow |
| `after_first_reliable_date` | Row occurs on or after the countline's first reliable date |
| `use_row_after_gate` | Row passes first reliable date and daily quality checks |

These flags determine whether a row can enter descriptive analysis, causal-style analysis, or neither.

### 4.6 Integrity Gate

The integrity gate is applied before modelling. It classifies treated and control countlines according to whether they have enough reliable data before and after the confirmed intervention date.

The gate assesses 55 countlines in the current project structure:

| Countline category | Count |
|---|---:|
| Total countlines assessed | 55 |
| Treated countlines | 21 |
| Control countlines | 34 |
| Pragmatic causal controls | 13 |
| Strict seasonal controls | 2 |
| Descriptive-only controls | 21 |

The pragmatic causal control rule requires:

```text
first reliable date before confirmed intervention date
at least 90 usable pre-intervention days
at least 180 usable post-intervention days
```

The strict seasonal control rule requires:

```text
first reliable date before confirmed intervention date
at least 365 usable pre-intervention days
at least 365 usable post-intervention days
```

Only two controls meet the strict seasonal threshold, which is an important limitation. The pragmatic threshold provides a workable exploratory comparison set, but it does not remove the need for manual control-site verification.

### 4.7 Treated and Control Inclusion

Countlines are assigned to one of the following analytical roles:

| Role | Meaning |
|---|---|
| Treated | Countline linked to an active travel scheme |
| Valid control | Control countline passing the pragmatic or strict gate |
| Descriptive-only control | Control countline retained for description but excluded from causal models |
| Descriptive-only treated | Treated countline/scheme lacking sufficient pre/post coverage for modelling |

The previous modelling-ready dataset contained:

| Output quantity | Count |
|---|---:|
| Source rows | 117,711 |
| Descriptive daily rows | 52,144 |
| Main causal daily rows | 28,344 |
| Descriptive countlines | 55 |
| Main causal countlines | 25 |
| Main causal schemes | 12d, 12f, 13 |

These numbers describe the archived modelling output base. The current report separately recalculates eligibility from the official city-region exports; schemes 12b and 12e remain descriptive because they lack quality-passing pre-installation treated observations.

### 4.8 Context Joining and Equity Classification

Vivacity countline locations are spatially joined to LSOA 2021. Each countline is then linked to IMD 2019 and Census 2021 context variables. The current context join successfully matched all dissertation countlines:

| Join check | Result |
|---|---:|
| Dissertation countlines matched to LSOA 2021 | 21 / 21 |
| Countlines joined to Census 2021 context | 21 / 21 |
| Countlines joined to IMD 2019 context | 21 / 21 |
| Unique treated LSOA 2021 areas | 6 |
| Daily sensor rows joined to LSOA context | 21,860 / 21,860 |

For equity interpretation, IMD decile is grouped into deprivation bands. Lower IMD deciles indicate greater deprivation. The current causal modelling set contains 25 scheme-countline records, representing 25 physical countlines across eight LSOA 2021 areas. In that set, more deprived LSOAs account for 19 causal countlines and more affluent LSOAs account for three. This supports an equity-context discussion but not a robust deprivation-interaction causal model.

### 4.9 Matched-Control Selection

Matched-control candidate areas are selected at LSOA 2021 level. Candidate LSOAs are ranked against treated sensor LSOAs using contextual similarity:

| Matching variable | Purpose |
|---|---|
| IMD decile | Overall deprivation similarity |
| Income decile | Income deprivation similarity |
| Employment decile | Labour-market context |
| Population density | Urban form similarity |
| Households with no car | Transport dependency / car access context |
| Census commute by bicycle | Baseline cycling context |
| Census commute on foot | Baseline walking context |

The matching process produces a shortlist of possible control countlines located in contextually similar non-intervention LSOAs. These are candidate controls, not final controls. Before being interpreted as controls, they must be manually checked to confirm that they are not themselves exposed to active travel, road, or public-realm interventions during the study period.

### 4.10 Weekly Modelling Dataset

After the integrity gate, daily rows are aggregated to weekly countline-level outcomes. Weekly aggregation reduces short-term noise while preserving time-series structure. For each countline-week, the workflow calculates:

| Weekly variable | Definition |
|---|---|
| `observed_days` | Number of valid daily observations in the week |
| `pedestrian_per_observed_day` | Weekly pedestrian total / observed days |
| `cyclist_per_observed_day` | Weekly cyclist total / observed days |
| `active_per_observed_day` | Weekly active travel total / observed days |
| `motorised_per_observed_day` | Weekly motorised total / observed days |
| `week_start` | Start date of week |
| `intervention_date` | Confirmed scheme intervention date |
| `analysis_period` | Pre or post relative to confirmed date |

The primary model outcomes are pedestrian, cyclist and active travel counts per observed countline-day. Active travel is retained as a summary outcome, but interpretation should begin with pedestrian and cyclist results separately.

### 4.11 Exploratory Model Specification

For modelled schemes, weekly countline-level models are fitted separately by scheme and outcome. The model form is:

```text
log1p(outcome_per_observed_day) ~
  time +
  post +
  post_time +
  treated_post +
  treated_post_time +
  seasonality +
  countline fixed effects
```

The two key terms are:

| Term | Interpretation |
|---|---|
| `treated_post` | Immediate treated-control level change after the confirmed intervention date |
| `treated_post_time_weeks` | Additional treated-control weekly slope change after the confirmed intervention date |

Seasonality is modelled using sine and cosine terms based on week of year. Standard errors are clustered by countline. Models are weighted by observed days so that partial weeks contribute proportionally.

### 4.12 Causal Diagnostics

Two diagnostic checks are used to assess whether stronger causal interpretation is plausible:

1. Pre-trend tests using only pre-intervention observations.
2. Event-study diagnostics using relative-week bins before and after intervention.

The previous diagnostic outputs found weak causal readiness for all three modelled schemes:

| Scheme | Pre-trend warnings | Event-study warnings | Interpretation |
|---|---:|---:|---|
| 12d | 2 | 0 pre; 2 post | Not credible for strong causal wording |
| 12f | 0 | 1 pre; 4 post | Partly credible but still exploratory |
| 13 | 0 | 1 pre; 0 post | Partly credible but still exploratory |

These diagnostics use the confirmed intervention dates. The diagnostic framework remains appropriate, while the short pre-period and limited control pool still constrain causal interpretation.

## 5. Data Preprocessing Summary

The preprocessing workflow can be summarised as follows:

1. Load raw Vivacity treated and control classified-count CSVs.
2. Standardise column names and mode-count fields.
3. Aggregate hourly/directional observations to daily countline totals.
4. Apply the full-day cutoff to 26 May 2026.
5. Create daily pedestrian, cyclist, active travel and motorised outcomes.
6. Encode confirmed exact scheme intervention dates.
7. Identify each countline's first reliable date.
8. Remove rows before the first reliable date from the analysis-ready dataset.
9. Remove low-availability and error-flagged days.
10. Assign pre/post periods using confirmed intervention dates.
11. Spatially join countlines to LSOA 2021.
12. Attach IMD 2019 and Census 2021 context variables.
13. Select matched-control candidates using LSOA contextual similarity.
14. Apply the integrity gate to determine causal, descriptive-only and excluded roles.
15. Aggregate valid daily rows to weekly countline-level modelling data.
16. Produce descriptive, mode-specific, route-type and modelling-ready outputs.

This preprocessing is not a technical side step; it is central to the dissertation's validity. Without the first reliable date rule, pre-sensor zeros could be misread as behavioural change. Without the integrity gate, weak controls could enter the model and distort treated-control comparisons. Without separating pedestrian/cyclist and road/path outcomes, meaningful differences in behaviour and route function could be hidden by overly broad aggregation.

## 6. Results

The modelled evidence is mixed and does not produce a clear positive uptake signal relative to controls. Most treated-control estimates are uncertain, and the statistically flagged terms are negative for 12d cyclist counts and 12f active travel. These results use the confirmed dates but should still be interpreted as exploratory rather than definitive scheme effects.

The results chapter should present pedestrian and cyclist outcomes separately first, then road/path route-type patterns, and only then active travel total as a summary. It should also report how many countlines and controls remain after the confirmed-date integrity gate.

## 7. Discussion

The discussion should argue that the dissertation contributes a transparent evaluation workflow for active travel monitoring data under real-world constraints. If the rerun still produces weak or mixed relationships, that should not be framed as failure. Instead, it should be interpreted as evidence that active travel evaluation depends on monitoring design, pre-intervention coverage, control-site quality, and the ability to distinguish walking from cycling and road from path use.

The discussion should avoid overclaiming causal effects or deprived-versus-affluent differences. The more defensible claim is that the dissertation shows how much can be learned from Vivacity sensor data, and where the current evidence base remains too uneven for strong causal or equity-impact conclusions.

## 8. Conclusion

This dissertation investigates active travel uptake around selected Liverpool City Region schemes using high-frequency Vivacity sensor data, contextualised with LSOA-level deprivation and Census variables. The study adopts an exploratory matched-control interrupted time-series design and places strong emphasis on data preprocessing, first reliable sensor dates, confirmed intervention dates, matched-control selection and causal diagnostics.

The analysis should be presented as a cautious but valuable contribution. It does not simply ask whether active travel schemes worked. It asks whether the available monitoring data are strong enough to evaluate walking and cycling trajectories across different neighbourhood contexts. The answer is likely to be qualified: Vivacity data provide detailed local evidence, but robust causal and equity interpretation requires reliable pre-intervention coverage, verified control sites, confirmed intervention dates, and disaggregated pedestrian/cyclist and road/path analysis.
