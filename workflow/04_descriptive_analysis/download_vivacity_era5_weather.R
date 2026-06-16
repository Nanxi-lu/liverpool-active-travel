suppressPackageStartupMessages({
  library(data.table)
  library(jsonlite)
})

base_dir <- "/Users/lu_nanxi/CASA/Dissertation_Data"
output_dir <- file.path(base_dir, "reports", "analysis_reports")
share_dir <- file.path(
  base_dir,
  "output_visualised",
  "reports",
  "analysis_reports"
)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(share_dir, recursive = TRUE, showWarnings = FALSE)

context <- fread(cmd = paste(
  "unzip -p",
  shQuote(file.path(
    base_dir,
    "data_archives",
    "05_context_spatial_census_imd_data.zip"
  )),
  shQuote(
    "context_data/processed/matching/all_vivacity_countlines_lsoa2021_context.csv"
  )
))
context <- context[, .(
  countline_id = as.integer(countline_id),
  longitude = as.numeric(countline_midpoint_lon),
  latitude = as.numeric(countline_midpoint_lat)
)]

make_pair <- function(pair_id, scheme_id, treated_ids, control_ids) {
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

pair_lookup <- rbindlist(list(
  make_pair(
    "12b | Knowsley 017B", "12b",
    c(46676L, 46686L, 46687L, 46688L),
    c(16364L, 16365L, 16388L, 16389L)
  ),
  make_pair(
    "12d | Liverpool 052A", "12d",
    c(16373L, 16374L, 16393L, 16394L, 23779L),
    c(22898L, 22899L, 22900L)
  ),
  make_pair(
    "12e | St. Helens 008C", "12e",
    c(51817L, 51818L),
    c(40740L, 40742L)
  ),
  make_pair(
    "12e | St. Helens 014D", "12e",
    c(51794L, 51795L, 51796L),
    c(23811L, 23812L, 23813L)
  ),
  make_pair(
    "12f | Wirral 006A", "12f",
    c(23799L, 23800L, 23801L),
    c(23802L, 23803L, 23804L)
  ),
  make_pair(
    "13 | Halton 010B", "13",
    c(47768L, 47769L, 47772L, 47773L),
    c(15104L, 15106L, 15160L, 15161L, 15162L)
  )
))

weather_sites <- merge(
  pair_lookup,
  context,
  by = "countline_id",
  all.x = TRUE
)[
  ,
  .(
    requested_longitude = mean(longitude),
    requested_latitude = mean(latitude),
    countlines = uniqueN(countline_id)
  ),
  by = .(pair_id, scheme_id, imd_role)
]
setorder(weather_sites, pair_id, imd_role)
weather_sites[, weather_site_id := sprintf("imd_context_%02d", .I)]

query_url <- paste0(
  "https://archive-api.open-meteo.com/v1/archive?",
  "latitude=",
  paste(sprintf("%.6f", weather_sites$requested_latitude), collapse = ","),
  "&longitude=",
  paste(sprintf("%.6f", weather_sites$requested_longitude), collapse = ","),
  "&start_date=2020-01-01",
  "&end_date=2025-05-31",
  "&daily=temperature_2m_mean,precipitation_sum,",
  "wind_speed_10m_max,shortwave_radiation_sum",
  "&timezone=Europe%2FLondon",
  "&models=era5"
)

response <- NULL
last_error <- NULL
for (attempt in seq_len(5L)) {
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
  stop("Weather download failed: ", last_error)
}
if (length(response) != nrow(weather_sites)) {
  stop(
    "Expected ",
    nrow(weather_sites),
    " weather locations but received ",
    length(response),
    "."
  )
}

weather_daily <- rbindlist(lapply(seq_len(nrow(weather_sites)), function(i) {
  location <- response[[i]]
  weather <- as.data.table(lapply(location$daily, unlist))
  weather[, `:=`(
    weather_site_id = weather_sites$weather_site_id[i],
    date = as.IDate(time),
    requested_longitude = weather_sites$requested_longitude[i],
    requested_latitude = weather_sites$requested_latitude[i],
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

daily_name <- "vivacity_era5_weather_daily.csv"
lookup_name <- "vivacity_era5_weather_site_lookup.csv"
for (directory in c(output_dir, share_dir)) {
  fwrite(weather_daily, file.path(directory, daily_name))
  fwrite(weather_sites, file.path(directory, lookup_name))
}

cat(sprintf(
  "Wrote %s daily rows for %s context locations (%s to %s).\n",
  nrow(weather_daily),
  uniqueN(weather_daily$weather_site_id),
  min(weather_daily$date),
  max(weather_daily$date)
))
