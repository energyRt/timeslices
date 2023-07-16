## code to prepare `DATASET` dataset goes here

tsl_formats <- c(
  # daily formats
  "d364", "d365", "d366",

  # year-day & 24 hours
  "d364_h24", "d365_h24", "d366_h24",

  # "y_d365", "y_d366",
  # "y_d365_h24", "y_d366_h24",

  # representative month & 24 hours
  "m12_h24"
  # "y_m12_h24"

)
# save(tsl_formats, file = "data/tsl_formats.RData")

tsl_levels <- list(
  d364 = list(
    YDAY = paste0("d", formatC(1:364, width = 3, flag = "0"))),
  d365 = list(
    YDAY = paste0("d", formatC(1:365, width = 3, flag = "0"))),
  d366 = list(
    YDAY = paste0("d", formatC(1:366, width = 3, flag = "0"))),
  #
  d364_h24 = list(
    YDAY = paste0("d", formatC(1:364, width = 3, flag = "0")),
    HOUR = paste0("h", formatC(0:23, width = 2, flag = "0"))),
  d365_h24 = list(
    YDAY = paste0("d", formatC(1:365, width = 3, flag = "0")),
    HOUR = paste0("h", formatC(0:23, width = 2, flag = "0"))),
  d366_h24 = list(
    YDAY = paste0("d", formatC(1:366, width = 3, flag = "0")),
    HOUR = paste0("h", formatC(0:23, width = 2, flag = "0"))),
  #
  m12_h24 = list(
    MONTH = paste0("d", formatC(1:12, width = 3, flag = "0")),
    HOUR = paste0("h", formatC(0:23, width = 2, flag = "0")))
)

tsl_sets <- list(
  d365_h24 = tidyr::expand_grid(
    ANNUAL = "ANNUAL",
    YDAY = tsl_levels$d365_h24$YDAY,
    HOUR = tsl_levels$d365_h24$HOUR)
)

usethis::use_data(tsl_formats, tsl_sets, tsl_levels, overwrite = TRUE)
