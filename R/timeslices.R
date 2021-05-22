#' Common formats of time-slices.
#'
#' @format A character vector with formats:
#' \describe{
#'   \item{d365}{daily time-slices, 365 a year (leap year's 366th day is disregarded)}
#'   \item{d365_h24}{time slices with year-day numbers and hours, 8760 in total}
#'   ...
#' }
"tsl_formats"
tsl_formats <- c(
  "d365", "d366",
  "d365_h24", "d366_h24",

  "y_d365", "y_d366",
  "y_d365_h24", "y_d366_h24",

  "m12_h24",
  "y_m12_h24"

)
# save(tsl_formats, file = "data/tsl_formats.RData")

#' Sets of the common formats with structure
#'
#' @rdname tsl_formats
#'
tsl_sets <- list(
  d365 = list(
    YDAY = paste0("d", formatC(1:365, width = 3, flag = "0"))),
  d366 = list(
    YDAY = paste0("d", formatC(1:366, width = 3, flag = "0"))),
  d365_h24 = list(
    YDAY = paste0("d", formatC(1:365, width = 3, flag = "0")),
    HOUR = paste0("h", formatC(0:23, width = 2, flag = "0"))),
  d366_h24 = list(
    YDAY = paste0("d", formatC(1:366, width = 3, flag = "0")),
    HOUR = paste0("h", formatC(0:23, width = 2, flag = "0")))
)
# save(tsl, file = "data/tsl.RData")

#' Convert date-time objects to time-slice
#'
#' @param dtm vector of timepoints in Date format
#' @param type character, type of the slices
#' @param d366.as.na logical, if
#'
#' @return
#' Character vector with time-slices names
#' @export
#'
#' @examples
#' dtm2tsl(lubridate::now())
#' dtm2tsl(lubridate::ymd("2020-12-31"))
#' dtm2tsl(lubridate::ymd("2020-12-31"), d366.as.na = F)
#' dtm2tsl(lubridate::now(tzone = "UTC"), type = "d365")
#' dtm2tsl(lubridate::ymd("2020-12-31"), type = "d365")
#' dtm2tsl(lubridate::ymd("2020-12-31"), type = "d365", d366.as.na = F)
#' dtm2tsl(lubridate::ymd("2020-12-31"), type = "d366")
dtm2tsl <- function(dtm, type = "d365_h24", d366.as.na = grepl("d365", type)) {
  stopifnot(is.timepoint(dtm))
  if (type == "d365_h24" | type == "d366_h24") {
    x <- paste0("d", formatC(yday(dtm), width = 3, flag = "0"), "_",
                "h", formatC(hour(dtm), width = 2, flag = "0"))
  } else if (type == "d365" | type == "d366") {
    x <- paste0("d", formatC(yday(dtm), width = 3, flag = "0"))
  } else if (type == "y_d365_h24" | type == "y_d366_h24") {
    x <- paste0("y", formatC(year(dtm), width = 4, flag = "0"), "_",
                "d", formatC(yday(dtm), width = 3, flag = "0"), "_",
                "h", formatC(hour(dtm), width = 2, flag = "0"))
  } else if (type == "m12_h24") {
    x <- paste0("m", formatC(month(dtm), width = 2, flag = "0"), "_",
                "h", formatC(hour(dtm), width = 2, flag = "0"))
  }
  if (d366.as.na) {
    x[grepl("d366", x)] <- NA
  }
  return(x)
}


# check
if (F) {

}

#' Convert time-slices to date-time, extract year, month, day of the year, or hour
#'
#' @param tsl character vector with time-slices
#' @param type character, type of the slices
#' @param tmz time-zone
#' @param year year, used when time-slices don't store year
#' @param mday day of month, for time slices without the information
#'
#' @return
#' Vector in Date-Time format
#' @export
#'
#' @examples
#' tsl <- c("y2007_d365_h15", NA, "d151_h22", "d001", "m10_h12")
#' tsl2dtm(tsl[1])
#' tsl2dtm(tsl[1:2])
#' tsl2dtm(tsl[2])
#' tsl2dtm(tsl[3])
#' tsl2dtm(tsl[4])
#' tsl2dtm(tsl[3], year = 2010)
#' tsl2dtm(tsl[4], year = 1900)
#' tsl2dtm(tsl[3:4], year = 1900)
tsl2dtm <- function(tsl, type = tsl_guess_format(tsl), tmz = "UTC",
                    year = NULL, mday = NULL) {
  if (is.null(type)) return(NULL)
  y <- NULL; m <- NULL; d <- NULL; h <- NULL
  if (grepl("y", type)) y <- tsl2year(tsl)
  if (grepl("m", type)) m <- tsl2month(tsl)
  if (grepl("d", type)) d <- tsl2yday(tsl)
  if (grepl("h", type)) h <- tsl2hour(tsl)

  # year
  if (is.null(y) || all(is.na(y))) {
    if (is.null(year)) return(NULL) # not enough info to create Date object
    if (length(year) == 1) {
      y <- rep(year, length(tsl))
    } else if (length(tsl) == length(year)) {
      y <- as.integer(year)
    } else {
      stop("length of 'year' should be equal to 1 or to the length of 'tsl'")
    }
  }

  if (type %in% c("d365_h24", "d366_h24", "y_d365_h24", "y_d366_h24")) {
    # yday-based
    dtm <- lubridate::ymd_h(paste0(y, "-01-01 0"), tz = tmz) + days(d) + hours(h)
  } else if (type %in% c("d365", "d366")) {
    # yday, no-hours
    dtm <- lubridate::ymd_h(paste0(y, "-01-01 0"), tz = tmz) + days(d)
  } else if (type %in% c("m12_h24", "y_m12_h24")) {
    # month-based
    if (is.null(mday)) return(NULL) # not enough info to create Date object
    dtm <- lubridate::ymd_h(paste0(y, "-", m,  "-", mday, " ", h), tz = tmz)
  }
  return(dtm)
}


#' @describeIn tsl2dtm Extract year from time-slices
#'
#' @param return.null logical, valid for the cased then all values are NA, then NULL will be returned if return.null = TRUE,
#'
#' @return
#' @export
#'
#' @examples
#' NULL
tsl2year <- function(tsl, return.null = T) {
  # browser()
  # library(stringr)
  y <- NULL
  y <- str_extract(tsl, "y[0-9]++")
  if (return.null) {
    if (all(is.na(y))) return(NULL)
  }
  y <- str_sub(y, 2, 5)
  y <- as.integer(y)
  return(y)
}

#' @describeIn tsl2dtm Extract the day of the year from time-slices
#'
#' @param return.null logical, valid for the cased then all values are NA, then NULL will be returned if return.null = TRUE,
#'
#' @return
#' @export
#'
#' @examples
#' NULL
tsl2yday <- function(tsl, return.null = T) {
  d <- str_extract(tsl, "d[0-9]++")
  if (return.null) {
    if (all(is.na(d))) return(NULL)
  }
  d <- str_sub(d, 2, 4)
  d <- as.integer(d)
  return(d)
}

#' @describeIn tsl2dtm Extract hour from time-slices
#'
#' @param return.null logical, valid for the cased then all values are NA, then NULL will be returned if return.null = TRUE,
#'
#' @return
#' @export
#'
#' @examples
#' NULL
tsl2hour <- function(tsl, return.null = T) {
  h <- str_extract(tsl, "h[0-9]++")
  if (return.null) {
    if (all(is.na(h))) return(NULL)
  }
  h <- str_sub(h, 2, 3)
  h <- as.integer(h)
  return(h)
}

#' @describeIn tsl2dtm Extract month from time-slices
#'
#' @param return.null logical, valid for the cased then all values are NA, then NULL will be returned if return.null = TRUE,
#'
#' @return
#' @export
#'
#' @examples
#' NULL
tsl2month <- function(tsl, return.null = T) {
  m <- str_extract(tsl, "m[0-9]++")
  if (return.null) {
    if (all(is.na(m))) return(NULL)
  }
  m <- str_sub(m, 2, 3)
  m <- as.integer(m)
  return(m)
}

#' Guess format of time-slices
#'
#' @param tsl
#'
#' @return
#' @export
#'
#' @examples
#' tsl <- c("y2007_d365_h15", NA, "d151_h22", "d001", "m10_h12")
#' tsl_guess_format(tsl)
#' tsl_guess_format(tsl[1])
#' tsl_guess_format(tsl[2])
#' tsl_guess_format(tsl[3])
#' tsl_guess_format(tsl[4])

tsl_guess_format <- function(tsl) {
  # browser()
  y <- grepl("y[0-9]+", tsl); ny <- sum(y, na.rm = T)
  m <- grepl("m[0-9]+", tsl); nm <- sum(m, na.rm = T)
  d <- grepl("d[0-9]+", tsl); nd <- sum(d, na.rm = T)
  h <- grepl("h[0-9]+", tsl); nh <- sum(h, na.rm = T)

  ii <- !is.na(tsl)
  if (!any(ii)) return(NULL)
  jj <- y | m | d | h # check

  type <- NULL
  if (ny > 0) {
    if (!all(y == jj)) return(NULL)
    type <- "y"
  }
  if (nd > 0) {
    if (!all(d == jj)) return(NULL)
    dd <- ifelse(any(grepl("366", tsl[ii])), 366, 365)
    type <- paste0(type, ifelse(!is.null(type), "_", ""), "d", dd)
  }
  if (nm > 0) {
    if (!all(n == jj)) return(NULL)
    mm <- tsl2month(tsl[ii])
    if (min(mm) < 1 | max(mm) > 12) return(NULL)
    type <- paste0(type, ifelse(!is.null(type), "_", ""), "m", 12)
  }
  if (nh > 0) {
    if (!all(h == jj)) return(NULL)
    hh <- tsl2hour(tsl[ii])
    if (min(hh, na.rm = T) < 0 | max(hh, na.rm = T) > 23) return(NULL)
    type <- paste0(type, ifelse(!is.null(type), "_", ""), "h", 24)
  }
  return(type)
}


