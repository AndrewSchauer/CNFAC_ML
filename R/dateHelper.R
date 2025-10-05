#' Robust conversion of many date/time formats to Date
#'
#' Handles Excel serials, UNIX epoch (sec/ms), and common string formats
#' like "11/16/2018 0:00", "2024-05-12", "2/20/2001 12:34 PM".
#' Returns Date (YYYY-MM-DD), dropping time.
#'
#' @param x character/numeric/Date/POSIXt
#' @return Date
#' @export
#' @importFrom anytime anydate
dateHelper <- function(x) {
  # Already date-like?
  if (inherits(x, "Date"))   return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x, tz = "UTC"))

  ch <- trimws(as.character(x))
  ch[ch == "" | ch %in% c("NA","NaN","null","NULL")] <- NA_character_

  # Start with all NA Dates
  out <- as.Date(rep(NA_real_, length(ch)), origin = "1970-01-01")

  # ---- Numeric handling: Excel serials / UNIX epoch ----
  num   <- suppressWarnings(as.numeric(ch))
  isnum <- !is.na(num)
  if (any(isnum)) {
    idx <- which(isnum)
    n   <- num[isnum]

    # Excel serials (Windows): days since 1899-12-30
    is_excel <- n > 20000 & n < 80000    # ~1955–2120
    if (any(is_excel)) {
      out[idx[is_excel]] <- as.Date(n[is_excel], origin = "1899-12-30")
    }

    # UNIX epoch milliseconds (>= 1e12)
    is_ms <- n >= 1e12
    if (any(is_ms)) {
      out[idx[is_ms]] <- as.Date(as.POSIXct(n[is_ms] / 1000, origin = "1970-01-01", tz = "UTC"))
    }

    # UNIX epoch seconds (>= 1e9)
    is_sec <- !is_ms & n >= 1e9
    if (any(is_sec)) {
      out[idx[is_sec]] <- as.Date(as.POSIXct(n[is_sec], origin = "1970-01-01", tz = "UTC"))
    }
  }

  # ---- Character fallback: robust parser (anytime) ----
  need <- is.na(out) & !is.na(ch)
  if (any(need)) {
    # strip simple trailing TZ tokens that confuse some inputs
    ch2 <- sub("\\s*(UTC|GMT|Z)\\s*$", "", ch[need], ignore.case = TRUE)
    out[need] <- suppressWarnings(anytime::anydate(ch2, tz = "UTC"))
  }

  out
}

# Optional alias if you want both names
#' @export
to_Date <- dateHelper
