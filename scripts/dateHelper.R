#' Robust conversion of many date/time formats to Date (strict)
#'
#' @param x A vector (character, numeric, Date, POSIXt)
#' @return Date
#' @export
#' @importFrom lubridate parse_date_time
dateHelper <- function(x) {
  if (inherits(x, "Date"))   return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x, tz = "UTC"))

  ch <- trimws(as.character(x))
  ch[ch == "" | ch %in% c("NA","NaN","null","NULL")] <- NA_character_

  # init
  out <- as.Date(rep(NA_real_, length(ch)), origin = "1970-01-01")

  # -------- numerics: Excel / epoch --------
  num   <- suppressWarnings(as.numeric(ch))
  isnum <- !is.na(num)
  if (any(isnum)) {
    idx <- which(isnum)
    n   <- num[isnum]

    # Excel serials (Windows): days since 1899-12-30
    is_excel <- n > 20000 & n < 80000
    if (any(is_excel)) out[idx[is_excel]] <- as.Date(n[is_excel], origin = "1899-12-30")

    # Epoch ms
    is_ms <- n >= 1e11
    if (any(is_ms)) out[idx[is_ms]] <- as.Date(as.POSIXct(n[is_ms]/1000, origin = "1970-01-01", tz = "UTC"))

    # Epoch sec
    is_sec <- !is_ms & n >= 1e8 & n < 1e11
    if (any(is_sec)) out[idx[is_sec]] <- as.Date(as.POSIXct(n[is_sec], origin = "1970-01-01", tz = "UTC"))
  }

  need <- is.na(out) & !is.na(ch)
  if (!any(need)) return(out)

  # -------- year-only strings: make NA (or set to Jan 1 if you prefer) --------
  yo <- need & grepl("^\\d{4}$", ch)
  if (any(yo)) {
    # Option A (safer): leave NA
    out[yo] <- as.Date(NA)
    # Option B (if you want 2001-01-01 instead):
    # out[yo] <- as.Date(paste0(ch[yo], "-01-01"))
  }

  # re-evaluate what's still unparsed
  need <- is.na(out) & !is.na(ch)
  if (!any(need)) return(out)

  s <- ch[need]

  # -------- month-year only (e.g., "1/2001" or "2001-02") -> assume day 1 --------
  my1 <- grepl("^\\d{1,2}/\\d{4}$", s)           # m/Y
  if (any(my1)) {
    out[which(need)[my1]] <- as.Date(paste0(s[my1], "/01"), format = "%m/%Y/%d")
  }
  my2 <- grepl("^\\d{4}-\\d{1,2}$", s)           # Y-m
  if (any(my2)) {
    out[which(need)[my2]] <- as.Date(paste0(s[my2], "-01"), format = "%Y-%m-%d")
  }

  # re-evaluate again
  need <- is.na(out) & !is.na(ch)
  if (!any(need)) return(out)

  # -------- explicit formats only; exact=TRUE avoids 2001 -> 20/01 accidents --------
  parsed <- suppressWarnings(lubridate::parse_date_time(
    ch[need],
    orders = c(
      # ISO-like
      "Y-m-d HMS","Y-m-d HM","Y-m-d H","Y-m-d",
      # US slashed
      "m/d/Y HMS","m/d/Y HM","m/d/Y",
      # US dashed
      "m-d-Y HMS","m-d-Y HM","m-d-Y",
      # Day-first (explicitly dashed/slashed)
      "d-m-Y HMS","d-m-Y HM","d-m-Y",
      "d/m/Y HMS","d/m/Y HM","d/m/Y",
      # Month names
      "b d Y HMS","b d Y HM","b d Y",
      "d-b-Y HMS","d-b-Y HM","d-b-Y",
      # 2-digit year variants (only if you truly have them)
      "m/d/y HMS","m/d/y HM","m/d/y",
      "d/m/y HMS","d/m/y HM","d/m/y"
    ),
    tz = "UTC",
    exact = TRUE
  ))
  out[need] <- as.Date(parsed, tz = "UTC")

  out
}

