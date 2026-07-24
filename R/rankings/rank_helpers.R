# Ranking helpers used by scripts and the Shiny app.

ROLE_POSITION_MAP <- c(
  pressing_striker = "FW",
  transition_winger = "W",
  ball_winning_midfielder = "CM",
  progressive_center_back = "CB",
  overlapping_fullback = "FB"
)

filter_role_pool <- function(df, role_id) {
  ensure_packages("dplyr")
  out <- dplyr::filter(df, .data$role_id == .env$role_id)
  pos <- ROLE_POSITION_MAP[[role_id]]
  if (!is.null(pos)) out <- dplyr::filter(out, .data$position_group == .env$pos)
  out
}

feasibility_tier_label <- function(score) {
  # Cautious language until acquisition model is validated
  dplyr::case_when(
    score >= 85 ~ "Estimated high plausibility",
    score >= 70 ~ "Estimated moderate plausibility",
    score >= 45 ~ "Estimated difficult",
    score >= 25 ~ "Estimated low plausibility",
    TRUE ~ "Insufficient public signal"
  )
}

#' Format a 0–100 score with within-shortlist rank.
#' Score itself must be fixed-reference; rank may change with filters.
#' Example: 78.2 (#2) — higher scores rank better unless higher_better = FALSE (e.g. Risk).
fmt_score_with_rank <- function(x, higher_better = TRUE, digits = 1) {
  x <- as.numeric(x)
  n <- length(x)
  if (!n) return(character())
  ord <- if (isTRUE(higher_better)) -x else x
  r <- rank(ord, ties.method = "min", na.last = "keep")
  ifelse(
    is.finite(x) & is.finite(r),
    paste0(formatC(x, format = "f", digits = digits), " (#", as.integer(r), ")"),
    "Not available"
  )
}

fmt_score_with_ref <- function(score, ref_pct, shortlist_rank = NULL, digits = 1) {
  score <- as.numeric(score)
  n <- length(score)
  if (!n) return(character())
  ref_pct <- if (is.null(ref_pct)) rep(NA_real_, n) else as.numeric(ref_pct)
  if (length(ref_pct) == 1L && n > 1L) ref_pct <- rep(ref_pct, n)
  ranks <- if (is.null(shortlist_rank)) {
    rep(NA_real_, n)
  } else {
    as.numeric(shortlist_rank)
  }
  if (length(ranks) == 1L && n > 1L) ranks <- rep(ranks, n)

  out <- rep("Not available", n)
  ok <- is.finite(score)
  if (!any(ok)) return(out)

  base <- formatC(score[ok], format = "f", digits = digits)
  bits <- base
  has_rank <- is.finite(ranks[ok])
  bits[has_rank] <- paste0(bits[has_rank], " · #", as.integer(ranks[ok][has_rank]), " filters")
  has_ref <- is.finite(ref_pct[ok])
  bits[has_ref] <- paste0(bits[has_ref], " · ", round(ref_pct[ok][has_ref]), "th ref")
  out[ok] <- bits
  out
}

# MLSPA / ASA salary guide figures are published in USD (including Canadian MLS clubs).
# FX rates used only if a non-USD currency is ever supplied by a source.
DEFAULT_FX_TO_USD <- c(USD = 1, CAD = 0.74, EUR = 1.08, GBP = 1.27, MXN = 0.055)

#' Convert a compensation amount to USD.
to_salary_usd <- function(amount, currency = "USD", fx = DEFAULT_FX_TO_USD) {
  amount <- as.numeric(amount)
  currency <- toupper(trimws(as.character(currency %||% "USD")))
  currency[!nzchar(currency) | is.na(currency)] <- "USD"
  rate <- fx[currency]
  rate[!is.finite(rate)] <- 1
  amount * as.numeric(rate)
}

#' Display helper: "$1,234,567 USD"
fmt_usd <- function(amount, currency = "USD", fx = DEFAULT_FX_TO_USD) {
  usd <- to_salary_usd(amount, currency, fx)
  ifelse(
    is.finite(usd),
    paste0("$", formatC(round(usd), format = "d", big.mark = ","), " USD"),
    "—"
  )
}
