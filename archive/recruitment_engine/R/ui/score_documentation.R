# Config-driven score documentation, role recipes, and metric dictionary for the About page.
# Values/formulas must come from the same config objects used in production scoring.

about_accordion <- function(title, ...) {
  tags$details(
    class = "about-accordion",
    tags$summary(title),
    div(class = "about-accordion-body", ...)
  )
}

load_metric_dictionary <- function() {
  tryCatch(load_yaml("config/metric_dictionary.yml"), error = function(e) list(metrics = list()))
}

metric_meta <- function(internal_name, dict = NULL) {
  dict <- dict %||% load_metric_dictionary()
  m <- dict$metrics[[internal_name]]
  if (is.null(m)) {
    return(list(
      display_name = internal_name,
      definition = "Definition not yet catalogued.",
      unit = "—",
      source = "—",
      native_or_proxy = "unknown",
      derivation = NULL,
      direction = "higher_better",
      soccer_rationale = "—",
      limitation = "—",
      shrinkage = FALSE,
      league_adjustment = FALSE,
      missing_handling = "NA"
    ))
  }
  m
}

roles_using_metric <- function(internal_name, roles_yaml = NULL) {
  roles_yaml <- roles_yaml %||% {
    if (exists("load_roles_config", mode = "function")) load_roles_config() else load_yaml("config/role_weights.yml")
  }
  hits <- character()
  for (rid in names(roles_yaml$roles %||% list())) {
    mets <- roles_yaml$roles[[rid]]$metrics %||% list()
    if (internal_name %in% names(mets)) {
      hits <- c(hits, sprintf("%s (%.2f)", roles_yaml$roles[[rid]]$display_name %||% rid, as.numeric(mets[[internal_name]])))
    }
  }
  if (!length(hits)) return("—")
  paste(hits, collapse = "; ")
}

normalize_weight_shares <- function(w) {
  w <- unlist(w)
  w[w < 0] <- 0
  if (sum(w) <= 0) return(w)
  w / sum(w)
}

live_overall_weight_labels <- function(w) {
  w <- normalize_weight_shares(w)
  c(
    Contribution = w[["projected_mls_performance"]] %||% 0,
    `Role Fit` = w[["tactical_role_fit"]] %||% 0,
    Value = w[["financial_value"]] %||% 0,
    Feasibility = w[["acquisition_feasibility"]] %||% 0,
    Development = w[["development_upside"]] %||% 0
  )
}

render_role_recipe_table <- function(role_id, roles_yaml = NULL, dict = NULL, spec = NULL) {
  roles_yaml <- roles_yaml %||% {
    if (exists("load_roles_config", mode = "function")) load_roles_config() else load_yaml("config/role_weights.yml")
  }
  dict <- dict %||% load_metric_dictionary()
  spec <- spec %||% tryCatch(load_product_config(), error = function(e) list())
  role <- roles_yaml$roles[[role_id]]
  if (is.null(role)) return(tags$p("Role not found."))
  proxies <- spec$proxy_metrics %||% character()
  cov <- spec$coverage$role_fit_unavailable_below %||% 0.50

  rows <- lapply(names(role$metrics %||% list()), function(nm) {
    meta <- metric_meta(nm, dict)
    w <- as.numeric(role$metrics[[nm]])
    native <- if (identical(meta$native_or_proxy, "native")) "Native" else if (identical(meta$native_or_proxy, "proxy") || nm %in% proxies) "Derived proxy" else meta$native_or_proxy
    c(
      meta$display_name %||% nm,
      nm,
      sprintf("%.2f", w),
      if (identical(meta$direction, "lower_better")) "Lower better" else "Higher better",
      native,
      meta$source %||% "—",
      meta$soccer_rationale %||% "—",
      if (isTRUE(meta$shrinkage)) "Yes" else "No",
      if (isTRUE(meta$league_adjustment)) "Yes" else "No",
      meta$missing_handling %||% "NA"
    )
  })

  tagList(
    h4(role$display_name %||% role_id),
    p(role$description %||% ""),
    p(HTML(sprintf(
      "<strong>Eligible position group:</strong> %s · <strong>Minimum coverage to show Role Fit:</strong> %.0f%% · Weights are <em>expert-configured</em> for the role definition — not learned from historical outcomes.",
      role$position_group %||% "—", 100 * as.numeric(cov)
    ))),
    div(style = "overflow-x:auto;",
      tags$table(
        class = "about-table",
        tags$thead(tags$tr(lapply(
          c("Display name", "Internal name", "Weight", "Direction", "Native/proxy", "Source",
            "Soccer rationale", "Shrinkage", "League adj.", "Missing handling"),
          tags$th
        ))),
        tags$tbody(lapply(seq_along(rows), function(i) {
          style <- if (i %% 2 == 0) "background:#f3f7f4;" else NULL
          tags$tr(style = style, lapply(rows[[i]], function(cell) tags$td(cell)))
        }))
      )
    )
  )
}

#' Role Fit breakdown for one player row (defensible trace).
role_fit_player_trace <- function(player_row, role_id, roles_yaml = NULL, dict = NULL, spec = NULL) {
  roles_yaml <- roles_yaml %||% {
    if (exists("load_roles_config", mode = "function")) load_roles_config() else load_yaml("config/role_weights.yml")
  }
  dict <- dict %||% load_metric_dictionary()
  spec <- spec %||% tryCatch(load_product_config(), error = function(e) list())
  role <- roles_yaml$roles[[role_id]]
  if (is.null(role) || !nrow(as.data.frame(player_row))) {
    return(list(coverage = NA_real_, score = NA_real_, confidence = NA_character_, rows = data.frame()))
  }
  weights <- unlist(role$metrics)
  cols <- paste0(names(weights), "_pct")
  # also try without _pct suffix variants used in data
  get_pct <- function(nm) {
    candidates <- c(paste0(nm, "_pct"), paste0("pct_", gsub("_p90$", "", nm)), paste0("pct_", nm))
    for (c in candidates) {
      if (c %in% names(player_row)) {
        v <- suppressWarnings(as.numeric(player_row[[c]][[1]]))
        if (length(v)) return(v[1])
      }
    }
    # direct column on row if already percentile-named
    alt <- paste0(nm, "_pct")
    if (alt %in% names(player_row)) return(suppressWarnings(as.numeric(player_row[[alt]][[1]])))
    NA_real_
  }

  parts <- lapply(names(weights), function(nm) {
    pct <- get_pct(nm)
    meta <- metric_meta(nm, dict)
    list(internal = nm, display = meta$display_name %||% nm, weight = as.numeric(weights[[nm]]), percentile = pct)
  })
  w <- vapply(parts, `[[`, numeric(1), "weight")
  pct <- vapply(parts, `[[`, numeric(1), "percentile")
  ok <- is.finite(pct)
  cov <- if (sum(abs(w)) < 1e-9) 0 else sum(abs(w[ok])) / sum(abs(w))
  unavailable <- spec$coverage$role_fit_unavailable_below %||% 0.5
  if (cov < unavailable || !any(ok)) {
    score <- NA_real_
    contrib <- rep(NA_real_, length(w))
  } else {
    ww <- w[ok] / sum(w[ok])
    score <- sum(ww * pct[ok])
    contrib <- ifelse(ok, (w / sum(w[ok])) * pct, NA_real_)
  }
  conf <- if (cov < unavailable) NA_character_ else if (cov < (spec$coverage$max_confidence_low_below %||% 0.7)) "Low" else if (cov < (spec$coverage$max_confidence_medium_below %||% 0.85)) "Medium" else "High"

  rows <- data.frame(
    Metric = vapply(parts, `[[`, character(1), "display"),
    Internal = vapply(parts, `[[`, character(1), "internal"),
    `Player percentile` = ifelse(is.finite(pct), round(pct, 1), NA_real_),
    Weight = round(w, 3),
    `Weighted contribution` = ifelse(is.finite(contrib), round(contrib, 2), NA_real_),
    Available = ok,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  list(coverage = cov, score = score, confidence = conf, rows = rows, role_name = role$display_name %||% role_id)
}

render_role_fit_trace_ui <- function(trace) {
  if (is.null(trace) || !nrow(trace$rows)) {
    return(tags$p("No Role Fit inputs available for this player and role."))
  }
  tagList(
    p(HTML(sprintf(
      "Available coverage: <strong>%.0f%%</strong><br/>Role Fit: <strong>%s</strong><br/>Confidence: <strong>%s</strong>",
      100 * (trace$coverage %||% 0),
      if (is.finite(trace$score)) sprintf("%.1f", trace$score) else "Not available",
      trace$confidence %||% "—"
    ))),
    div(style = "overflow-x:auto;",
      tags$table(
        class = "about-table",
        tags$thead(tags$tr(
          tags$th("Metric"), tags$th("Percentile"), tags$th("Role weight"),
          tags$th("Contribution to score")
        )),
        tags$tbody(lapply(seq_len(nrow(trace$rows)), function(i) {
          r <- trace$rows[i, ]
          tags$tr(
            tags$td(r$Metric),
            tags$td(if (is.finite(r[["Player percentile"]])) r[["Player percentile"]] else "Not available"),
            tags$td(r$Weight),
            tags$td(if (is.finite(r[["Weighted contribution"]])) round(r[["Weighted contribution"]], 1) else "—")
          )
        }))
      )
    ),
    tags$p(class = "help-text", "Missing metrics stay Not available — never filled with 50.")
  )
}

render_metric_dictionary_table <- function(filter_text = "", roles_yaml = NULL, dict = NULL, spec = NULL) {
  dict <- dict %||% load_metric_dictionary()
  roles_yaml <- roles_yaml %||% {
    if (exists("load_roles_config", mode = "function")) load_roles_config() else load_yaml("config/role_weights.yml")
  }
  spec <- spec %||% tryCatch(load_product_config(), error = function(e) list())
  names_all <- names(dict$metrics %||% list())
  if (!nzchar(filter_text %||% "")) {
    keep <- names_all
  } else {
    ft <- tolower(filter_text)
    keep <- names_all[vapply(names_all, function(nm) {
      m <- dict$metrics[[nm]]
      grepl(ft, tolower(paste(nm, m$display_name %||% "", m$definition %||% "", m$source %||% "")), fixed = TRUE)
    }, logical(1))]
  }

  rows <- lapply(keep, function(nm) {
    m <- metric_meta(nm, dict)
    c(
      m$display_name %||% nm,
      nm,
      m$definition %||% "—",
      m$unit %||% "—",
      m$source %||% "—",
      if (identical(m$native_or_proxy, "native")) "Native" else "Derived proxy",
      m$derivation %||% "—",
      if (identical(m$direction, "lower_better")) "Lower better" else "Higher better",
      roles_using_metric(nm, roles_yaml),
      "Season × position (MLS position when enough MLS rows)",
      if (isTRUE(m$shrinkage)) "EB shrinkage applied" else "No shrinkage",
      if (isTRUE(m$league_adjustment)) "League factor applied" else "No league factor",
      m$missing_handling %||% "NA",
      "Configured / unvalidated heuristic",
      m$soccer_rationale %||% "—",
      m$limitation %||% "—"
    )
  })

  div(style = "overflow-x:auto; max-height: 420px;",
    tags$table(
      class = "about-table metric-dict-table",
      tags$thead(tags$tr(lapply(
        c("Display name", "Internal name", "Definition", "Unit", "Source", "Native/proxy",
          "Derivation", "Direction", "Roles (weight)", "Reference group", "Shrinkage",
          "League adj.", "Missing", "Validation", "Rationale", "Limitation"),
        tags$th
      ))),
      tags$tbody(lapply(seq_along(rows), function(i) {
        style <- if (i %% 2 == 0) "background:#f3f7f4;" else NULL
        tags$tr(style = style, lapply(rows[[i]], function(cell) tags$td(as.character(cell))))
      }))
    )
  )
}

render_how_scores_calculated <- function(cfg = NULL, spec = NULL, example_player = NULL, example_role = "transition_winger") {
  cfg <- cfg %||% tryCatch(load_config(), error = function(e) list())
  spec <- spec %||% tryCatch(load_product_config(), error = function(e) list())
  thr <- tryCatch(load_thresholds(), error = function(e) list())
  fw <- cfg$scoring$weights %||% list()
  m0 <- spec$hybrid$learned$shrinkage$default_m0 %||% 600
  lambda <- cfg$scoring$risk_penalty_weight %||% 0.15
  gate <- thr$feasibility_gate$low_threshold %||% spec$feasibility_gate$low_threshold %||% 35
  cov_lo <- thr$coverage$role_fit_unavailable_below %||% 0.5
  conf_lo <- thr$coverage$max_confidence_low_below %||% 0.70
  conf_med <- thr$coverage$max_confidence_medium_below %||% 0.85
  peaks <- spec$age_curves %||% list()

  example_block <- NULL
  if (!is.null(example_player) && nrow(as.data.frame(example_player))) {
    tr <- role_fit_player_trace(example_player, example_role, spec = spec)
    pname <- example_player$display_name[[1]] %||% "Selected player"
    example_block <- tagList(
      h4(sprintf("Example: %s as a %s", pname, tr$role_name %||% example_role)),
      render_role_fit_trace_ui(tr),
      p("The displayed contributions use the available weights after they are renormalized.")
    )
  }

  div(
    h2("How the Scores Work"),
    p("The app uses transparent scoring rules to organize players for further scouting."),
    p("These scores are not probabilities and are not confirmed transfer recommendations. They summarize public information under the selected club, role, and recruitment settings."),
    p("All scores run from 0 to 100."),
    tags$ul(
      tags$li("Higher is better for performance, fit, value, feasibility, and development."),
      tags$li("Lower is better for Model Uncertainty."),
      tags$li("When important information is missing, the app displays Not available rather than inventing a value.")
    ),
    p(tags$strong("Current model status:"), " Transparent heuristic system; historical validation is still in progress."),

    about_accordion(
      "Estimated MLS-Scale Contribution",
      h4("What does it mean?"),
      p("This score summarizes how strong the player’s recent on-field performance appears after accounting for:"),
      tags$ul(
        tags$li("Tactical role"),
        tags$li("Minutes played"),
        tags$li("Small-sample reliability"),
        tags$li("Current league"),
        tags$li("Assumed differences in league strength"),
        tags$li("Comparison with similar MLS players")
      ),
      p("It does not use:"),
      tags$ul(
        tags$li("Salary"),
        tags$li("Cost"),
        tags$li("Age"),
        tags$li("Club budget"),
        tags$li("Acquisition feasibility")
      ),
      p("It should be interpreted as an adjusted performance index, not a guaranteed prediction of future production."),
      h4("How is it calculated?"),
      p("First, the app combines the player’s available role-relevant performance percentiles:"),
      tags$pre(class = "formula-code", "Raw Contribution = weighted average of available contribution metrics"),
      p("The exact metrics and weights depend on the selected role."),
      p("The score is then reduced when the player has a limited sample:"),
      tags$pre(class = "formula-code", paste(
        "Minutes Factor = clip(Minutes / 2500, 0.70, 1.00)",
        "Contribution = clip(Raw Contribution × Minutes Factor, 0, 100)",
        sep = "\n"
      )),
      h4("What do the terms mean?"),
      tags$ul(
        tags$li(HTML("<strong>Raw Contribution:</strong> weighted average of the player’s adjusted performance percentiles")),
        tags$li(HTML("<strong>Minutes:</strong> minutes in the selected evaluation period")),
        tags$li(HTML("<strong>Minutes Factor:</strong> how much the limited sample reduces the score")),
        tags$li(HTML("<strong>clip:</strong> prevents the result from going below or above the stated range"))
      ),
      h4("Example"),
      p("A player has:"),
      tags$ul(
        tags$li("Raw Contribution: 70"),
        tags$li("Minutes: 1,800")
      ),
      tags$pre(class = "formula-code", paste(
        "Minutes Factor = 1800 / 2500 = 0.72",
        "Contribution = 70 × 0.72 = 50.4",
        sep = "\n"
      )),
      h4("Missing data"),
      p("The formula uses the available positive-weight metrics and recalculates their weights."),
      p("If none of the required contribution metrics are available:"),
      tags$blockquote("Estimated MLS-Scale Contribution: Not available"),
      h4("Limitation"),
      p("This is currently a configured performance index. It should not be called a validated next-season forecast until it is tested against future MLS results.")
    ),

    about_accordion(
      "Tactical Role Fit",
      h4("What does it mean?"),
      p("Role Fit measures how closely the player’s observed statistical profile matches the selected tactical role."),
      p("A Pressing Striker and a Progressive Center Back are evaluated using different metrics."),
      h4("How is it calculated?"),
      tags$pre(class = "formula-code", paste(
        "Role Fit_i = Σ_{j ∈ O_i} ( w_j / Σ_{j ∈ O_i} w_j ) × p_ij",
        sep = "\n"
      )),
      p("Where:"),
      tags$ul(
        tags$li(HTML("<strong>i</strong> represents the player")),
        tags$li(HTML("<strong>j</strong> represents a metric in the selected role")),
        tags$li(HTML("<strong>p_ij</strong> is the player’s percentile for that metric")),
        tags$li(HTML("<strong>w_j</strong> is the configured importance of that metric")),
        tags$li(HTML("<strong>O_i</strong> is the set of role metrics available for the player"))
      ),
      h4("Metric coverage"),
      p("The app also measures how much of the complete role definition is supported by available data:"),
      tags$pre(class = "formula-code", "Coverage_i = (Σ_{j ∈ O_i} |w_j|) / (Σ_{j ∈ R} |w_j|)"),
      p("Where:"),
      tags$ul(
        tags$li(HTML("<strong>R</strong> is the complete set of metrics for the role")),
        tags$li(HTML("<strong>O_i</strong> is the subset available for the player"))
      ),
      h4("Confidence rules"),
      tags$ul(
        tags$li(HTML(sprintf("Below %.0f%% coverage: Role Fit unavailable", 100 * as.numeric(cov_lo)))),
        tags$li(HTML(sprintf("%.0f%% to below %.0f%%: Low confidence", 100 * as.numeric(cov_lo), 100 * as.numeric(conf_lo)))),
        tags$li(HTML(sprintf("%.0f%% to below %.0f%%: Medium confidence", 100 * as.numeric(conf_lo), 100 * as.numeric(conf_med)))),
        tags$li(HTML(sprintf("%.0f%% or more: eligible for High confidence", 100 * as.numeric(conf_med))))
      ),
      example_block,
      h4("Missing data"),
      p("Missing metrics:"),
      tags$ul(
        tags$li("Remain unavailable"),
        tags$li("Do not receive a score of 50"),
        tags$li("Do not create artificial similarity between players"),
        tags$li("Reduce coverage and confidence")
      ),
      h4("Limitation"),
      p("Some pressing, progression, and defensive measures are derived proxies. They are not direct tracking measures of physical pressure or off-ball movement.")
    ),

    about_accordion(
      "Compensation-Adjusted Value",
      h4("What does it mean?"),
      p("This score compares the player’s Estimated MLS-Scale Contribution with their known guaranteed-compensation tier."),
      p("It asks:"),
      tags$blockquote("Is the player providing strong modeled contribution relative to their known compensation?"),
      h4("Compensation penalties"),
      tags$table(
        class = "about-table",
        tags$thead(tags$tr(tags$th("Compensation tier"), tags$th("Penalty"))),
        tags$tbody(
          tags$tr(tags$td("Tier 1"), tags$td("20")),
          tags$tr(style = "background:#f3f7f4;", tags$td("Tier 2"), tags$td("40")),
          tags$tr(tags$td("Tier 3"), tags$td("60")),
          tags$tr(style = "background:#f3f7f4;", tags$td("Tier 4"), tags$td("80")),
          tags$tr(tags$td("Tier 5"), tags$td("95"))
        )
      ),
      h4("Formula"),
      tags$pre(class = "formula-code", "Value = clip(Contribution − Cost Penalty + 50, 0, 100)"),
      h4("Example"),
      p("A player has:"),
      tags$ul(
        tags$li("Contribution: 56.6"),
        tags$li("Compensation tier: 2"),
        tags$li("Tier 2 penalty: 40")
      ),
      tags$pre(class = "formula-code", "56.6 − 40 + 50 = 66.6"),
      p(tags$strong("Compensation-Adjusted Value: 66.6")),
      h4("Missing data"),
      p("When guaranteed compensation is unknown:"),
      tags$blockquote("Compensation-Adjusted Value: Not available"),
      p("The app does not estimate or invent salary."),
      h4("Limitation"),
      p("This score does not include:"),
      tags$ul(
        tags$li("Transfer fees"),
        tags$li("MLS trade cost"),
        tags$li("General Allocation Money"),
        tags$li("Salary Budget Charge"),
        tags$li("Contract length"),
        tags$li("Agent demands"),
        tags$li("Total acquisition cost")
      ),
      p("It is a compensation-efficiency index, not financial surplus.")
    ),

    about_accordion(
      "Acquisition Feasibility",
      h4("What does it mean?"),
      p("Feasibility estimates how clear and plausible the public acquisition pathway appears."),
      p("It does not claim that the player is officially available."),
      h4("Current inputs"),
      p("The current rule considers:"),
      tags$ul(
        tags$li("Known compensation tier"),
        tags$li("Current league"),
        tags$li("Current playing-time situation"),
        tags$li("Confirmed MLS roster status when available")
      ),
      h4("Formula"),
      tags$pre(class = "formula-code",
               "Feasibility = 0.35(Cost Score) + 0.30(League Score) + 0.20(Minutes Score) + 0.15(Roster Status Score)"),
      p("Only inputs supported by public information should be used."),
      p("When an input is unknown:"),
      tags$ul(
        tags$li("It remains unavailable"),
        tags$li("The remaining known weights are renormalized"),
        tags$li("Feasibility confidence is reduced")
      ),
      p("Nationality must not be used as a substitute for MLS domestic or international status."),
      h4("Example"),
      p("Suppose a player has:"),
      tags$table(
        class = "about-table",
        tags$thead(tags$tr(tags$th("Input"), tags$th("Subscore"), tags$th("Weight"))),
        tags$tbody(
          tags$tr(tags$td("Compensation tier"), tags$td("80"), tags$td("35%")),
          tags$tr(style = "background:#f3f7f4;", tags$td("Current league"), tags$td("85"), tags$td("30%")),
          tags$tr(tags$td("Playing-time situation"), tags$td("75"), tags$td("20%")),
          tags$tr(style = "background:#f3f7f4;", tags$td("Official roster status"), tags$td("Unknown"), tags$td("—"))
        )
      ),
      p("The known weights total 85%, so they are renormalized:"),
      tags$pre(class = "formula-code",
               "Feasibility = [0.35(80) + 0.30(85) + 0.20(75)] / 0.85 = 80.6"),
      p(HTML("<strong>Feasibility: 80.6</strong><br/>Confidence: Reduced because official roster status is unavailable")),
      h4("Feasibility gate"),
      p(HTML(sprintf(
        "When Feasibility is below <strong>%.0f</strong>, the Overall score cannot exceed 54.",
        as.numeric(gate)
      ))),
      p("This prevents a statistically interesting but publicly unrealistic target from receiving the strongest recommendation."),
      h4("Limitation"),
      p("The app cannot observe:"),
      tags$ul(
        tags$li("Whether the player wants to move"),
        tags$li("Whether the current club is willing to sell or trade"),
        tags$li("Exact contract demands"),
        tags$li("Agent preferences"),
        tags$li("Private club budgets"),
        tags$li("Unpublished MLS rights or mechanisms")
      )
    ),

    about_accordion(
      "Development Upside",
      h4("What does it mean?"),
      p("Development Upside estimates the player’s potential to improve."),
      p("It is separate from the player’s current contribution."),
      h4("Inputs"),
      p("The score may use:"),
      tags$ul(
        tags$li("Age relative to a position-specific development curve"),
        tags$li("Year-over-year performance change"),
        tags$li("Minutes trajectory")
      ),
      h4("Formula"),
      tags$pre(class = "formula-code",
               "Development = 0.55(Age Outlook) + 0.25(Year-over-Year Trend) + 0.20(Minutes Trajectory)"),
      p("When one of the trend components is unavailable, the score uses the available components and renormalizes their weights."),
      h4("Age outlook"),
      p("The app uses a smooth curve rather than abrupt age bands."),
      p("Configured approximate positional peaks currently include:"),
      tags$ul(
        tags$li(sprintf("Forward: %s", peaks$F %||% 25.5)),
        tags$li(sprintf("Winger or attacking midfielder: %s", peaks$W_AM %||% 25.8)),
        tags$li(sprintf("Central midfielder: %s", peaks$CM %||% 26.5)),
        tags$li(sprintf("Fullback: %s", peaks$FB %||% 26.8)),
        tags$li(sprintf("Center back: %s", peaks$CB %||% 27.2)),
        tags$li(sprintf("Goalkeeper: %s", peaks$GK %||% 29.0))
      ),
      p("A younger player does not automatically receive a better current Contribution score."),
      h4("Missing data"),
      p("If year-over-year performance or minutes history is unavailable:"),
      tags$ul(
        tags$li("The component remains missing"),
        tags$li("The score is based on the available evidence"),
        tags$li("Development confidence is reduced")
      ),
      h4("Limitation"),
      p("Age is not the same as potential. The score cannot observe training quality, coachability, physical development, or personal circumstances.")
    ),

    about_accordion(
      "Model Uncertainty",
      h4("What does it mean?"),
      p("Model Uncertainty measures how cautious the user should be about the displayed scores."),
      p("Lower is better."),
      h4("Inputs"),
      p("The current score combines:"),
      tags$ul(
        tags$li("Sample-size uncertainty"),
        tags$li("League-translation uncertainty"),
        tags$li("Missing-metric uncertainty")
      ),
      h4("Formula"),
      tags$pre(class = "formula-code",
               "Model Uncertainty = 0.40(Sample Uncertainty) + 0.35(Translation Uncertainty) + 0.25(Missing-Data Uncertainty)"),
      h4("Sample uncertainty"),
      tags$table(
        class = "about-table",
        tags$thead(tags$tr(tags$th("Minutes"), tags$th("Sample uncertainty"))),
        tags$tbody(
          tags$tr(tags$td("Under 700"), tags$td("80")),
          tags$tr(style = "background:#f3f7f4;", tags$td("700 to under 1,200"), tags$td("55")),
          tags$tr(tags$td("1,200 to under 2,000"), tags$td("35")),
          tags$tr(style = "background:#f3f7f4;", tags$td("2,000 or more"), tags$td("20"))
        )
      ),
      h4("Translation uncertainty"),
      tags$pre(class = "formula-code", "Translation Uncertainty = 100 × configured league uncertainty"),
      h4("Missing-data uncertainty"),
      tags$pre(class = "formula-code", "Missing-Data Uncertainty = 100 × (1 − Role Metric Coverage)"),
      h4("Example"),
      p("A player has:"),
      tags$ul(
        tags$li("Sample uncertainty: 20"),
        tags$li("Translation uncertainty: 5"),
        tags$li("Missing-data uncertainty: 25")
      ),
      tags$pre(class = "formula-code", "0.40(20) + 0.35(5) + 0.25(25) = 16.0"),
      p(tags$strong("Model Uncertainty: 16.0")),
      h4("Optional Overall penalty"),
      p("When enabled:"),
      tags$pre(class = "formula-code", sprintf(
        "Adjusted Overall = Overall × (1 − %.2f × Model Uncertainty / 100)",
        as.numeric(lambda)
      )),
      h4("Limitation"),
      p("This score measures uncertainty in the public-data model. It does not measure injury risk, character risk, or medical risk.")
    ),

    about_accordion(
      "Sample-Size Adjustment",
      h4("Why is this needed?"),
      p("A player can produce unusually strong statistics in a small number of minutes."),
      p("The app therefore combines the player’s observed rate with the typical rate for similar players."),
      h4("Formula"),
      tags$pre(class = "formula-code", paste(
        "Adjusted Rate = Reliability Weight × Player Rate + Prior Weight × Comparison-Group Rate",
        "",
        sprintf("Reliability Weight = Minutes / (Minutes + %s)", m0),
        sprintf("Prior Weight = %s / (Minutes + %s)", m0, m0),
        sep = "\n"
      )),
      p("The two weights always add to 1."),
      h4("What is the reliability weight?"),
      p("The Reliability Weight is the share of the adjusted statistic assigned to the player’s own observed performance."),
      tags$ul(
        tags$li("More minutes → greater trust in the player’s own rate"),
        tags$li("Fewer minutes → greater reliance on the comparison-group average")
      ),
      h4("Example"),
      p("A player has:"),
      tags$ul(
        tags$li("900 minutes"),
        tags$li("Observed rate: 0.40"),
        tags$li("Comparison-group rate: 0.30")
      ),
      tags$pre(class = "formula-code", paste(
        sprintf("Reliability Weight = 900 / (900 + %s) = %.2f", m0, 900 / (900 + as.numeric(m0))),
        sprintf("Prior Weight = %s / (900 + %s) = %.2f", m0, m0, as.numeric(m0) / (900 + as.numeric(m0))),
        sprintf("Adjusted Rate = %.2f(0.40) + %.2f(0.30) = %.2f",
                900 / (900 + as.numeric(m0)),
                as.numeric(m0) / (900 + as.numeric(m0)),
                900 / (900 + as.numeric(m0)) * 0.40 + as.numeric(m0) / (900 + as.numeric(m0)) * 0.30),
        sep = "\n"
      )),
      p(HTML(sprintf("The app uses the adjusted rate rather than fully trusting the observed 0.40. Current prior strength is fixed at <strong>%s</strong> minutes and has not yet been learned separately for each metric.", m0)))
    ),

    about_accordion(
      "Club Fit",
      h4("What does it mean?"),
      p("Club Fit explains how well the player matches the selected club’s public-data recruitment context."),
      p("It may consider:"),
      tags$ul(
        tags$li("Tactical style"),
        tags$li("Selected role"),
        tags$li("Recruitment pathway"),
        tags$li("Budget range"),
        tags$li("Immediate versus development priority")
      ),
      p("Related fields include:"),
      tags$ul(
        tags$li("Style Fit"),
        tags$li("Budget Fit"),
        tags$li("Pathway Fit"),
        tags$li("WhyClub explanation")
      ),
      h4("Effect on Overall"),
      p("Under the visible shortlist-weight system, Club Fit is explanatory and does not directly enter the Overall formula."),
      p("This prevents club estimates from secretly overriding the score weights selected by the user."),
      h4("Limitation"),
      p("Club profiles are public-data assumptions. They are not confidential sporting plans.")
    ),

    about_accordion(
      "Overall Score",
      h4("What does it mean?"),
      p("Overall organizes the shortlist using the component weights selected in the sidebar."),
      h4("Formula"),
      tags$pre(class = "formula-code", paste(
        "Overall = w_C×C + w_R×R + w_V×V + w_F×F + w_D×D",
        "",
        "C: Estimated MLS-Scale Contribution",
        "R: Tactical Role Fit",
        "V: Compensation-Adjusted Value",
        "F: Acquisition Feasibility",
        "D: Development Upside",
        "Each w: visible sidebar weight (normalized to sum to 1)",
        sep = "\n"
      )),
      h4("Default weights"),
      tags$table(
        class = "about-table",
        tags$thead(tags$tr(tags$th("Component"), tags$th("Default weight"))),
        tags$tbody(
          tags$tr(tags$td("Estimated MLS-Scale Contribution"),
                  tags$td(sprintf("%.0f%%", 100 * (fw$projected_mls_performance %||% 0.30)))),
          tags$tr(style = "background:#f3f7f4;", tags$td("Tactical Role Fit"),
                  tags$td(sprintf("%.0f%%", 100 * (fw$tactical_role_fit %||% 0.25)))),
          tags$tr(tags$td("Compensation-Adjusted Value"),
                  tags$td(sprintf("%.0f%%", 100 * (fw$financial_value %||% 0.20)))),
          tags$tr(style = "background:#f3f7f4;", tags$td("Acquisition Feasibility"),
                  tags$td(sprintf("%.0f%%", 100 * (fw$acquisition_feasibility %||% 0.15)))),
          tags$tr(tags$td("Development Upside"),
                  tags$td(sprintf("%.0f%%", 100 * (fw$development_upside %||% 0.10))))
        )
      ),
      h4("Missing components"),
      p("Missing components must not be replaced with 50."),
      p("Instead:"),
      tags$ul(
        tags$li("The missing component is excluded."),
        tags$li("The remaining weights are renormalized."),
        tags$li("Overall confidence is reduced."),
        tags$li("The missing field remains visibly marked as unavailable.")
      ),
      p("Example: If Value is unavailable, the remaining weights total 80%. Their normalized weights become Contribution 30/80, Role Fit 25/80, Feasibility 15/80, Development 10/80."),
      h4("Additional rules"),
      tags$ul(
        tags$li("Apply the Model Uncertainty penalty only when selected."),
        tags$li(HTML(sprintf("If Feasibility is below <strong>%.0f</strong>, Overall is capped at 54.", as.numeric(gate))))
      ),
      h4("Limitation"),
      p("Overall is a decision-priority score. It is not the probability that the player will succeed or complete a transfer.")
    ),

    about_accordion(
      "Roster Need Score",
      h4("What does it mean?"),
      p("This score estimates how strongly a club may need a player at a particular position and tactical role."),
      h4("Components"),
      tags$table(
        class = "about-table",
        tags$thead(tags$tr(tags$th("Component"), tags$th("Weight"))),
        tags$tbody(
          tags$tr(tags$td("Starter-quality gap"), tags$td("30%")),
          tags$tr(style = "background:#f3f7f4;", tags$td("Effective-depth gap"), tags$td("25%")),
          tags$tr(tags$td("Succession risk"), tags$td("15%")),
          tags$tr(style = "background:#f3f7f4;", tags$td("Availability and continuity risk"), tags$td("10%")),
          tags$tr(tags$td("Tactical-coverage gap"), tags$td("10%")),
          tags$tr(style = "background:#f3f7f4;", tags$td("Compensation-efficiency opportunity"), tags$td("10%"))
        )
      ),
      h4("Formula"),
      p("For each component:"),
      tags$pre(class = "formula-code", paste(
        "Gap = 100 − Club Percentile",
        "Need Score = Σ w_k × Gap_k",
        sep = "\n"
      )),
      p("A higher score means the club appears weaker or less secure in that role relative to other MLS clubs."),
      h4("Reference population"),
      p("The selected club is compared with other MLS clubs for the same tactical role."),
      h4("Limitation"),
      p("The current roster data is incomplete. This is a planning signal, not an official statement that the club needs to replace a player.")
    ),

    about_accordion(
      "Current Player vs Target Scores",
      p("These scores answer separate questions."),
      h4("Role Similarity"),
      p("How closely does the target resemble the current player in role-relevant metrics?"),
      tags$ul(
        tags$li("Calculated using available role percentiles."),
        tags$li("At least 50% metric coverage is required."),
        tags$li("High similarity does not mean the target is better.")
      ),
      h4("Upgrade Potential"),
      tags$pre(class = "formula-code", "Upgrade Potential = 50 + 0.70(ΔContribution) + 0.30(ΔRole Fit)"),
      p("Where ΔContribution and ΔRole Fit are target minus incumbent."),
      p("Age and compensation do not increase sporting Upgrade Potential."),
      h4("Lower-Cost Alternative"),
      p("This label requires:"),
      tags$ul(
        tags$li("Known compensation for both players"),
        tags$li("A lower compensation or estimated cost tier for the target"),
        tags$li("Similar-enough contribution"),
        tags$li("Adequate Role Fit")
      ),
      p("A target can never be called lower-cost when their known cost is higher."),
      h4("Developmental Successor"),
      p("This label considers younger age, Development Upside, future contribution, Role Fit, and Acquisition Feasibility."),
      p("A successor does not have to be an immediate upgrade."),
      h4("Complementary Profile"),
      p("This label identifies whether the target adds qualities the current player or roster lacks."),
      p("A complementary target may have low similarity."),
      h4("Fallback"),
      p("When none of the decision rules is supported:"),
      tags$blockquote("No clear recruitment advantage identified"),
      h4("Limitation"),
      p("These comparison labels use transparent decision rules. They have not yet been validated against historical transfer outcomes.")
    )
  )
}

render_score_docs_sections <- function(cfg, spec, example_player = NULL, example_role = "transition_winger",
                                       metric_filter = "", selected_recipe_role = "transition_winger") {
  tagList(
    render_how_scores_calculated(cfg, spec, example_player, example_role),

    h2("Model Assumptions and Validation"),
    tags$ul(
      tags$li("Transparent heuristic system — treat scores as directional until backtests complete."),
      tags$li("League factors are assumed league-strength adjustments pending mover-based validation."),
      tags$li("Role weights are expert-configured for role definitions — not learned from historical outcomes."),
      tags$li("Contribution is a configured performance index until a learned model is fitted and published."),
      tags$li(HTML(sprintf("Sample-size prior strength is currently <strong>%s</strong> minutes for rate metrics.",
                           spec$hybrid$learned$shrinkage$default_m0 %||% 600))),
      tags$li("Missing data remain missing.")
    )
  )
}
