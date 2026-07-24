# Formula & Variable Glossary — rendered from the same config SoT as production scoring.
# Do not hard-code alternate "conceptual" equations here; load numbers from YAML/code contracts.

load_formula_sot <- function(cfg = NULL, spec = NULL) {
  cfg <- cfg %||% tryCatch(load_config(), error = function(e) list())
  spec <- spec %||% tryCatch(load_product_config(), error = function(e) load_model_spec())
  thr <- tryCatch(load_thresholds(), error = function(e) list())
  leagues <- tryCatch(load_yaml("config/league_tiers.yml"), error = function(e) list())
  roles <- tryCatch({
    if (exists("load_roles_config", mode = "function")) load_roles_config() else load_yaml("config/role_weights.yml")
  }, error = function(e) list())
  rules <- tryCatch(load_yaml("config/recommendation_rules.yml"), error = function(e) list())

  shrink_metrics <- c(
    "npxg_p90", "xa_p90", "pressures_p90", "tackles_p90", "interceptions_p90",
    "progressive_passes_p90", "progressive_carries_p90", "goals_added_p90",
    "shots_p90", "crosses_p90"
  )
  default_m0 <- as.numeric(spec$hybrid$learned$shrinkage$default_m0 %||% 600)
  m0_by_metric <- setNames(
    vapply(shrink_metrics, function(m) {
      tryCatch(as.numeric(shrinkage_m0_for(m, cfg, spec)), error = function(e) default_m0)
    }, numeric(1)),
    shrink_metrics
  )

  list(
    cfg = cfg,
    spec = spec,
    thresholds = thr,
    leagues = leagues,
    roles = roles,
    rules = rules,
    shrink_metrics = shrink_metrics,
    m0_by_metric = m0_by_metric,
    default_m0 = default_m0,
    blend_m0 = as.numeric(cfg$project$blend$prior_strength_minutes %||% 700),
    blend_min_ytd = as.numeric(cfg$project$blend$min_ytd_share %||% 0.15),
    blend_max_ytd = as.numeric(cfg$project$blend$max_ytd_share %||% 0.90),
    risk_lambda = as.numeric(cfg$scoring$risk_penalty_weight %||% 0.15),
    flat_weights = cfg$scoring$weights %||% list(),
    need_weights = cfg$roster_needs$weights %||% list(),
    coverage = thr$coverage %||% spec$coverage %||% list(),
    feasibility_gate = as.numeric(
      thr$feasibility_gate$low_threshold %||% spec$feasibility_gate$low_threshold %||% 35
    ),
    sporting = spec$sporting_score %||% list(),
    recruitment = spec$recruitment_priority %||% list(),
    age_peaks = spec$age_curves %||% list(),
    contribution_status = spec$hybrid$learned$contribution$status %||% "fallback_heuristic",
    shrinkage_status = spec$hybrid$learned$shrinkage$status %||% "fallback_fixed_m0",
    league_status = spec$hybrid$learned$league_translation$status %||% "assumed_tier_priors"
  )
}

fmt_num <- function(x, digits = 2) {
  if (length(x) != 1 || !is.finite(as.numeric(x))) return("—")
  format(round(as.numeric(x), digits), nsmall = digits, trim = TRUE)
}

# One definition block used under every production formula.
formula_def_block <- function(rows) {
  # rows: list of named lists with keys matching the audit checklist
  tags$table(
    class = "about-table formula-def-table",
    tags$thead(tags$tr(
      tags$th("Symbol / variable"),
      tags$th("Unit"),
      tags$th("Allowed range"),
      tags$th("Higher/lower"),
      tags$th("Source"),
      tags$th("Type"),
      tags$th("Reference population"),
      tags$th("Missing values"),
      tags$th("Current config value")
    )),
    tags$tbody(lapply(rows, function(r) {
      tags$tr(
        tags$td(HTML(r$symbol %||% "")),
        tags$td(r$unit %||% ""),
        tags$td(r$range %||% ""),
        tags$td(r$direction %||% ""),
        tags$td(r$source %||% ""),
        tags$td(r$type %||% ""),
        tags$td(r$ref %||% ""),
        tags$td(r$missing %||% ""),
        tags$td(HTML(r$config %||% ""))
      )
    }))
  )
}

formula_example <- function(...) {
  div(class = "formula-example", tags$strong("Numeric example: "), ...)
}

formula_code <- function(...) {
  tags$pre(class = "formula-code", paste(..., sep = "\n"))
}

render_shrinkage_formula <- function(sot) {
  m0_rows <- lapply(names(sot$m0_by_metric), function(m) {
    sprintf("%s → prior_strength (m₀) = %s minutes", m, fmt_num(sot$m0_by_metric[[m]], 0))
  })
  m0_ex <- sot$default_m0
  mins <- 900
  obs <- 0.40
  prior <- 0.30
  rw <- mins / (mins + m0_ex)
  pw <- m0_ex / (mins + m0_ex)
  adj <- rw * obs + pw * prior

  tagList(
    p(HTML(paste0(
      "Production function: <code>empirical_bayes_shrink()</code> in ",
      "<code>R/utilities/load_project.R</code>, applied in <code>build_features()</code>. ",
      "Status: <strong>", htmltools::htmlEscape(sot$shrinkage_status), "</strong>."
    ))),
    formula_code(
      "reliability_weight_i = minutes_i / (minutes_i + prior_strength)",
      "prior_weight_i       = prior_strength / (minutes_i + prior_strength)",
      "adjusted_rate_i      = reliability_weight_i * observed_rate_i",
      "                     + prior_weight_i * prior_rate",
      "",
      "# Equivalent production line:",
      "# w <- minutes / (minutes + m0)",
      "# w * rate + (1 - w) * prior_rate",
      "",
      "# Identity: reliability_weight_i + prior_weight_i = 1"
    ),
    p(HTML(paste0(
      "<strong>prior_strength (m₀)</strong> is the number of minutes that controls how quickly ",
      "the model trusts the player’s own rate. Larger m₀ keeps the estimate closer to the prior longer."
    ))),
    p(tags$strong("Current prior_strength by metric (from config / shrinkage_m0_for):")),
    tags$ul(lapply(m0_rows, tags$li)),
    formula_def_block(list(
      list(
        symbol = "<code>minutes_i</code>",
        unit = "minutes",
        range = "≥ 0",
        direction = "More minutes → higher reliability weight",
        source = "ASA / processed player-season minutes",
        type = "Observed",
        ref = "Player-season row",
        missing = "If non-finite, weight undefined / NA propagates",
        config = "—"
      ),
      list(
        symbol = "<code>prior_strength</code> (m₀)",
        unit = "minutes",
        range = "Positive; grid if learned {200…1800}",
        direction = "Higher → more prior influence",
        source = "config/model.yml hybrid.learned.shrinkage.default_m0 (or learned artifact)",
        type = "Configured (fallback); learnable",
        ref = "—",
        missing = "Falls back to 600",
        config = paste0("<strong>", fmt_num(sot$default_m0, 0), "</strong> for all listed metrics currently")
      ),
      list(
        symbol = "<code>observed_rate_i</code>",
        unit = "per-90 rate (metric-specific)",
        range = "Depends on metric",
        direction = "Usually higher better for attack metrics",
        source = "ASA / feature pipeline",
        type = "Observed",
        ref = "Player in season",
        missing = "NA propagates; never filled with 50",
        config = "—"
      ),
      list(
        symbol = "<code>prior_rate</code>",
        unit = "Same as observed_rate",
        range = "Depends on metric",
        direction = "Same as metric",
        source = "Mean of metric within (league_id, position_group)",
        type = "Estimated (group mean)",
        ref = "league_id × position_group on the feature table",
        missing = "NA prior → NA adjusted rate",
        config = "Computed each build"
      ),
      list(
        symbol = "<code>reliability_weight_i</code>, <code>prior_weight_i</code>",
        unit = "unitless share",
        range = "[0, 1]; sum to 1",
        direction = "Reliability weight higher with more minutes",
        source = "Derived",
        type = "Derived",
        ref = "—",
        missing = "—",
        config = "—"
      ),
      list(
        symbol = "<code>adjusted_rate_i</code>",
        unit = "Same as observed_rate",
        range = "Depends on metric",
        direction = "Same as metric",
        source = "Derived",
        type = "Estimated",
        ref = "Same as prior",
        missing = "NA if inputs NA",
        config = "—"
      )
    )),
    formula_example(HTML(sprintf(
      "minutes_i = %s, prior_strength = %s, observed_rate_i = %s, prior_rate = %s → ",
      mins, fmt_num(m0_ex, 0), obs, prior
    )), HTML(sprintf(
      "reliability_weight_i = %s, prior_weight_i = %s, adjusted_rate_i = %s.",
      fmt_num(rw, 3), fmt_num(pw, 3), fmt_num(adj, 3)
    )))
  )
}

render_role_fit_formula <- function(sot) {
  cov_lo <- sot$coverage$role_fit_unavailable_below %||% 0.50
  conf_lo <- sot$coverage$max_confidence_low_below %||% 0.70
  conf_med <- sot$coverage$max_confidence_medium_below %||% 0.85
  tagList(
    p(HTML("Production: <code>score_role_fit_with_coverage()</code> in <code>R/features/build_features.R</code>.")),
    formula_code(
      "coverage_i = sum(|w_j| for observed j) / sum(|w_j| for all role metrics j)",
      "if coverage_i < coverage_unavailable_threshold or no observed metrics:",
      "  RoleFit_i = NA",
      "else:",
      "  RoleFit_i = sum( (w_j / sum(w_observed)) * percentile_j )  for observed j"
    ),
    formula_def_block(list(
      list(
        symbol = "<code>w_j</code>",
        unit = "unitless weight",
        range = "Typically ≥ 0; sum ≈ 1 per role",
        direction = "Higher weight → metric matters more",
        source = "config/roles.yml (role_weights.yml)",
        type = "Configured (expert)",
        ref = "Selected tactical role",
        missing = "—",
        config = "Per-role metric weights in roles.yml"
      ),
      list(
        symbol = "<code>percentile_j</code>",
        unit = "percentile score",
        range = "0–100",
        direction = "Higher better",
        source = "Feature percentiles after shrink/translate",
        type = "Estimated",
        ref = "Usually position_group; MLS position when ≥8 MLS values",
        missing = "Stay NA — never filled with 50",
        config = "—"
      ),
      list(
        symbol = "<code>coverage_i</code>",
        unit = "fraction",
        range = "0–1",
        direction = "Higher = more complete role recipe",
        source = "Derived",
        type = "Derived",
        ref = "Role metric set",
        missing = "0 if nothing observed",
        config = paste0("unavailable below <strong>", fmt_num(cov_lo, 2), "</strong>")
      ),
      list(
        symbol = "<code>RoleFit_i</code>",
        unit = "index points",
        range = "0–100 or NA",
        direction = "Higher better",
        source = "Derived",
        type = "Estimated",
        ref = "Selected role",
        missing = "NA when coverage below threshold",
        config = sprintf(
          "confidence caps: Low &lt;%s; Medium &lt;%s; else High-eligible",
          fmt_num(conf_lo, 2), fmt_num(conf_med, 2)
        )
      )
    )),
    formula_example(HTML(
      "Role weights 0.5 and 0.5; only first metric observed at percentile 80 → coverage = 0.50; ",
      "RoleFit = 80. If coverage threshold is 0.50, score is available with Low confidence cap."
    ))
  )
}

render_league_formula <- function(sot) {
  tier_lines <- lapply(names(sot$leagues$tiers %||% list()), function(id) {
    t <- sot$leagues$tiers[[id]]
    sprintf(
      "%s: attack=%s, creation=%s, defense=%s, uncertainty=%s",
      id,
      fmt_num(t$translation_factor_attack, 2),
      fmt_num(t$translation_factor_creation, 2),
      fmt_num(t$translation_factor_defense, 2),
      fmt_num(t$uncertainty, 2)
    )
  })
  tagList(
    p(HTML(paste0(
      "Production: <code>build_features()</code> after shrinkage. Status: <strong>",
      htmltools::htmlEscape(sot$league_status), "</strong>. ",
      "Unknown league → factor 0.70, uncertainty 0.25."
    ))),
    formula_code(
      "proj_npxg_i      = shrunk_npxg_i * translation_factor_attack(league_i)",
      "proj_xa_i        = shrunk_xa_i * translation_factor_creation(league_i)",
      "proj_pressures_i = shrunk_pressures_i * translation_factor_defense(league_i)",
      "proj_gplus_i     = shrunk_gplus_i * mean(attack, creation, defense factors)"
    ),
    p(tags$strong("Current configured league factors (config/league_tiers.yml):")),
    tags$ul(lapply(tier_lines, tags$li)),
    formula_def_block(list(
      list(
        symbol = "<code>translation_factor_*</code>",
        unit = "unitless multiplier",
        range = "Typically ~0.55–1.15",
        direction = "Higher → more of source production kept on MLS scale",
        source = "config/league_tiers.yml",
        type = "Configured (assumed)",
        ref = "Source league_id",
        missing = "Unknown league → 0.70",
        config = "See list above"
      ),
      list(
        symbol = "<code>tf_uncertainty</code>",
        unit = "fraction",
        range = "0–1",
        direction = "Higher uncertainty worse (feeds Model Uncertainty)",
        source = "league_tiers.yml uncertainty",
        type = "Configured",
        ref = "Source league",
        missing = "0.25 if unknown league",
        config = "Per league in YAML"
      )
    )),
    formula_example(HTML(
      "USLC player shrunk_npxg = 0.35, attack factor = 0.78 → proj_npxg = 0.273 on MLS scale."
    ))
  )
}

render_contribution_formula <- function(sot) {
  status <- sot$contribution_status
  tagList(
    p(HTML(paste0(
      "Production: <code>score_contribution_hybrid()</code> → heuristic ",
      "<code>score_contribution_index_heuristic()</code> while status is ",
      "<strong>", htmltools::htmlEscape(status), "</strong>. ",
      "Column exposed as <code>score_projected_mls</code> / contribution index."
    ))),
    formula_code(
      "# Heuristic path (current production):",
      "raw_i = weighted_mean(observed contribution-blend percentiles for role)",
      "      - 0.15 * (penalty_blend - 50)   # only for negative-weight terms, if any",
      "minutes_factor_i = clip(minutes_i / 2500, 0.7, 1.0)",
      "Contribution_i = clip(raw_i * minutes_factor_i, 0, 100)",
      "",
      "# If no role columns resolve:",
      "# raw_i = 0.40*pct_proj_npxg + 0.35*pct_proj_xa + 0.25*pct_proj_press"
    ),
    p("Contribution blend weights come from config/model.yml contribution_by_role (mapped from tactical role)."),
    formula_def_block(list(
      list(
        symbol = "<code>Contribution_i</code>",
        unit = "index points",
        range = "0–100 or NA",
        direction = "Higher better",
        source = "Derived from role blend + minutes factor",
        type = "Estimated (heuristic); learned ridge when fitted",
        ref = "MLS / position reference percentiles on translated metrics",
        missing = "Renormalize over observed positive weights; NA if none",
        config = paste0("status = ", status)
      ),
      list(
        symbol = "<code>minutes_factor_i</code>",
        unit = "unitless",
        range = "[0.7, 1.0]",
        direction = "Higher with more minutes",
        source = "Derived from minutes / 2500",
        type = "Configured rule",
        ref = "—",
        missing = "Uses available minutes",
        config = "clip(minutes/2500, 0.7, 1)"
      ),
      list(
        symbol = "Role blend weights",
        unit = "unitless",
        range = "Sum ~1; may include negative fouling terms",
        direction = "—",
        source = "config/model.yml contribution_by_role",
        type = "Configured",
        ref = "Mapped contribution role",
        missing = "—",
        config = "See model.yml contribution_by_role"
      )
    )),
    formula_example(HTML(
      "raw = 70, minutes = 1800 → minutes_factor = clip(1800/2500,0.7,1) = 0.72 → Contribution = 50.4."
    ))
  )
}

render_value_formula <- function(sot) {
  tagList(
    p(HTML("Production: <code>score_compensation_adjusted_value()</code> in <code>R/models/component_scores.R</code>.")),
    formula_code(
      "cost_penalty_i = { tier1→20, tier2→40, tier3→60, tier4→80, tier5→95 }",
      "if cost_tier unknown or Contribution non-finite:",
      "  Value_i = NA",
      "else:",
      "  Value_i = clip(Contribution_i - cost_penalty_i + 50, 0, 100)"
    ),
    formula_def_block(list(
      list(
        symbol = "<code>Contribution_i</code>",
        unit = "index",
        range = "0–100",
        direction = "Higher better",
        source = "Estimated Near-Term Contribution",
        type = "Estimated",
        ref = "See Contribution",
        missing = "→ Value NA",
        config = "—"
      ),
      list(
        symbol = "<code>cost_tier</code>",
        unit = "integer tier",
        range = "1–5 or NA",
        direction = "Higher tier = higher pay band",
        source = "MLSPA/ASA guaranteed compensation bands",
        type = "Observed when salary known",
        ref = "USD guaranteed compensation cutoffs",
        missing = "Unknown → Value NA (never invented)",
        config = "1:&lt;$150k (non-MLS); 2:&lt;$300k; 3:&lt;$700k; 4:&lt;$1.5M; 5:≥$1.5M"
      ),
      list(
        symbol = "<code>cost_penalty_i</code>",
        unit = "index points",
        range = "{20,40,60,80,95}",
        direction = "Higher penalty hurts Value",
        source = "Hard-coded mapping in score_compensation_adjusted_value",
        type = "Configured rule",
        ref = "—",
        missing = "NA with unknown tier",
        config = "<strong>20 / 40 / 60 / 80 / 95</strong>"
      ),
      list(
        symbol = "<code>Value_i</code>",
        unit = "index points",
        range = "0–100 or NA",
        direction = "Higher better",
        source = "Derived",
        type = "Estimated",
        ref = "—",
        missing = "Not available",
        config = "—"
      )
    )),
    formula_example(HTML(
      "Contribution = 70, cost_tier = 2 → penalty = 40 → Value = clip(70 − 40 + 50, 0, 100) = 80."
    ))
  )
}

render_overall_formula <- function(sot) {
  ss <- sot$sporting
  rp <- sot$recruitment
  # Balanced atomic defaults before club nudges / renormalization
  w_contrib <- (rp$sporting %||% 0.65) * (ss$contribution %||% 0.55)
  w_role <- (rp$sporting %||% 0.65) * (ss$role_fit %||% 0.30)
  w_dev <- (rp$sporting %||% 0.65) * (ss$development %||% 0.15)
  w_style <- rp$style %||% 0.15
  w_value <- rp$compensation_value %||% 0.15
  w_path <- rp$pathway %||% 0.05
  wsum <- w_contrib + w_role + w_dev + w_style + w_value + w_path
  fw <- sot$flat_weights
  tagList(
    p(HTML(paste0(
      "Production UI path: <code>apply_decision_layer()</code> → <code>club_utility_score()</code> ",
      "in <code>R/models/decision_layer.R</code>. ",
      "Sidebar sliders are overrides/nudges; balanced defaults come from model.yml ",
      "sporting_score × recruitment_priority."
    ))),
    formula_code(
      "raw_i = w_contribution * Contribution_i",
      "      + w_role_fit * RoleFit_i",
      "      + w_development * Development_i",
      "      + w_style * ClubStyleFit_i",
      "      + w_value * Value_i",
      "      + w_pathway * PathwayFit_i",
      "      + w_feasibility * Feasibility_i   # usually 0 in balanced mode",
      "",
      "Overall_i = raw_i * (1 - λ * ModelUncertainty_i / 100)",
      "if Feasibility_i < feasibility_gate: Overall_i = min(Overall_i, 54)",
      "Overall_i = clip(Overall_i, 0, 100)",
      "",
      "# Missing components inside club_utility_score coalesce to 50 before blending."
    ),
    p(HTML(sprintf(
      "Balanced atomic weights before club nudges (then renormalized): contribution=%s, role_fit=%s, development=%s, style=%s, value=%s, pathway=%s (sum=%s).",
      fmt_num(w_contrib, 4), fmt_num(w_role, 4), fmt_num(w_dev, 4),
      fmt_num(w_style, 2), fmt_num(w_value, 2), fmt_num(w_path, 2), fmt_num(wsum, 4)
    ))),
    p(HTML(sprintf(
      "Documented flat weights in config.yml (used when an explicit weight vector is passed to apply_overall_score): Contribution %s, Role Fit %s, Value %s, Feasibility %s, Development %s.",
      fmt_num(fw$projected_mls_performance %||% 0.30, 2),
      fmt_num(fw$tactical_role_fit %||% 0.25, 2),
      fmt_num(fw$financial_value %||% 0.20, 2),
      fmt_num(fw$acquisition_feasibility %||% 0.15, 2),
      fmt_num(fw$development_upside %||% 0.10, 2)
    ))),
    formula_def_block(list(
      list(
        symbol = "<code>w_*</code>",
        unit = "unitless share",
        range = "≥ 0; renormalized to sum 1",
        direction = "Higher weight → component matters more",
        source = "config/model.yml sporting_score + recruitment_priority (+ club profile / priority template)",
        type = "Configured",
        ref = "Selected club + priority",
        missing = "—",
        config = sprintf(
          "sporting={c:%s,r:%s,d:%s}; recruitment={sporting:%s,style:%s,value:%s,pathway:%s}",
          fmt_num(ss$contribution %||% 0.55, 2), fmt_num(ss$role_fit %||% 0.30, 2),
          fmt_num(ss$development %||% 0.15, 2), fmt_num(rp$sporting %||% 0.65, 2),
          fmt_num(rp$style %||% 0.15, 2), fmt_num(rp$compensation_value %||% 0.15, 2),
          fmt_num(rp$pathway %||% 0.05, 2)
        )
      ),
      list(
        symbol = "<code>λ</code> (uncertainty_penalty)",
        unit = "unitless",
        range = "Typically 0–1",
        direction = "Higher λ → stronger Overall reduction for uncertain players",
        source = "config.yml scoring.risk_penalty_weight; decision layer default 0.15",
        type = "Configured",
        ref = "—",
        missing = "—",
        config = paste0("<strong>", fmt_num(sot$risk_lambda, 2), "</strong>")
      ),
      list(
        symbol = "<code>feasibility_gate</code>",
        unit = "index points",
        range = "0–100",
        direction = "Below gate → Overall capped at 54",
        source = "thresholds.yml / model.yml",
        type = "Configured rule",
        ref = "—",
        missing = "Uses coalesce 50 for feasibility in blend",
        config = paste0("<strong>", fmt_num(sot$feasibility_gate, 0), "</strong>")
      ),
      list(
        symbol = "<code>Overall_i</code>",
        unit = "index points",
        range = "0–100",
        direction = "Higher better",
        source = "Derived",
        type = "Estimated (policy blend)",
        ref = "Club/context dependent",
        missing = "Components coalesce to 50 in utility blend",
        config = "—"
      )
    )),
    formula_example(HTML(sprintf(
      "raw = 72, ModelUncertainty = 40, λ = %s → Overall = 72 × (1 − %s×40/100) = %s (before feasibility gate).",
      fmt_num(sot$risk_lambda, 2), fmt_num(sot$risk_lambda, 2),
      fmt_num(72 * (1 - sot$risk_lambda * 40 / 100), 1)
    )))
  )
}

render_uncertainty_penalty_formula <- function(sot) {
  tagList(
    p(HTML("Model Uncertainty production: <code>score_model_uncertainty()</code> (stored as <code>score_risk</code>).")),
    formula_code(
      "sample_u_i = 80 if minutes < 700; 55 if < 1200; 35 if < 2000; else 20",
      "translation_u_i = 100 * tf_uncertainty_i",
      "missing_u_i = clip(100 * (1 - role_metric_coverage_i), 0, 100)",
      "ModelUncertainty_i = clip(0.40*sample_u_i + 0.35*translation_u_i + 0.25*missing_u_i, 0, 100)",
      "",
      "AdjustedOverall_i = Overall_i * (1 - λ * ModelUncertainty_i / 100)"
    ),
    formula_def_block(list(
      list(
        symbol = "<code>ModelUncertainty_i</code>",
        unit = "index points",
        range = "0–100",
        direction = "Lower better",
        source = "Derived",
        type = "Estimated",
        ref = "—",
        missing = "tf_uncertainty defaults 0.15; coverage defaults 1 if absent",
        config = "weights 0.40 / 0.35 / 0.25"
      ),
      list(
        symbol = "<code>λ</code>",
        unit = "unitless",
        range = "0–1",
        direction = "Higher → stronger penalty",
        source = "config.yml risk_penalty_weight",
        type = "Configured",
        ref = "—",
        missing = "—",
        config = paste0("<strong>", fmt_num(sot$risk_lambda, 2), "</strong>")
      )
    )),
    formula_example(HTML(
      "minutes = 800 → sample_u = 55; tf_uncertainty = 0.15 → translation_u = 15; coverage = 0.80 → missing_u = 20; ",
      "ModelUncertainty = 0.40×55 + 0.35×15 + 0.25×20 = 32.25."
    ))
  )
}

render_development_formula <- function(sot) {
  peaks <- sot$age_peaks
  peak_nums <- peaks[vapply(peaks, function(x) is.numeric(x) || (is.character(x) && grepl("^[0-9.]+$", x)), logical(1))]
  peak_txt <- if (length(peak_nums)) {
    paste(vapply(names(peak_nums), function(nm) {
      sprintf("%s=%s", nm, fmt_num(peak_nums[[nm]], 1))
    }, character(1)), collapse = "; ")
  } else "default 26.5"
  tagList(
    p(HTML("Production: <code>score_development()</code> in <code>R/models/component_scores.R</code>.")),
    formula_code(
      "peak = age_peak_for_position(position_group)   # from model.yml age_curves",
      "age_curve = 100 * exp(-0.5 * ((age - (peak - 3)) / 4.5)^2)",
      "age_ups = clip(age_curve - min(45, max(0, age - peak) * 6), 5, 95)",
      "Development_i = weighted_mean of available bits:",
      "  age_ups (weight 0.55)",
      "  + scale_0_100(yoy_delta) (0.25) if finite",
      "  + scale_0_100(minutes) (0.20) if finite"
    ),
    formula_def_block(list(
      list(
        symbol = "<code>peak</code>",
        unit = "years of age",
        range = "~25–29 by position",
        direction = "—",
        source = "config/model.yml age_curves",
        type = "Configured (learnable if artifact)",
        ref = "position_group",
        missing = "default 26.5",
        config = htmltools::htmlEscape(peak_txt)
      ),
      list(
        symbol = "<code>Development_i</code>",
        unit = "index points",
        range = "0–100",
        direction = "Higher better",
        source = "Derived",
        type = "Estimated",
        ref = "Position age curve",
        missing = "Drop YoY/minutes terms if NA; age term always used",
        config = "weights 0.55 / 0.25 / 0.20"
      )
    )),
    formula_example(HTML(
      "Forward peak = 25.5, age = 22 → age sits near the rising side of the continuous curve; ",
      "Development blends that age_ups with YoY and minutes when available."
    ))
  )
}

render_feasibility_formula <- function(sot) {
  tagList(
    p(HTML("Production: <code>score_feasibility()</code> heuristic in <code>R/models/component_scores.R</code>.")),
    formula_code(
      "Feasibility_i = 0.35*cost_score + 0.30*league_score + 0.20*minutes_score + 0.15*domestic_score",
      "cost_score: NA→45; tier1→95; 2→80; 3→55; 4→30; else→10",
      "league_score: uslc→85; mlsnp→80; mls→60; else→50",
      "minutes_score: MLS & minutes_share > 0.75 → 45; else 75",
      "domestic_score: domestic/homegrown→80; international→55; else→60"
    ),
    formula_def_block(list(
      list(
        symbol = "<code>Feasibility_i</code>",
        unit = "index points",
        range = "0–100",
        direction = "Higher better (more plausible pathway)",
        source = "Derived public-data rule",
        type = "Estimated (configured rule)",
        ref = "—",
        missing = "Neutral stand-ins in sub-scores (e.g. cost NA→45)",
        config = paste0("gate low_threshold = <strong>", fmt_num(sot$feasibility_gate, 0), "</strong>")
      )
    )),
    formula_example(HTML(
      "cost_tier=2 → 80; league=uslc → 85; minutes_score=75; domestic=60 → ",
      "Feasibility = 0.35×80 + 0.30×85 + 0.20×75 + 0.15×60 = 77.5."
    ))
  )
}

render_need_formula <- function(sot) {
  nw <- sot$need_weights
  tagList(
    p(HTML("Production: <code>analyze_club_role_needs()</code> in <code>R/models/roster_analysis.R</code>.")),
    formula_code(
      "gap_component = 100 - percentile_among_clubs(raw_component)",
      "NeedScore = clip( Σ w_k * gap_k , 0, 100)",
      "",
      "starter_quality = proj * (0.6 + 0.4 * role_fit/100)",
      "player depth weight mixes role_fit, contribution, sample reliability, availability,",
      "minutes capacity, flexibility (see player_role_depth_weight)"
    ),
    p(HTML(sprintf(
      "Current weights (config.yml roster_needs.weights): starter_quality_gap=%s, effective_depth_gap=%s, succession_risk=%s, availability_risk=%s, tactical_coverage_gap=%s, financial_efficiency_opportunity=%s.",
      fmt_num(nw$starter_quality_gap %||% 0.30, 2),
      fmt_num(nw$effective_depth_gap %||% 0.25, 2),
      fmt_num(nw$succession_risk %||% 0.15, 2),
      fmt_num(nw$availability_risk %||% 0.10, 2),
      fmt_num(nw$tactical_coverage_gap %||% 0.10, 2),
      fmt_num(nw$financial_efficiency_opportunity %||% 0.10, 2)
    ))),
    formula_def_block(list(
      list(
        symbol = "<code>NeedScore</code>",
        unit = "index points",
        range = "0–100",
        direction = "Higher = greater estimated need",
        source = "Derived",
        type = "Estimated",
        ref = "All clubs’ slot distributions for same tactical role",
        missing = "Uses available roster public data; incomplete roster → lower confidence",
        config = "weights above"
      ),
      list(
        symbol = "Blend prior_strength for depth sample_rel",
        unit = "minutes",
        range = "Positive",
        direction = "—",
        source = "config.yml project.blend.prior_strength_minutes",
        type = "Configured",
        ref = "—",
        missing = "—",
        config = paste0("<strong>", fmt_num(sot$blend_m0, 0), "</strong>")
      )
    )),
    formula_example(HTML(
      "If all six gaps are 50 and weights sum to 1 → NeedScore = 50."
    ))
  )
}

render_comparison_formula <- function(sot) {
  inv <- sot$rules$invariants %||% sot$spec$invariants %||% list()
  tagList(
    p(HTML("Production: <code>compare_incumbent_target()</code> in <code>R/models/roster_comparison.R</code>.")),
    formula_code(
      "RoleSimilarity = 50 + 50 * cosine_similarity(role_pct vectors centered at 50)",
      "  (requires sim_coverage ≥ 0.50; jointly missing metrics excluded)",
      "UpgradePotential = clip(50 + 0.7*ΔContribution + 0.3*ΔRoleFit, 0, 100)",
      "FinancialEfficiency uses known cost tiers only; more expensive cannot be lower-cost",
      "Succession mixes Development, ΔDevelopment, age bonus, Feasibility"
    ),
    formula_def_block(list(
      list(
        symbol = "<code>RoleSimilarity</code>",
        unit = "index points",
        range = "0–100 or NA",
        direction = "Higher = more similar profiles",
        source = "Derived cosine on role percentiles",
        type = "Estimated",
        ref = "Selected role metric set",
        missing = "NA if coverage &lt; 0.50",
        config = "coverage floor 0.50"
      ),
      list(
        symbol = "<code>UpgradePotential</code>",
        unit = "index points",
        range = "0–100",
        direction = "Higher = more upgrade vs incumbent",
        source = "ΔContribution, ΔRoleFit only",
        type = "Estimated",
        ref = "Incumbent vs target",
        missing = "Uses available deltas",
        config = "0.7 / 0.3 blend"
      ),
      list(
        symbol = "Immediate upgrade ΔContribution margin",
        unit = "index points",
        range = "—",
        direction = "—",
        source = "recommendation_rules.yml / model.yml invariants",
        type = "Configured rule",
        ref = "—",
        missing = "Blocks label if unmet",
        config = paste0("<strong>", fmt_num(inv$immediate_upgrade$min_contribution_margin %||% sot$spec$invariants$immediate_upgrade$min_contribution_margin %||% 8, 0), "</strong>")
      )
    )),
    formula_example(HTML(
      "ΔContribution = +10, ΔRoleFit = +4 → UpgradePotential = clip(50 + 0.7×10 + 0.3×4) = 58.2."
    ))
  )
}

#' Comprehensive variable glossary rows (for the glossary table).
variable_glossary_rows <- function(sot) {
  list(
    list("minutes_i", "Player minutes in evaluation window", "minutes", "≥0", "More → more reliable", "ASA/processed", "Observed", "Player-season", "NA propagates", "—"),
    list("prior_strength / m₀ (rates)", "Minutes controlling trust in own rate", "minutes", ">0", "Higher → more prior", "model.yml default_m0", "Configured", "—", "Fallback 600", paste0(fmt_num(sot$default_m0, 0), " for all shrink metrics")),
    list("prior_strength (blend)", "Minutes in 2026 YTD vs 2025 blend", "minutes", ">0", "Higher → slower shift to YTD", "config.yml blend.prior_strength_minutes", "Configured", "—", "—", fmt_num(sot$blend_m0, 0)),
    list("observed_rate_i", "Raw per-90 metric", "metric units", "metric-specific", "Usually higher better", "ASA", "Observed", "Player", "NA kept", "—"),
    list("prior_rate", "Group mean rate", "metric units", "metric-specific", "Same as metric", "Computed", "Estimated", "league×position", "NA→NA adjusted", "—"),
    list("reliability_weight_i", "Share on observed rate", "fraction", "[0,1]", "Higher with minutes", "Derived", "Derived", "—", "—", "sums to 1 with prior_weight"),
    list("prior_weight_i", "Share on prior rate", "fraction", "[0,1]", "Higher with m₀", "Derived", "Derived", "—", "—", "1 − reliability_weight"),
    list("adjusted_rate_i", "EB-shrunk rate", "metric units", "metric-specific", "Same as metric", "Derived", "Estimated", "league×position", "NA if inputs NA", "—"),
    list("translation_factor_*", "League→MLS multiplier", "unitless", "~0.55–1.15", "Higher keeps more production", "league_tiers.yml", "Configured", "league_id", "Unknown→0.70", "Per league YAML"),
    list("tf_uncertainty", "League translation uncertainty", "fraction", "0–1", "Higher worse", "league_tiers.yml", "Configured", "league_id", "Unknown→0.25", "Per league"),
    list("percentile_j / pct_*", "Reference percentile", "points", "0–100", "Higher better", "Feature build", "Estimated", "position / MLS position", "NA never→50", "—"),
    list("w_j (role)", "Role Fit metric weight", "unitless", "≥0 sum~1", "Higher→more influence", "roles.yml", "Configured", "tactical role", "—", "Per role"),
    list("coverage_i", "Role metric coverage", "fraction", "0–1", "Higher better", "Derived", "Derived", "role metrics", "0 if none", paste0("unavailable <", fmt_num(sot$coverage$role_fit_unavailable_below %||% 0.5, 2))),
    list("RoleFit_i", "Tactical Role Fit", "points", "0–100 or NA", "Higher better", "score_role_fit_with_coverage", "Estimated", "selected role", "NA if low coverage", "thresholds.yml"),
    list("Contribution_i", "Estimated Near-Term Contribution", "points", "0–100 or NA", "Higher better", "contribution heuristic/hybrid", "Estimated", "MLS/position refs", "NA if no blend inputs", sot$contribution_status),
    list("minutes_factor_i", "Contribution minutes damper", "unitless", "[0.7,1]", "Higher with minutes", "clip(minutes/2500,…)", "Configured rule", "—", "—", "0.7–1.0"),
    list("cost_tier", "Guaranteed pay band", "tier", "1–5 or NA", "Higher=more expensive", "MLSPA/ASA", "Observed", "USD bands", "Unknown allowed", "See cost tier table"),
    list("cost_penalty_i", "Value penalty by tier", "points", "20/40/60/80/95", "Higher hurts Value", "component_scores.R", "Configured rule", "—", "NA with unknown tier", "20/40/60/80/95"),
    list("Value_i", "Compensation-Adjusted Value", "points", "0–100 or NA", "Higher better", "score_compensation_adjusted_value", "Estimated", "—", "NA if pay unknown", "—"),
    list("Feasibility_i", "Acquisition Feasibility", "points", "0–100", "Higher better", "score_feasibility", "Estimated rule", "—", "Neutral sub-scores", paste0("gate ", fmt_num(sot$feasibility_gate, 0))),
    list("Development_i", "Development Upside", "points", "0–100", "Higher better", "score_development", "Estimated", "position age curve", "Drop NA YoY/minutes", "age_curves in model.yml"),
    list("ModelUncertainty_i", "Model Uncertainty (score_risk)", "points", "0–100", "Lower better", "score_model_uncertainty", "Estimated", "—", "defaults for tf/coverage", "0.40/0.35/0.25 mix"),
    list("λ", "Uncertainty penalty strength", "unitless", "0–1", "Higher→stronger cut", "config.yml risk_penalty_weight", "Configured", "—", "—", fmt_num(sot$risk_lambda, 2)),
    list("Overall_i", "Overall recruitment score", "points", "0–100", "Higher better", "club_utility_score", "Estimated policy", "club context", "components→50 in blend", "model.yml weights"),
    list("NeedScore", "Roster need score", "points", "0–100", "Higher=more need", "analyze_club_role_needs", "Estimated", "clubs×role distribution", "incomplete roster caution", "config.yml roster_needs.weights"),
    list("RoleSimilarity", "Incumbent–target similarity", "points", "0–100 or NA", "Higher=more similar", "cosine on role pcts", "Estimated", "role metrics", "NA if coverage<0.5", "0.50 floor"),
    list("UpgradePotential", "Sporting upgrade vs incumbent", "points", "0–100", "Higher better", "0.7ΔC+0.3ΔR", "Estimated", "pair", "—", "0.7/0.3"),
    list("blend_weight_2026", "YTD share in blended period", "fraction", paste0("[", fmt_num(sot$blend_min_ytd, 2), ",", fmt_num(sot$blend_max_ytd, 2), "]"), "Higher→more YTD", "evaluation_periods.R", "Derived", "—", "—", paste0("m0=", fmt_num(sot$blend_m0, 0)))
  )
}

render_variable_glossary <- function(sot) {
  rows <- variable_glossary_rows(sot)
  tags$table(
    class = "about-table formula-def-table",
    tags$thead(tags$tr(
      tags$th("Variable"),
      tags$th("Meaning"),
      tags$th("Unit"),
      tags$th("Range"),
      tags$th("Higher/lower"),
      tags$th("Source"),
      tags$th("Type"),
      tags$th("Reference population"),
      tags$th("Missing"),
      tags$th("Current config")
    )),
    tags$tbody(lapply(rows, function(r) {
      tags$tr(lapply(r, function(cell) tags$td(HTML(as.character(cell)))))
    }))
  )
}

#' Technical methodology section driven entirely from production SoT.
render_technical_methodology <- function(cfg = NULL, spec = NULL) {
  sot <- load_formula_sot(cfg, spec)
  div(
    h2("Technical methodology"),
    p(HTML(paste0(
      "Equations below are the <strong>production formulas</strong> from the scoring code and ",
      "<code>config/model.yml</code>, <code>config/config.yml</code>, <code>config/thresholds.yml</code>, ",
      "<code>config/league_tiers.yml</code>, and <code>config/roles.yml</code>. ",
      "Configuration values are loaded at render time — not copied by hand."
    ))),
    p(HTML(sprintf(
      "Model version <strong>%s</strong> · status <strong>%s</strong> · contribution path <strong>%s</strong> · shrinkage <strong>%s</strong> · league factors <strong>%s</strong>.",
      htmltools::htmlEscape(sot$spec$model_version %||% "?"),
      htmltools::htmlEscape(sot$spec$status %||% "?"),
      htmltools::htmlEscape(sot$contribution_status),
      htmltools::htmlEscape(sot$shrinkage_status),
      htmltools::htmlEscape(sot$league_status)
    ))),

    about_accordion("Sample size and reliability (production shrinkage)", render_shrinkage_formula(sot)),
    about_accordion("Role Fit and metric coverage", render_role_fit_formula(sot)),
    about_accordion("League adjustment", render_league_formula(sot)),
    about_accordion("Estimated Near-Term Contribution", render_contribution_formula(sot)),
    about_accordion("Compensation-Adjusted Value", render_value_formula(sot)),
    about_accordion("Overall score", render_overall_formula(sot)),
    about_accordion("Model Uncertainty and uncertainty penalty", render_uncertainty_penalty_formula(sot)),
    about_accordion("Development Upside", render_development_formula(sot)),
    about_accordion("Acquisition Feasibility", render_feasibility_formula(sot)),
    about_accordion("Roster Need Score", render_need_formula(sot)),
    about_accordion("Player similarity and comparison rules", render_comparison_formula(sot)),

    h3("Formula and Variable Glossary"),
    p("Every variable used by the production formulas above:"),
    div(style = "overflow-x:auto;", render_variable_glossary(sot)),

    about_accordion(
      "Evaluation-period blend (related reliability)",
      formula_code(
        "blend_weight_2026 = clamp(min_ytd_share, max_ytd_share,",
        "                          minutes_2026 / (minutes_2026 + blend_prior_strength))",
        "blend_weight_2025 = 1 - blend_weight_2026"
      ),
      p(HTML(sprintf(
        "Current values: blend_prior_strength = <strong>%s</strong> minutes; min_ytd_share = <strong>%s</strong>; max_ytd_share = <strong>%s</strong> (config.yml).",
        fmt_num(sot$blend_m0, 0), fmt_num(sot$blend_min_ytd, 2), fmt_num(sot$blend_max_ytd, 2)
      ))),
      formula_example(HTML(sprintf(
        "minutes_2026 = 600 → raw share = 600/(600+%s) = %s → clamped to [%s, %s].",
        fmt_num(sot$blend_m0, 0),
        fmt_num(600 / (600 + sot$blend_m0), 3),
        fmt_num(sot$blend_min_ytd, 2),
        fmt_num(sot$blend_max_ytd, 2)
      )))
    ),

    about_accordion(
      "Recommendation score thresholds (config)",
      {
        rt <- sot$thresholds$recommendation_score_thresholds %||% list()
        tags$ul(
          tags$li(HTML(sprintf(
            "<strong>Priority Review</strong> — Overall ≥ %s, Model Uncertainty ≤ %s, Feasibility ≥ %s",
            fmt_num(rt$priority_review$min_overall %||% 70, 0),
            fmt_num(rt$priority_review$max_model_uncertainty %||% 45, 0),
            fmt_num(rt$priority_review$min_feasibility %||% 55, 0)
          ))),
          tags$li(HTML(sprintf(
            "<strong>Development Watch</strong> — Development ≥ %s, Overall ≥ %s, Feasibility ≥ %s",
            fmt_num(rt$development_watch$min_development %||% 70, 0),
            fmt_num(rt$development_watch$min_overall %||% 55, 0),
            fmt_num(rt$development_watch$min_feasibility %||% 50, 0)
          ))),
          tags$li(HTML(sprintf(
            "<strong>Monitor</strong> — Overall ≥ %s, Model Uncertainty ≤ %s",
            fmt_num(rt$monitor$min_overall %||% 55, 0),
            fmt_num(rt$monitor$max_model_uncertainty %||% 65, 0)
          ))),
          tags$li(HTML(sprintf(
            "<strong>Low Priority</strong> — Feasibility &lt; %s (auto) or no stronger category",
            fmt_num(rt$low_priority$max_feasibility_for_auto_low %||% 30, 0)
          )))
        )
      }
    ),

    about_accordion(
      "Validation status",
      p(HTML(sprintf(
        "Model version <strong>%s</strong>. Estimated Near-Term Contribution status: <strong>%s</strong>.",
        htmltools::htmlEscape(sot$spec$model_version %||% "?"),
        htmltools::htmlEscape(sot$spec$status %||% "unvalidated_heuristic")
      ))),
      p("Until historical backtesting is complete, treat outputs as directional — not validated predictions.")
    )
  )
}
