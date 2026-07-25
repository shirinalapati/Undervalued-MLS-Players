# About & Methodology — 2026 MLS Value Index

render_about_value_index <- function(provenance, vi_cfg, n_official, n_players) {
  perf <- provenance$performance_through %||%
    provenance$performance_data_through %||%
    provenance$data_cutoff_utc %||% "unknown"
  # Prefer a readable date when we have YYYY-MM-DD
  if (grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", as.character(perf))) {
    perf_disp <- tryCatch(
      format(as.Date(perf), "%B %d, %Y"),
      error = function(e) as.character(perf)
    )
  } else if (!is.null(provenance$data_cutoff_utc)) {
    perf_disp <- tryCatch({
      t <- as.POSIXct(provenance$data_cutoff_utc, tz = "UTC")
      format(t, "%B %d, %Y, %H:%M UTC")
    }, error = function(e) as.character(provenance$data_cutoff_utc))
  } else {
    perf_disp <- as.character(perf)
  }

  mlspa_raw <- provenance$mlspa_compensation_as_of %||%
    provenance$salary_as_of %||% "unknown"
  mlspa_disp <- tryCatch({
    if (grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", as.character(mlspa_raw))) {
      format(as.Date(substr(as.character(mlspa_raw), 1, 10)), "%B %d, %Y")
    } else {
      as.character(mlspa_raw)
    }
  }, error = function(e) as.character(mlspa_raw))

  question <- value_index_research_question(vi_cfg)

  tagList(
    div(
      class = "section-card",
      h2("2026 MLS Value Index"),
      h3("Research question"),
      tags$blockquote(question),
      p("The 2026 MLS Value Index identifies players whose position-adjusted on-ball impact ranks meaningfully higher than their compensation among MLS players at the same position."),
      p("The production model measures position-adjusted on-ball impact through Goals Added only. It does not estimate adjusted team impact, off-ball contribution, or plus-minus."),
      p("This is a current-value analysis. It does not attempt to estimate transfer fees, trade value, future contracts, or whether a player is available to move."),
      tags$ul(
        tags$li(HTML(paste0("<strong>Players evaluated:</strong> ", format(n_players, big.mark = ",")))),
        tags$li(HTML(paste0("<strong>Eligible for the official ranking:</strong> ", format(n_official, big.mark = ","))))
      )
    ),

    div(
      class = "section-card",
      h3("Data sources and cutoff dates"),
      h4("Performance data"),
      tags$ul(
        tags$li("American Soccer Analysis Goals Added"),
        tags$li("Related public performance metrics"),
        tags$li(HTML(paste0(
          "<strong>Performance data through:</strong> ", perf_disp,
          " <em>(updates every day)</em>"
        )))
      ),
      h4("Compensation data"),
      tags$ul(
        tags$li("2026 MLSPA annualized guaranteed compensation"),
        tags$li("Accessed through American Soccer Analysis"),
        tags$li(HTML(paste0("<strong>Compensation data as of:</strong> ", mlspa_disp)))
      ),
      p("The production rankings include current MLS players only."),
      p("The following player pools are excluded:"),
      tags$ul(
        tags$li("MLS NEXT Pro"),
        tags$li("USL Championship"),
        tags$li("NCAA"),
        tags$li("International leagues"),
        tags$li("Synthetic or demonstration players")
      ),
      p("Guaranteed compensation is not the same as:"),
      tags$ul(
        tags$li("MLS Salary Budget Charge"),
        tags$li("Transfer fee"),
        tags$li("Trade cost"),
        tags$li("General Allocation Money value"),
        tags$li("Total acquisition cost"),
        tags$li("Total contract value")
      )
    ),

    div(
      class = "section-card",
      h3("How the index works"),
      p("There is not one giant equation that does everything at once. The model is a short pipeline. Steps 1–3 build the inputs; step 4 is the final ranking formula."),
      tags$ol(
        tags$li("Estimate each player’s position-adjusted sporting impact."),
        tags$li("Compare the player’s compensation with others at the same position."),
        tags$li("Calculate the gap between performance and compensation (Value Surplus)."),
        tags$li(HTML(paste0(
          "<strong>Final ranking formula — Undervaluation Score:</strong> ",
          "rank that Value Surplus against eligible players in the same position group. ",
          "This is the score used to order the official rankings."
        )))
      ),
      tags$pre(
        class = "formula-code",
        paste(
          "Value Surplus = Sporting Impact − Compensation Percentile",
          "Undervaluation Score = position-group percentile of Value Surplus",
          "                       among officially eligible players",
          sep = "\n"
        )
      )
    ),

    div(
      class = "section-card",
      h3("1. Position-Adjusted Sporting Impact"),
      h4("What it measures"),
      p("Position-Adjusted Sporting Impact measures how strongly a player has performed relative to MLS players in the same position group."),
      p("The primary performance measure is:"),
      tags$blockquote("Reliability-adjusted, blended total Goals Added per 96 minutes"),
      p("Total Goals Added combines the estimated value of a player’s on-ball actions in a common unit."),
      p("Its components include:"),
      tags$ul(
        tags$li("Shooting"),
        tags$li("Passing"),
        tags$li("Receiving"),
        tags$li("Dribbling"),
        tags$li("Interrupting"),
        tags$li("Fouling")
      ),
      p("The individual components are displayed on Player Profile pages to explain how each player creates value."),
      p("They are not separately reweighted inside the primary Sporting Impact score because total Goals Added already combines them."),
      h4("Calculation"),
      p("First, the player’s Goals Added rate is adjusted for sample size:"),
      tags$pre(class = "formula-code", "Adjusted G+/96 = reliability-adjusted player G+/96"),
      p("The adjusted rate is then converted into a percentile among MLS players in the same position group:"),
      tags$pre(class = "formula-code", "Sporting Impact = position-group percentile of Adjusted G+/96"),
      h4("Interpretation"),
      tags$ul(
        tags$li("Sporting Impact of 90 means the player’s adjusted performance is higher than approximately 90% of eligible players at the same position."),
        tags$li("Sporting Impact of 50 is near the positional median."),
        tags$li("Sporting Impact of 20 indicates below-median performance within the position group.")
      ),
    ),

    div(
      class = "section-card",
      h3("2. Sample-size adjustment"),
      p("A player can produce unusually strong or weak numbers over a small number of minutes."),
      p("The model therefore moves small-sample performance toward the typical rate for MLS players at the same position."),
      h4("Reliability adjustment"),
      tags$pre(
        class = "formula-code",
        paste(
          "Reliability Weight = Player Minutes / (Player Minutes + 600)",
          "Prior Weight = 1 − Reliability Weight",
          "Adjusted Rate = Reliability Weight × Observed Rate + Prior Weight × Position Prior",
          sep = "\n"
        )
      ),
      p("Where:"),
      tags$ul(
        tags$li("Observed Rate is the player’s actual G+/96."),
        tags$li("Position Prior is the typical G+/96 for the player’s MLS position group."),
        tags$li("600 minutes is the current prior-strength setting.")
      ),
      p("More minutes cause the model to trust the player’s own performance more."),
      p("Fewer minutes cause the estimate to remain closer to the positional average."),
      p("The current 600-minute prior is a configured assumption and has not yet been learned separately for each metric.")
    ),

    div(
      class = "section-card",
      h3("3. 2025+2026 season to date"),
      p("The default evaluation combines:"),
      tags$ul(
        tags$li("2026 season-to-date performance"),
        tags$li("2025 full-season performance")
      ),
      p("This prevents a short stretch of 2026 performance from completely replacing a larger 2025 sample."),
      h4("2026 weight"),
      tags$pre(
        class = "formula-code",
        "w2026 = clamp(2026 Minutes / (2026 Minutes + 700), 0.15, 0.90)"
      ),
      h4("Blended rate"),
      tags$pre(
        class = "formula-code",
        "Blended G+/96 = w2026 × Adjusted 2026 G+/96 + (1 − w2026) × Adjusted 2025 G+/96"
      ),
      h4("Interpretation"),
      tags$ul(
        tags$li("Players with more 2026 minutes receive more weight from 2026."),
        tags$li("Players with fewer 2026 minutes retain more influence from 2025.")
      ),
      p(HTML(paste0(
        "<strong>What 15% and 90% mean:</strong> ",
        "After the weight is calculated from minutes, it is limited to a minimum of 15% and a maximum of 90%."
      ))),
      tags$ul(
        tags$li(HTML("<strong>15% floor:</strong> Even with very few 2026 minutes, at least 15% of the blended rate still comes from 2026 (so the result is never pure 2025 when both seasons exist).")),
        tags$li(HTML("<strong>90% ceiling:</strong> Even with a large 2026 sample, at least 10% of the blended rate still comes from 2025 (so a short run of 2026 form cannot fully erase the prior season)."))
      ),
      p("Players without valid 2025 data are evaluated using their reliability-adjusted 2026 performance only and receive lower Data Confidence.")
    ),

    div(
      class = "section-card",
      h3("4. Compensation Percentile"),
      p("Compensation Percentile compares a player’s 2026 guaranteed compensation with MLS players in the same position group."),
      tags$pre(class = "formula-code", "Compensation Percentile = position-group percentile of guaranteed compensation"),
      p("Higher means more expensive."),
      h4("Interpretation"),
      tags$ul(
        tags$li("Compensation Percentile of 80 means the player earns more than approximately 80% of eligible players at the same position."),
        tags$li("Compensation Percentile of 25 means the player earns more than approximately 25% of positional peers.")
      ),
      p("Position-specific percentiles are used because compensation patterns differ substantially by position."),
      p("The Player Profile also displays the player’s league-wide compensation percentile for additional context.")
    ),

    div(
      class = "section-card",
      h3("5. Value Surplus"),
      p("Value Surplus measures the difference between the player’s performance standing and compensation standing."),
      tags$pre(class = "formula-code", "Value Surplus = Sporting Impact − Compensation Percentile"),
      h4("Example"),
      p("A player has:"),
      tags$ul(
        tags$li("Sporting Impact: 84"),
        tags$li("Compensation Percentile: 32")
      ),
      tags$pre(class = "formula-code", "84 − 32 = 52"),
      p("The player’s modeled sporting performance ranks 52 percentile points higher than their compensation."),
      h4("Interpretation"),
      tags$ul(
        tags$li("Positive Value Surplus: performance standing exceeds compensation standing."),
        tags$li("Near zero: performance and compensation are broadly aligned."),
        tags$li("Negative Value Surplus: compensation standing exceeds current modeled performance.")
      ),
      p("Value Surplus is a percentile-point difference."),
      p("It is not expressed in dollars.")
    ),

    div(
      class = "section-card",
      h3("6. Undervaluation Score (final ranking formula)"),
      p("This is the final ranking score. Earlier steps (sample-size adjustment, 2025+2026 blending, Sporting Impact, and Compensation Percentile) only exist to compute Value Surplus cleanly. They are not separate competing rankings."),
      p("The Undervaluation Score ranks each player’s Value Surplus against eligible players in the same position group."),
      tags$pre(
        class = "formula-code",
        paste(
          "Value Surplus = Sporting Impact − Compensation Percentile",
          "Undervaluation Score = position-group percentile of Value Surplus",
          "                       among officially eligible players at that position",
          sep = "\n"
        )
      ),
      h4("Interpretation"),
      tags$ul(
        tags$li("Score of 90 means the player has a better performance-versus-compensation gap than approximately 90% of eligible positional peers."),
        tags$li("Score of 50 is near the positional median."),
        tags$li("Score of 10 indicates relatively poor compensation efficiency under the current model.")
      ),
      p("The score is position-adjusted. A striker and a center back are evaluated against their own positional peers rather than through one universal raw-performance scale.")
    ),

    div(
      class = "section-card",
      h3("Official ranking eligibility"),
      p("A player must meet all of the following requirements:"),
      tags$ul(
        tags$li("Currently associated with an MLS first-team club"),
        tags$li("Valid position group"),
        tags$li("Known 2026 guaranteed compensation"),
        tags$li("Available Sporting Impact score"),
        tags$li("At least 450 minutes during the 2026 season"),
        tags$li("No synthetic or demonstration data")
      ),
      p("Players with Sporting Impact below 55 are not labeled Undervalued, even when their compensation is low."),
      p("This prevents low-performing, inexpensive players from ranking highly solely because they are inexpensive."),
      p("Players below 450 minutes may appear in a separate:"),
      tags$blockquote("Small-Sample Watchlist"),
      p("They do not receive an official undervaluation ranking.")
    ),

    div(
      class = "section-card",
      h3("Value labels"),
      p("Labels summarize compensation efficiency under the current public-data model. No player with Value Surplus ≤ 0 can be labeled Undervalued, Strong Value, or Elite Value."),
      h4("Elite Value"),
      tags$ul(
        tags$li("Value Surplus ≥ 25"),
        tags$li("Undervaluation Score ≥ 90"),
        tags$li("Sporting Impact ≥ 70"),
        tags$li("Data Confidence Medium or High")
      ),
      h4("Strong Value"),
      tags$ul(
        tags$li("Value Surplus ≥ 15"),
        tags$li("Undervaluation Score ≥ 75"),
        tags$li("Sporting Impact ≥ 60"),
        tags$li("Data Confidence Medium or High")
      ),
      h4("Undervalued"),
      tags$ul(
        tags$li("Value Surplus ≥ 5"),
        tags$li("Undervaluation Score ≥ 60"),
        tags$li("Sporting Impact ≥ 55")
      ),
      h4("Fair Value"),
      tags$ul(
        tags$li("Value Surplus > −15 and Value Surplus < 5, or"),
        tags$li("the player fails an impact / confidence floor required for a higher label")
      ),
      h4("Below Expected Value by Current Model"),
      tags$ul(
        tags$li("Value Surplus ≤ −15")
      ),
      p("This does not mean the contract is objectively poor. Public Goals Added cannot measure every tactical, leadership, commercial, or organizational contribution."),
      h4("Small-Sample Watchlist"),
      p("The player has potentially interesting value signals but does not meet the official minutes threshold."),
      h4("Insufficient Evidence"),
      p("Used when:"),
      tags$ul(
        tags$li("Compensation is unavailable"),
        tags$li("Sporting Impact is unavailable"),
        tags$li("Position is uncertain"),
        tags$li("Required performance data is missing"),
        tags$li("The player is not eligible for the production ranking")
      )
    ),

    div(
      class = "section-card",
      h3("Data Confidence"),
      p("Data Confidence summarizes how much evidence supports the evaluation."),
      p("It considers:"),
      tags$ul(
        tags$li("Minutes played"),
        tags$li("Availability of 2025 and 2026 performance"),
        tags$li("Position certainty"),
        tags$li("Performance-data completeness"),
        tags$li("Compensation availability")
      ),
      p("Possible labels:"),
      tags$ul(
        tags$li("High"),
        tags$li("Medium"),
        tags$li("Low"),
        tags$li("Insufficient")
      ),
      p("Data Confidence does not measure player quality."),
      p("It measures confidence in the available evaluation.")
    ),

    div(
      class = "section-card",
      h3("Metric dictionary"),
      tags$table(
        class = "about-table",
        style = "width:100%; border-collapse:collapse;",
        tags$thead(
          tags$tr(
            tags$th("Display name"),
            tags$th("Internal field"),
            tags$th("Purpose"),
            tags$th("Missing-data behavior")
          )
        ),
        tags$tbody(
          tags$tr(
            tags$td("Position-Adjusted Sporting Impact"),
            tags$td("sporting_impact"),
            tags$td("Primary performance score"),
            tags$td("Unavailable when total G+ is missing")
          ),
          tags$tr(
            tags$td("Compensation Percentile"),
            tags$td("compensation_percentile"),
            tags$td("Position-relative compensation standing"),
            tags$td("No official ranking when unavailable")
          ),
          tags$tr(
            tags$td("Value Surplus"),
            tags$td("value_surplus"),
            tags$td("Performance percentile minus compensation percentile"),
            tags$td("Unavailable when either input is missing")
          ),
          tags$tr(
            tags$td("Undervaluation Score"),
            tags$td("undervaluation_score"),
            tags$td("Position-relative ranking of Value Surplus"),
            tags$td("Calculated only for officially eligible players")
          ),
          tags$tr(
            tags$td("Shooting G+/96"),
            tags$td("goals_added_shooting_p96"),
            tags$td("Explanatory attacking contribution (per 96 minutes)"),
            tags$td("Remains missing")
          ),
          tags$tr(
            tags$td("Passing G+/96"),
            tags$td("goals_added_passing_p96"),
            tags$td("Explanatory passing contribution (per 96 minutes)"),
            tags$td("Remains missing")
          ),
          tags$tr(
            tags$td("Receiving G+/96"),
            tags$td("goals_added_receiving_p96"),
            tags$td("Explanatory receiving contribution (per 96 minutes)"),
            tags$td("Remains missing")
          ),
          tags$tr(
            tags$td("Dribbling G+/96"),
            tags$td("goals_added_dribbling_p96"),
            tags$td("Explanatory ball-carrying contribution (per 96 minutes)"),
            tags$td("Remains missing")
          ),
          tags$tr(
            tags$td("Interrupting G+/96"),
            tags$td("goals_added_defending_p96"),
            tags$td("Explanatory defensive contribution (per 96 minutes)"),
            tags$td("Remains missing")
          ),
          tags$tr(
            tags$td("Fouling G+/96"),
            tags$td("goals_added_fouling_p96"),
            tags$td("Explanatory value from fouling actions (per 96 minutes)"),
            tags$td("Remains missing")
          ),
          tags$tr(
            tags$td("Model Confidence"),
            tags$td("model_confidence"),
            tags$td("100 − Model Uncertainty; higher means more stable evidence"),
            tags$td("Still reported when scores exist")
          )
        )
      ),
      p("All visible Goals Added rates use per-96-minute units. Missing metrics are never replaced with artificial average values.")
    ),

    div(
      class = "section-card",
      h3("Goalkeepers"),
      p("Goalkeepers are excluded."),
      p("The currently available public metrics do not support a goalkeeper valuation model that is sufficiently comparable with the outfield methodology."),
      p("A separate goalkeeper-specific model may be developed later using appropriate goalkeeping measures.")
    ),

    div(
      class = "section-card",
      h3("Important limitations"),
      p("The 2026 MLS Value Index is not a model of:"),
      tags$ul(
        tags$li("Transfer fees"),
        tags$li("MLS trade value"),
        tags$li("General Allocation Money"),
        tags$li("Salary Budget Charge"),
        tags$li("Total acquisition cost"),
        tags$li("Contract length"),
        tags$li("Future salary"),
        tags$li("Player availability"),
        tags$li("Future transfer interest")
      ),
      p("Guaranteed compensation does not equal the full cost of employing or acquiring a player."),
      p("Public Goals Added also cannot fully measure:"),
      tags$ul(
        tags$li("Off-ball positioning"),
        tags$li("Tactical discipline"),
        tags$li("Leadership"),
        tags$li("Communication"),
        tags$li("Training performance"),
        tags$li("Physical tracking output"),
        tags$li("Medical status"),
        tags$li("Character"),
        tags$li("Role-specific coaching instructions")
      ),
      p("The 2026 season is still in progress. Season-to-date results may change as players accumulate more minutes.")
    ),

    div(
      class = "section-card",
      h3("Validation status"),
      p("The 2026 MLS Value Index is primarily a descriptive current-value ranking."),
      p("Historical stability checks evaluate whether:"),
      tags$ul(
        tags$li("Sporting Impact remains reasonably stable"),
        tags$li("Top-ranked players continue to perform well"),
        tags$li("Rankings are sensitive to small samples"),
        tags$li("Results differ meaningfully by position"),
        tags$li("The model improves on simpler performance-versus-compensation comparisons")
      ),
      p("The index is not presented as a guaranteed breakout or future-performance forecast.")
    ),

    div(
      class = "section-card",
      h3("How to interpret the results"),
      p("Use the project to answer:"),
      tags$blockquote(question),
      p("Do not use it to answer:"),
      tags$blockquote("Which player should a club definitely sign or trade for?"),
      p("The model is best used to:"),
      tags$ul(
        tags$li("Identify possible compensation-efficient players"),
        tags$li("Compare players with positional peers"),
        tags$li("Understand how players generate measurable on-ball impact"),
        tags$li("Find strong performers on relatively modest compensation"),
        tags$li("Create a starting point for deeper scouting and roster analysis")
      )
    )
  )
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
