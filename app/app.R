# MLS Recruitment Intelligence — Shiny dashboard
# Launch: shiny::runApp("app")

library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
library(yaml)
library(jsonlite)

# Project root = parent of app/
app_dir <- normalizePath(getwd())
root <- if (basename(app_dir) == "app") dirname(app_dir) else app_dir
setwd(root)

source("R/utilities/load_project.R")
source("R/utilities/model_spec.R")
source("R/utilities/data_provenance.R")
source("R/models/learned/artifact_io.R")
source("R/models/learned/contribution_model.R")
source("R/models/learned/league_translation_model.R")
source("R/models/learned/age_curves_model.R")
source("R/models/learned/shrinkage_model.R")
source("R/models/learned/cost_model.R")
source("R/models/learned/feasibility_model.R")
source("R/models/decision_layer.R")
source("R/features/build_features.R")
source("R/models/component_scores.R")
source("R/models/club_conditioning.R")
source("R/models/current_rosters.R")
source("R/models/roster_analysis.R")
source("R/models/roster_comparison.R")
source("R/rankings/rank_helpers.R")
source("R/ui/about_methodology_page.R")
source("R/ui/score_documentation.R")
source("R/ui/score_traces.R")

cfg <- load_config()
model_spec <- load_product_config()
clubs_yaml <- load_yaml("config/club_profiles.yml")
roles_yaml <- load_roles_config()
team_map <- load_club_team_map()

load_period_scores <- function(period) {
  path <- file.path(cfg$paths$processed, paste0("player_component_scores_", period, ".csv"))
  if (!file.exists(path)) {
    path <- file.path(cfg$paths$processed, "player_component_scores.csv")
  }
  if (!file.exists(path)) {
    stop("Run the pipeline through scripts/05_train_models.R before launching the app.")
  }
  readr::read_csv(path, show_col_types = FALSE)
}

#' Stamp of processed outputs — changes when daily refresh writes new files.
data_files_stamp <- function() {
  paths <- c(
    cfg$paths$provenance,
    file.path(cfg$paths$processed, "player_component_scores_blended.csv"),
    file.path(cfg$paths$processed, "player_component_scores_ytd_2026.csv"),
    file.path(cfg$paths$processed, "player_component_scores_full_2025.csv")
  )
  infos <- file.info(paths)
  paste(na.omit(as.character(infos$mtime)), collapse = "|")
}

load_live_data_bundle <- function() {
  by_period <- list(
    ytd_2026 = tryCatch(load_period_scores("ytd_2026"), error = function(e) NULL),
    full_2025 = tryCatch(load_period_scores("full_2025"), error = function(e) NULL),
    blended = tryCatch(load_period_scores("blended"), error = function(e) NULL)
  )
  if (all(vapply(by_period, is.null, logical(1)))) {
    by_period$blended <- load_period_scores("blended")
  }
  sample_df <- by_period$blended %||% by_period$ytd_2026 %||% by_period$full_2025
  prov <- reconcile_provenance_with_data(cfg, sample_df)
  list(
    provenance = prov,
    is_synthetic = is_synthetic_dataset(sample_df, prov),
    players_by_period = by_period,
    stamp = data_files_stamp(),
    loaded_at = Sys.time()
  )
}

default_period <- cfg$project$evaluation_period_default %||% "blended"
live_bundle <- load_live_data_bundle()
# Startup aliases (About panel + initial UI); server hot-reloads via reactive
provenance <- live_bundle$provenance
is_synthetic <- live_bundle$is_synthetic
players_by_period <- live_bundle$players_by_period
auto_reload <- isTRUE(cfg$deployment$auto_reload %||% TRUE)
refresh_check_sec <- as.integer(cfg$deployment$refresh_check_seconds %||% 60)

clubs <- clubs_yaml$clubs
club_choices <- setNames(vapply(clubs, `[[`, "", "club_id"),
                         vapply(clubs, `[[`, "", "club_name"))
role_choices <- setNames(names(roles_yaml$roles),
                         vapply(roles_yaml$roles, `[[`, "", "display_name"))
period_choices <- c(
  "Blended recent performance (2026 YTD + 2025 prior)" = "blended",
  "2026 YTD (season-to-date — not full-season)" = "ytd_2026",
  "2025 full season (completed)" = "full_2025"
)

find_club <- function(id) {
  for (c in clubs) if (identical(c$club_id, id)) return(c)
  NULL
}

ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { background: #f6f7f5; color: #1c2420; font-family: 'Source Sans 3', 'Helvetica Neue', sans-serif; }
    .title-block { background: linear-gradient(120deg, #0b3d2e 0%, #1f6f54 55%, #c4d6a5 100%);
                   color: #f7faf7; padding: 1.4rem 1.6rem; margin-bottom: 1rem; border-radius: 0 0 12px 12px; }
    .title-block h1 { margin: 0; font-size: 1.6rem; letter-spacing: 0.02em; }
    .title-block p { margin: 0.35rem 0 0; opacity: 0.9; font-size: 0.95rem; }
    .well { background: #fff; border: 1px solid #d7ddd6; border-radius: 10px; padding: 1rem 1.15rem; }
    .score-pill { display:inline-block; padding:0.15rem 0.55rem; border-radius:6px; background:#e8f0ea; margin-right:0.35rem; }
    .about-page { max-width: 920px; line-height: 1.55; font-size: 15px; padding-bottom: 2rem; }
    .about-page h1 { margin-top: 0.2rem; color: #0b3d2e; font-size: 1.55rem; }
    .about-page h2 { margin-top: 1.6rem; color: #0b3d2e; font-size: 1.25rem; }
    .about-page h3 { margin-top: 1.2rem; color: #1f6f54; font-size: 1.08rem; }
    .about-page h4 { margin-top: 0.9rem; color: #0b3d2e; font-size: 1.0rem; }
    .about-page blockquote { border-left: 4px solid #1f6f54; padding: 0.4rem 0 0.4rem 1rem; margin: 0.8rem 0; color: #333; font-style: italic; background: #eef5f0; }
    .about-page code { font-size: 12.5px; color: #0b3d2e; background: #eef5f0; padding: 0.1rem 0.35rem; border-radius: 4px; }
    .about-page .about-table { width: 100%; border-collapse: collapse; font-size: 13px; margin: 0.6rem 0 1rem; }
    .about-page .about-table th { text-align: left; padding: 0.35rem 0.5rem; border-bottom: 1px solid #c5d0c8; }
    .about-page .about-table td { padding: 0.35rem 0.5rem; }
    .about-page .about-accordion { background: #fff; border: 1px solid #d7ddd6; border-radius: 8px; margin: 0.55rem 0; padding: 0.55rem 0.9rem; }
    .about-page .about-accordion summary { cursor: pointer; font-weight: 600; color: #0b3d2e; }
    .about-page .about-accordion-body { margin-top: 0.65rem; }
    .about-page .formula-code { background: #f3f7f4; border: 1px solid #d7ddd6; border-radius: 6px;
      padding: 0.75rem 0.9rem; font-size: 12.5px; overflow-x: auto; white-space: pre-wrap; color: #0b3d2e; }
    .about-page .formula-example { margin: 0.75rem 0 0.35rem; padding: 0.55rem 0.75rem;
      background: #eef5f0; border-radius: 6px; font-size: 13.5px; }
    .about-page .formula-def-table { font-size: 12px; margin: 0.75rem 0; }
    .about-page .formula-def-table th { white-space: nowrap; }
    .about-page .score-name { font-weight: 600; }
    .about-page .score-tip { margin-left: 0.25rem; color: #5a6a60; font-size: 12px; cursor: help; font-weight: 400; }
    .about-page .about-tagline { font-size: 1.15rem; color: #1f6f54; margin: -0.25rem 0 1rem; }
    .about-page .about-tech-cta { margin: 1.5rem 0 1rem; }
    .about-page .about-status-line { margin-top: 1.25rem; padding: 0.9rem 1rem; background: #eef5f0; border: 1px solid #c5d0c8; border-radius: 8px; font-size: 13px; color: #33443c; }
    .about-page .about-status-line p { margin: 0.2rem 0; }
    .methodology-modal { max-height: 70vh; overflow-y: auto; }
    .example-box { background: #eef5f0; border-color: #b7d0c2; }
    .example-box p { margin: 0.35rem 0; }
    .help-text { font-size: 11px; color: #5a6a60; margin: -6px 0 12px; line-height: 1.35; }
    .banner-synthetic { background: #8a1c1c; color: #fff; padding: 0.75rem 1.2rem; font-weight: 700;
                        letter-spacing: 0.02em; margin-bottom: 0.75rem; border-radius: 8px; }
    .banner-live { background: #0b3d2e; color: #f3faf5; padding: 0.65rem 1.2rem; margin-bottom: 0.75rem;
                   border-radius: 8px; font-size: 0.95rem; }
    .cutoff-chip { display:inline-block; background: rgba(255,255,255,0.15); padding: 0.15rem 0.55rem;
                   border-radius: 6px; margin-left: 0.35rem; }
  "))),
  uiOutput("data_banner"),
  div(class = "title-block",
      h1("MLS Recruitment Intelligence"),
      p("2026 recruitment shortlists: attainable, role-fit, and value — with explicit evaluation periods and data cutoffs")
  ),
  tabsetPanel(
    id = "main_tabs",
    type = "tabs",
    tabPanel(
      "About, How to Use & Methodology",
      value = "about",
      br(),
      uiOutput("about_page")
    ),
    tabPanel(
      "Shortlist",
      value = "shortlist",
      br(),
      sidebarLayout(
        sidebarPanel(
          width = 3,
          h4("Recruitment setup"),
          selectInput(
            "eval_period",
            "Evaluation period",
            choices = period_choices,
            selected = default_period
          ),
          tags$p(class = "help-text",
                 "Default blends 2026 YTD with 2025 priors by minutes/reliability. 2026 alone is YTD, not full-season."),
          selectInput("club", "MLS club", choices = club_choices, selected = "sanjose"),
          checkboxInput("use_club_defaults", "Auto-apply club defaults when club changes", TRUE),
          tags$p(class = "help-text", "Turns on club-specific age/budget/priority/role suggestions."),
          selectInput("role", "Tactical role", choices = role_choices, selected = "pressing_striker"),
          sliderInput("age", "Age range", min = 17, max = 36, value = c(18, 28)),
          selectInput("max_cost", "Max cost tier", choices = c("Low"=1,"Moderate"=2,"High"=3,"Very high"=4,"Any"=5), selected = 3),
          selectInput("leagues", "Current leagues", multiple = TRUE,
                      choices = c("MLS"="mls","MLS NEXT Pro"="mlsnp","USL Championship"="uslc"),
                      selected = c("mls","mlsnp","uslc")),
          selectInput("domestic", "Domestic / international",
                      choices = c("Any"="any", "Domestic preference (nationality hint only)"="domestic", "International OK"="intl"), selected = "any"),
          tags$p(class = "help-text", "Nationality ≠ MLS international-roster status. Official roster status preferred when available."),
          selectInput("priority", "Priority",
                      choices = c("Balanced"="balanced", "Development prospect"="development", "Immediate contributor"="immediate"),
                      selected = "development"),
          tags$p(class = "help-text",
                 "Changes who ranks: Development ≤24 & upside-weighted; Immediate ≥22 with 1200+ minutes & contribution-weighted; Balanced mixes both."),
          selectInput(
            "rec_filter",
            "Recommendation (Rec)",
            choices = c(
              "Priority Review" = "Priority Review",
              "Development Watch" = "Development Watch",
              "Monitor" = "Monitor",
              "Low Priority" = "Low Priority"
            ),
            selected = c("Priority Review", "Development Watch", "Monitor", "Low Priority"),
            multiple = TRUE
          ),
          tags$p(class = "help-text",
                 "Cautious labels until backtests exist. Priority Review ≠ confirmed pursue."),
          sliderInput(
            "min_minutes",
            "Minimum minutes",
            min = 100, max = 3000,
            value = cfg$project$min_minutes_full %||% 450,
            step = 50
          ),
          tags$p(class = "help-text", "Drop players with fewer minutes (noisy stats). Use a lower floor for 2026 YTD."),
          sliderInput("risk_tol", "Max model-uncertainty tolerance", min = 20, max = 100, value = 75),
          tags$p(class = "help-text", "Hide players with model uncertainty above this."),
          tags$hr(),
          h4("Overall score weights"),
          tags$p(class = "help-text",
                 "These sliders are the only weights used for Overall on the Shortlist. They are normalized to sum to 100%."),
          sliderInput("w_proj", "Estimated Near-Term Contribution", 0, 1, cfg$scoring$weights$projected_mls_performance, 0.05),
          tags$p(class = "help-text", title = "Directional on-field usefulness for the next complete MLS season. Sporting signals only.",
                 "Sporting estimate only — not salary or feasibility."),
          sliderInput("w_role", "Tactical Role Fit", 0, 1, cfg$scoring$weights$tactical_role_fit, 0.05),
          tags$p(class = "help-text", title = "How closely observed traits match the selected tactical role. See Role Fit calculation on About.",
                 "Match to the selected tactical role (coverage-gated). Open About → Role Fit for the metric recipe."),
          sliderInput("w_fin", "Compensation-Adjusted Value", 0, 1, cfg$scoring$weights$financial_value, 0.05),
          tags$p(class = "help-text", title = "Contribution vs known guaranteed-compensation tier. Not acquisition surplus.",
                 "Uses known guaranteed compensation only — not acquisition surplus."),
          sliderInput("w_fea", "Acquisition Feasibility", 0, 1, cfg$scoring$weights$acquisition_feasibility, 0.05),
          tags$p(class = "help-text", title = "How realistic an acquisition may be from public information.",
                 "Estimated acquisition plausibility (not a claim the player is available)."),
          sliderInput("w_dev", "Development Upside", 0, 1, cfg$scoring$weights$development_upside, 0.05),
          tags$p(class = "help-text", title = "Room to improve based on age curve, YoY change, and minutes trajectory.",
                 "Youth / growth outlook (continuous age curve)."),
          checkboxInput("apply_risk", "Apply model-uncertainty penalty", TRUE),
          tags$p(class = "help-text", "If checked, higher Model Uncertainty lowers Overall."),
          uiOutput("overall_weight_formula"),
          tags$p(style="font-size:11px;color:#666;margin-top:12px;",
                 clubs_yaml$meta$disclaimer)
        ),
        mainPanel(
          width = 9,
          uiOutput("status_banner"),
          uiOutput("club_banner"),
          uiOutput("overall_weight_banner"),
          tags$p(class = "help-text", style = "margin-top:0;",
                 sprintf(
                   "Top %s candidates for club/role/filters. ",
                   cfg$project$shortlist_size %||% 100
                 ),
                 "Overall score, shortlist rank, and reference percentile are shown separately. ",
                 "Club Fit / Style / Age / Budget / WhyClub are in the expandable club-context detail below."),
          DT::DTOutput("shortlist_table"),
          br(),
          uiOutput("shortlist_club_context"),
          br(),
          div(
            style = "margin-top: 1rem;",
            actionButton("btn_compare_roster", "Compare with Current Player", class = "btn-primary"),
            tags$p(
              class = "help-text",
              style = "margin: 0.65rem 0 0.85rem; clear: both;",
              "Select a shortlist row, then open Current Player vs Target against this club's roster."
            )
          ),
          uiOutput("export_controls")
        )
      )
    ),
    tabPanel(
      "Roster Overview",
      value = "roster",
      br(),
      fluidRow(
        column(
          3,
          selectInput("roster_club", "MLS club", choices = club_choices, selected = "sanjose"),
          selectInput("roster_position", "Position filter", choices = POSITION_CHOICES, selected = "any"),
          selectInput("roster_role", "Tactical role (for gap suggestions)", choices = role_choices,
                      selected = "pressing_striker"),
          checkboxInput("roster_override_need", "Override estimated need (manual role/position)", FALSE),
          tags$p(class = "help-text",
                 "Gap signals are public-data estimates, not official club priorities. Override to force any role."),
          actionButton("btn_use_gap", "Use selected gap as recruitment setup", class = "btn-default")
        ),
        column(
          9,
          uiOutput("roster_status"),
          h4("Inferred current roster from available public data"),
          tags$p(class = "help-text",
                 "Built mainly from public player-team, salary, and minutes records — not a complete official MLS roster. ",
                 "May miss recent signings, loans, injured players, supplemental roster players, and off-roster Homegrowns. ",
                 "Score cells show value (#rank on this roster); Model Uncertainty #1 = lowest uncertainty. ",
                 "Salary is MLSPA guaranteed compensation in USD."),
          checkboxInput("roster_show_inactive", "Also show on-books players with no 2026 minutes (loans/unused)", FALSE),
          DT::DTOutput("roster_table"),
          br(),
          h4("Roster needs by position × tactical role"),
          tags$p(class = "help-text", style = "color:#8a1c1c;font-weight:600;",
                 "Public-data roster-planning signals (Need Score 0–100 vs MLS peers). ",
                 "Not official club priorities. Do not treat as an order to replace a named player."),
          tags$p(class = "help-text",
                 "Need Score = 30% starter-quality gap + 25% effective-depth gap + 15% succession + ",
                 "10% availability + 10% tactical coverage + 10% efficiency opportunity. ",
                 "Efficiency is labeled separately unless quality/depth/succession gaps are also elevated."),
          DT::DTOutput("gap_table")
        )
      )
    ),
    tabPanel(
      "Current Player vs Target",
      value = "roster_compare",
      br(),
      fluidRow(
        column(
          3,
          selectInput("cmp_club", "MLS club", choices = club_choices, selected = "sanjose"),
          selectInput("cmp_position", "Position", choices = POSITION_CHOICES, selected = "FW"),
          selectInput("cmp_role", "Tactical role", choices = role_choices, selected = "pressing_striker"),
          selectInput("cmp_objective", "Recruitment objective", choices = RECRUITMENT_OBJECTIVES,
                      selected = "upgrade"),
          selectizeInput("cmp_incumbents", "Current roster player(s)", choices = NULL, multiple = TRUE,
                         options = list(placeholder = "Select one or more incumbents")),
          selectizeInput("cmp_target", "Recruitment target", choices = NULL, multiple = FALSE,
                         options = list(placeholder = "Select target (shortlist or search)")),
          checkboxInput("cmp_target_from_shortlist", "Limit targets to current shortlist", TRUE),
          actionButton("btn_run_cmp", "Run comparison", class = "btn-success"),
          br(), br(),
          uiOutput("cmp_export_controls")
        ),
        column(
          9,
          uiOutput("cmp_header"),
          fluidRow(
            column(6, plotOutput("cmp_concept_plot", height = 300)),
            column(6, plotOutput("cmp_metric_plot", height = 300))
          ),
          h4("Score & delta summary"),
          DT::DTOutput("cmp_summary_table"),
          br(),
          h4("Role-adjusted metric deltas"),
          DT::DTOutput("cmp_metric_table"),
          br(),
          h4("Roster-impact explanation"),
          verbatimTextOutput("cmp_narrative")
        )
      )
    ),
    tabPanel(
      "Player profile",
      value = "profile",
      br(),
      selectInput("selected_player", "Player from current shortlist", choices = NULL),
      uiOutput("profile_header"),
      plotOutput("component_plot", height = 280),
      plotOutput("radar_plot", height = 320),
      verbatimTextOutput("profile_text"),
      br(),
      uiOutput("profile_score_traces")
    ),
    tabPanel(
      "Shortlist Compare",
      value = "compare",
      br(),
      selectizeInput("compare_ids", "Compare 2–4 players from current shortlist",
                     choices = NULL, multiple = TRUE, options = list(maxItems = 4)),
      plotOutput("compare_plot", height = 360),
      tableOutput("compare_table")
    ),
  )
)

server <- function(input, output, session) {
  # Hot-reload processed ASA/MLSPA outputs when daily refresh writes new files
  data_store <- reactiveVal(live_bundle)

  if (isTRUE(auto_reload)) {
    observe({
      invalidateLater(max(15L, refresh_check_sec) * 1000, session)
      stamp <- data_files_stamp()
      cur <- data_store()
      if (!identical(stamp, cur$stamp) && nzchar(stamp)) {
        new_bundle <- tryCatch(load_live_data_bundle(), error = function(e) NULL)
        if (!is.null(new_bundle)) {
          data_store(new_bundle)
          showNotification(
            paste("Data refreshed —", cutoff_label(new_bundle$provenance)),
            type = "message", duration = 6
          )
        }
      }
    })
  }

  provenance_r <- reactive(data_store()$provenance)
  is_synthetic_r <- reactive(isTRUE(data_store()$is_synthetic))
  players_by_period_r <- reactive(data_store()$players_by_period)

  output$about_page <- renderUI({
    by_period <- players_by_period_r()
    sample_df <- by_period[[default_period]] %||% by_period$blended %||% by_period$ytd_2026 %||% by_period$full_2025
    about_methodology_page(
      provenance = provenance_r(),
      model_spec = model_spec,
      cfg = cfg,
      n_rows = nrow(sample_df %||% data.frame()),
      is_synthetic = is_synthetic_r()
    )
  })

  output$about_score_docs <- renderUI({
    sl <- tryCatch({
      isolate(active_shortlist())
    }, error = function(e) NULL)
    example <- NULL
    ex_role <- isolate(input$role) %||% "transition_winger"
    if (!is.null(sl) && nrow(sl)) {
      example <- sl[1, , drop = FALSE]
      if ("role_id" %in% names(example)) ex_role <- example$role_id[[1]] %||% ex_role
    }
    # Do not react to dictionary search/role recipe inputs here — child outputs handle those.
    render_score_docs_sections(
      cfg = cfg,
      spec = model_spec,
      example_player = example,
      example_role = ex_role,
      metric_filter = "",
      selected_recipe_role = ex_role
    )
  })

  output$data_banner <- renderUI({
    prov <- provenance_r()
    if (isTRUE(is_synthetic_r())) {
      div(class = "banner-synthetic",
          "⚠ SYNTHETIC DEMO DATA — Not real 2026 MLS rosters/salaries. ",
          "Do not use these rankings as genuine scouting reports. Exports are watermarked/blocked.")
    } else {
      div(class = "banner-live",
          tags$strong("2026 live-season product"),
          span(class = "cutoff-chip", cutoff_label(prov)),
          span(class = "cutoff-chip", "2026 stats are season-to-date (not full-season)"),
          if (isTRUE(auto_reload)) {
            span(class = "cutoff-chip", "Auto-reloads when daily refresh finishes")
          })
    }
  })

  # When club changes, optionally push club-specific defaults into the filters
  observeEvent(input$club, {
    club <- find_club(input$club)
    req(club)
    if (!isTRUE(input$use_club_defaults)) return()

    updateSelectInput(session, "role", selected = suggested_role_for_club(club))
    updateSelectInput(session, "priority", selected = suggested_priority_for_club(club))
    updateSelectInput(session, "max_cost", selected = as.character(budget_tier_to_max_cost(club$budget_tier)))

    avg_age <- club$average_squad_age %||% 26.5
    if ((club$development_priority %||% 0.5) >= 0.65) {
      updateSliderInput(session, "age", value = c(18, 25))
    } else if ((club$immediate_impact_priority %||% 0.5) >= 0.65) {
      updateSliderInput(session, "age", value = c(23, 30))
    } else {
      updateSliderInput(session, "age", value = c(max(17, floor(avg_age - 4)), min(36, ceiling(avg_age + 3))))
    }

    if ((club$domestic_player_priority %||% 0.5) >= 0.6) {
      updateSelectInput(session, "domestic", selected = "domestic")
    } else {
      updateSelectInput(session, "domestic", selected = "any")
    }

    # Budget clubs lean toward feeder leagues; star clubs keep MLS in pool
    if ((club$financial_value_weight %||% 0.5) >= 0.65 || (club$development_priority %||% 0.5) >= 0.65) {
      updateSelectInput(session, "leagues", selected = c("mls", "mlsnp", "uslc"))
    } else if ((club$immediate_impact_priority %||% 0.5) >= 0.7) {
      updateSelectInput(session, "leagues", selected = c("mls", "uslc"))
    }
  }, ignoreInit = FALSE)

  observeEvent(input$eval_period, {
    period <- input$eval_period %||% default_period
    floor <- if (identical(period, "ytd_2026")) {
      cfg$project$min_minutes_ytd %||% 180
    } else {
      cfg$project$min_minutes_full %||% 450
    }
    updateSliderInput(session, "min_minutes", value = floor)
  }, ignoreInit = TRUE)

  players_active <- reactive({
    period <- input$eval_period %||% default_period
    by_p <- players_by_period_r()
    df <- by_p[[period]]
    if (is.null(df)) df <- by_p$blended
    req(!is.null(df), nrow(df) > 0)
    df
  })

  active_shortlist <- reactive({
    club <- find_club(input$club)
    players <- players_active()
    req(club, players, input$role, input$age, input$max_cost, input$leagues)

    w <- c(
      projected_mls_performance = input$w_proj,
      tactical_role_fit = input$w_role,
      financial_value = input$w_fin,
      acquisition_feasibility = input$w_fea,
      development_upside = input$w_dev
    )
    if (sum(w) <= 0) w <- unlist(cfg$scoring$weights)
    w <- normalize_weight_shares(w)

    # Score against fixed role × league × minutes reference (filters must not rescale scores).
    d_ref <- filter_role_pool(
      players |>
        dplyr::filter(
          minutes >= input$min_minutes,
          league_id %in% input$leagues
        ),
      input$role
    )
    req(nrow(d_ref) > 0)

    d_scored <- club_conditioned_score(
      d_ref, club, as.list(w),
      priority = input$priority,
      club_blend = 0.55,
      apply_soft_filters = FALSE
    )
    # Sidebar sliders are the sole Overall weight source (not a hidden decision-layer override).
    risk_lambda <- as.numeric(cfg$scoring$risk_penalty_weight %||% 0.15)
    d_scored <- apply_overall_score(
      d_scored,
      weights = as.list(w),
      apply_risk = isTRUE(input$apply_risk),
      risk_lambda = risk_lambda
    )
    gate <- tryCatch(load_thresholds()$feasibility_gate$low_threshold, error = function(e) 35) %||% 35
    d_scored$score_overall <- ifelse(
      as.numeric(d_scored$score_feasibility) < gate,
      pmin(d_scored$score_overall, 54),
      d_scored$score_overall
    )
    d_scored$score_overall <- clip(d_scored$score_overall, 0, 100)
    d_scored$overall_weights_json <- as.character(jsonlite::toJSON(as.list(w), auto_unbox = TRUE))
    # Preserve fixed-reference percentile before filter/rank
    d_scored$reference_percentile_overall <- percentile_rank(d_scored$score_overall)

    # UI filters change who appears and shortlist rank only
    d <- d_scored |>
      dplyr::filter(
        age >= input$age[1], age <= input$age[2],
        is.na(cost_tier) | cost_tier <= as.integer(input$max_cost),
        score_risk <= input$risk_tol
      )

    if (identical(input$domestic, "domestic")) {
      if ("intl_roster_status" %in% names(d) && any(!is.na(d$intl_roster_status))) {
        d <- dplyr::filter(d, tolower(intl_roster_status) %in% c("domestic", "homegrown"))
      } else if ("nationality_hint_usa_can" %in% names(d)) {
        d <- dplyr::filter(d, nationality_hint_usa_can == 1L)
      } else if ("is_domestic_player" %in% names(d)) {
        d <- dplyr::filter(d, is_domestic_player == 1L)
      }
    }
    req(nrow(d) > 0)

    out <- rank_shortlist(d, n = nrow(d))
    # Keep pre-filter reference percentile (rank_shortlist may overwrite)
    out$reference_percentile_overall <- d$reference_percentile_overall[match(out$player_id, d$player_id)]

    recs <- input$rec_filter
    if (length(recs)) {
      out <- dplyr::filter(out, .data$recommendation %in% recs)
    }
    req(nrow(out) > 0)
    out <- out |>
      dplyr::slice_head(n = as.integer(cfg$project$shortlist_size %||% 100)) |>
      dplyr::mutate(rank = dplyr::row_number())
    out$evaluation_period <- input$eval_period %||% default_period
    out$season_label <- evaluation_period_label(out$evaluation_period[[1]], cfg)
    out$data_cutoff_label <- cutoff_label(provenance_r())
    out$is_synthetic <- is_synthetic_r()
    out
  })

  output$status_banner <- renderUI({
    period <- input$eval_period %||% default_period
    tagList(
      div(class = "well",
          tags$strong(evaluation_period_label(period, cfg)),
          tags$br(),
          cutoff_label(provenance_r()),
          if (!identical(period, "full_2025")) {
            tags$div(style = "margin-top:6px;color:#8a1c1c;font-weight:600;",
                     "Note: any 2026 statistics shown are season-to-date, not full-season.")
          } else {
            tags$div(style = "margin-top:6px;color:#0b3d2e;",
                     "Using completed 2025 full-season statistics only.")
          }
      )
    )
  })

  output$export_controls <- renderUI({
    if (is_synthetic_r()) {
      tagList(
        tags$p(style = "color:#8a1c1c;font-weight:700;",
               "Exports disabled for genuine scouting use while synthetic demo data is loaded."),
        downloadButton("dl_csv_demo", "Download DEMO-ONLY CSV (watermarked)", class = "btn-warning")
      )
    } else {
      downloadButton("dl_csv", "Export CSV shortlist", class = "btn-success")
    }
  })

  output$club_banner <- renderUI({
    club <- find_club(input$club)
    req(club)
    div(class = "well",
        tags$strong(club$club_name),
        span(class="score-pill", club$tactical_archetype),
        span(class="score-pill", paste("Budget:", club$budget_tier)),
        span(class="score-pill", paste("Dev:", club$development_priority)),
        span(class="score-pill", paste("Immediate:", club$immediate_impact_priority)),
        tags$div(style="margin-top:6px;font-size:13px;color:#444;",
                 em(clubs_yaml$meta$label),
                 tags$br(),
                 sprintf("Suggested role: %s · priority: %s · max cost tier: %s",
                         suggested_role_for_club(club),
                         suggested_priority_for_club(club),
                         budget_tier_to_max_cost(club$budget_tier)))
    )
  })

  output$shortlist_table <- DT::renderDT({
    sl <- active_shortlist()
    req(sl)
    n_shown <- nrow(sl)
    sl |>
      transmute(
        Player = display_name,
        League = toupper(league_id),
        Age = round(age, 1),
        Overall = round(score_overall, 1),
        `Shortlist rank` = paste0(rank, " of ", n_shown),
        `Reference percentile` = ifelse(
          is.finite(reference_percentile_overall),
          paste0(round(reference_percentile_overall), "th"),
          "—"
        ),
        `Estimated Contribution` = ifelse(is.finite(score_projected_mls), round(score_projected_mls, 1), NA_real_),
        `Role Fit` = ifelse(is.finite(score_role_fit), round(score_role_fit, 1), NA_real_),
        `Compensation-Adjusted Value` = ifelse(is.finite(score_financial_value), round(score_financial_value, 1), NA_real_),
        Feasibility = ifelse(is.finite(score_feasibility), round(score_feasibility, 1), NA_real_),
        `Development Upside` = ifelse(is.finite(score_development), round(score_development, 1), NA_real_),
        `Model Uncertainty` = ifelse(is.finite(score_risk), round(score_risk, 1), NA_real_),
        Recommendation = recommendation
      )
  }, selection = "single", options = list(
    pageLength = min(25L, as.integer(cfg$project$shortlist_size %||% 100)),
    lengthMenu = list(c(15, 25, 50, 100), c("15", "25", "50", "100")),
    scrollX = TRUE
  ), rownames = FALSE)

  output$overall_weight_formula <- renderUI({
    w <- live_overall_weight_labels(c(
      projected_mls_performance = input$w_proj %||% 0.3,
      tactical_role_fit = input$w_role %||% 0.25,
      financial_value = input$w_fin %||% 0.2,
      acquisition_feasibility = input$w_fea %||% 0.15,
      development_upside = input$w_dev %||% 0.1
    ))
    tags$p(
      class = "help-text",
      style = "margin-top:8px;font-weight:600;color:#0b3d2e;",
      sprintf(
        "Current Overall weights: Contribution %.0f%% · Role Fit %.0f%% · Value %.0f%% · Feasibility %.0f%% · Development %.0f%%%s",
        100 * w[["Contribution"]], 100 * w[["Role Fit"]], 100 * w[["Value"]],
        100 * w[["Feasibility"]], 100 * w[["Development"]],
        if (isTRUE(input$apply_risk)) " · uncertainty penalty ON" else " · uncertainty penalty OFF"
      )
    )
  })

  output$overall_weight_banner <- renderUI({
    w <- live_overall_weight_labels(c(
      projected_mls_performance = input$w_proj %||% 0.3,
      tactical_role_fit = input$w_role %||% 0.25,
      financial_value = input$w_fin %||% 0.2,
      acquisition_feasibility = input$w_fea %||% 0.15,
      development_upside = input$w_dev %||% 0.1
    ))
    div(
      class = "well",
      style = "padding:0.65rem 1rem;margin-bottom:0.75rem;",
      tags$strong("Current Overall formula (normalized)"),
      tags$div(style = "margin-top:4px;font-size:13px;",
               sprintf(
                 "Contribution %.0f%% + Role Fit %.0f%% + Compensation-Adjusted Value %.0f%% + Feasibility %.0f%% + Development Upside %.0f%%%s",
                 100 * w[["Contribution"]], 100 * w[["Role Fit"]], 100 * w[["Value"]],
                 100 * w[["Feasibility"]], 100 * w[["Development"]],
                 if (isTRUE(input$apply_risk)) {
                   sprintf(" × (1 − %.2f × Model Uncertainty / 100)", cfg$scoring$risk_penalty_weight %||% 0.15)
                 } else {
                   ""
                 }
               ))
    )
  })

  output$shortlist_club_context <- renderUI({
    sl <- active_shortlist()
    req(sl)
    idx <- input$shortlist_table_rows_selected
    if (!length(idx)) {
      return(div(class = "well",
                 tags$strong("Club context"),
                 tags$p(class = "help-text", "Select a shortlist row to see Club Fit, Style Fit, Age Fit, Budget Fit, and WhyClub.")))
    }
    p <- sl[idx[[1]], , drop = FALSE]
    div(
      class = "well",
      tags$strong(sprintf("Club context — %s", p$display_name[[1]])),
      tags$p(class = "help-text",
             "These scores describe club-specific fit. They are not separate Overall ingredients when using the Shortlist weight sliders."),
      tags$ul(
        tags$li(sprintf("Club Fit: %s", ifelse(is.finite(p$score_club_personalization), round(p$score_club_personalization, 1), "—"))),
        tags$li(sprintf("Style Fit: %s", ifelse(is.finite(p$score_club_fit), round(p$score_club_fit, 1), "—"))),
        tags$li(sprintf("Age Fit: %s", ifelse(is.finite(p$score_age_fit), round(p$score_age_fit, 1), "—"))),
        tags$li(sprintf("Budget Fit: %s", ifelse(is.finite(p$score_budget_fit), round(p$score_budget_fit, 1), "—"))),
        tags$li(sprintf("WhyClub: %s", p$why_club %||% "—"))
      )
    )
  })

  observe({
    sl <- active_shortlist()
    req(sl)
    ch <- setNames(sl$player_id, sl$display_name)
    updateSelectInput(session, "selected_player", choices = ch, selected = ch[[1]])
    updateSelectizeInput(session, "compare_ids", choices = ch, server = TRUE)
  })

  selected_row <- reactive({
    sl <- active_shortlist()
    req(sl, input$selected_player)
    dplyr::filter(sl, player_id == input$selected_player) |> slice_head(n = 1)
  })

  output$profile_header <- renderUI({
    p <- selected_row()
    req(p)
    tagList(
      h3(p$display_name),
      p(sprintf("%s | age %.0f | %s | confidence %s",
                toupper(p$league_id), p$age, p$role_id, p$confidence)),
      p(tags$strong("Recommendation: "), p$recommendation)
    )
  })

  output$component_plot <- renderPlot({
    p <- selected_row(); req(p)
    scores <- tibble(
      component = c(
        "Estimated Near-Term Contribution", "Tactical Role Fit", "Acquisition Feasibility",
        "Development Upside", "Compensation-Adjusted Value", "Model Uncertainty", "Club Fit", "Overall"
      ),
      value = c(p$score_projected_mls, p$score_role_fit, p$score_feasibility, p$score_development,
                p$score_financial_value, p$score_risk, p$score_club_fit, p$score_overall)
    )
    ggplot(scores, aes(reorder(component, value), value)) +
      geom_col(fill = "#1f6f54") + coord_flip() +
      labs(title = "Component breakdown", x = NULL, y = "Score") +
      theme_minimal(base_size = 13)
  })

  output$radar_plot <- renderPlot({
    p <- selected_row(); req(p)
    radar <- tibble(
      metric = c("nPxG","xA","Press","Progress","Defend","Retention"),
      value = c(p$pct_proj_npxg, p$pct_proj_xa, p$pct_proj_press,
                p$pct_prog_pass, p$defensive_actions_p90_pct, p$ball_retention_pct)
    )
    ggplot(radar, aes(metric, value)) +
      geom_col(fill = "#0b3d2e", width = 0.65) +
      ylim(0, 100) +
      labs(title = "Percentile profile (role-relevant proxies)", x = NULL, y = "Percentile") +
      theme_minimal(base_size = 13)
  })

  output$profile_text <- renderText({
    p <- selected_row(); req(p)
    paste0(
      "STRENGTHS\n", p$strengths, "\n\n",
      "MAIN RISKS OR LIMITATIONS\n", p$risks_text, "\n\n",
      "VIDEO QUESTIONS\n", p$video_questions, "\n\n",
      "WHY THIS CLUB/ROLE\n", p$explanation
    )
  })

  output$profile_score_traces <- renderUI({
    p <- selected_row()
    req(p)
    render_player_score_traces(
      p,
      role_id = input$role %||% p$role_id[[1]],
      spec = model_spec
    )
  })

  output$compare_plot <- renderPlot({
    req(length(input$compare_ids) >= 2)
    sl <- active_shortlist()
    d <- sl |> filter(player_id %in% input$compare_ids)
    long <- d |>
      select(display_name, score_projected_mls, score_role_fit, score_feasibility,
             score_development, score_financial_value, score_risk) |>
      pivot_longer(-display_name, names_to = "metric", values_to = "value") |>
      mutate(metric = dplyr::recode(
        metric,
        score_projected_mls = "Estimated Near-Term Contribution",
        score_role_fit = "Tactical Role Fit",
        score_feasibility = "Acquisition Feasibility",
        score_development = "Development Upside",
        score_financial_value = "Compensation-Adjusted Value",
        score_risk = "Model Uncertainty"
      ))
    ggplot(long, aes(metric, value, fill = display_name)) +
      geom_col(position = "dodge") +
      coord_flip() +
      labs(title = "Player comparison", x = NULL, y = "Score", fill = NULL) +
      theme_minimal(base_size = 13)
  })

  output$compare_table <- renderTable({
    req(length(input$compare_ids) >= 2)
    active_shortlist() |>
      filter(player_id %in% input$compare_ids) |>
      transmute(Player = display_name, Overall = round(score_overall,1),
                `Near-Term Contrib.` = round(score_projected_mls,1),
                `Role Fit` = round(score_role_fit,1),
                Feasibility = round(score_feasibility,1),
                `Comp.-Adj. Value` = round(score_financial_value,1),
                `Model Uncertainty` = round(score_risk,1))
  })

  output$dl_csv <- downloadHandler(
    filename = function() {
      paste0(
        "shortlist_", input$club, "_", input$eval_period, "_",
        gsub("[^0-9]", "", provenance_r()$data_cutoff_utc %||% format(Sys.time(), "%Y%m%d%H%M")),
        ".csv"
      )
    },
    content = function(file) {
      if (is_synthetic_r()) {
        stop("Genuine export blocked: synthetic demo data is loaded.")
      }
      sl <- active_shortlist()
      sl$export_disclaimer <- paste(
        "Live ASA/MLSPA-backed shortlist.",
        cutoff_label(provenance_r()),
        "2026 figures are season-to-date unless evaluation period is 2025 full season."
      )
      readr::write_csv(sl, file)
    }
  )

  output$dl_csv_demo <- downloadHandler(
    filename = function() paste0("DEMO_ONLY_shortlist_", input$club, "_", Sys.Date(), ".csv"),
    content = function(file) {
      sl <- active_shortlist()
      sl$export_disclaimer <- paste(
        "SYNTHETIC DEMO DATA — NOT A GENUINE SCOUTING REPORT.",
        "Do not use for roster decisions.",
        cutoff_label(provenance_r())
      )
      # Watermark player names so they cannot be mistaken for real exports
      sl$display_name <- paste0("[DEMO] ", sl$display_name)
      readr::write_csv(sl, file)
    }
  )

  # ---- Roster overview + incumbent vs target ----
  observeEvent(input$club, {
    updateSelectInput(session, "roster_club", selected = input$club)
    updateSelectInput(session, "cmp_club", selected = input$club)
  }, ignoreInit = TRUE)

  observeEvent(input$roster_club, {
    updateSelectInput(session, "cmp_club", selected = input$roster_club)
  }, ignoreInit = TRUE)

  roster_club_obj <- reactive({
    find_club(input$roster_club %||% input$club %||% "sanjose")
  })

  club_roster <- reactive({
    club <- roster_club_obj()
    req(club)
    club_roster_players(
      players_active(), club$club_id, team_map,
      active_only = !isTRUE(input$roster_show_inactive),
      cfg = cfg
    )
  })

  # League-wide club × role benchmarks for percentile gaps (recomputed per evaluation period)
  league_need_bench <- reactive({
    build_league_slot_benchmarks(
      players_active(), clubs, team_map, cfg, names(ROLE_POSITION_MAP)
    )
  })

  roster_gaps <- reactive({
    club <- roster_club_obj()
    req(club)
    analyze_club_role_needs(
      players_df = players_active(),
      club = club,
      clubs_list = clubs,
      team_map = team_map,
      roles_yaml = roles_yaml,
      cfg = cfg,
      league_bench = league_need_bench()
    )
  })

  output$roster_status <- renderUI({
    club <- roster_club_obj()
    req(club)
    n <- nrow(club_roster())
    g <- tryCatch(roster_gaps(), error = function(e) NULL)
    top <- if (!is.null(g) && nrow(g)) g[1, ] else NULL
    div(class = "well",
        tags$strong(club$club_name),
        span(class = "score-pill", paste(n, "roster players inferred")),
        span(class = "cutoff-chip", style = "background:#e8f0ea;color:#0b3d2e;",
             cutoff_label(provenance_r())),
        if (!is.null(top)) {
          span(class = "score-pill",
               sprintf("Top need: %s %s (%.0f)", top$position_group, top$tactical_role, top$need_score))
        },
        tags$div(style = "margin-top:6px;font-size:13px;color:#666;",
                 "Current roster = 2026 MLSPA/ASA salary-guide membership for this club. ",
                 "Default list is active_2026 only (2026 minutes for this club) — loans/unused excluded. ",
                 "Needs use the same active roster. Public-data estimates, not official priorities.")
    )
  })

  output$roster_table <- DT::renderDT({
    rost <- club_roster()
    req(rost)
    if (!identical(input$roster_position, "any")) {
      rost <- dplyr::filter(rost, position_group == input$roster_position)
    }
    roster_summary_table(rost)
  }, selection = "single", options = list(pageLength = 20, scrollX = TRUE), rownames = FALSE)

  output$gap_table <- DT::renderDT({
    g <- roster_gaps()
    req(g)
    if (!identical(input$roster_position, "any")) {
      g <- dplyr::filter(g, position_group == input$roster_position)
    }
    if (isTRUE(input$roster_override_need) && nzchar(input$roster_role %||% "")) {
      # Manual override: pin selected role to top for recruitment setup, keep all rows
      g <- dplyr::bind_rows(
        dplyr::filter(g, tactical_role == input$roster_role),
        dplyr::filter(g, tactical_role != input$roster_role)
      )
    }
    g |>
      transmute(
        Priority = priority,
        `Need score` = need_score,
        Confidence = confidence,
        `Need type` = need_type,
        Position = position_group,
        Role = tactical_role,
        `Current best` = current_best,
        `Best backup` = best_backup,
        `Starter-quality %ile` = starter_quality_percentile,
        `Effective-depth %ile` = effective_depth_percentile,
        `Minutes concentration %` = minutes_concentration,
        `Succession risk` = succession_risk,
        `Starter gap` = starter_quality_gap,
        `Depth gap` = effective_depth_gap,
        `Availability risk` = availability_risk,
        `Tactical gap` = tactical_coverage_gap,
        `Efficiency opportunity` = financial_efficiency_opportunity,
        Evidence = evidence_summary,
        `Suggested profile` = suggested_recruitment_profile,
        Limitations = public_data_limitations
      )
  }, selection = "single", options = list(pageLength = 5, scrollX = TRUE), rownames = FALSE)

  observeEvent(input$btn_use_gap, {
    g <- roster_gaps()
    req(g)
    idx <- input$gap_table_rows_selected
    row <- if (length(idx)) {
      # Re-apply same filters as table for correct row mapping
      g2 <- g
      if (!identical(input$roster_position, "any")) {
        g2 <- dplyr::filter(g2, position_group == input$roster_position)
      }
      if (isTRUE(input$roster_override_need) && nzchar(input$roster_role %||% "")) {
        g2 <- dplyr::bind_rows(
          dplyr::filter(g2, tactical_role == input$roster_role),
          dplyr::filter(g2, tactical_role != input$roster_role)
        )
      }
      g2[idx[[1]], ]
    } else {
      g[1, ]
    }
    role <- row$suggested_role %||% row$tactical_role
    if (!is.null(role) && !is.na(role) && role %in% names(role_choices)) {
      updateSelectInput(session, "role", selected = role)
      updateSelectInput(session, "roster_role", selected = role)
      updateSelectInput(session, "cmp_role", selected = role)
    }
    pos <- row$position_group
    if (!is.null(pos) && !is.na(pos) && pos %in% unname(POSITION_CHOICES)) {
      updateSelectInput(session, "roster_position", selected = pos)
      updateSelectInput(session, "cmp_position", selected = pos)
    }
    updateSelectInput(session, "club", selected = input$roster_club)
    showNotification(
      sprintf(
        "Applied %s / %s need (score %.0f, %s) as recruitment setup — override anytime. Not a replace order.",
        row$position_group, row$tactical_role, row$need_score, row$priority
      ),
      type = "message", duration = 6
    )
  })

  # Incumbent choices for selected club/role/position
  observe({
    club_id <- input$cmp_club %||% "sanjose"
    role <- input$cmp_role %||% "pressing_striker"
    pos <- input$cmp_position %||% "any"
    inc <- roster_for_role(players_active(), club_id, role, position = pos, team_map = team_map)
    # If role filter empty, fall back to full club roster at position
    if (!nrow(inc)) {
      inc <- club_roster_players(players_active(), club_id, team_map)
      if (!identical(pos, "any")) inc <- dplyr::filter(inc, position_group == pos)
    }
    ch <- setNames(inc$asa_player_id, paste0(inc$display_name, " (", inc$position_group, ", ",
                                             round(inc$minutes), "′)"))
    updateSelectizeInput(session, "cmp_incumbents", choices = ch, server = TRUE)
  })

  observe({
    if (isTRUE(input$cmp_target_from_shortlist)) {
      sl <- active_shortlist()
      req(sl)
      ch <- setNames(sl$asa_player_id, paste0(sl$display_name, " [shortlist]"))
    } else {
      # External / attainable pool at role (exclude own roster)
      club_id <- input$cmp_club %||% "sanjose"
      role <- input$cmp_role %||% "pressing_striker"
      own_ids <- asa_team_ids_for_club(club_id, team_map)
      pool <- filter_role_pool(players_active(), role) |>
        dplyr::filter(!.data$team_id %in% own_ids) |>
        dplyr::group_by(.data$asa_player_id) |>
        dplyr::slice_max(order_by = .data$score_overall, n = 1, with_ties = FALSE) |>
        dplyr::ungroup() |>
        dplyr::arrange(dplyr::desc(.data$score_overall)) |>
        dplyr::slice_head(n = 200)
      ch <- setNames(pool$asa_player_id, paste0(pool$display_name, " (", toupper(pool$league_id), ")"))
    }
    updateSelectizeInput(session, "cmp_target", choices = ch, server = TRUE)
  })

  comparison_result <- reactiveVal(NULL)

  run_comparison <- function() {
    club <- find_club(input$cmp_club)
    req(club, input$cmp_role, length(input$cmp_incumbents) >= 1, input$cmp_target)
    role <- input$cmp_role
    obj <- input$cmp_objective %||% "upgrade"

    # Score rows at the selected role
    role_pool <- dplyr::filter(players_active(), role_id == role)
    incumbents <- role_pool |>
      dplyr::filter(.data$asa_player_id %in% input$cmp_incumbents) |>
      dplyr::group_by(.data$asa_player_id) |>
      dplyr::slice_head(n = 1) |>
      dplyr::ungroup()
    # If incumbents not in role pool (position mismatch), take any row for that player
    if (!nrow(incumbents)) {
      incumbents <- players_active() |>
        dplyr::filter(.data$asa_player_id %in% input$cmp_incumbents) |>
        dplyr::group_by(.data$asa_player_id) |>
        dplyr::slice_max(order_by = .data$minutes, n = 1, with_ties = FALSE) |>
        dplyr::ungroup()
    }
    target <- role_pool |>
      dplyr::filter(.data$asa_player_id == input$cmp_target) |>
      dplyr::slice_head(n = 1)
    if (!nrow(target)) {
      target <- players_active() |>
        dplyr::filter(.data$asa_player_id == input$cmp_target) |>
        dplyr::slice_max(order_by = .data$minutes, n = 1, with_ties = FALSE)
    }
    req(nrow(incumbents) > 0, nrow(target) > 0)

    # Club-condition for compatibility fields
    w <- unlist(cfg$scoring$weights)
    incumbents <- club_conditioned_score(incumbents, club, as.list(w), priority = "balanced",
                                         club_blend = 0.4, apply_soft_filters = FALSE)
    target <- club_conditioned_score(target, club, as.list(w), priority = "balanced",
                                     club_blend = 0.4, apply_soft_filters = FALSE)

    rost <- club_roster_players(players_active(), club$club_id, team_map)
    bundle <- compare_target_to_incumbents(
      incumbents, target[1, , drop = FALSE],
      role_id = role, club = club, objective = obj,
      roles_yaml = roles_yaml, roster_df = rost
    )
    bundle$data_cutoff <- cutoff_label(provenance_r())
    bundle$season_label <- evaluation_period_label(input$eval_period %||% default_period, cfg)
    comparison_result(bundle)
  }

  observeEvent(input$btn_run_cmp, run_comparison())

  pending_roster_cmp <- reactiveVal(NULL)

  observeEvent(input$btn_compare_roster, {
    sl <- active_shortlist()
    req(sl)
    idx <- input$shortlist_table_rows_selected
    target_asa <- if (length(idx)) sl$asa_player_id[[idx[[1]]]] else sl$asa_player_id[[1]]
    pos <- ROLE_POSITION_MAP[[input$role]] %||% "FW"
    pending_roster_cmp(list(
      target = target_asa,
      club = input$club,
      role = input$role,
      position = pos
    ))
    updateSelectInput(session, "cmp_club", selected = input$club)
    updateSelectInput(session, "cmp_role", selected = input$role)
    updateSelectInput(session, "cmp_position", selected = pos)
    updateCheckboxInput(session, "cmp_target_from_shortlist", value = TRUE)
    updateTabsetPanel(session, "main_tabs", selected = "roster_compare")
    showNotification("Opened Current Player vs Target — adjust incumbents/objective, then Run comparison.",
                     type = "message", duration = 5)
  })

  observeEvent(list(input$cmp_incumbents, input$cmp_target, pending_roster_cmp()), {
    pend <- pending_roster_cmp()
    if (is.null(pend)) return()
    updateSelectizeInput(session, "cmp_target", selected = pend$target)
    inc <- roster_for_role(players_active(), pend$club, pend$role,
                           position = pend$position, team_map = team_map)
    if (nrow(inc)) {
      updateSelectizeInput(session, "cmp_incumbents", selected = inc$asa_player_id[[1]])
    }
    pending_roster_cmp(NULL)
  }, ignoreInit = TRUE)

  output$cmp_header <- renderUI({
    bundle <- comparison_result()
    if (is.null(bundle)) {
      return(div(class = "well",
                 "Select club, role, incumbent(s), target, and objective — then Run comparison. ",
                 tags$br(), cutoff_label(provenance_r())))
    }
    div(class = "well",
        tags$strong("Incumbent vs target comparison"),
        span(class = "score-pill", bundle$summary$relationship[[1]]),
        span(class = "cutoff-chip", style = "background:#e8f0ea;color:#0b3d2e;", bundle$data_cutoff),
        tags$div(style = "margin-top:6px;", bundle$season_label),
        if (!identical(input$eval_period %||% default_period, "full_2025")) {
          tags$div(style = "color:#8a1c1c;font-weight:600;margin-top:4px;",
                   "2026 statistics are season-to-date, not full-season.")
        }
    )
  })

  output$cmp_concept_plot <- renderPlot({
    bundle <- comparison_result()
    req(bundle)
    # Average concept scores across selected incumbents
    s <- bundle$summary
    long <- tidyr::pivot_longer(
      s[, c("incumbent", "role_similarity", "upgrade_potential", "complementarity",
            "financial_efficiency", "succession_value")],
      -incumbent, names_to = "concept", values_to = "value"
    )
    long$concept <- dplyr::recode(
      long$concept,
      role_similarity = "Role similarity",
      upgrade_potential = "Upgrade potential",
      complementarity = "Complementarity",
      financial_efficiency = "Financial efficiency",
      succession_value = "Succession value"
    )
    ggplot(long, aes(concept, value, fill = incumbent)) +
      geom_col(position = "dodge", width = 0.7) +
      coord_flip() + ylim(0, 100) +
      labs(title = "Comparison concepts (separate scores)", x = NULL, y = "Score", fill = "Incumbent") +
      theme_minimal(base_size = 12)
  })

  output$cmp_metric_plot <- renderPlot({
    bundle <- comparison_result()
    req(bundle)
    md <- bundle$comparisons[[1]]$metric_delta
    md <- md |>
      dplyr::mutate(metric = reorder(metric, delta)) |>
      dplyr::slice_head(n = 10)
    ggplot(md, aes(metric, delta, fill = delta > 0)) +
      geom_col() + coord_flip() +
      scale_fill_manual(values = c("TRUE" = "#1f6f54", "FALSE" = "#8a1c1c"), guide = "none") +
      labs(title = "Role-adjusted metric deltas (target − incumbent)",
           subtitle = paste(bundle$comparisons[[1]]$target_name, "vs",
                            bundle$comparisons[[1]]$incumbent_name),
           x = NULL, y = "Δ percentile") +
      theme_minimal(base_size = 12)
  })

  output$cmp_summary_table <- DT::renderDT({
    bundle <- comparison_result()
    req(bundle)
    bundle$summary |>
      transmute(
        Incumbent = incumbent,
        Target = target,
        Relationship = relationship,
        `Role sim` = role_similarity,
        Upgrade = upgrade_potential,
        Complement = complementarity,
        `Fin. eff.` = financial_efficiency,
        Succession = succession_value,
        `Δ Near-Term Contrib.` = delta_projected,
        `Δ Role Fit` = delta_role_fit,
        `Δ Development` = delta_development,
        `Δ Model Uncertainty` = delta_risk,
        `Yrs younger` = age_years_younger,
        `Cost tier Δ` = cost_tier_cheaper,
        `Inc age` = incumbent_age,
        `Tgt age` = target_age,
        `Inc min` = incumbent_minutes,
        `Tgt min` = target_minutes,
        `Inc tier` = incumbent_cost_tier,
        `Tgt tier` = target_cost_tier,
        Feasibility = target_feasibility,
        `Model Uncertainty` = target_risk,
        Confidence = target_confidence,
        Pathway = pathway
      )
  }, options = list(scrollX = TRUE, pageLength = 5), rownames = FALSE)

  output$cmp_metric_table <- DT::renderDT({
    bundle <- comparison_result()
    req(bundle)
    rows <- lapply(bundle$comparisons, function(cmp) {
      cmp$metric_delta |>
        dplyr::mutate(
          incumbent_name = cmp$incumbent_name,
          target_name = cmp$target_name,
          .before = 1
        )
    })
    dplyr::bind_rows(rows) |>
      transmute(
        Incumbent = incumbent_name,
        Target = target_name,
        Metric = metric,
        IncumbentPct = round(incumbent, 1),
        TargetPct = round(target, 1),
        Delta = round(delta, 1),
        RoleWeight = round(weight, 3)
      )
  }, options = list(scrollX = TRUE, pageLength = 12), rownames = FALSE)

  output$cmp_narrative <- renderText({
    bundle <- comparison_result()
    req(bundle)
    paste(vapply(bundle$comparisons, `[[`, "", "narrative"), collapse = "\n\n────────\n\n")
  })

  output$cmp_export_controls <- renderUI({
    if (is_synthetic_r()) {
      tagList(
        tags$p(style = "color:#8a1c1c;font-weight:700;",
               "Genuine Excel export blocked while synthetic demo data is loaded."),
        downloadButton("dl_cmp_xlsx_demo", "Download DEMO-ONLY Excel", class = "btn-warning")
      )
    } else {
      downloadButton("dl_cmp_xlsx", "Export Excel comparisons", class = "btn-success")
    }
  })

  output$dl_cmp_xlsx <- downloadHandler(
    filename = function() {
      paste0(
        "roster_compare_", input$cmp_club, "_", input$cmp_role, "_",
        gsub("[^0-9]", "", provenance_r()$data_cutoff_utc %||% format(Sys.time(), "%Y%m%d%H%M")),
        ".xlsx"
      )
    },
    content = function(file) {
      if (is_synthetic_r()) stop("Genuine export blocked: synthetic demo data is loaded.")
      bundle <- comparison_result()
      req(bundle)
      export_roster_comparisons_xlsx(bundle, file, provenance_r())
    }
  )

  output$dl_cmp_xlsx_demo <- downloadHandler(
    filename = function() paste0("DEMO_ONLY_roster_compare_", Sys.Date(), ".xlsx"),
    content = function(file) {
      bundle <- comparison_result()
      req(bundle)
      bundle$summary$target <- paste0("[DEMO] ", bundle$summary$target)
      bundle$summary$narrative <- paste("SYNTHETIC DEMO — NOT A GENUINE SCOUTING REPORT.\n",
                                        bundle$summary$narrative)
      export_roster_comparisons_xlsx(bundle, file, provenance_r())
    }
  )
}

shinyApp(ui, server)
