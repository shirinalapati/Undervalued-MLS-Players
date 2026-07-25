# 2026 MLS Value Index — Shiny dashboard
# Launch: shiny::runApp("app", port = 7788)

library(shiny)
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(plotly)

app_dir <- normalizePath(getwd())
root <- if (basename(app_dir) == "app") dirname(app_dir) else app_dir
setwd(root)

source("R/utilities/load_project.R")
source("R/utilities/data_provenance.R")
source("R/models/value_index.R")
source("R/ui/about_value_index.R")
source("R/exports/value_excel.R")

cfg <- load_config()
vi_cfg <- load_value_index_config()
`%||%` <- function(a, b) if (!is.null(a)) a else b

load_period_scores <- function(period) {
  path <- file.path(cfg$paths$processed, paste0("player_value_scores_", period, ".csv"))
  if (!file.exists(path)) {
    path <- file.path(cfg$paths$processed, "player_value_scores.csv")
  }
  if (!file.exists(path)) {
    stop("Run scripts/06_generate_value_index.R before launching the app.")
  }
  readr::read_csv(path, show_col_types = FALSE)
}

data_files_stamp <- function() {
  paths <- c(
    cfg$paths$provenance,
    file.path(cfg$paths$processed, "player_value_scores_blended.csv"),
    file.path(cfg$paths$processed, "player_value_scores_ytd_2026.csv"),
    file.path(cfg$paths$processed, "player_value_scores_full_2025.csv")
  )
  infos <- file.info(paths)
  paste(na.omit(as.character(infos$mtime)), collapse = "|")
}

load_live_data_bundle <- function() {
  by_period <- list(
    blended = tryCatch(load_period_scores("blended"), error = function(e) NULL),
    ytd_2026 = tryCatch(load_period_scores("ytd_2026"), error = function(e) NULL),
    full_2025 = tryCatch(load_period_scores("full_2025"), error = function(e) NULL)
  )
  if (all(vapply(by_period, is.null, logical(1)))) {
    stop("No Value Index score files found in data/processed/.")
  }
  sample_df <- by_period$blended %||% by_period$ytd_2026 %||% by_period$full_2025
  if (isTRUE(sample_df$is_synthetic[[1]]) && identical(cfg$project$mode, "live")) {
    stop("Live mode refuses synthetic Value Index data.")
  }
  prov <- reconcile_provenance_with_data(cfg, sample_df)
  list(
    provenance = prov,
    players_by_period = by_period,
    stamp = data_files_stamp(),
    loaded_at = Sys.time()
  )
}

live_bundle <- load_live_data_bundle()
auto_reload <- isTRUE(cfg$deployment$auto_reload %||% TRUE)
refresh_check_sec <- as.integer(cfg$deployment$refresh_check_seconds %||% 60)

fmt_money <- function(x) {
  ifelse(is.finite(x), paste0("$", format(round(x), big.mark = ",", scientific = FALSE)), "—")
}

fmt_num <- function(x, digits = 1) {
  ifelse(is.finite(x), format(round(x, digits), nsmall = digits), "—")
}

ui <- fluidPage(
  tags$head(
    tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Fraunces:opsz,wght@9..144,600;9..144,700&family=IBM+Plex+Sans:wght@400;500;600&display=swap"
    ),
    tags$link(rel = "stylesheet", type = "text/css", href = "value_index.css")
  ),
  div(
    class = "vi-banner",
    h1("2026 MLS Value Index"),
    p(class = "subtitle", "Identifying Undervalued Players in Major League Soccer"),
    uiOutput("banner_meta")
  ),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      class = "sidebar-panel",
      selectInput(
        "period", "Evaluation period",
        choices = c(
          "2025+2026 season to date" = "blended",
          "2026 season-to-date" = "ytd_2026",
          "2025 full season" = "full_2025"
        ),
        selected = cfg$project$evaluation_period_default %||% "blended"
      ),
      selectInput("position", "Position group", choices = c("All" = "all")),
      selectInput("club", "Club", choices = c("All" = "all")),
      sliderInput("age_range", "Age", 15, 45, c(15, 45), step = 1),
      sliderInput("min_minutes", "Minimum 2026 minutes", 0, 2500, 450, step = 50),
      numericInput("max_comp", "Maximum compensation (USD)", value = NA, min = 0, step = 50000),
      checkboxGroupInput(
        "confidence", "Data Confidence",
        choices = c("High", "Medium", "Low", "Insufficient"),
        selected = c("High", "Medium", "Low")
      ),
      checkboxGroupInput(
        "labels", "Value Label",
        choices = c(
          "Elite Value", "Strong Value", "Undervalued", "Fair Value",
          "Below Expected Value by Current Model", "Small-Sample Watchlist", "Insufficient Evidence"
        ),
        selected = c(
          "Elite Value", "Strong Value", "Undervalued", "Fair Value",
          "Below Expected Value by Current Model"
        )
      ),
      tags$p(class = "help-text", "Official undervaluation ranks require ≥450 2026 minutes and known compensation."),
      downloadButton("dl_excel", "Download Excel workbook"),
      downloadButton("dl_csv", "Download filtered CSV")
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        id = "main_tabs",
        tabPanel("About & Methodology", uiOutput("about_page")),
        tabPanel(
          "MLS Value Rankings",
          div(
            class = "section-card",
            uiOutput("rankings_help"),
            DT::DTOutput("rankings_table"),
            tags$hr(),
            tags$details(
              tags$summary("Players excluded because compensation is unavailable"),
              DT::DTOutput("missing_comp_table")
            )
          )
        ),
        tabPanel(
          "Position Rankings",
          div(
            class = "section-card",
            selectInput("pos_focus", "Position board", choices = NULL),
            tags$p(
              class = "help-text",
              "Sidebar filters (club, age, minutes, compensation, confidence, value label) apply here. The Position board chooses the group; the sidebar Position filter is ignored on this tab."
            ),
            fluidRow(
              column(6, h4("Top Value Surplus"), DT::DTOutput("pos_surplus")),
              column(6, h4("Top Sporting Impact"), DT::DTOutput("pos_impact"))
            ),
            fluidRow(
              column(6, h4("Low pay, above-average impact"), DT::DTOutput("pos_cheap")),
              column(6, h4("High pay, below-median impact"), DT::DTOutput("pos_expensive"))
            ),
            fluidRow(
              column(6, h4("Young value (under 24)"), DT::DTOutput("pos_young")),
              column(6, h4("Veteran value (28+)"), DT::DTOutput("pos_vet"))
            ),
            plotOutput("pos_surplus_dist", height = 280),
            uiOutput("pos_surplus_note")
          )
        ),
        tabPanel(
          "Team Value",
          div(
            class = "section-card",
            selectInput("team_focus", "MLS club", choices = NULL),
            tags$p(
              class = "help-text",
              "Sidebar filters (age, minutes, compensation, confidence, value label) apply to the plotted roster. The club selector above chooses the team; the sidebar Club filter is ignored on this tab."
            ),
            fluidRow(
              column(4, uiOutput("team_kpis")),
              column(
                8,
                plotOutput(
                  "team_scatter",
                  height = 360,
                  click = "team_scatter_click",
                  hover = hoverOpts(
                    id = "team_scatter_hover",
                    delay = 10,
                    delayType = "throttle",
                    nullOutside = FALSE
                  )
                ),
                uiOutput("team_scatter_tooltip"),
                tags$p(
                  class = "help-text",
                  "Each dot is a club player with known 2026 guaranteed compensation and a Sporting Impact score. Players missing pay data are listed below the chart and are not plotted. Hover a dot for the name (it stays until you hover another player); click to open the Player Profile."
                )
              )
            ),
            uiOutput("team_missing_pay"),
            fluidRow(
              column(6, h4("Most undervalued"), DT::DTOutput("team_undervalued")),
              column(6, h4("Strongest Sporting Impact"), DT::DTOutput("team_impact"))
            ),
            plotOutput("team_salary_alloc", height = 280),
            tags$p(class = "help-text", "Below expected value according to the current public-data model — not an objective overpaid judgment.")
          )
        ),
        tabPanel(
          "Player Profile",
          div(
            class = "section-card",
            selectInput("player", "Player", choices = NULL),
            tags$p(
              class = "help-text",
              "Sidebar filters narrow the player list. Evaluation period still changes the scores shown for the selected player."
            ),
            uiOutput("player_header"),
            h4("Why the player ranks here"),
            uiOutput("player_why"),
            h4("Metric breakdown (explanatory Goals Added components)"),
            DT::DTOutput("player_metrics"),
            h4("Compensation context"),
            uiOutput("player_comp"),
            h4("Trend"),
            plotOutput("player_trend", height = 260),
            h4("Caveats"),
            uiOutput("player_caveats"),
            plotOutput("player_scatter_context", height = 300)
          )
        ),
        tabPanel(
          "Compare Players",
          div(
            class = "section-card",
            selectizeInput("compare_players", "Select 2–4 players", choices = NULL, multiple = TRUE, options = list(maxItems = 4)),
            tags$p(
              class = "help-text",
              "Sidebar filters narrow who you can add to the comparison."
            ),
            uiOutput("compare_warning"),
            DT::DTOutput("compare_table"),
            tags$p(
              class = "help-text",
              "Grouped bars compare 0–100 Index fields. Model Confidence = 100 − Model Uncertainty (higher is more stable evidence). Value Surplus is shown separately because it can be negative."
            ),
            plotlyOutput("compare_index_chart", height = "420px"),
            plotlyOutput("compare_surplus_chart", height = "300px"),
            uiOutput("compare_legend")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  bundle <- reactiveVal(live_bundle)

  if (auto_reload) {
    observe({
      invalidateLater(refresh_check_sec * 1000)
      stamp <- data_files_stamp()
      if (!identical(stamp, bundle()$stamp)) {
        tryCatch({
          bundle(load_live_data_bundle())
          showNotification("Value Index data refreshed.", type = "message")
        }, error = function(e) {
          showNotification(paste("Data refresh failed:", e$message), type = "error")
        })
      }
    })
  }

  players_raw <- reactive({
    b <- bundle()
    period <- input$period %||% "blended"
    df <- b$players_by_period[[period]]
    if (is.null(df)) df <- b$players_by_period$blended
    df
  })

  apply_sidebar_filters <- function(df, use_sidebar_position = TRUE) {
    if (isTRUE(use_sidebar_position) &&
        !identical(input$position, "all") &&
        !is.null(input$position)) {
      df <- df |> dplyr::filter(position_label == input$position)
    }
    if (!identical(input$club, "all") && !is.null(input$club)) {
      df <- df |> dplyr::filter(club == input$club)
    }
    conf <- input$confidence
    labs <- input$labels
    if (is.null(conf) || !length(conf)) conf <- unique(df$data_confidence)
    if (is.null(labs) || !length(labs)) labs <- unique(df$value_label)
    df <- df |>
      dplyr::filter(
        # Integer slider bounds are inclusive of fractional ages in [min, max+1)
        is.finite(age),
        age >= input$age_range[[1]],
        age < input$age_range[[2]] + 1,
        minutes_2026 >= input$min_minutes,
        data_confidence %in% conf,
        value_label %in% labs
      )
    if (is.finite(input$max_comp)) {
      df <- df |> dplyr::filter(!compensation_known | compensation <= input$max_comp)
    }
    df
  }

  filtered <- reactive({
    apply_sidebar_filters(players_raw(), use_sidebar_position = TRUE)
  })

  filtered_player_names <- reactive({
    sort(unique(filtered()$display_name))
  })

  observe({
    df <- players_raw()
    pos <- sort(unique(df$position_label))
    clubs <- sort(unique(df$club[df$official_eligible %in% TRUE | df$compensation_known %in% TRUE]))
    updateSelectInput(session, "position", choices = c("All" = "all", setNames(pos, pos)))
    updateSelectInput(session, "club", choices = c("All" = "all", setNames(clubs, clubs)))
    # Keep age slider wide enough for every scored player
    ages <- df$age[is.finite(df$age)]
    if (length(ages)) {
      age_min <- max(15L, as.integer(floor(min(ages))))
      age_max <- max(age_min + 1L, as.integer(ceiling(max(ages))))
      updateSliderInput(session, "age_range", min = age_min, max = age_max, step = 1)
    }
    updateSelectInput(
      session, "pos_focus",
      choices = setNames(pos, pos),
      selected = if (!is.null(isolate(input$pos_focus)) && isolate(input$pos_focus) %in% pos) {
        isolate(input$pos_focus)
      } else {
        pos[[1]]
      }
    )
    updateSelectInput(
      session, "team_focus",
      choices = setNames(clubs, clubs),
      selected = if (!is.null(isolate(input$team_focus)) && isolate(input$team_focus) %in% clubs) {
        isolate(input$team_focus)
      } else {
        clubs[[1]]
      }
    )
  })

  # Update Profile/Compare choice lists only when the filtered name set changes.
  # Do NOT depend on compare_players selection — that caused selectize to reset/crash.
  observeEvent(
    filtered_player_names(),
    {
      names <- filtered_player_names()
      if (!length(names)) {
        updateSelectInput(session, "player", choices = character(0))
        updateSelectizeInput(
          session, "compare_players",
          choices = character(0), selected = character(0), server = TRUE
        )
        return()
      }

      cur_player <- isolate(input$player)
      if (is.null(cur_player) || !cur_player %in% names) cur_player <- names[[1]]
      updateSelectInput(session, "player", choices = names, selected = cur_player)

      keep_compare <- intersect(isolate(input$compare_players) %||% character(0), names)
      updateSelectizeInput(
        session, "compare_players",
        choices = names,
        selected = keep_compare,
        server = TRUE
      )
    },
    ignoreNULL = FALSE
  )

  official_filtered <- reactive({
    filtered() |>
      dplyr::filter(official_eligible) |>
      dplyr::arrange(display_rank, dplyr::desc(undervaluation_score), dplyr::desc(value_surplus))
  })

  production_counts <- reactive({
    value_index_counts(players_raw())
  })

  output$banner_meta <- renderUI({
    b <- bundle()
    df <- players_raw()
    counts <- production_counts()
    p(
      class = "vi-meta",
      sprintf(
        "%s · Players evaluated: %s · Official eligible: %s · %s",
        unique(df$season_label)[[1]] %||% input$period,
        format(counts$n_players_evaluated, big.mark = ","),
        format(counts$n_official_eligible, big.mark = ","),
        cutoff_label(b$provenance, cfg = cfg)
      )
    )
  })

  output$about_page <- renderUI({
    counts <- production_counts()
    render_about_value_index(
      bundle()$provenance, vi_cfg,
      n_official = counts$n_official_eligible,
      n_players = counts$n_players_evaluated
    )
  })

  output$rankings_help <- renderUI({
    counts <- production_counts()
    n_show <- nrow(official_filtered())
    tags$p(
      class = "help-text",
      sprintf(
        "Showing %s of %s official eligible players (MLS only, known compensation). Display Rank is sequential across the full official pool and does not reset by position. Scores are fixed for the evaluation period and do not change with filters.",
        format(n_show, big.mark = ","),
        format(counts$n_official_eligible, big.mark = ",")
      )
    )
  })

  rankings_df <- reactive({
    official_filtered() |>
      dplyr::transmute(
        `Display Rank` = display_rank,
        `Position Rank` = dplyr::coalesce(position_rank, undervaluation_rank),
        Player = display_name,
        Club = club,
        Position = position_label,
        Age = round(age, 1),
        `2026 Minutes` = round(minutes_2026),
        `Guaranteed Compensation` = compensation,
        `Sporting Impact` = round(sporting_impact, 1),
        `Compensation Percentile` = round(compensation_percentile, 1),
        `Value Surplus` = round(value_surplus, 1),
        `Undervaluation Score` = round(undervaluation_score, 1),
        `Data Confidence` = data_confidence,
        `Value Label` = value_label
      )
  })

  output$rankings_table <- DT::renderDT({
    DT::datatable(
      rankings_df(),
      selection = "single",
      rownames = FALSE,
      options = list(pageLength = 25, scrollX = TRUE)
    ) |>
      DT::formatCurrency("Guaranteed Compensation", digits = 0)
  })

  observeEvent(input$rankings_table_rows_selected, {
    idx <- input$rankings_table_rows_selected
    if (length(idx)) {
      nm <- rankings_df()$Player[[idx]]
      updateSelectInput(session, "player", selected = nm)
      updateTabsetPanel(session, "main_tabs", selected = "Player Profile")
    }
  })

  output$missing_comp_table <- DT::renderDT({
    df <- players_raw() |>
      dplyr::filter(!compensation_known) |>
      dplyr::transmute(
        Player = display_name, Club = club, Position = position_label,
        Age = round(age, 1), `2026 Minutes` = round(minutes_2026),
        `Sporting Impact` = round(sporting_impact, 1)
      )
    DT::datatable(df, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
  })

  pos_pool <- reactive({
    req(input$pos_focus)
    apply_sidebar_filters(players_raw(), use_sidebar_position = FALSE) |>
      dplyr::filter(official_eligible, position_label == input$pos_focus)
  })

  slim_pos <- function(df, n = 10) {
    df |>
      dplyr::slice_head(n = n) |>
      dplyr::transmute(
        `Position Rank` = dplyr::coalesce(position_rank, undervaluation_rank),
        Player = display_name,
        Club = club,
        `Sporting Impact` = round(sporting_impact, 1),
        `Compensation Percentile` = round(compensation_percentile, 1),
        `Value Surplus` = round(value_surplus, 1),
        `Undervaluation Score` = round(undervaluation_score, 1)
      )
  }

  output$pos_surplus <- DT::renderDT({
    DT::datatable(slim_pos(pos_pool() |> dplyr::arrange(dplyr::desc(value_surplus))), rownames = FALSE, options = list(dom = "t"))
  })
  output$pos_impact <- DT::renderDT({
    DT::datatable(slim_pos(pos_pool() |> dplyr::arrange(dplyr::desc(sporting_impact))), rownames = FALSE, options = list(dom = "t"))
  })
  output$pos_cheap <- DT::renderDT({
    DT::datatable(
      slim_pos(pos_pool() |> dplyr::filter(sporting_impact >= 55) |> dplyr::arrange(compensation_percentile)),
      rownames = FALSE, options = list(dom = "t")
    )
  })
  output$pos_expensive <- DT::renderDT({
    med <- median(pos_pool()$sporting_impact, na.rm = TRUE)
    DT::datatable(
      slim_pos(pos_pool() |> dplyr::filter(sporting_impact < med) |> dplyr::arrange(dplyr::desc(compensation_percentile))),
      rownames = FALSE, options = list(dom = "t")
    )
  })
  output$pos_young <- DT::renderDT({
    DT::datatable(
      slim_pos(pos_pool() |> dplyr::filter(age < 24) |> dplyr::arrange(dplyr::desc(undervaluation_score))),
      rownames = FALSE, options = list(dom = "t")
    )
  })
  output$pos_vet <- DT::renderDT({
    DT::datatable(
      slim_pos(pos_pool() |> dplyr::filter(age >= 28) |> dplyr::arrange(dplyr::desc(undervaluation_score))),
      rownames = FALSE, options = list(dom = "t")
    )
  })

  output$pos_surplus_dist <- renderPlot({
    df <- pos_pool()
    ggplot(df, aes(value_surplus)) +
      geom_histogram(bins = 20, fill = "#2d6a4f", color = "white") +
      geom_vline(xintercept = 0, linetype = 2, color = "#666") +
      labs(
        title = paste("Value Surplus distribution —", input$pos_focus),
        x = "Value Surplus", y = "Players"
      ) +
      theme_minimal(base_family = "IBM Plex Sans")
  })

  output$pos_surplus_note <- renderUI({
    req(input$pos_focus)
    df <- pos_pool()
    n <- nrow(df)
    if (!n || !any(is.finite(df$value_surplus))) {
      return(tags$p(class = "help-text", "Not enough eligible players to summarize this distribution."))
    }
    vs <- df$value_surplus[is.finite(df$value_surplus)]
    n_pos <- sum(vs > 0)
    n_neg <- sum(vs < 0)
    n_near <- sum(abs(vs) <= 15)
    med <- round(stats::median(vs), 1)
    share_pos <- round(100 * n_pos / length(vs))
    share_neg <- round(100 * n_neg / length(vs))

    notes <- list(
      "Strikers" = "Most MLS strikers cluster near zero when impact and pay standings are similar; a long right tail would mean a few high-impact, lower-paid strikers, while mass on the left means many strikers are paid above their current modeled impact.",
      "Wingers and attacking midfielders" = "Tall bars show where most wide/AM players sit on the surplus scale. A pile-up around zero means pay and impact are usually aligned in this group; skew right means more creators look cheap relative to impact, skew left means more look expensive.",
      "Central and defensive midfielders" = "Bar height is the count of eligible midfielders in each surplus band. Use it to see whether this position group is mostly balanced (center mass near zero), value-heavy (more players on the right), or pay-heavy (more players on the left).",
      "Fullbacks and wingbacks" = "This is a headcount histogram: each bar is how many fullbacks fall in that Value Surplus range. Shape tells you whether surplus is evenly spread or concentrated—e.g. a left-heavy chart means many fullbacks currently sit below expected value on pay vs impact.",
      "Center backs" = "Each bar counts how many center backs share a similar Value Surplus. Compare the mass left of zero vs right of zero to see whether this CB pool is mostly overcompensated relative to modeled impact, balanced, or rich in compensation-efficient CBs."
    )
    role_note <- notes[[input$pos_focus]] %||%
      "Each bar counts how many eligible players fall in that Value Surplus range."

    tags$div(
      class = "help-text",
      style = "margin-top:0.75rem;",
      tags$p(HTML(paste0(
        "<strong>What this chart shows — ", input$pos_focus, ":</strong> ",
        "It is a histogram of eligible players in this position group. ",
        "The x-axis is Value Surplus; the y-axis is how many players fall in each band. ",
        role_note
      ))),
      tags$p(HTML(paste0(
        "<strong>In this view:</strong> ",
        format(n, big.mark = ","), " eligible players · ",
        "median surplus ", med, " · ",
        share_pos, "% above zero · ",
        share_neg, "% below zero · ",
        n_near, " within ±15 of aligned pay/impact."
      )))
    )
  })

  team_club_players <- reactive({
    req(input$team_focus)
    # Team selector wins over sidebar Club; other sidebar filters apply
    apply_sidebar_filters(players_raw(), use_sidebar_position = TRUE) |>
      dplyr::filter(club == input$team_focus)
  })

  team_pool <- reactive({
    # Plottable dots: need both pay (x) and Sporting Impact (y)
    team_club_players() |>
      dplyr::filter(
        compensation_known,
        is.finite(compensation_percentile),
        is.finite(sporting_impact)
      )
  })

  output$team_kpis <- renderUI({
    all_p <- team_club_players()
    df <- team_pool()
    tagList(
      tags$p(HTML(paste0("<strong>Club players in data:</strong> ", nrow(all_p)))),
      tags$p(HTML(paste0("<strong>Plotted (known pay + impact):</strong> ", nrow(df)))),
      tags$p(HTML(paste0("<strong>Team median Sporting Impact:</strong> ", fmt_num(median(df$sporting_impact, na.rm = TRUE))))),
      tags$p(HTML(paste0("<strong>Team median Compensation Percentile:</strong> ", fmt_num(median(df$compensation_percentile, na.rm = TRUE))))),
      tags$p(HTML(paste0("<strong>Total known guaranteed compensation:</strong> ", fmt_money(sum(df$compensation, na.rm = TRUE)))))
    )
  })

  # Sticky hover name — update when near a point; do not clear on tiny mouse moves
  team_hover_name <- reactiveVal(NULL)
  observeEvent(input$team_focus, team_hover_name(NULL), ignoreInit = TRUE)

  observe({
    hover <- input$team_scatter_hover
    df <- team_pool()
    if (is.null(hover) || !nrow(df)) return()
    hit <- nearPoints(
      df, hover,
      xvar = "compensation_percentile", yvar = "sporting_impact",
      threshold = 18, maxpoints = 1
    )
    if (nrow(hit)) team_hover_name(hit$display_name[[1]])
  })

  output$team_scatter <- renderPlot({
    df <- team_pool()
    # Plot itself does not depend on hover (avoids flicker / disappearing labels)
    ggplot(df, aes(compensation_percentile, sporting_impact)) +
      geom_abline(slope = 1, intercept = 0, linetype = 2, color = "#888") +
      geom_point(aes(color = value_label), size = 4, alpha = 0.9) +
      labs(
        title = paste(input$team_focus, "— Sporting Impact vs Compensation"),
        x = "Compensation Percentile", y = "Sporting Impact", color = NULL
      ) +
      coord_equal(xlim = c(0, 100), ylim = c(0, 100)) +
      theme_minimal(base_family = "IBM Plex Sans")
  })

  output$team_scatter_tooltip <- renderUI({
    nm <- team_hover_name()
    if (is.null(nm) || !nzchar(nm)) {
      return(tags$div(
        class = "vi-hover-chip",
        tags$span("Hover a dot to see the player name")
      ))
    }
    df <- team_pool()
    hit <- df[df$display_name == nm, , drop = FALSE]
    if (!nrow(hit)) {
      return(tags$div(class = "vi-hover-chip", tags$span("Hover a dot to see the player name")))
    }
    hit <- hit[1, ]
    tags$div(
      class = "vi-hover-chip vi-hover-chip-active",
      tags$strong(hit$display_name),
      tags$span(paste0(
        " · ", hit$position_label, " · ", hit$value_label,
        " · Impact ", fmt_num(hit$sporting_impact),
        " · Compensation Percentile ", fmt_num(hit$compensation_percentile),
        " · Value Surplus ", fmt_num(hit$value_surplus)
      ))
    )
  })

  observeEvent(input$team_scatter_click, {
    df <- team_pool()
    hit <- nearPoints(
      df, input$team_scatter_click,
      xvar = "compensation_percentile", yvar = "sporting_impact",
      threshold = 18, maxpoints = 1
    )
    if (!nrow(hit)) return()
    team_hover_name(hit$display_name[[1]])
    updateSelectInput(session, "player", selected = hit$display_name[[1]])
    updateTabsetPanel(session, "main_tabs", selected = "Player Profile")
  })

  output$team_missing_pay <- renderUI({
    missing <- team_club_players() |> dplyr::filter(!compensation_known)
    if (!nrow(missing)) return(NULL)
    tags$details(
      tags$summary(sprintf(
        "Club players not plotted (missing guaranteed compensation): %s",
        nrow(missing)
      )),
      tags$ul(
        lapply(missing$display_name, tags$li)
      )
    )
  })

  output$team_undervalued <- DT::renderDT({
    DT::datatable(
      slim_pos(team_pool() |> dplyr::filter(official_eligible) |> dplyr::arrange(dplyr::desc(undervaluation_score))),
      rownames = FALSE, options = list(dom = "t", pageLength = 8)
    )
  })
  output$team_impact <- DT::renderDT({
    DT::datatable(
      slim_pos(team_pool() |> dplyr::arrange(dplyr::desc(sporting_impact))),
      rownames = FALSE, options = list(dom = "t", pageLength = 8)
    )
  })

  output$team_salary_alloc <- renderPlot({
    df <- team_pool() |>
      dplyr::group_by(position_label) |>
      dplyr::summarise(compensation = sum(compensation, na.rm = TRUE), .groups = "drop")
    ggplot(df, aes(reorder(position_label, compensation), compensation)) +
      geom_col(fill = "#1b4332") +
      coord_flip() +
      scale_y_continuous(labels = scales::dollar) +
      labs(title = "Known guaranteed compensation by position", x = NULL, y = NULL) +
      theme_minimal(base_family = "IBM Plex Sans")
  })

  selected_player <- reactive({
    req(input$player)
    players_raw() |> dplyr::filter(display_name == input$player) |> dplyr::slice(1)
  })

  # Rank within position group among peers with a finite value (1 = highest).
  # Official eligible pool when available; otherwise all positional peers.
  fmt_pos_rank <- function(player_row, col, peers_df, higher_is_better = TRUE) {
    x <- player_row[[col]][[1]]
    if (!is.finite(x)) return("—")
    pg <- player_row$position_group[[1]]
    pool <- peers_df |>
      dplyr::filter(
        .data$position_group == pg,
        is.finite(.data[[col]])
      )
    if (isTRUE(any(peers_df$official_eligible %in% TRUE)) &&
        col %in% c(
          "sporting_impact", "compensation_percentile", "value_surplus",
          "undervaluation_score", "compensation"
        )) {
      elig_pool <- pool |> dplyr::filter(official_eligible %in% TRUE)
      if (nrow(elig_pool)) pool <- elig_pool
    }
    if (!nrow(pool)) return("—")
    vals <- pool[[col]]
    ranks <- if (isTRUE(higher_is_better)) {
      rank(-vals, ties.method = "min")
    } else {
      rank(vals, ties.method = "min")
    }
    hit <- which(pool$asa_player_id == player_row$asa_player_id[[1]])
    if (!length(hit)) {
      # fallback match by display name if id missing
      hit <- which(pool$display_name == player_row$display_name[[1]])
    }
    if (!length(hit)) return("—")
    sprintf("%s of %s in position", format(ranks[[hit[[1]]]], big.mark = ","), format(nrow(pool), big.mark = ","))
  }

  output$player_header <- renderUI({
    p <- selected_player()
    peers <- players_raw()
    conf <- dplyr::coalesce(p$model_confidence, 100 - p$model_uncertainty)
    # temporary column for ranking model confidence consistently
    peers$model_confidence_rankval <- dplyr::coalesce(peers$model_confidence, 100 - peers$model_uncertainty)
    p$model_confidence_rankval <- conf

    tagList(
      h3(p$display_name),
      p(sprintf("%s · %s · Age %.1f · %s 2026 minutes", p$club, p$position_label, round(p$age, 1), round(p$minutes_2026))),
      tags$span(class = "value-chip", p$value_label),
      tags$ul(
        tags$li(HTML(paste0(
          "<strong>Sporting Impact:</strong> ", fmt_num(p$sporting_impact),
          " <span class=\"help-text\">(", fmt_pos_rank(p, "sporting_impact", peers), ")</span>"
        ))),
        tags$li(HTML(paste0(
          "<strong>Compensation Percentile:</strong> ", fmt_num(p$compensation_percentile),
          " <span class=\"help-text\">(", fmt_pos_rank(p, "compensation_percentile", peers), ")</span>"
        ))),
        tags$li(HTML(paste0(
          "<strong>Value Surplus:</strong> ", fmt_num(p$value_surplus),
          " <span class=\"help-text\">(", fmt_pos_rank(p, "value_surplus", peers), ")</span>"
        ))),
        tags$li(HTML(paste0(
          "<strong>Undervaluation Score:</strong> ", fmt_num(p$undervaluation_score),
          " <span class=\"help-text\">(", fmt_pos_rank(p, "undervaluation_score", peers), ")</span>"
        ))),
        tags$li(HTML(paste0(
          "<strong>Display Rank / Position Rank:</strong> ",
          ifelse(is.finite(p$display_rank), p$display_rank, "—"),
          " / ",
          ifelse(is.finite(dplyr::coalesce(p$position_rank, p$undervaluation_rank)),
                 dplyr::coalesce(p$position_rank, p$undervaluation_rank), "—")
        ))),
        tags$li(HTML(paste0("<strong>Data Confidence:</strong> ", p$data_confidence))),
        tags$li(HTML(paste0(
          "<strong>Model Confidence:</strong> ", fmt_num(conf),
          " <span class=\"help-text\">(", fmt_pos_rank(p, "model_confidence_rankval", peers), ")</span>"
        ))),
        tags$li(HTML(paste0(
          "<strong>Guaranteed compensation:</strong> ", fmt_money(p$compensation),
          " <span class=\"help-text\">(", fmt_pos_rank(p, "compensation", peers), ")</span>"
        )))
      )
    )
  })

  output$player_why <- renderUI({
    p <- selected_player()
    comps <- c(
      Shooting = p$pct_goals_added_shooting,
      Passing = p$pct_goals_added_passing,
      Receiving = p$pct_goals_added_receiving,
      Dribbling = p$pct_goals_added_dribbling,
      Interrupting = p$pct_goals_added_defending,
      Fouling = p$pct_goals_added_fouling
    )
    comps <- comps[is.finite(comps)]
    strong <- if (length(comps)) names(sort(comps, decreasing = TRUE))[1:min(2, length(comps))] else character()
    weak <- if (length(comps)) names(sort(comps, decreasing = FALSE))[1:min(2, length(comps))] else character()
    tagList(
      p(sprintf(
        "%s posts a Sporting Impact of %.0f with a compensation standing of %.0f in the %s group (Value Surplus %.0f).",
        p$display_name, p$sporting_impact, p$compensation_percentile, p$position_label, p$value_surplus
      )),
      p(sprintf("Strongest explanatory traits: %s. Softest traits: %s.",
                paste(strong, collapse = ", "), paste(weak, collapse = ", "))),
      p(sprintf(
        "Sample reliability: %s Data Confidence using %.0f minutes in 2026%s.",
        p$data_confidence, p$minutes_2026,
        if (isTRUE(p$has_2025_prior)) " with a 2025 prior" else " without a 2025 prior"
      ))
    )
  })

  output$player_metrics <- DT::renderDT({
    p <- selected_player()
    p96 <- vi_cfg$shrinkage$p96_factor %||% (96 / 90)
    rate_p96 <- function(p96_val, p90_val) {
      if (is.finite(p96_val)) return(p96_val)
      if (is.finite(p90_val)) return(p90_val * p96)
      NA_real_
    }
    df <- data.frame(
      Component = c("Shooting", "Passing", "Receiving", "Dribbling", "Interrupting", "Fouling"),
      `Goals Added per 96` = round(c(
        rate_p96(p$goals_added_shooting_p96, p$goals_added_shooting_p90),
        rate_p96(p$goals_added_passing_p96, p$goals_added_passing_p90),
        rate_p96(p$goals_added_receiving_p96, p$goals_added_receiving_p90),
        rate_p96(p$goals_added_dribbling_p96, p$goals_added_dribbling_p90),
        rate_p96(p$goals_added_defending_p96, p$goals_added_defending_p90),
        rate_p96(p$goals_added_fouling_p96, p$goals_added_fouling_p90)
      ), 2),
      `Position percentile` = round(c(
        p$pct_goals_added_shooting, p$pct_goals_added_passing, p$pct_goals_added_receiving,
        p$pct_goals_added_dribbling, p$pct_goals_added_defending, p$pct_goals_added_fouling
      ), 1),
      check.names = FALSE
    )
    DT::datatable(df, rownames = FALSE, options = list(dom = "t"))
  })

  output$player_comp <- renderUI({
    p <- selected_player()
    tagList(
      tags$ul(
        tags$li(HTML(paste0("<strong>Player:</strong> ", fmt_money(p$compensation)))),
        tags$li(HTML(paste0("<strong>Position median:</strong> ", fmt_money(p$position_median_compensation)))),
        tags$li(HTML(paste0("<strong>Position compensation percentile:</strong> ", fmt_num(p$compensation_percentile)))),
        tags$li(HTML(paste0("<strong>League-wide compensation percentile:</strong> ", fmt_num(p$compensation_percentile_league))))
      )
    )
  })

  output$player_trend <- renderPlot({
    p <- selected_player()
    b <- bundle()
    rows <- lapply(c("full_2025", "ytd_2026", "blended"), function(period) {
      df <- b$players_by_period[[period]]
      if (is.null(df)) return(NULL)
      hit <- df[df$asa_player_id == p$asa_player_id, , drop = FALSE]
      if (!nrow(hit)) return(NULL)
      data.frame(period = period, sporting_impact = hit$sporting_impact[[1]], stringsAsFactors = FALSE)
    })
    plot_df <- dplyr::bind_rows(rows)
    if (!nrow(plot_df)) return(NULL)
    plot_df$period <- factor(plot_df$period, levels = c("full_2025", "ytd_2026", "blended"),
                             labels = c("2025 full", "2026 YTD", "2025+2026"))
    ggplot(plot_df, aes(period, sporting_impact, group = 1)) +
      geom_line(color = "#1b4332", linewidth = 1) +
      geom_point(size = 3, color = "#2d6a4f") +
      ylim(0, 100) +
      labs(title = "Sporting Impact by evaluation period", x = NULL, y = "Sporting Impact") +
      theme_minimal(base_family = "IBM Plex Sans")
  })

  output$player_caveats <- renderUI({
    p <- selected_player()
    tags$ul(
      if (!isTRUE(p$compensation_known)) tags$li("Guaranteed compensation unavailable."),
      if (p$minutes_2026 < 450) tags$li("Below official 450-minute threshold for 2026."),
      if (!isTRUE(p$has_2025_prior)) tags$li("No 2025 MLS prior — higher sample uncertainty."),
      tags$li(sprintf(
        "Model Confidence: %.0f (100 − Model Uncertainty; statistical, not salary-based).",
        dplyr::coalesce(p$model_confidence, 100 - p$model_uncertainty)
      )),
      tags$li("Public Goals Added measures measurable on-ball impact and does not capture all off-ball or tactical contributions."),
      tags$li("Validation status: descriptive with historical stability checks.")
    )
  })

  output$player_scatter_context <- renderPlot({
    p <- selected_player()
    df <- players_raw() |> dplyr::filter(position_group == p$position_group, compensation_known)
    ggplot(df, aes(compensation_percentile, sporting_impact)) +
      geom_abline(slope = 1, intercept = 0, linetype = 2, color = "#999") +
      geom_point(alpha = 0.35, color = "#88a899") +
      geom_point(data = p, color = "#9a3412", size = 4) +
      coord_equal(xlim = c(0, 100), ylim = c(0, 100)) +
      labs(title = "Player vs positional peers", x = "Compensation Percentile", y = "Sporting Impact") +
      theme_minimal(base_family = "IBM Plex Sans")
  })

  compare_df <- reactive({
    picks <- input$compare_players
    if (is.null(picks) || length(picks) < 2) return(NULL)
    players_raw() |> dplyr::filter(display_name %in% picks)
  })

  output$compare_warning <- renderUI({
    picks <- input$compare_players
    if (is.null(picks) || length(picks) == 0) {
      return(tags$p(class = "help-text", "Select 2–4 players to compare."))
    }
    if (length(picks) == 1) {
      return(tags$p(class = "help-text", "Select at least one more player."))
    }
    df <- compare_df()
    if (is.null(df) || nrow(df) < 2) return(NULL)
    if (length(unique(df$position_group)) > 1) {
      div(
        class = "warning-box",
        "Selected players span multiple position groups. Position-adjusted scores are comparable; raw component rates are not equivalent across roles."
      )
    }
  })

  output$compare_table <- DT::renderDT({
    df <- compare_df()
    if (is.null(df) || nrow(df) < 2) {
      return(DT::datatable(data.frame(Note = "Select 2–4 players to see the comparison table."), rownames = FALSE, options = list(dom = "t")))
    }
    out <- df |>
      dplyr::transmute(
        Player = display_name, Club = club, Position = position_label, Age = round(age, 1),
        Minutes = round(minutes_2026), Compensation = compensation,
        `Sporting Impact` = round(sporting_impact, 1),
        `Compensation Percentile` = round(compensation_percentile, 1),
        `Value Surplus` = round(value_surplus, 1),
        `Undervaluation Score` = round(undervaluation_score, 1),
        Coverage = round(metric_coverage, 2),
        `Model Confidence` = round(dplyr::coalesce(model_confidence, 100 - model_uncertainty), 1)
      )
    DT::datatable(out, rownames = FALSE, options = list(dom = "t", scrollX = TRUE)) |>
      DT::formatCurrency("Compensation", digits = 0)
  })

  compare_colors <- c("#1b4332", "#d94801", "#3d5a80", "#7b2cbf")

  compare_palette <- reactive({
    df <- compare_df()
    if (is.null(df) || !nrow(df)) return(character(0))
    names <- unique(as.character(df$display_name))
    setNames(compare_colors[seq_along(names)], names)
  })

  output$compare_legend <- renderUI({
    pal <- compare_palette()
    if (!length(pal)) return(NULL)
    tags$div(
      class = "compare-legend",
      tags$strong("Player legend"),
      tags$div(
        style = "margin-top:0.5rem; display:flex; flex-direction:column; gap:0.45rem;",
        lapply(names(pal), function(nm) {
          tags$div(
            style = "display:flex; align-items:center; gap:0.65rem; color:#102a1f; font-size:0.95rem;",
            tags$span(
              style = paste0(
                "display:inline-block; width:1.1rem; height:1.1rem; border-radius:3px; ",
                "background:", pal[[nm]], "; flex:0 0 auto;"
              )
            ),
            tags$span(nm)
          )
        })
      )
    )
  })

  compare_empty_plot <- function(title) {
    plot_ly() |>
      layout(
        title = list(text = title, x = 0),
        annotations = list(list(
          text = "Select 2–4 players to compare",
          xref = "paper", yref = "paper", x = 0.5, y = 0.5,
          showarrow = FALSE, font = list(color = "#4b6357", size = 14)
        )),
        xaxis = list(visible = FALSE),
        yaxis = list(visible = FALSE)
      ) |>
      config(displayModeBar = FALSE)
  }

  output$compare_index_chart <- renderPlotly({
    df <- compare_df()
    if (is.null(df) || nrow(df) < 2) return(compare_empty_plot("0–100 Index fields"))

    metric_order <- c(
      "Sporting Impact",
      "Compensation Percentile",
      "Undervaluation Score",
      "Model Confidence"
    )
    pal <- compare_palette()
    long <- df |>
      dplyr::transmute(
        Player = as.character(display_name),
        `Sporting Impact` = sporting_impact,
        `Compensation Percentile` = compensation_percentile,
        `Undervaluation Score` = undervaluation_score,
        `Model Confidence` = dplyr::coalesce(model_confidence, 100 - model_uncertainty)
      ) |>
      tidyr::pivot_longer(
        -Player,
        names_to = "Index field",
        values_to = "Score"
      ) |>
      dplyr::mutate(
        `Index field` = factor(`Index field`, levels = metric_order),
        hover = paste0(
          "<b>", Player, "</b><br>",
          "Field: ", `Index field`, "<br>",
          "Value: ", ifelse(is.finite(Score), sprintf("%.1f", Score), "—")
        )
      )

    plot_ly(
      long,
      x = ~`Index field`,
      y = ~Score,
      color = ~Player,
      colors = unname(pal[unique(long$Player)]),
      type = "bar",
      text = ~hover,
      hoverinfo = "text"
    ) |>
      layout(
        barmode = "group",
        title = list(
          text = "0–100 Index fields (grouped bars)",
          x = 0,
          font = list(size = 16, color = "#1b4332")
        ),
        xaxis = list(title = "Index field", categoryorder = "array", categoryarray = metric_order),
        yaxis = list(title = "Score (0–100)", range = c(0, 100), zeroline = TRUE),
        legend = list(orientation = "h", y = -0.25),
        margin = list(l = 60, r = 20, t = 50, b = 90),
        hovermode = "closest"
      ) |>
      config(displayModeBar = FALSE)
  })

  output$compare_surplus_chart <- renderPlotly({
    df <- compare_df()
    if (is.null(df) || nrow(df) < 2) return(compare_empty_plot("Value Surplus"))

    pal <- compare_palette()
    plot_df <- df |>
      dplyr::transmute(
        Player = as.character(display_name),
        `Value Surplus` = value_surplus,
        hover = paste0(
          "<b>", display_name, "</b><br>",
          "Value Surplus: ",
          ifelse(is.finite(value_surplus), sprintf("%.1f", value_surplus), "—")
        )
      ) |>
      dplyr::arrange(`Value Surplus`)

    plot_ly(
      plot_df,
      x = ~`Value Surplus`,
      y = ~Player,
      type = "bar",
      orientation = "h",
      marker = list(color = unname(pal[plot_df$Player])),
      text = ~hover,
      hoverinfo = "text",
      showlegend = FALSE
    ) |>
      layout(
        title = list(
          text = "Value Surplus (separate scale; can be negative)",
          x = 0,
          font = list(size = 16, color = "#1b4332")
        ),
        xaxis = list(title = "Value Surplus (percentile points)", zeroline = TRUE),
        yaxis = list(title = "", categoryorder = "array", categoryarray = plot_df$Player),
        margin = list(l = 140, r = 20, t = 50, b = 50)
      ) |>
      config(displayModeBar = FALSE)
  })

  output$dl_csv <- downloadHandler(
    filename = function() paste0("mls_value_index_", input$period, ".csv"),
    content = function(file) readr::write_csv(official_filtered(), file)
  )

  output$dl_excel <- downloadHandler(
    filename = function() "MLS_Value_Index_2026.xlsx",
    content = function(file) {
      export_value_index_xlsx(
        players_raw(), file, vi_cfg = vi_cfg, provenance = bundle()$provenance
      )
    }
  )
}

shinyApp(ui, server)
