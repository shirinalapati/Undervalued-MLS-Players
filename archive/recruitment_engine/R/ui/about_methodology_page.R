# About / How to Use page — plain English for staff, recruiters, coaches, students.
# Full technical methodology lives in docs/methodology.md (opened via button).

about_simple_table <- function(headers, rows) {
  tags$table(
    class = "about-table",
    tags$thead(tags$tr(lapply(headers, function(h) tags$th(h)))),
    tags$tbody(lapply(seq_along(rows), function(i) {
      style <- if (i %% 2 == 0) "background:#f3f7f4;" else NULL
      tags$tr(style = style, lapply(rows[[i]], function(cell) tags$td(HTML(as.character(cell)))))
    }))
  )
}

format_about_date <- function(x) {
  if (is.null(x) || !nzchar(as.character(x)[1]) || identical(as.character(x)[1], "unknown")) {
    return("Unknown")
  }
  raw <- as.character(x)[1]
  d <- tryCatch(as.Date(substr(raw, 1, 10)), error = function(e) as.Date(NA))
  if (is.na(d)) return(raw)
  format(d, "%B %d, %Y")
}

#' Build the staff-facing About page (no formula glossary / code paths).
about_methodology_page <- function(provenance, model_spec, cfg, n_rows, is_synthetic) {
  perf <- format_about_date(provenance$performance_through %||% provenance$data_cutoff_local)
  mlspa <- format_about_date(provenance$mlspa_compensation_as_of %||% provenance$salary_as_of)
  mode_label <- if (isTRUE(is_synthetic)) "Demo" else "Live"
  season <- cfg$project$product_season %||% 2026

  div(
    class = "about-page",

    h1("MLS Recruitment Intelligence"),
    p(class = "about-tagline", "Find realistic recruitment targets for any MLS club"),

    p("MLS Recruitment Intelligence helps users identify players who may be worth scouting for a particular MLS club, position, and tactical role."),
    p("It does not simply rank the most famous or statistically productive players."),
    p("Instead, it asks:"),
    tags$blockquote("Which players appear good enough, suitable enough, affordable enough, and realistic enough to investigate further?"),
    p("The application considers a player’s on-field performance, tactical role, compensation, likely acquisition pathway, development potential, and the reliability of the available data."),

    h2("What can I do with this app?"),
    p("You can use the application to:"),
    tags$ul(
      tags$li("Generate a recruitment shortlist for an MLS club"),
      tags$li("Search for players who fit a specific tactical role"),
      tags$li("Compare potential targets"),
      tags$li("Compare a target with a player already on the selected club"),
      tags$li("Review estimated roster weaknesses"),
      tags$li("Export findings to Excel"),
      tags$li("Create questions for further video and live scouting")
    ),
    p("The application is designed to support scouting discussions."),
    p("It does not make final signing decisions."),

    h2("How to use the app"),
    h3("1. Choose an MLS club"),
    p("Select the club you are evaluating."),
    p("The app applies an estimated club profile based on public information, including factors such as:"),
    tags$ul(
      tags$li("Playing style"),
      tags$li("Budget level"),
      tags$li("Preference for younger players or immediate contributors"),
      tags$li("Common recruitment pathways")
    ),
    p("These are public-data estimates, not confidential information from the club."),
    p("You can override the suggested settings."),

    h3("2. Choose a tactical role"),
    p("Select the type of player the club needs."),
    p("Examples include:"),
    tags$ul(
      tags$li("Pressing Striker"),
      tags$li("Transition Winger"),
      tags$li("Ball-Winning Midfielder"),
      tags$li("Possession Midfielder"),
      tags$li("Progressive Center Back"),
      tags$li("Defensive Fullback")
    ),
    p("Players are evaluated differently for each role."),
    p("A player who fits well as a transition winger may not fit well as a possession-oriented creator."),
    p("There is no single universal player ranking."),

    h3("3. Set your filters"),
    p("You can narrow the player pool by:"),
    tags$ul(
      tags$li("Age"),
      tags$li("League"),
      tags$li("Minutes played"),
      tags$li("Compensation tier"),
      tags$li("Development versus immediate-impact preference"),
      tags$li("Maximum model uncertainty")
    ),
    p("Lowering the minimum-minutes requirement will show more players, but those players’ statistics may be less reliable."),
    p("Raising the minimum will produce a smaller, more reliable shortlist."),

    h3("4. Review the shortlist"),
    p("The app ranks the eligible players for the selected club and role."),
    p("A high ranking means:"),
    tags$blockquote("The player appears worth investigating under the selected settings."),
    p("It does not mean:"),
    tags$blockquote("The club should definitely sign this player."),
    p("Click a player to review their strengths, limitations, data confidence, compensation information, and questions for video scouting."),

    h2("Understanding the main scores"),
    p("Every score ranges from 0 to 100 unless it is unavailable."),
    p("For most scores, higher is better."),
    p("For Model Uncertainty, lower is better."),

    h3("Estimated Contribution"),
    p("This estimates how useful the player may be on the field during the next complete MLS season."),
    p("It considers:"),
    tags$ul(
      tags$li("Recent performance"),
      tags$li("Position"),
      tags$li("Minutes played"),
      tags$li("Current league"),
      tags$li("Sample reliability"),
      tags$li("League-strength assumptions")
    ),
    p("It does not use salary, transfer cost, club budget, or age."),
    p("The score is currently a directional estimate, not a fully validated prediction."),

    h3("Role Fit"),
    p("Role Fit measures how closely the player’s available statistics match the selected tactical role."),
    p("For example, a Pressing Striker may be evaluated using available information related to:"),
    tags$ul(
      tags$li("Shot involvement"),
      tags$li("Non-penalty expected goals"),
      tags$li("Defensive contribution"),
      tags$li("Transition play"),
      tags$li("Receiving"),
      tags$li("Ball retention")
    ),
    p("Different roles use different metrics."),
    p("If too many required metrics are missing, Role Fit is not shown."),
    p("Missing data is never replaced with an artificial average score."),

    h3("Compensation-Adjusted Value"),
    p("This compares the player’s Estimated Contribution with their known guaranteed compensation."),
    p("A productive player on lower compensation will generally receive a higher value score."),
    p("This score is only available when reliable compensation data exists."),
    p("It is not an estimate of:"),
    tags$ul(
      tags$li("Transfer fee"),
      tags$li("MLS trade value"),
      tags$li("Salary Budget Charge"),
      tags$li("Total acquisition cost"),
      tags$li("Total contract value")
    ),

    h3("Acquisition Feasibility"),
    p("This estimates how plausible an acquisition appears using public information."),
    p("It may consider:"),
    tags$ul(
      tags$li("Current league"),
      tags$li("Current club"),
      tags$li("Compensation"),
      tags$li("Playing time"),
      tags$li("Known roster status"),
      tags$li("Possible trade, transfer, free-agent, or development pathway")
    ),
    p("The app cannot know whether a player or club would actually agree to a move."),
    p("Feasibility should therefore be interpreted as:"),
    tags$blockquote("How clear and realistic does the public acquisition pathway appear?"),

    h3("Development Upside"),
    p("Development Upside estimates the player’s potential to improve."),
    p("It considers:"),
    tags$ul(
      tags$li("Age relative to the player’s position"),
      tags$li("Recent improvement"),
      tags$li("Minutes trajectory"),
      tags$li("Performance stability"),
      tags$li("Available sample size")
    ),
    p("A younger player does not automatically receive a better current-performance score."),
    p("Age only affects the separate development analysis."),

    h3("Model Uncertainty"),
    p("Model Uncertainty shows how cautious you should be about the other scores."),
    p("Uncertainty increases when:"),
    tags$ul(
      tags$li("The player has limited minutes"),
      tags$li("Important metrics are missing"),
      tags$li("The player comes from a league with an uncertain MLS translation"),
      tags$li("The player’s tactical role is unclear")
    ),
    p("A player can have a high score and high uncertainty."),
    p("That means the player may be interesting, but more evidence is needed."),

    h3("Club Fit"),
    p("Club Fit measures how well the player matches the selected club’s estimated recruitment context."),
    p("It may consider:"),
    tags$ul(
      tags$li("Tactical style"),
      tags$li("Selected role"),
      tags$li("Budget range"),
      tags$li("Recruitment pathway"),
      tags$li("Immediate versus development priority")
    ),
    p("Club Fit is specific to the selected club and settings."),
    p("It is not a universal measure of player ability."),

    h2("What does the Overall score mean?"),
    p("The Overall score combines:"),
    tags$ul(
      tags$li("Estimated Contribution"),
      tags$li("Role Fit"),
      tags$li("Compensation-Adjusted Value"),
      tags$li("Acquisition Feasibility"),
      tags$li("Development Upside"),
      tags$li("Club context")
    ),
    p("The score helps organize the shortlist."),
    p("It should not be interpreted as a precise probability of transfer success."),
    p("Users can adjust the importance of each component."),
    p("For example:"),
    tags$ul(
      tags$li("Increase Contribution when seeking an immediate starter."),
      tags$li("Increase Development when seeking a younger long-term player."),
      tags$li("Increase Value when working with a smaller budget."),
      tags$li("Increase Feasibility when a realistic acquisition pathway is especially important.")
    ),
    p("When compensation or another component is unavailable, the app should clearly show that information is missing rather than pretending it is known."),

    h2("Recommendations"),
    p("The application uses cautious recommendation labels."),
    h3("Priority Review"),
    p("The player has a strong overall profile, acceptable feasibility, and relatively reliable data."),
    p("Meaning:"),
    tags$blockquote("Worth serious video and scouting review."),
    h3("Development Watch"),
    p("The player has meaningful long-term potential but may not yet be ready to contribute immediately."),
    p("Meaning:"),
    tags$blockquote("Continue tracking the player’s development."),
    h3("Monitor"),
    p("The player has some useful qualities, but additional evidence is needed."),
    p("Meaning:"),
    tags$blockquote("Keep the player on a broader watchlist."),
    h3("Low Priority"),
    p("The player does not currently offer a strong enough combination of performance, role fit, feasibility, and value."),
    h3("Insufficient Evidence"),
    p("The available data is not strong enough to make a responsible recommendation."),
    p("These labels are scouting priorities, not instructions to sign or reject a player."),

    h2("Roster Overview"),
    p("The Roster Overview estimates where a selected MLS club may have a need."),
    p("It considers more than the number of players listed at a position."),
    p("It may examine:"),
    tags$ul(
      tags$li("Quality of the current starter"),
      tags$li("Quality of backup options"),
      tags$li("Tactical-role coverage"),
      tags$li("Dependence on one player"),
      tags$li("Age and succession concerns"),
      tags$li("Compensation efficiency")
    ),
    p("For example, a club may have three forwards but still lack a strong pressing striker."),
    p("Roster needs are based on incomplete public information and can be manually overridden."),

    h2("Compare a target with a current player"),
    p("The Current Player vs Target page helps answer different recruitment questions."),
    h3("Immediate upgrade"),
    p("Could the target provide meaningfully better current performance?"),
    h3("Direct replacement"),
    p("Does the target offer a similar role and comparable or better contribution?"),
    h3("Rotation or depth"),
    p("Could the target provide reliable backup minutes?"),
    h3("Developmental successor"),
    p("Could the target eventually replace the current player?"),
    h3("Lower-cost alternative"),
    p("Could the target provide similar value at lower known cost?"),
    h3("Complementary profile"),
    p("Does the target add a useful quality that the current player or roster lacks?"),
    p("A cheaper or younger player is not automatically an upgrade."),
    p("When no clear benefit exists, the app should say:"),
    tags$blockquote("No clear recruitment advantage identified."),

    h2("Evaluation periods"),
    h3("2026 season-to-date"),
    p("Uses only available 2026 performance."),
    p("This is the most current option, but some players may have limited samples."),
    h3("2025 full season"),
    p("Uses the completed 2025 season."),
    p("This is less current but generally more stable."),
    h3("Blended recent performance"),
    p("Combines 2026 season-to-date performance with 2025 information."),
    p("Players with more 2026 minutes receive more weight from 2026."),
    p("Players with fewer 2026 minutes retain more influence from 2025."),
    p("This is the default option."),

    h2("Why minutes matter"),
    p("A player can produce excellent numbers over a small number of minutes by chance."),
    p("The app handles this by being more cautious with small samples."),
    p("In simple terms:"),
    tags$ul(
      tags$li("More minutes means the app trusts the player’s own statistics more."),
      tags$li("Fewer minutes means the player’s statistics are moved closer to the typical level for similar players.")
    ),
    p("Example:"),
    p("Suppose a player scores at an unusually high rate over only 200 minutes."),
    p("The app does not assume that rate will continue unchanged. It combines the player’s rate with the normal rate for comparable players."),
    p("As the player accumulates more minutes, the app relies more heavily on their own performance."),
    p("This process is called statistical shrinkage."),

    h2("Compensation tiers"),
    p("Cost tiers use known annual guaranteed compensation."),
    about_simple_table(
      c("Tier", "Guaranteed compensation"),
      list(
        c("1", "Under $150,000"),
        c("2", "$150,000 to under $300,000"),
        c("3", "$300,000 to under $700,000"),
        c("4", "$700,000 to under $1,500,000"),
        c("5", "$1,500,000 or more")
      )
    ),
    p("When compensation is unavailable:"),
    tags$blockquote("Cost tier: Unknown"),
    p("The app does not invent salary information."),

    h2("Current data coverage"),
    p("This is a live 2026 product."),
    tags$ul(
      tags$li(HTML(paste0("<strong>Performance data through:</strong> ", perf))),
      tags$li(HTML(paste0("<strong>MLSPA compensation data as of:</strong> ", mlspa)))
    ),
    p("The 2026 statistics are season-to-date, not complete-season totals."),
    p("Official MLS roster profiles and transaction histories have not yet been systematically incorporated."),
    p("The current roster view is mainly based on:"),
    tags$ul(
      tags$li("Public player-team records"),
      tags$li("Compensation records"),
      tags$li("Recent performance and minutes")
    ),
    p("As a result, it may miss:"),
    tags$ul(
      tags$li("Recent signings"),
      tags$li("Recent departures"),
      tags$li("Loans"),
      tags$li("Injured players"),
      tags$li("Players without recent minutes"),
      tags$li("Supplemental roster players"),
      tags$li("Certain Homegrown or developmental players")
    ),
    p("Roster and feasibility conclusions should therefore be treated as estimates."),

    h2("Important limitations"),
    p("The app cannot observe:"),
    tags$ul(
      tags$li("Private club budgets"),
      tags$li("Exact transfer fees"),
      tags$li("Exact trade demands"),
      tags$li("Full contract details"),
      tags$li("Medical information"),
      tags$li("Training performance"),
      tags$li("Player character"),
      tags$li("Agent preferences"),
      tags$li("Internal scouting grades"),
      tags$li("Confidential tactical plans"),
      tags$li("Complete roster mechanisms"),
      tags$li("Whether a player wants to move")
    ),
    p("The app supports scouting conversations."),
    p("It does not replace professional judgment."),

    h2("How should I interpret the results?"),
    p("Use the app to answer:"),
    tags$blockquote("Which players appear worth investigating further?"),
    p("Do not use it to answer:"),
    tags$blockquote("Which player should the club definitely sign?"),
    p("The app is most useful for:"),
    tags$ul(
      tags$li("Narrowing a large player pool"),
      tags$li("Finding players who match a tactical role"),
      tags$li("Comparing candidates"),
      tags$li("Comparing targets with current players"),
      tags$li("Understanding performance, cost, development, and uncertainty tradeoffs"),
      tags$li("Creating questions for video and live scouting")
    ),

    # Transparent calculation docs (config-driven) — after plain English
    uiOutput("about_score_docs"),

    div(
      class = "about-status-line",
      tags$p(HTML(paste0("<strong>Mode:</strong> ", mode_label))),
      tags$p(HTML(paste0("<strong>Product season:</strong> ", season))),
      tags$p(HTML(paste0("<strong>Players evaluated:</strong> ", format(n_rows, big.mark = ",")))),
      tags$p(HTML(paste0("<strong>Performance data through:</strong> ", perf))),
      tags$p(HTML(paste0("<strong>Compensation data as of:</strong> ", mlspa)))
    )
  )
}
