# Shiny modules (extension points)

MVP ships as a single `app/app.R` for clarity.

Split into modules when extending:

- `mod_filters.R` — club / role / budget inputs  
- `mod_shortlist.R` — ranked table + export  
- `mod_player_profile.R` — component charts + video questions  
- `mod_compare.R` — 2–4 player comparison  

Keep scoring logic in `R/models/` — the app should only filter, reweight, and display.
