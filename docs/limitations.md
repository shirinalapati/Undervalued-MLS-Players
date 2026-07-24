# Limitations

Honest limitation disclosure is part of the portfolio standard for this project.

## Data coverage

- MVP centers on ASA-covered North American leagues. CPL, NCAA, and most international leagues are incomplete or absent.  
- Some role metrics (e.g., true pressure intensity, physical testing) are **proxied** or missing.  
- Contract expiry, release clauses, and agent dynamics are largely unobserved.

## Financial information

- Salaries (MLS) are periodic public snapshots, not live payroll systems.  
- Transfer fees are rarely public and reliable; the model uses **cost tiers**.  
- “Financial value” is a standardized surplus index, **not** a dollar NPV.

## League translation

- MVP translation factors are **priors**, not precisely estimated causal effects.  
- Small historical transfer samples make metric-specific models unstable.  
- Players can fail/succeed for tactical and cultural reasons invisible in box stats.

## Statistical uncertainty

- Soccer season stats are noisy; shrinkage helps but does not eliminate variance.  
- Confidence intervals are approximate.  
- Rankings near each other are often statistically indistinguishable — treat shortlists as bands, not strict ordinal truth.

## Club profiles

- Profiles are **public-data-based estimates**.  
- They may misstate internal priorities, budget flexibility, or coach preferences.  
- Always allow user overrides in the dashboard.

## Identity matching

- Cross-source name matching can err (common names, accents, loans).  
- Low-confidence matches are penalized but not impossible to miss.

## Availability & roster rules

- MLS roster mechanics (TAM/GAM, U22 initatives, international slots, buy-downs) are simplified.  
- Model does not optimize cap sheets.

## Ethical / usage

- Do not present outputs as guaranteed performance forecasts.  
- Attribute ASA and any other providers.  
- Respect API rate limits and site terms; do not deploy abusive scrapers.

## Implied next upgrades

1. Empirical translation model from MLS inbound transfers  
2. Richer availability features (minutes share, age-26+ contracts)  
3. FBref metric crosswalk for pressures/progressive actions  
4. Formal Bayesian hierarchical projections  
5. Cap/roster constraint module co-developed with public CBA summaries
