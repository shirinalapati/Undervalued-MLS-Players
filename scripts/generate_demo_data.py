#!/usr/bin/env python3
"""Generate demo CSVs without R (portfolio bootstrap / CI without R).

Synthetic players only — not real athletes.
"""
from __future__ import annotations

import json
import math
import random
from pathlib import Path

import csv

ROOT = Path(__file__).resolve().parents[1]
DEMO = ROOT / "data" / "external" / "demo"
SEED = 42
N = 180


def clip(x, lo, hi):
    return max(lo, min(hi, x))


def main() -> None:
    random.seed(SEED)
    DEMO.mkdir(parents=True, exist_ok=True)

    leagues = ["mls", "mlsnp", "uslc"]
    positions = ["FW", "W", "CM", "FB", "CB"]
    role_map = {
        "FW": "pressing_striker",
        "W": "transition_winger",
        "CM": "ball_winning_midfielder",
        "CB": "progressive_center_back",
        "FB": "overlapping_fullback",
    }
    first = ["Alex", "Jordan", "Sam", "Casey", "Riley", "Morgan", "Avery", "Quinn", "Parker", "Reese"]
    last = ["Nguyen", "Patel", "Garcia", "Schmidt", "Okoye", "Silva", "Andersen", "Kowalski", "Mensah", "Ivanov"]
    league_boost = {"mls": 0.15, "mlsnp": -0.25, "uslc": -0.10}

    teams = []
    for lg in leagues:
        for i in range(1, 9):
            teams.append(
                {
                    "team_id": f"{lg}_team_{i:02d}",
                    "team_name": f"Team {lg.upper()} {i}",
                    "league_id": lg,
                    "is_mls_club": 1 if lg == "mls" else 0,
                }
            )

    rows = []
    for i in range(1, N + 1):
        league = random.choices(leagues, weights=[0.35, 0.3, 0.35])[0]
        pos = random.choice(positions)
        age = round(random.uniform(18, 33), 1)
        latent = random.gauss(0, 1)
        press = clip(latent + random.gauss(0, 0.7) + (0.3 if pos in {"FW", "CM"} else 0), -3, 3)
        progress = clip(latent + random.gauss(0, 0.7), -3, 3)
        create = clip(latent + random.gauss(0, 0.8), -3, 3)
        finish = clip(latent + random.gauss(0, 0.8) + (0.4 if pos == "FW" else 0), -3, 3)
        defend = clip(random.gauss(0, 1) + (0.4 if pos in {"CB", "CM", "FB"} else -0.2), -3, 3)
        boost = league_boost[league]
        minutes = max(200, int(random.gauss(1800, 600)))
        name = f"{random.choice(first)} {random.choice(last)}"
        team_pool = [t for t in teams if t["league_id"] == league]
        team = random.choice(team_pool)
        if league == "mls":
            salary = max(80000, int(math.exp(random.gauss(math.log(350000), 0.7))))
        elif league == "uslc":
            salary = max(30000, int(math.exp(random.gauss(math.log(70000), 0.5))))
        else:
            salary = max(20000, int(math.exp(random.gauss(math.log(45000), 0.5))))
        if salary < 150000 and league != "mls":
            cost_tier = 1
        elif salary < 300000:
            cost_tier = 2
        elif salary < 700000:
            cost_tier = 3
        elif salary < 1500000:
            cost_tier = 4
        else:
            cost_tier = 5

        rows.append(
            {
                "player_id": f"demo_{i:03d}",
                "display_name": name,
                "normalized_name": name.lower(),
                "league_id": league,
                "position_group": pos,
                "age": age,
                "is_domestic_player": 1 if random.random() < 0.55 else 0,
                "season_year": 2025,
                "tactical_role": role_map[pos],
                "minutes": minutes,
                "team_strength": random.gauss(0, 1),
                "npxg_p90": max(0.01, 0.25 + 0.12 * finish + 0.05 * boost + random.gauss(0, 0.05)),
                "xa_p90": max(0.01, 0.15 + 0.10 * create + 0.04 * boost + random.gauss(0, 0.04)),
                "shots_p90": max(0.2, 1.8 + 0.6 * finish + random.gauss(0, 0.3)),
                "pressures_p90": max(1, 12 + 3 * press + random.gauss(0, 2)),
                "tackles_p90": max(0.2, 1.5 + 0.6 * defend + random.gauss(0, 0.3)),
                "interceptions_p90": max(0.2, 1.2 + 0.5 * defend + random.gauss(0, 0.25)),
                "progressive_passes_p90": max(0.5, 4 + 1.2 * progress + random.gauss(0, 0.6)),
                "progressive_carries_p90": max(0.3, 3 + 1.0 * progress + random.gauss(0, 0.5)),
                "goals_added_p90": max(-0.05, 0.08 + 0.04 * latent + random.gauss(0, 0.02)),
                "aerial_win_pct": clip(0.45 + 0.08 * defend + random.gauss(0, 0.07), 0.15, 0.85),
                "pass_completion_pct": clip(0.78 + 0.04 * progress + random.gauss(0, 0.04), 0.55, 0.95),
                "crosses_p90": max(0, 1.2 + (1.5 if pos == "FB" else 0) + random.gauss(0, 0.5)),
                "yoy_delta": random.gauss(0.02, 0.08) - max(0, (age - 26) * 0.01),
                "salary": salary,
                "cost_tier": cost_tier,
                "minutes_share": clip(minutes / 3060, 0.05, 1),
                "team_id": team["team_id"],
            }
        )

    with (DEMO / "demo_player_season.csv").open("w", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=list(rows[0].keys()))
        writer.writeheader()
        writer.writerows(rows)

    with (DEMO / "demo_teams.csv").open("w", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=list(teams[0].keys()))
        writer.writeheader()
        writer.writerows(teams)

    (DEMO / "demo_meta.json").write_text(
        json.dumps(
            {
                "generated_at": "python-bootstrap",
                "seed": SEED,
                "n_players": N,
                "note": "Synthetic demo players — not real athletes.",
            },
            indent=2,
        )
    )
    print(f"Wrote {N} demo players to {DEMO}")


if __name__ == "__main__":
    main()
