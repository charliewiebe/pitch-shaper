# Pitch Shaper

An R Shiny app for exploring how spin and release traits shape a pitch's movement and its graded quality.

**Open the app:** https://01a131f5-80c8-7107-fb34-61a3a8060255.share.connect.posit.cloud/

**Portfolio:** https://charliewiebe.github.io

Choose a pitch type, or start from any MLB pitcher's arsenal, then move the sliders for velocity, spin rate, spin direction, active spin, arm angle, release height and extension. The app predicts the pitch's induced vertical break (IVB) and horizontal break (HB), plots it against real MLB pitches, and grades it against right- and left-handed batters. The background dots are measured MLB pitcher-season averages, colored on the same grade scale.

The app runs on a free hosting tier that sleeps when idle, so the first load can take up to a minute.

## How it works

The app connects two core models: the sliders feed an expected movement model, and its predicted movement (with velocity and release traits) feeds the pitch grader. A third model builds the grader's target. All three are trained on about 2.8 million pitches of public Statcast data, 2023 through 2026.

**1. Expected run value (the grader's target).** Each pitch's run value is replaced with an expected value to strip out noise. Balls, called strikes, whiffs and fouls get the average run value of that outcome in that count. Balls in play get a gradient-boosted estimate from exit velocity, launch angle and count (out-of-fold R² 0.43 against Statcast's `delta_run_exp`; correlation 0.97 with Savant's xwOBA).

**2. Pitch grader.** A LightGBM model predicts expected run value per pitch from velocity, IVB, HB, arm angle, release height, extension, and differences from the pitcher's primary fastball. It controls for pitch location, count, platoon and batter quality (the batter's xwOBA, whiff rate, chase rate and xwOBAcon). Validation holds out 15% of pitchers entirely.

To grade a pitch independent of where it was thrown, the app scores it against a reference set of 1,000 real pitches per pitch type and platoon side, each with its own location, count, batter and pitch-to-pitch scatter, and averages the predictions. Grades are scaled so 100 is the average MLB pitch and 10 points is one standard deviation. The dot's color is the straight average of the grades against right- and left-handed batters.

**3. Expected movement model.** A linear model with pitch-type interactions predicts IVB and HB from season-level spin and release traits. Its features follow the physics of the Magnus effect: the horizontal and vertical components of active spin, computed from spin rate, active spin percentage and spin direction. Cross-validated with pitchers grouped into folds, it reaches R² 0.98 for HB and 0.97 for IVB. Grip and seam effects are not modeled yet.

## Repository layout

```
app.R                          Shiny app
manifest.json                  package list for Posit Connect Cloud
app_artifacts/
  app_data.rds                 reference sets, movement model coefficients, slider ranges
  grader.txt                   the LightGBM pitch grader
  season_table_regraded.csv    MLB pitcher-season averages and grades (background dots, pitcher search)
pipeline/
  01_statcast_pull_explore.R   pulls pitch-level Statcast data from Baseball Savant and explores it
  02_build_models.R            builds expected run value, the movement models and the pitch grader
  03_regrade.R                 builds the reference sets and app_data.rds
reports/                       text output from 02 and 03
```

## Run it locally

Install the packages once, then run the app from this folder:

```r
install.packages(c("shiny", "plotly", "lightgbm", "data.table", "scales", "jsonlite"))
shiny::runApp()
```

## Rebuild the models

Run the three pipeline scripts in order from the repository's top folder, for example `Rscript pipeline/01_statcast_pull_explore.R`. All paths are relative to that folder.

- `01` downloads pitch-level data from Baseball Savant into `statcast_data/` (large, so not included here).
- `02` also needs Baseball Savant's season-level active spin leaderboards (`active_spin_2023.csv` through `active_spin_2026.csv`) and pitch traits export (`pitch_traits_2023_through_2026.csv`), saved in the top folder or in Downloads. It writes a large cache to `model_out/` and the model files to `app_artifacts/`.
- `03` rebuilds `app_artifacts/app_data.rds` and `season_table_regraded.csv` from those.

Data: Statcast via [Baseball Savant](https://baseballsavant.mlb.com). This project is not affiliated with MLB.
