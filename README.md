# necbl-pitching-optimizer (continuous)

Plug any pitcher against any lineup → **outing xRV** (your run-value currency) plus a base-out run distribution. Rank arms to find the best matchup per lineup. Continuous — every hitter is their own numbers, no archetypes, no clustering.

## How it works
- **Hitter object** (`build_hitter_object.R`): per (hitter × 6 leverage buckets — 0-0, 3-2, two-strike, ahead, even, behind) swing / chase / in-zone-swing / whiff using YOUR zone+outcome logic, plus xwOBACON contact contour and GB/FB/LD mix. Thin samples are **KDE-shrunk** toward similar hitters (handedness hard-filter + whiff/chase/EV/FB%/xwOBACON) and the hitter's own overall rate, weighted by sample.
- **Pitcher object** (`build_pitcher_object.R`): per-bucket zone% / induced swing-whiff-chase / contact / HBP and GB/FB-induced tendency. Straight aggregation, light league shrink.
- **Kernel** (`matchup_kernel_continuous.R`): per bucket, **log5** combines hitter × pitcher × league rates → per-pitch outcome → count Markov chain → PA line + PA xRV. log5 is the standard batter×pitcher matchup combinator; it bakes both sides in without the leakage of feeding a hitter's whiff rate to predict whiff. Contact = the hitter's xwOBACON contour tilted by the pitcher's GB/FB.
- **Sim** (`lineup_sim_continuous.R`): outing xRV = Σ PA xRV over the projected outing (headline); base-out Monte Carlo gives the run distribution (secondary).

**Whose math:** run-value weighting = your `pitching_plus.R::calculate_run_values` (bundled at build into `data/run_values.rds`); contact = your `xwoba_model.rds` (baked into the hitter contours at build); zone/outcome logic = yours. The Pitching+ RF is intentionally **not** used — the per-bucket interaction replaces its league-average-swinger assumption, which was the whole point. log5 and the sim layer are the new pieces.

## Tunable parameters (yours to set — not buried)
- `GBFB_TILT` (kernel): strength of the pitcher GB/FB tilt on contact. Default 1.0; 0 = off.
- `TTO_BOOST` (kernel): per-time-through contour multipliers (league prior).
- `ADV` (sim): base-out advancement probabilities. **Only affect the MC distribution, not xRV.**
- KDE: `KDE_BANDWIDTH`, `SHRINK_K` (hitter build); `P_SHRINK_K` (pitcher build).

## Run locally
```r
install.packages(c("xgboost","bslib","reactable","plotly","lubridate"))
# pitch source: a combined Trackman .rds (preferred) OR a folder of CSVs
$env:PITCH_RDS="C:/path/navs_all_data.rds"      # or  $env:CSV_DIR="C:/Users/bengi/Navs CSVs"
$env:XS_DIR="../xStatsNECBL"; $env:SP_DIR="../NECBLStuffPlus"
Rscript build_all.R
shiny::runApp("app_continuous.R")
```
Inputs: a combined pitch table (Drive `navs_all_data.rds` or your CSV folder), your two model repos cloned at `../` (the build pulls the run-value fn + xwOBACON model from them). The deployed app needs none of those — run-values and contours are bundled into `data/`.

## Deploy
Secrets: `GDRIVE_SERVICE_ACCOUNT_JSON`, `SHINYAPPS_ACCOUNT/TOKEN/SECRET` (existing), plus `PITCH_RDS_DRIVE_ID` (Drive id of your combined pitch rds). `workflow_dispatch` once, then the 11:00 cron.

## Sanity check
Against a roughly league-average lineup an arm's xRV should land near his Pitching+ baseline; deviations by opponent are the actual signal. Untested as shipped — validate the local `build_all.R` run first.
