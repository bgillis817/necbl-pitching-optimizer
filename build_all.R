# ============================================================================
#  build_all.R  - continuous lineup-sim pipeline, in order:
#    STEP 1  build_hitter_object.R    pitch data + xwOBACON -> data/hitter_object.rds
#    STEP 2  build_pitcher_object.R   pitch data           -> data/pitcher_object.rds
#    STEP 3  bundle your run-value function -> data/run_values.rds (deploy needs no SP repo)
#  Honors env vars if set (local), else CI defaults.
#  LOCAL (PowerShell):
#    $env:PITCH_RDS="C:/path/navs_all_data.rds"   # or $env:CSV_DIR="C:/.../Navs CSVs"
#    $env:XS_DIR="../xStatsNECBL"; $env:SP_DIR="../NECBLStuffPlus"
#    Rscript build_all.R
# ============================================================================
setdef <- function(k,v) if (Sys.getenv(k)=="") do.call(Sys.setenv, setNames(list(v),k))
setdef("SP_DIR","necbl_sp"); setdef("XS_DIR","necbl_xs"); setdef("OUT_DIR","data"); setdef("PITCH_CACHE","data/pitches_cache.rds")

source("continuous_common.R")
source("build_hitter_object.R")                 # STEP 1
rm(list=setdiff(ls(),c("setdef"))); gc()

source("continuous_common.R")
source("build_pitcher_object.R")                # STEP 2
rm(list=setdiff(ls(),c("setdef"))); gc()

# STEP 3: bundle the run-value function so the deployed app is self-contained
source("continuous_common.R")
crv <- load_run_values(Sys.getenv("SP_DIR"))
saveRDS(crv, file.path(Sys.getenv("OUT_DIR"),"run_values.rds"))
cat("== build_all complete ==\n")
