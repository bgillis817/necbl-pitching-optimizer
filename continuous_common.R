# ============================================================================
#  continuous_common.R  - shared helpers for the continuous lineup sim.
#  Sourced by the builders, the kernel, and the app.
#
#  Pitch source (one combined Trackman table, raw columns intact):
#    PITCH_RDS  path to a combined pitch .rds (e.g. navs_all_data.rds)  [preferred]
#    CSV_DIR    folder of Trackman CSVs (recursive) if no rds
# ============================================================================
suppressPackageStartupMessages({ library(tidyverse); library(lubridate) })

LEVERAGE_BUCKETS <- c("0-0","3-2","two_strike","ahead","even","behind")

# ---- your zone + outcome flags (verbatim from your code) -------------------
add_flags <- function(df) {
  df %>%
    mutate(across(c(PlateLocSide, PlateLocHeight, ExitSpeed, Angle), as.numeric)) %>%
    filter(!is.na(PlateLocSide), !is.na(PlateLocHeight)) %>%
    mutate(
      InZone    = dplyr::between(PlateLocHeight, 1.59, 3.41) & dplyr::between(PlateLocSide, -1, 1),
      IsSwing   = PitchCall %in% c("FoulBall","StrikeSwinging","InPlay"),
      IsWhiff   = PitchCall == "StrikeSwinging",
      IsContact = PitchCall %in% c("FoulBall","InPlay"),
      IsFoul    = PitchCall == "FoulBall",
      IsBIP     = PitchCall == "InPlay",
      IsCalledStrike = PitchCall == "StrikeCalled",
      IsBall    = PitchCall %in% c("BallCalled","HitByPitch"),
      IsHBP     = PitchCall == "HitByPitch",
      is_gb     = TaggedHitType == "GroundBall",
      is_fb     = TaggedHitType == "FlyBall",
      is_ld     = TaggedHitType == "LineDrive")
}

# ---- leverage bucket from (balls, strikes) --------------------------------
leverage_bucket <- function(balls, strikes) {
  b <- suppressWarnings(as.integer(balls)); s <- suppressWarnings(as.integer(strikes))
  dplyr::case_when(
    b == 0 & s == 0 ~ "0-0",
    b == 3 & s == 2 ~ "3-2",
    s == 2          ~ "two_strike",
    s > b           ~ "ahead",
    b > s           ~ "behind",
    TRUE            ~ "even")
}

# ---- load combined pitch data ---------------------------------------------
# Source priority: cache > DRIVE_FOLDER > PITCH_RDS > CSV_DIR
load_pitches <- function() {
  cache <- Sys.getenv("PITCH_CACHE", "")
  if (cache != "" && file.exists(cache) && Sys.getenv("REFRESH","0") != "1") {
    message("pitch source: cache ", cache); return(readRDS(cache))
  }
  df_ <- Sys.getenv("DRIVE_FOLDER", "")
  pr  <- Sys.getenv("PITCH_RDS", ""); cd <- Sys.getenv("CSV_DIR", "")
  if (df_ != "") {
    suppressPackageStartupMessages({ library(googledrive); library(data.table) })
    googledrive::drive_auth(path = Sys.getenv("GDRIVE_KEY_PATH"))
    message("pitch source: Google Drive folder '", df_, "'")
    folder <- googledrive::drive_get(df_)
    if (!nrow(folder)) stop("Drive folder not found: ", df_)
    files <- googledrive::drive_ls(folder, pattern = "\\.csv$")
    if (!nrow(files)) stop("no CSVs in Drive folder: ", df_)
    message("found ", nrow(files), " CSVs; downloading")
    tmp <- tempdir(); lst <- vector("list", nrow(files))
    for (i in seq_len(nrow(files))) {
      tf <- file.path(tmp, files$name[i])
      ok <- tryCatch({ googledrive::drive_download(files$id[i], path = tf, overwrite = TRUE, verbose = FALSE)
        x <- data.table::fread(tf, stringsAsFactors = FALSE,
                               na.strings = c("","NA","N/A","null","NULL"), fill = TRUE,
                               colClasses = "character"); TRUE }, error = function(e) FALSE)
      if (ok) lst[[i]] <- x
      if (file.exists(tf)) unlink(tf)
    }
    d <- as_tibble(data.table::rbindlist(lst[!sapply(lst, is.null)], use.names = TRUE, fill = TRUE))
  } else if (pr != "" && file.exists(pr)) {
    message("pitch source: ", pr); d <- readRDS(pr)
  } else if (cd != "") {
    f <- list.files(cd, "\\.csv$", full.names = TRUE, recursive = TRUE)
    if (!length(f)) stop("no CSVs under ", cd)
    message("pitch source: ", length(f), " CSVs in ", cd)
    d <- map_dfr(f, function(x) tryCatch(
      readr::read_csv(x, show_col_types = FALSE, col_types = readr::cols(.default = readr::col_character())),
      error = function(e) { message("skip ", basename(x), ": ", e$message); NULL }), .progress = TRUE)
  } else stop("Set DRIVE_FOLDER (+ GDRIVE_KEY_PATH), or PITCH_RDS, or CSV_DIR.")
  need <- c("PitchCall","PlateLocHeight","PlateLocSide","Balls","Strikes","Batter",
            "BatterSide","Pitcher","PitcherId","PitcherThrows","TaggedPitchType",
            "TaggedHitType","ExitSpeed","Angle","Date","BatterTeam","PitcherTeam")
  miss <- setdiff(need, names(d)); if (length(miss)) stop("pitch source missing: ", paste(miss, collapse=", "))
  if ("BatterId" %in% names(d) && inherits(d$BatterId, "integer64")) d$BatterId <- as.character(d$BatterId)
  if (inherits(d$PitcherId, "integer64")) d$PitcherId <- as.character(d$PitcherId)
  px <- d %>%
    mutate(Date = as.Date(Date), season = as.character(year(Date)),
           PitcherThrows = ifelse(PitcherThrows == "RIght","Right",PitcherThrows),
           BatterSide    = ifelse(BatterSide == "RIght","Right",BatterSide),
           bucket = leverage_bucket(Balls, Strikes)) %>%
    filter(PitcherThrows %in% c("Left","Right"), BatterSide %in% c("Left","Right")) %>%
    add_flags()
  if (cache != "") { try(saveRDS(px, cache), silent = TRUE); message("cached pitches -> ", cache) }
  px
}

# ---- log5: combine batter rate b, pitcher rate p vs league l --------------
# returns the matchup rate. Clamped to avoid 0/1 blowups.
log5 <- function(b, p, l) {
  b <- pmin(pmax(b, 1e-4), 1-1e-4); p <- pmin(pmax(p, 1e-4), 1-1e-4); l <- pmin(pmax(l, 1e-4), 1-1e-4)
  num <- (b * p / l)
  num / (num + ((1-b)*(1-p)/(1-l)))
}

# ---- your Pitching+ run-value weights (sourced from pitching_plus.R) -------
# strips the auto-run; gives calculate_run_values exactly as you defined it.
load_run_values <- function(sp_dir = Sys.getenv("SP_DIR","../NECBLStuffPlus")) {
  src <- readLines(file.path(sp_dir, "pitching_plus.R"))
  src <- src[!grepl("^\\s*results\\s*<-\\s*run_necbl_pitching_plus_no_threshold\\(\\)", src)]
  env <- new.env(); eval(parse(text = paste(src, collapse = "\n")), envir = env)
  env$calculate_run_values        # function(prob_df, strikes)
}

# ---- xwOBACON scorer (your saved model) -----------------------------------
load_xwobacon <- function(xs_dir = Sys.getenv("XS_DIR","../xStatsNECBL")) {
  source(file.path(xs_dir, "pipeline_functions.R"), local = TRUE)
  xm <- readRDS(file.path(xs_dir, "xwoba_model.rds"))
  model <- xm$ultimate_results$model
  cuf <- get("create_ultimate_features")
  # returns one row per input (out,single,double,triple,home_run); NA if dropped
  function(ev, la, bearing = 0) {
    n <- length(ev)
    bearing <- if (length(bearing) == 1) rep(bearing, n) else bearing
    d <- tibble(.rid = seq_len(n), ExitSpeed = as.numeric(ev), Angle = as.numeric(la),
                Bearing = as.numeric(bearing), PlayResult = "Single")
    fr <- cuf(d)
    pr <- predict(model, xgboost::xgb.DMatrix(as.matrix(fr$features)), reshape = TRUE)
    colnames(pr) <- c("out","single","double","triple","home_run")
    out <- matrix(NA_real_, n, 5, dimnames = list(NULL, colnames(pr)))
    keep_ids <- if (".rid" %in% names(fr$pa_data)) fr$pa_data$.rid else seq_len(nrow(pr))
    out[keep_ids, ] <- pr
    # fill any NA rows with column means so downstream means are stable
    cm <- colMeans(pr); for (j in 1:5) out[is.na(out[,j]), j] <- cm[j]
    out
  }
}
WOBA_VEC <- c(out = 0, single = .888, double = 1.271, triple = 1.616, home_run = 2.101)
