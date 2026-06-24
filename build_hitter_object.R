# ============================================================================
#  build_hitter_object.R  - per (hitter, season) the inputs the kernel needs:
#    - per leverage-bucket rates: zswing, ozswing(chase), zwhiff, ozwhiff,
#      contact_per_swing, foul_per_contact   (KDE-shrunk toward similar hitters)
#    - xwOBACON contact contour {out,1B,2B,3B,HR}
#    - batted-ball type mix (GB/FB/LD)
#    - handedness, PA, team
#  Saves data/hitter_object.rds (+ league bucket rates).
#  RUN: PITCH_RDS=navs_all_data.rds XS_DIR=../xStatsNECBL Rscript build_hitter_object.R
# ============================================================================
suppressPackageStartupMessages({ library(tidyverse) })
source("continuous_common.R")
OUT_DIR <- Sys.getenv("OUT_DIR","data"); dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)
MIN_PA <- as.integer(Sys.getenv("HITTER_MIN_PA","5"))
KDE_BW <- as.numeric(Sys.getenv("KDE_BANDWIDTH","1.0"))   # gaussian bw in z-space
SHRINK_K <- as.numeric(Sys.getenv("SHRINK_K","60"))        # neighbor pseudo-pitches

px <- load_pitches()

# ---- PA count helper (needs PA identifiers if present) --------------------
pa_keys <- intersect(c("GameID","PAofInning","Inning","Top/Bottom"), names(px))
px <- px %>% mutate(.pa_id = if (length(pa_keys)) do.call(paste, c(across(all_of(pa_keys)), sep="|")) else row_number())

# ---- per (hitter, season, bucket) raw counts ------------------------------
bucket_rates <- function(d) {
  d %>% summarise(
    n_pitch   = n(),
    iz        = sum(InZone),
    oz        = sum(!InZone),
    iz_swing  = sum(InZone & IsSwing),
    oz_swing  = sum(!InZone & IsSwing),
    iz_whiff  = sum(InZone & IsWhiff),
    oz_whiff  = sum(!InZone & IsWhiff),
    swings    = sum(IsSwing),
    contacts  = sum(IsContact),
    fouls     = sum(IsFoul),
    .groups = "drop") %>%
    mutate(
      zswing   = iz_swing / pmax(iz,1),
      chase    = oz_swing / pmax(oz,1),
      zwhiff   = iz_whiff / pmax(iz_swing,1),
      ozwhiff  = oz_whiff / pmax(oz_swing,1),
      contact_per_swing = contacts / pmax(swings,1),
      foul_per_contact  = fouls / pmax(contacts,1))
}

h_bucket <- px %>% group_by(Batter, BatterId = if ("BatterId" %in% names(px)) BatterId else NA,
                            season, bucket) %>% group_modify(~ bucket_rates(.x)) %>% ungroup()
h_overall <- px %>% group_by(Batter, season) %>% group_modify(~ bucket_rates(.x)) %>% ungroup() %>%
  mutate(bucket = "ALL")

league_bucket <- px %>% group_by(bucket) %>% group_modify(~ bucket_rates(.x)) %>% ungroup()

# ---- hitter-level descriptors for similarity ------------------------------
hand <- px %>% count(Batter, season, BatterSide) %>% group_by(Batter, season) %>%
  slice_max(n, n=1, with_ties = FALSE) %>% ungroup() %>% transmute(Batter, season, BatterSide)
team <- px %>% count(Batter, season, BatterTeam) %>% group_by(Batter, season) %>%
  slice_max(n, n=1, with_ties=FALSE) %>% ungroup() %>% transmute(Batter, season, BatterTeam)

bip <- px %>%
  mutate(ExitSpeed = suppressWarnings(as.numeric(ExitSpeed)),
         Angle     = suppressWarnings(as.numeric(Angle)),
         Bearing   = if ("Bearing" %in% names(px)) suppressWarnings(as.numeric(Bearing)) else 0) %>%
  filter(IsBIP, !is.na(ExitSpeed), ExitSpeed > 0, !is.na(Angle))
message("scoring batted balls through xwOBACON: ", nrow(bip))

# score ALL batted balls once (vectorized), attach the 5-class probs
xwobacon <- load_xwobacon()
pr <- xwobacon(bip$ExitSpeed, bip$Angle, ifelse(is.na(bip$Bearing), 0, bip$Bearing))
bip <- bind_cols(bip, as_tibble(pr))   # adds out,single,double,triple,home_run

h_contact <- bip %>% group_by(Batter, season) %>%
  summarise(n_bip = n(),
            gb = mean(is_gb, na.rm=TRUE), fb = mean(is_fb, na.rm=TRUE), ld = mean(is_ld, na.rm=TRUE),
            avg_ev = mean(ExitSpeed, na.rm=TRUE), avg_la = mean(Angle, na.rm=TRUE),
            o=mean(out), s1=mean(single), s2=mean(double), s3=mean(triple), hr=mean(home_run),
            .groups="drop") %>%
  mutate(contour = pmap(list(o,s1,s2,s3,hr), function(o,s1,s2,s3,hr){
            v <- c(out=o,single=s1,double=s2,triple=s3,home_run=hr); v/sum(v) }),
         xwobacon = map_dbl(contour, ~ sum(.x * WOBA_VEC))) %>%
  select(-o,-s1,-s2,-s3,-hr)

pa_count <- px %>% group_by(Batter, season) %>% summarise(pa = n_distinct(.pa_id), .groups="drop")

# overall discipline descriptors for similarity space
h_disc <- h_overall %>% transmute(Batter, season, whiff = (iz_whiff+oz_whiff)/pmax(swings,1),
                                  chase, swing = swings/pmax(n_pitch,1))

desc <- pa_count %>%
  left_join(hand, by=c("Batter","season")) %>% left_join(team, by=c("Batter","season")) %>%
  left_join(h_contact %>% select(Batter,season,avg_ev,fb,xwobacon,n_bip,gb,ld,contour), by=c("Batter","season")) %>%
  left_join(h_disc, by=c("Batter","season")) %>%
  filter(pa >= MIN_PA)

# ---- KDE similarity space: handedness hard-filter + z-scored features -----
sim_feats <- c("whiff","chase","avg_ev","fb","xwobacon")
Z <- desc %>% select(all_of(sim_feats)) %>% mutate(across(everything(), ~ {
  m <- mean(.x, na.rm=TRUE); s <- sd(.x, na.rm=TRUE); ifelse(is.na(.x), 0, (.x-m)/ifelse(s>0,s,1)) }))
desc_z <- bind_cols(desc %>% select(Batter, season, BatterSide, pa, n_bip), Z)

# shrink one bucket-rate column for one hitter toward KDE neighbors + own sample
shrink_rate <- function(target_row, col, denom_col, hb, dz, league_val) {
  # neighbors: same handedness
  nb <- dz %>% filter(BatterSide == target_row$BatterSide)
  d2 <- rowSums((as.matrix(nb[sim_feats]) -
                 matrix(as.numeric(target_row[sim_feats]), nrow(nb), length(sim_feats), byrow=TRUE))^2)
  w  <- exp(-d2 / (2*KDE_BW^2))
  nb_rates <- hb %>% filter(bucket == target_row$bucket) %>%
    select(Batter, season, val = !!sym(col), den = !!sym(denom_col))
  m <- nb %>% select(Batter, season) %>% mutate(w = w) %>% inner_join(nb_rates, by=c("Batter","season"))
  kde <- if (nrow(m)) sum(m$w * m$val * m$den) / pmax(sum(m$w * m$den),1e-9) else league_val
  own_val <- target_row[[paste0("own_",col)]]; own_den <- target_row[[paste0("own_",denom_col)]]
  if (is.na(own_val)) own_val <- kde; if (is.na(own_den)) own_den <- 0
  (own_val*own_den + kde*SHRINK_K) / (own_den + SHRINK_K)
}

# assemble shrunk per-bucket rates for every hitter
rate_cols  <- c("zswing","chase","zwhiff","ozwhiff","contact_per_swing","foul_per_contact")
denom_cols <- c("iz","oz","iz_swing","oz_swing","swings","contacts")
lg_lookup  <- league_bucket %>% select(bucket, all_of(rate_cols))

shrunk <- map_dfr(LEVERAGE_BUCKETS, function(bk) {
  own <- h_bucket %>% filter(bucket == bk) %>%
    select(Batter, season, all_of(rate_cols), all_of(denom_cols)) %>%
    rename_with(~ paste0("own_", .x), -c(Batter, season))
  rows <- desc_z %>% mutate(bucket = bk) %>% left_join(own, by=c("Batter","season"))
  lgv <- lg_lookup %>% filter(bucket == bk)
  out <- rows
  for (i in seq_along(rate_cols)) {
    rc <- rate_cols[i]; dc <- denom_cols[i]; lv <- lgv[[rc]]
    out[[rc]] <- vapply(seq_len(nrow(rows)), function(j)
      shrink_rate(rows[j,], rc, dc, h_bucket, desc_z, lv), numeric(1))
  }
  out %>% select(Batter, season, bucket, all_of(rate_cols))
})

hitter_object <- list(
  buckets = shrunk,
  contact = h_contact %>% select(Batter, season, gb, fb, ld, contour, xwobacon, n_bip),
  meta    = desc %>% select(Batter, season, BatterSide, BatterTeam, pa),
  league_bucket = league_bucket %>% select(bucket, all_of(rate_cols)),
  buckets_list = LEVERAGE_BUCKETS)
saveRDS(hitter_object, file.path(OUT_DIR, "hitter_object.rds"))
message("== hitter_object.rds: ", n_distinct(paste(shrunk$Batter, shrunk$season)), " hitter-seasons ==")
