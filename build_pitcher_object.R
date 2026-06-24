# ============================================================================
#  build_pitcher_object.R  - per (pitcher, season) the pitcher side of the
#  matchup: per leverage-bucket zone%, induced whiff/chase/contact rates,
#  HBP rate, and GB/FB-induced tendency. Pitcher samples are large, so only a
#  light shrink toward league by sample. Saves data/pitcher_object.rds.
#  RUN: PITCH_RDS=navs_all_data.rds Rscript build_pitcher_object.R
# ============================================================================
suppressPackageStartupMessages({ library(tidyverse) })
source("continuous_common.R")
OUT_DIR <- Sys.getenv("OUT_DIR","data"); dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)
MIN_PITCH <- as.integer(Sys.getenv("PITCHER_MIN_PITCH","30"))
SHRINK_K  <- as.numeric(Sys.getenv("P_SHRINK_K","40"))

px <- load_pitches()

prate <- function(d) {
  d %>% summarise(
    n_pitch  = n(),
    zone     = sum(InZone)/n(),                       # how often in zone
    iz       = sum(InZone), oz = sum(!InZone),
    iz_swing = sum(InZone & IsSwing), oz_swing = sum(!InZone & IsSwing),
    iz_whiff = sum(InZone & IsWhiff), oz_whiff = sum(!InZone & IsWhiff),
    swings   = sum(IsSwing), contacts = sum(IsContact), fouls = sum(IsFoul),
    hbp      = sum(IsHBP),
    .groups="drop") %>%
    mutate(
      pz_swing  = iz_swing/pmax(iz,1),                # swing induced in zone
      pchase    = oz_swing/pmax(oz,1),                # chase induced
      pzwhiff   = iz_whiff/pmax(iz_swing,1),          # whiff induced in-zone
      pozwhiff  = oz_whiff/pmax(oz_swing,1),          # whiff induced o-zone
      pcontact_per_swing = contacts/pmax(swings,1),
      pfoul_per_contact  = fouls/pmax(contacts,1),
      hbp_rate  = hbp/pmax(n_pitch,1))
}

pcols  <- c("zone","pz_swing","pchase","pzwhiff","pozwhiff","pcontact_per_swing","pfoul_per_contact","hbp_rate")
denoms <- c("n_pitch","iz","oz","iz_swing","oz_swing","swings","contacts","n_pitch")

p_bucket <- px %>% group_by(PitcherId, Pitcher, PitcherTeam, season, PitcherThrows, bucket) %>%
  group_modify(~ prate(.x)) %>% ungroup()
league <- px %>% group_by(bucket) %>% group_modify(~ prate(.x)) %>% ungroup()

# light shrink each pitcher-bucket rate toward league-bucket by its denom
shrunk <- p_bucket
for (i in seq_along(pcols)) {
  rc <- pcols[i]; dc <- denoms[i]
  lg <- league %>% select(bucket, lv = !!sym(rc))
  shrunk <- shrunk %>% left_join(lg, by="bucket") %>%
    mutate(!!rc := (.data[[rc]]*.data[[dc]] + lv*SHRINK_K)/(.data[[dc]]+SHRINK_K)) %>% select(-lv)
}

# GB/FB induced (BIP-level), per pitcher-season
gbfb <- px %>% filter(IsBIP) %>% group_by(PitcherId, season) %>%
  summarise(gb_induced = mean(is_gb, na.rm=TRUE), fb_induced = mean(is_fb, na.rm=TRUE),
            ld_induced = mean(is_ld, na.rm=TRUE), .groups="drop")
lg_gbfb <- px %>% filter(IsBIP) %>% summarise(gb = mean(is_gb,na.rm=TRUE), fb = mean(is_fb,na.rm=TRUE))

# total pitch volume filter
vol <- px %>% count(PitcherId, season, name = "total_pitches") %>% filter(total_pitches >= MIN_PITCH)

pitcher_object <- list(
  buckets = shrunk %>% inner_join(vol, by=c("PitcherId","season")) %>%
    select(PitcherId, Pitcher, PitcherTeam, season, PitcherThrows, bucket, all_of(pcols)),
  gbfb = gbfb,
  league_bucket = league %>% select(bucket, all_of(pcols)),
  league_gbfb = c(gb = lg_gbfb$gb, fb = lg_gbfb$fb),
  buckets_list = LEVERAGE_BUCKETS)
saveRDS(pitcher_object, file.path(OUT_DIR, "pitcher_object.rds"))
message("== pitcher_object.rds: ", n_distinct(paste(pitcher_object$buckets$PitcherId, pitcher_object$buckets$season)), " pitcher-seasons ==")
