# ============================================================================
#  matchup_kernel_continuous.R
#  (pitcher, hitter) -> PA outcome line + PA xRV (your run-value currency),
#  via log5 per-bucket interaction -> count Markov chain. Contact = the
#  hitter's xwOBACON contour, tilted by the pitcher's GB/FB tendency.
#
#  TUNABLE PARAMETERS (you set these; not buried):
#    GBFB_TILT  strength of pitcher GB/FB tilt on contact (0 = off). default 1.0
#    TTO_BOOST  per-time-through contour multipliers (league prior).
#  These are the ONLY non-yours numbers in the xRV path.
#
#  source("continuous_common.R"); source("matchup_kernel_continuous.R")
#  km <- load_cont_kernel("data")
# ============================================================================
suppressPackageStartupMessages({ library(tidyverse) })

GBFB_TILT <- as.numeric(Sys.getenv("GBFB_TILT","1.0"))
TTO_BOOST <- list(`1`=c(out=1,single=1,double=1,triple=1,home_run=1),
                  `2`=c(out=.97,single=1.03,double=1.07,triple=1.05,home_run=1.10),
                  `3`=c(out=.93,single=1.06,double=1.14,triple=1.10,home_run=1.22))

load_cont_kernel <- function(dir="data") {
  ho <- readRDS(file.path(dir,"hitter_object.rds"))
  po <- readRDS(file.path(dir,"pitcher_object.rds"))
  crv <- if (file.exists(file.path(dir,"run_values.rds"))) readRDS(file.path(dir,"run_values.rds")) else load_run_values()
  list(ho=ho, po=po, crv=crv,
       lgb_h = ho$league_bucket, lgb_p = po$league_bucket, lg_gbfb = po$league_gbfb,
       buckets = LEVERAGE_BUCKETS)
}

# rate rows (named vector) for a hitter / pitcher / league at one bucket
.hrow <- function(km, batter, season, bk) {
  r <- km$ho$buckets %>% filter(Batter==batter, season==.env$season, bucket==bk)
  if (!nrow(r)) r <- km$lgb_h %>% filter(bucket==bk)
  r[1,]
}
.prow <- function(km, pid, season, bk) {
  r <- km$po$buckets %>% filter(PitcherId==pid, season==.env$season, bucket==bk)
  if (!nrow(r)) r <- km$lgb_p %>% filter(bucket==bk)
  r[1,]
}

# per-pitch outcome primitive at one bucket via log5 hitter x pitcher x league
.primitive <- function(km, h, p, lgh, lgp) {
  # safe rate: use hitter/pitcher value, fall back to league if NA/missing
  nz <- function(x, fallback) { x <- suppressWarnings(as.numeric(x)); ifelse(is.na(x), fallback, x) }
  lz <- function(col) nz(lgh[[col]], 0.2); lp <- function(col, d) nz(lgp[[col]], d)
  z   <- nz(p$zone, lz("zone"))
  zsw <- log5(nz(h$zswing,  lz("zswing")),  nz(p$pz_swing, lz("zswing")),  lz("zswing"))
  ch  <- log5(nz(h$chase,   lz("chase")),   nz(p$pchase,   lz("chase")),   lz("chase"))
  zwh <- log5(nz(h$zwhiff,  lz("zwhiff")),  nz(p$pzwhiff,  lz("zwhiff")),  lz("zwhiff"))
  owh <- log5(nz(h$ozwhiff, lz("ozwhiff")), nz(p$pozwhiff, lz("ozwhiff")), lz("ozwhiff"))
  fpc <- log5(nz(h$foul_per_contact, lz("foul_per_contact")), nz(p$pfoul_per_contact, lz("foul_per_contact")), lz("foul_per_contact"))
  ss  <- z*zsw*zwh + (1-z)*ch*owh
  contact <- z*zsw*(1-zwh) + (1-z)*ch*(1-owh)
  foul <- contact*fpc; bip <- contact*(1-fpc)
  called <- z*(1-zsw); ball <- (1-z)*(1-ch)
  hbp <- nz(p$hbp_rate, 0); if (is.na(hbp)) hbp <- 0
  v <- c(ball=ball, called=called, ss=ss, foul=foul, bip=bip)
  s <- sum(v); if (is.na(s) || s <= 0) v <- c(ball=.33,called=.25,ss=.12,foul=.15,bip=.15) else v <- v/s
  v <- v * (1-hbp); c(v, hbp=hbp)
}

# hitter contact contour tilted by pitcher GB/FB
.contour <- function(km, batter, season, pid, tto) {
  hc <- km$ho$contact %>% filter(Batter==batter, season==.env$season)
  ctr <- if (nrow(hc) && !is.null(hc$contour[[1]])) hc$contour[[1]] else km$lg_gbfb["gb"]*0 +
           c(out=.68,single=.20,double=.07,triple=.01,home_run=.04)
  ctr <- ifelse(is.na(ctr), 0, ctr); if (sum(ctr) <= 0) ctr <- c(out=.68,single=.20,double=.07,triple=.01,home_run=.04)
  pg <- km$po$gbfb %>% filter(PitcherId==pid, season==.env$season)
  if (nrow(pg) && !is.na(pg$gb_induced[1])) {
    dt <- (pg$gb_induced[1] - as.numeric(km$lg_gbfb["gb"])) * GBFB_TILT
    if (is.na(dt)) dt <- 0
    mult <- c(out=1+dt, single=1, double=1-dt, triple=1-dt, home_run=1-dt)
    ctr <- ctr * pmax(mult, 0.2)
  }
  b <- TTO_BOOST[[as.character(min(max(tto,1),3))]]
  ctr <- ctr * b[names(ctr)]; ctr/sum(ctr)
}

# map a per-pitch primitive + contour to YOUR prob_df columns, get per-pitch xRV
.pitch_xrv <- function(km, prim, ctr, strikes) {
  pdf <- data.frame(
    BallCalled = prim["ball"], StrikeCalled = prim["called"], StrikeSwinging = prim["ss"],
    Foul = prim["foul"], HitByPitch = prim["hbp"],
    Out = prim["bip"]*ctr["out"], Single = prim["bip"]*ctr["single"],
    Double = prim["bip"]*ctr["double"], Triple = prim["bip"]*ctr["triple"],
    HomeRun = prim["bip"]*ctr["home_run"], check.names = FALSE)
  km$crv(pdf, strikes)                                    # your run-value weights
}

# absorbing count Markov: returns PA terminal line + PA xRV (your currency)
pa_eval <- function(km, batter, season, pid, tto = 1) {
  # precompute primitive per bucket once
  prim_cache <- list()
  get_prim <- function(bk) {
    if (is.null(prim_cache[[bk]])) {
      h <- .hrow(km,batter,season,bk); p <- .prow(km,pid,season,bk)
      lgh <- km$lgb_h %>% filter(bucket==bk); lgp <- km$lgb_p %>% filter(bucket==bk)
      prim_cache[[bk]] <<- .primitive(km,h,p,lgh,lgp)
    }
    prim_cache[[bk]]
  }
  ctr <- .contour(km,batter,season,pid,tto)
  # propagate visit mass over (b,s); accumulate terminal mass + xRV
  M <- matrix(0, 4, 3, dimnames=list(0:3,0:2)); M["0","0"] <- 1
  term <- c(BB=0,K=0,HBP=0,BIP=0); xrv <- 0
  order_states <- with(expand.grid(b=0:3, s=0:2), data.frame(b,s))[order(expand.grid(b=0:3,s=0:2)$b+expand.grid(b=0:3,s=0:2)$s),]
  for (k in seq_len(nrow(order_states))) {
    b <- order_states$b[k]; s <- order_states$s[k]; m <- M[as.character(b),as.character(s)]
    if (m <= 0) next
    bk <- leverage_bucket(b,s); q <- get_prim(bk)
    ball<-q["ball"]; called<-q["called"]; ss<-q["ss"]; foul<-q["foul"]; bip<-q["bip"]; hbp<-q["hbp"]
    # 2-strike foul self-loop: effective pitches = m/(1-foul); renormalize exits
    if (s==2 && foul>0 && foul<1) {
      eff <- m/(1-foul); sc <- 1/(1-foul)
      ball<-ball*sc; called<-called*sc; ss<-ss*sc; bip<-bip*sc; hbp<-hbp*sc; foul<-0
    } else eff <- m
    xrv <- xrv + eff * .pitch_xrv(km, q, ctr, s)
    # transitions (use renormalized where applicable)
    strike <- called + ss
    term["HBP"] <- term["HBP"] + m*hbp
    term["BIP"] <- term["BIP"] + m*bip
    if (b+1>=4) term["BB"] <- term["BB"] + m*ball else M[as.character(b+1),as.character(s)] <- M[as.character(b+1),as.character(s)] + m*ball
    if (s+1>=3) term["K"]  <- term["K"]  + m*strike else M[as.character(b),as.character(s+1)] <- M[as.character(b),as.character(s+1)] + m*strike
    if (s<2) M[as.character(b),as.character(s+1)] <- M[as.character(b),as.character(s+1)] + m*foul
  }
  line <- c(K=unname(term["K"]), BB=unname(term["BB"]), HBP=unname(term["HBP"]),
            out=unname(term["BIP"])*ctr["out"], single=unname(term["BIP"])*ctr["single"],
            double=unname(term["BIP"])*ctr["double"], triple=unname(term["BIP"])*ctr["triple"],
            home_run=unname(term["BIP"])*ctr["home_run"])
  names(line) <- c("K","BB","HBP","out","single","double","triple","home_run")
  list(line = line/sum(line), xrv = unname(xrv))
}
