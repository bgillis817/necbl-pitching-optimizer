# ============================================================================
#  lineup_sim_continuous.R
#  Outing-level outputs for (pitcher vs lineup): xRV (your run-value currency,
#  the headline) and a base-out Monte Carlo run distribution (secondary).
#  source("continuous_common.R"); source("matchup_kernel_continuous.R"); source(this)
# ============================================================================
suppressPackageStartupMessages({ library(tidyverse) })

# advancement constants — YOURS TO SET (only affect the MC distribution, not xRV)
ADV <- list(s2_score=0.5, s1_to3=0.3, d1_score=0.6)

resolve_lineup_c <- function(km, batters, season=NULL) {
  meta <- km$ho$meta
  if (!is.null(season)) meta <- meta %>% filter(season==as.character(season))
  meta <- meta %>% distinct(Batter, BatterSide)
  tibble(Batter=batters) %>%
    left_join(meta, by="Batter") %>%
    mutate(BatterSide = ifelse(is.na(BatterSide), "Right", BatterSide))
}
team_lineup_c <- function(km, team, season, n=9) {
  km$ho$meta %>% filter(season==as.character(season), BatterTeam==team) %>%
    arrange(desc(pa)) %>% head(n) %>% pull(Batter)
}

# expected lines + per-PA xRV for a pitcher vs the 9 (TTO-aware)
outing_lines <- function(km, lineup, pid, season, innings=6) {
  n_pa <- round(innings*4.3)
  rows <- map_dfr(seq_len(n_pa), function(i){
    slot <- ((i-1) %% length(lineup))+1; tto <- min(((i-1)%/%length(lineup))+1,3)
    e <- pa_eval(km, lineup[slot], season, pid, tto)
    tibble(i=i, slot=slot, Batter=lineup[slot], tto=tto, xrv=e$xrv, line=list(e$line))
  })
  rows
}

# headline: outing xRV = sum of PA xRV over the projected outing
outing_xrv <- function(rows) sum(rows$xrv)

# base-out MC for the run distribution (secondary view)
.adv <- function(st, ev, rng) {
  b<-st$bases; o<-st$outs; r<-st$runs
  if (ev %in% c("K","out")) o<-o+1
  else if (ev %in% c("BB","HBP")) { if(b[1]){if(b[2]){if(b[3]) r<-r+1; b[3]<-TRUE}; b[2]<-TRUE}; b[1]<-TRUE }
  else if (ev=="single"){ if(b[3]){r<-r+1;b[3]<-FALSE}; if(b[2]){if(rng()<ADV$s2_score) r<-r+1 else b[3]<-TRUE;b[2]<-FALSE}
                          if(b[1]){if(rng()<ADV$s1_to3) b[3]<-TRUE else b[2]<-TRUE;b[1]<-FALSE}; b[1]<-TRUE }
  else if (ev=="double"){ if(b[3]){r<-r+1;b[3]<-FALSE}; if(b[2]){r<-r+1;b[2]<-FALSE}
                          if(b[1]){if(rng()<ADV$d1_score) r<-r+1 else b[3]<-TRUE;b[1]<-FALSE}; b[2]<-TRUE }
  else if (ev=="triple"){ r<-r+sum(b); b[]<-FALSE; b[3]<-TRUE }
  else if (ev=="home_run"){ r<-r+sum(b)+1; b[]<-FALSE }
  list(bases=b, outs=o, runs=r)
}
.draw <- function(d,u){ cs<-cumsum(d); names(cs)[which(u<=cs)[1]] }

outing_mc <- function(rows, n_sims=3000, seed=1) {
  lines <- rows$line; np <- length(lines)
  set.seed(seed); pool <- runif(n_sims*np*3); i<-0L; rng<-function(){i<<-i+1L; pool[i]}
  vapply(seq_len(n_sims), function(s){
    st<-list(bases=c(FALSE,FALSE,FALSE),outs=0L,runs=0)
    for (k in seq_len(np)){ ev<-.draw(lines[[k]],rng()); st<-.adv(st,ev,rng)
      if(st$outs>=3L){st$outs<-0L;st$bases<-c(FALSE,FALSE,FALSE)} }
    st$runs}, numeric(1))
}

# one matchup: xRV + distribution + per-hitter breakdown
sim_matchup <- function(km, lineup, pid, season, innings=6, n_sims=3000) {
  rows <- outing_lines(km, lineup, pid, season, innings)
  runs <- outing_mc(rows, n_sims)
  intel <- rows %>% filter(tto==1) %>% transmute(slot, Batter,
    xrv=round(xrv,3),
    K=map_dbl(line,~.x["K"]), BB=map_dbl(line,~.x["BB"]),
    HR=map_dbl(line,~.x["home_run"]), BIPout=map_dbl(line,~.x["out"]))
  list(xrv=outing_xrv(rows), mean_runs=mean(runs),
       p10=quantile(runs,.10), p90=quantile(runs,.90), scoreless=mean(runs==0),
       runs=runs, intel=intel)
}

# rank a set of arms vs one lineup, by xRV (headline)
rank_matchups <- function(km, lineup, pitcher_ids, season, innings=6, n_sims=1500) {
  map_dfr(pitcher_ids, function(pid){
    r <- tryCatch(sim_matchup(km, lineup, pid, season, innings, n_sims), error=function(e) NULL)
    if (is.null(r)) return(NULL)
    tibble(PitcherId=pid, xrv=r$xrv, mean_runs=r$mean_runs,
           p10=r$p10, p90=r$p90, scoreless=r$scoreless)
  }) %>% arrange(xrv)
}

# ============== MODE C: sequence arms by lowest total xRV ==================
# arms_df: tibble(PitcherId, length)  (length = batters faced, min 1).
# Finds the ORDER of arms minimizing total outing xRV. Exact for <=7 arms,
# greedy beyond. TTO is taken from the lineup turn (pos %/% 9).
sequence_xrv <- function(km, lineup, arms_df, season) {
  arms_df <- as_tibble(arms_df)
  arms_df$length <- pmax(1L, as.integer(arms_df$length))
  ids <- arms_df$PitcherId; len <- setNames(arms_df$length, ids)
  total <- sum(len)
  # cache pa xrv per (pid, slot, tto) since orders reuse them
  cache <- new.env()
  pa_x <- function(pid, slot, tto){
    key <- paste(pid, slot, tto); v <- cache[[key]]
    if (is.null(v)) { v <- pa_eval(km, lineup[slot], season, pid, tto)$xrv; cache[[key]] <- v }
    v
  }
  eval_order <- function(ord){
    pos <- 0L; tot <- 0
    for (pid in ord){
      for (j in seq_len(len[[pid]])){
        slot <- (pos %% length(lineup)) + 1
        tto  <- min((pos %/% length(lineup)) + 1, 3)
        tot <- tot + pa_x(pid, slot, tto); pos <- pos + 1L
      }
    }
    tot
  }
  if (length(ids) <= 7) {
    perms <- gtools::permutations(length(ids), length(ids), ids)
    res <- map_dfr(seq_len(nrow(perms)), function(i){
      ord <- perms[i,]; tibble(order = paste(ord, collapse=" \u2192 "), xrv = eval_order(ord)) })
    res %>% arrange(xrv)
  } else {
    # greedy: build the order one arm at a time, picking the next arm that
    # yields the lowest running xRV given what's placed so far.
    rem <- ids; ord <- character()
    while (length(rem)) {
      best <- map_dfr(rem, function(p) tibble(p=p, x=eval_order(c(ord,p))))
      pick <- best$p[which.min(best$x)]; ord <- c(ord, pick); rem <- setdiff(rem, pick)
    }
    tibble(order = paste(ord, collapse=" \u2192 "), xrv = eval_order(ord))
  }
}
