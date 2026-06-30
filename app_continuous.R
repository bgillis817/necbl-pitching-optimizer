# ============================================================================
#  app_continuous.R - pick a pitcher + a lineup -> outing xRV, run distribution,
#  per-hitter breakdown; or rank a set of arms vs the lineup by xRV.
#  Needs data/{hitter_object,pitcher_object}.rds. Sources kernel + sim.
#  run: shiny::runApp("app_continuous.R")
# ============================================================================
suppressPackageStartupMessages({
  library(shiny); library(bslib); library(tidyverse); library(reactable); library(plotly)
})
source("continuous_common.R")
source("matchup_kernel_continuous.R")
source("lineup_sim_continuous.R")
km <- load_cont_kernel("data")

CURRENT_SEASON <- "2026"
PITCHERS <- km$po$buckets %>% filter(season==CURRENT_SEASON) %>%
  distinct(PitcherId, Pitcher, PitcherTeam) %>%
  mutate(label = paste0(Pitcher," (",PitcherTeam,")")) %>% arrange(Pitcher)
pid_of  <- function(l) PITCHERS$PitcherId[match(l, PITCHERS$label)]
TEAMS   <- sort(unique(km$ho$meta$BatterTeam[km$ho$meta$season==CURRENT_SEASON]))

navy <- bs_theme(version=5, bg="#0F172A", fg="#F1F5F9", primary="#2563EB",
  base_font=font_google("Inter"), heading_font=font_google("Inter"))
rt <- reactableTheme(backgroundColor="#1E293B", color="#F1F5F9", borderColor="#334155",
  stripedColor="#273448", highlightColor="#2D3F58",
  headerStyle=list(backgroundColor="#0F172A", color="#94A3B8"))

ui <- page_navbar(title="NECBL Matchup Sim", theme=navy,
  nav_panel("Pitcher vs Lineup", icon=icon("crosshairs"),
    layout_sidebar(sidebar=sidebar(width=340,
      selectInput("team","Opponent", choices=TEAMS),
      uiOutput("lineup_ui"),
      hr(),
      radioButtons("mode","Mode", c("Single pitcher","Rank arms","Sequence arms")),
      conditionalPanel("input.mode=='Single pitcher'",
        selectInput("sp","Pitcher", choices=PITCHERS$label)),
      conditionalPanel("input.mode=='Rank arms'",
        selectizeInput("arms","Arms to rank", choices=PITCHERS$label, multiple=TRUE)),
      conditionalPanel("input.mode=='Sequence arms'",
        selectizeInput("seqarms","Available arms", choices=PITCHERS$label, multiple=TRUE),
        selectizeInput("seqstarter","Starter (optional \u2014 locks 1st)", choices=PITCHERS$label,
                       multiple=FALSE, options=list(placeholder="none", allowEmptyOption=TRUE, onInitialize=I('function(){this.setValue("");}'))),
        textInput("seqlens","Batters per arm (comma, matches order above; min 1)","18,6,6"),
        p("Finds the order with the lowest total outing xRV.", class="text-muted small")),
      sliderInput("innings","Innings", 1, 9, 6),
      sliderInput("sims","MC sims", 500, 5000, 2500, step=500),
      actionButton("go","Run", class="btn-primary w-100"),
      hr(), p("xRV = expected run value of sequence.",
              class="text-muted small")),
    layout_columns(col_widths=c(7,5),
      card(card_header("Result"), uiOutput("head"), reactableOutput("tbl"),
           plotlyOutput("dist", height="220px"),
           uiOutput("detail_head"), reactableOutput("detail_tbl"),
           plotlyOutput("detail_dist", height="300px")),
      card(card_header("Lineup"), reactableOutput("lineup_tbl"))))))

server <- function(input, output, session){
  output$lineup_ui <- renderUI({
    req(input$team)
    sel <- team_lineup_c(km, input$team, CURRENT_SEASON, 9)
    choices <- km$ho$meta %>%
      filter(BatterTeam==input$team, season==CURRENT_SEASON) %>%
      distinct(Batter) %>% arrange(Batter) %>% pull(Batter)
    selectizeInput("batters","Lineup (auto top-9 by PA; editable)",
      choices=choices, selected=sel, multiple=TRUE, options=list(maxItems=9))
  })
  lineup <- reactive({ req(input$batters); input$batters })
  output$lineup_tbl <- renderReactable({
    d <- resolve_lineup_c(km, lineup(), CURRENT_SEASON) %>% mutate(Slot=row_number()) %>%
      select(Slot, Batter, Side=BatterSide)
    reactable(d, theme=rt, defaultPageSize=9)
  })

  res <- eventReactive(input$go, {
    req(length(lineup())>=1)
    ctx <- list(lineup=lineup(), innings=input$innings, sims=input$sims, season=CURRENT_SEASON)
    if (input$mode=="Single pitcher") {
      list(kind="single", ctx=ctx,
           r=sim_matchup(km, lineup(), pid_of(input$sp), CURRENT_SEASON,
                         innings=input$innings, n_sims=input$sims))
    } else if (input$mode=="Rank arms") {
      req(length(input$arms)>=1)
      tbl <- rank_matchups(km, lineup(), pid_of(input$arms), CURRENT_SEASON,
                           innings=input$innings, n_sims=input$sims) %>%
        left_join(PITCHERS %>% select(PitcherId, Pitcher), by="PitcherId")
      list(kind="rank", ctx=ctx, tbl=tbl)
    } else {
      req(length(input$seqarms)>=1)
      lens <- as.integer(str_split(input$seqlens, ",")[[1]])
      # starter: include even if not in the available list (auto-add to front)
      starter_lbl <- if (!is.null(input$seqstarter) && nzchar(input$seqstarter)) input$seqstarter else NULL
      arm_lbls <- input$seqarms
      if (!is.null(starter_lbl) && !(starter_lbl %in% arm_lbls)) arm_lbls <- c(starter_lbl, arm_lbls)
      lens <- head(rep(lens, length.out=length(arm_lbls)), length(arm_lbls))
      arms_df <- tibble(PitcherId = pid_of(arm_lbls), length = pmax(1L, lens))
      starter_pid <- if (!is.null(starter_lbl)) pid_of(starter_lbl) else NULL
      tbl <- sequence_xrv(km, lineup(), arms_df, CURRENT_SEASON, starter=starter_pid)
      nm <- PITCHERS %>% select(PitcherId, Pitcher)
      relabel <- function(s) reduce(seq_len(nrow(nm)), function(a,i)
        gsub(nm$PitcherId[i], nm$Pitcher[i], a, fixed=TRUE), .init=s)
      tbl$order_ids <- tbl$order
      tbl <- tbl %>% mutate(order = map_chr(order, relabel))
      list(kind="seq", ctx=ctx, tbl=tbl,
           lens=setNames(arms_df$length, arms_df$PitcherId))
    }
  })

  output$head <- renderUI({
    o <- res(); if (o$kind!="single") return(NULL); r<-o$r
    tags$table(class="table table-dark table-sm",
      tags$tr(tags$td(strong("Outing xRV")), tags$td(strong(round(r$xrv,2)))),
      tags$tr(tags$td("Mean runs"), tags$td(round(r$mean_runs,2))),
      tags$tr(tags$td("P10 / P90"), tags$td(paste0(round(r$p10,1)," / ",round(r$p90,1)))),
      tags$tr(tags$td("Scoreless %"), tags$td(paste0(round(100*r$scoreless,1),"%"))))
  })
  output$tbl <- renderReactable({
    o <- res()
    if (o$kind=="single") {
      d <- o$r$intel %>% transmute(Slot=slot, Batter, xRV=xrv,
        `K%`=round(100*K,1), `BB%`=round(100*BB,1), `HR%`=round(100*HR,1))
      reactable(d, theme=rt, defaultPageSize=9)
    } else if (o$kind=="rank") {
      d <- o$tbl %>% transmute(Pitcher, xRV=round(xrv,2), `Mean R`=round(mean_runs,2),
        P10=round(p10,1), P90=round(p90,1), `Scoreless%`=round(100*scoreless,1))
      reactable(d, theme=rt, striped=TRUE, highlight=TRUE, defaultPageSize=12,
        selection="single", onClick="select")
    } else {
      d <- o$tbl %>% transmute(`Order (best first)`=order, `Total xRV`=round(xrv,2))
      reactable(d, theme=rt, striped=TRUE, highlight=TRUE, defaultPageSize=15,
        selection="single", onClick="select")
    }
  })

  # ---- drill-down: click a Rank/Sequence row -> full breakdown -------------
  sel <- reactive(getReactableState("tbl","selected"))
  # compute the clicked combo's sim once, reuse across head/table/dist
  detail <- reactive({
    o <- res(); i <- sel(); if (o$kind=="single" || is.null(i)) return(NULL)
    if (o$kind=="rank") {
      pid <- o$tbl$PitcherId[i]
      r <- sim_matchup(km, o$ctx$lineup, pid, o$ctx$season, o$ctx$innings, o$ctx$sims)
      list(kind="rank", label=o$tbl$Pitcher[i], r=r)
    } else {
      ord <- str_split(o$tbl$order_ids[i], " \u2192 ")[[1]]
      s <- sim_sequence(km, o$ctx$lineup, ord, o$lens, o$ctx$season, o$ctx$sims)
      seg <- sequence_detail(km, o$ctx$lineup, ord, o$lens, o$ctx$season) %>%
        left_join(PITCHERS %>% select(PitcherId, Pitcher), by="PitcherId") %>%
        transmute(Arm=Pitcher, Batters=batters, `Segment xRV`=round(segment_xrv,2), Faced=faced)
      list(kind="seq", label=o$tbl$order[i], s=s, seg=seg)
    }
  })
  output$detail_head <- renderUI({
    d <- detail(); if (is.null(d)) return(NULL)
    m <- if (d$kind=="rank") d$r else d$s
    tags$table(class="table table-dark table-sm",
      tags$tr(tags$td(strong(paste("Breakdown:", d$label))), tags$td("")),
      tags$tr(tags$td("Outing xRV"), tags$td(strong(round(m$xrv,2)))),
      tags$tr(tags$td("Mean runs"), tags$td(round(m$mean_runs,2))),
      tags$tr(tags$td("P10 / P90"), tags$td(paste0(round(m$p10,1)," / ",round(m$p90,1)))),
      tags$tr(tags$td("Scoreless %"), tags$td(paste0(round(100*m$scoreless,1),"%"))))
  })
  output$detail_tbl <- renderReactable({
    d <- detail(); if (is.null(d)) return(NULL)
    if (d$kind=="rank") {
      tb <- d$r$intel %>% transmute(Slot=slot, Batter, xRV=xrv,
        `K%`=round(100*K,1), `BB%`=round(100*BB,1), `HR%`=round(100*HR,1))
      reactable(tb, theme=rt, defaultPageSize=9)
    } else {
      reactable(d$seg, theme=rt, defaultPageSize=10)
    }
  })
  output$detail_dist <- renderPlotly({
    d <- detail(); if (is.null(d)) return(NULL)
    runs <- if (d$kind=="rank") d$r$runs else d$s$runs
    plot_ly(x=runs, type="histogram", marker=list(color="#2563EB")) %>%
      layout(paper_bgcolor="#1E293B", plot_bgcolor="#1E293B", font=list(color="#F1F5F9"),
             xaxis=list(title="runs allowed", color="#94A3B8"),
             yaxis=list(title="", color="#94A3B8"), bargap=0.05, margin=list(t=6,b=30))
  })
  output$dist <- renderPlotly({
    o <- res(); if (o$kind!="single") return(NULL)
    plot_ly(x=o$r$runs, type="histogram", marker=list(color="#2563EB")) %>%
      layout(paper_bgcolor="#1E293B", plot_bgcolor="#1E293B", font=list(color="#F1F5F9"),
             xaxis=list(title="runs allowed", color="#94A3B8"),
             yaxis=list(title="", color="#94A3B8"), bargap=0.05, margin=list(t=6,b=30))
  })
}
shinyApp(ui, server)
