# app.R  -  Pitch Shaper

ART_DIR          <- "app_artifacts"
ANCHOR_TO_ACTUAL <- TRUE
LIVE_DELAY_MS    <- 80
CLOUD_MIN_N      <- 100

suppressPackageStartupMessages({ library(shiny); library(plotly); library(lightgbm) })
A <- readRDS(file.path(ART_DIR, "app_data.rds"))
G <- lgb.load(file.path(ART_DIR, "grader.txt"))

PT      <- A$pitch_types
PT_NAME <- c(FF = "Four-Seam", SI = "Sinker", FC = "Cutter", SL = "Slider", ST = "Sweeper", SV = "Slurve",
             CU = "Curveball", CH = "Changeup", FS = "Splitter")[PT]
FB_TYPES <- intersect(c("FF", "SI", "FC"), PT)
GF <- A$grader_feats; STUFF <- A$stuff
COL_BAD <- "#e34948"; COL_MID <- "#cfcfca"; COL_GOOD <- "#2a78d6"; INK <- "#1a1a19"; LIM <- c(70, 130)
grade_col <- function(g) scales::gradient_n_pal(c(COL_BAD, COL_MID, COL_GOOD))(scales::rescale(pmin(pmax(g, LIM[1]), LIM[2]), from = LIM))
ax2min <- function(a) (a - 180) * 2          # spin axis (deg, RHP), clock minutes, 0 = 12:00

# pitcher table
PIT_FILE <- file.path(ART_DIR, "season_table_regraded.csv")
PIT <- if (file.exists(PIT_FILE)) as.data.frame(data.table::fread(PIT_FILE, encoding = "UTF-8")) else NULL
YEAR <- if (is.null(PIT)) max(A$cloud$game_year) else max(PIT$game_year)
if (is.null(PIT)) {
  CLOUD <- A$cloud[A$cloud$game_year == YEAR, ]; ARS <- NULL
} else {
  PIT$is_lhp <- as.numeric(PIT$p_throws == "L")
  CLOUD <- with(PIT[PIT$game_year == YEAR & PIT$n >= CLOUD_MIN_N, ],
                data.frame(pt, is_lhp, player_name, game_year, n, release_speed, hb_arm, ivb, grade = plus_new, stringsAsFactors = FALSE))
  ARS <- PIT[PIT$game_year == YEAR & !is.na(PIT$active_spin) & is.finite(PIT$release_spin_rate) & is.finite(PIT$axis_m) & PIT$pt %in% PT, ]
}
pitcher_choices <- if (is.null(ARS)) NULL else {
  u <- ARS[!duplicated(ARS$pitcher), c("pitcher", "player_name", "p_throws")]; u <- u[order(u$player_name), ]
  setNames(as.character(u$pitcher), sprintf("%s (%sHP)", u$player_name, u$p_throws))
}

# arm angle, spin direction / release height, per pitch type
link_slopes <- function() {
  tilt <- c(FF = -1.80, SI = -1.95, CH = -1.76, CU = -1.88, FS = -1.70, FC = -1.89, ST = -0.87, SL = 0, SV = 0)[PT]
  relz <- setNames(rep(0.032, length(PT)), PT)
  if (!is.null(PIT)) for (p in PT) {
    d <- PIT[PIT$pt == p & PIT$n >= 100 & !is.na(PIT$active_spin) & is.finite(PIT$arm_angle) & is.finite(PIT$axis_m), ]
    if (nrow(d) < 30) next
    c0 <- atan2(sum(d$n * sin(d$axis_m * pi / 180)), sum(d$n * cos(d$axis_m * pi / 180))) * 180 / pi
    tm <- ax2min(((d$axis_m - c0 + 180) %% 360) - 180 + c0)
    tilt[[p]] <- if (abs(cor(d$arm_angle, tm)) >= 0.25) unname(coef(lm(tm ~ d$arm_angle, weights = d$n))[2]) else 0
    relz[[p]] <- unname(coef(lm(d$release_pos_z ~ d$arm_angle, weights = d$n))[2])
  }
  list(tilt = as.list(round(tilt, 3)), relz = as.list(round(relz, 4)))
}
LINK <- link_slopes()

# sliders
SPEC <- list(
  velo = list(var = "release_speed",     label = "Velocity (mph)",        step = 0.1),
  spin = list(var = "release_spin_rate", label = "Spin rate (rpm)",       step = 10),
  tilt = list(var = "axis_m",            label = "Spin direction (tilt)", step = 1),
  eff  = list(var = "active_spin",       label = "Active spin (%)",       step = 1),
  arm  = list(var = "arm_angle",         label = "Arm angle (deg)",       step = 1),
  relz = list(var = "release_pos_z",     label = "Release height (ft)",   step = 0.05),
  ext  = list(var = "release_extension", label = "Extension (ft)",        step = 0.1))
slider_spec <- function(p, k, lhp = 0, row = NULL) {
  s <- SPEC[[k]]; r <- A$ranges[A$ranges$pt == p & A$ranges$var == s$var, ]
  d <- A$defaults[A$defaults$pt == p & A$defaults$is_lhp == lhp, ]; if (!nrow(d)) d <- A$defaults[A$defaults$pt == p, ][1, ]
  lo <- r$lo; hi <- r$hi; v <- if (is.null(row)) d[[s$var]] else row[[s$var]]
  if (k == "tilt") { ctr <- (lo + hi) / 2; v <- ctr + ((v - ctr + 180) %% 360) - 180; lo <- ax2min(lo); hi <- ax2min(hi); v <- ax2min(v) }
  if (k == "eff") hi <- min(hi, 100)
  if (k == "velo") hi <- hi + 1
  v  <- round(v / s$step) * s$step
  lo <- min(floor(lo / s$step) * s$step, v); hi <- max(ceiling(hi / s$step) * s$step, v)
  if (is.null(row)) v <- min(max(v, lo), hi)
  list(min = lo, max = hi, value = v, step = s$step, label = s$label)
}
slider_ui <- function(p) lapply(names(SPEC), function(k) {
  s <- slider_spec(p, k)
  el <- sliderInput(paste0(k, "_", p), s$label, min = s$min, max = s$max, value = s$value, step = s$step, ticks = FALSE, width = "100%")
  if (k == "tilt") div(class = "pg-tilt", el) else el
})

# modeling
move_pred <- function(p, velo, spin, eff, axis, arm, ext, relz) {
  a  <- axis * pi / 180; m <- A$move
  nd <- data.frame(pt_f = factor(p, levels = m$xlevels$pt_f), spin_x = -spin * eff * sin(a) / 1000, spin_z = -spin * eff * cos(a) / 1000,
                   release_speed = velo, release_spin_rate = spin, eff = eff, arm_angle = arm, release_extension = ext,
                   release_pos_z = relz, col_share = m$col_share)
  X <- model.matrix(m$terms, model.frame(m$terms, nd, xlev = m$xlevels), contrasts.arg = m$contrasts)
  c(hb = sum(X[1, names(m$hb)] * m$hb), ivb = sum(X[1, names(m$ivb)] * m$ivb))
}
move_from <- function(p, v) move_pred(p, v$velo, v$spin, v$eff / 100, 180 + v$tilt / 2, v$arm, v$ext, v$relz)
CTX <- lapply(A$pool, function(pl) {
  X <- matrix(NA_real_, nrow(pl$ctx), length(GF), dimnames = list(NULL, GF))
  for (v in intersect(colnames(pl$ctx), GF)) X[, v] <- pl$ctx[, v]
  for (v in intersect(names(A$bat_means), GF)) X[, v] <- A$bat_means[[v]]
  X
})
grade_xrv <- function(s, fb, is_fb, lhp) {
  out <- c(opp = NA_real_, same = NA_real_)
  for (h in 0:1) {
    kk <- paste(s$pt, h); X <- CTX[[kk]]; D <- A$pool[[kk]]$dev
    for (v in intersect(STUFF, GF)) X[, v] <- s[[v]] + D[, v]
    dif <- function(v, ref) if (is_fb) 0 else s[[v]] + D[, v] - ref
    if ("velo_diff" %in% GF) X[, "velo_diff"] <- dif("release_speed", fb$release_speed)
    if ("hb_diff" %in% GF)   X[, "hb_diff"]   <- dif("hb_arm", fb$hb_arm)
    if ("ivb_diff" %in% GF)  X[, "ivb_diff"]  <- dif("ivb", fb$ivb)
    if ("is_fb" %in% GF)     X[, "is_fb"]     <- as.numeric(is_fb)
    if ("is_lhp" %in% GF)    X[, "is_lhp"]    <- lhp
    if ("same_hand" %in% GF) X[, "same_hand"] <- h
    out[h + 1L] <- 100 * mean(predict(G, X)) + A$offset[[paste(s$pt, h, lhp)]]
  }
  out
}
to_plus <- function(x) 100 - 10 * (x - A$scale[["mu"]]) / A$scale[["sd"]]

# ui
JS <- sprintf("
  window.pgHand = 'R'; window.pgBusy = false; window.pgHold = 0; window.pgBase = {}; var PG = %s;
  (function() { try { var bs = Shiny.inputBindings.getBindings();
      for (var i = 0; i < bs.length; i++) if (bs[i].binding && bs[i].binding.name === 'shiny.sliderInput')
        bs[i].binding.getRatePolicy = function() { return { policy: 'throttle', delay: %d }; }; } catch (e) {} })();
  function pgSl(id) { return $('#' + id).data('ionRangeSlider'); }
  var pgSent = {}, pgTimer = {};
  function pgPush(id) {
    var s = pgSl(id); if (!s || !window.Shiny || !Shiny.setInputValue) return; var now = Date.now(); clearTimeout(pgTimer[id]);
    if (pgSent[id] && now - pgSent[id] < %d) { pgTimer[id] = setTimeout(function() { pgPush(id); }, %d); return; }
    pgSent[id] = now; Shiny.setInputValue(id, s.result.from); }
  function pgTilt(n) { var m = (window.pgHand === 'L') ? -n : n; m = ((Math.round(m) %% 720) + 720) %% 720;
    var h = Math.floor(m / 60), mm = m - 60 * h; if (h === 0) h = 12; return h + ':' + (mm < 10 ? '0' : '') + mm; }
  function pgInitTilt() { $('.pg-tilt input').each(function() { var s = $(this).data('ionRangeSlider'); if (s) s.update({ prettify: pgTilt, prettify_enabled: true }); }); }
  function pgRebase(p) { var a = pgSl('arm_' + p), t = pgSl('tilt_' + p), z = pgSl('relz_' + p);
    if (a && t && z) window.pgBase[p] = { arm: a.result.from, tilt: t.result.from, relz: z.result.from }; }
  function pgSet(id, v) { var s = pgSl(id); if (!s) return; var st = s.options.step || 1; v = Math.round(v / st) * st;
    v = Math.min(s.options.max, Math.max(s.options.min, v)); s.update({ from: v }); $('#' + id).trigger('change'); pgPush(id); }
  $(document).on('change', 'input.js-range-slider', function() {
    if (window.pgBusy || Date.now() < window.pgHold) return;
    var m = this.id.match(/^([a-z]+)_([A-Z]+)$/); if (!m) return; var k = m[1], p = m[2]; pgPush(this.id);
    if (k === 'arm' && $('#link').prop('checked')) {
      var b = window.pgBase[p]; if (!b) { pgRebase(p); return; }
      var arm = pgSl(this.id).result.from; window.pgBusy = true;
      if (PG.tilt[p]) pgSet('tilt_' + p, b.tilt + PG.tilt[p] * (arm - b.arm));
      if (PG.relz[p]) pgSet('relz_' + p, b.relz + PG.relz[p] * (arm - b.arm));
      window.pgBusy = false;
    } else if (k === 'arm' || k === 'tilt' || k === 'relz') pgRebase(p);
  });
  $(document).on('change', '#link', function() { PG.pt.forEach(pgRebase); });
  $(document).on('shiny:updateinput', function() { window.pgHold = Date.now() + 250; clearTimeout(window.pgT);
    window.pgT = setTimeout(function() { pgInitTilt(); PG.pt.forEach(pgRebase); }, 270); });
  $(document).on('shiny:sessioninitialized', function() {
    setTimeout(function() { pgInitTilt(); PG.pt.forEach(pgRebase); }, 80);
    Shiny.addCustomMessageHandler('pgHand', function(h) { window.pgHand = h; pgInitTilt(); });
    Shiny.addCustomMessageHandler('pgTabs', function(on) { $('#pitch a[data-value]').each(function() {
      $(this).parent().toggleClass('pg-off', on.indexOf($(this).attr('data-value')) < 0); }); });
  });", jsonlite::toJSON(list(tilt = LINK$tilt, relz = LINK$relz, pt = PT), auto_unbox = TRUE), LIVE_DELAY_MS, LIVE_DELAY_MS, LIVE_DELAY_MS)

ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { background:#fcfcfb; color:#1a1a19; font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',Helvetica,Arial,sans-serif; }
    h2.pg-title { font-size:22px; font-weight:650; margin:16px 0 2px; }
    .pg-sub { color:#6b6b66; font-size:13px; margin-bottom:10px; }
    .pg-top { display:flex; gap:28px; align-items:flex-end; flex-wrap:wrap; margin-bottom:6px; }
    .pg-top .form-group { margin-bottom:6px; } .pg-top .btn { margin-bottom:6px; }
    .nav-pills > li > a { padding:7px 14px; color:#1a1a19; border-radius:6px; }
    .nav-pills > li.active > a, .nav-pills > li.active > a:hover, .nav-pills > li.active > a:focus { background:#1a1a19; color:#fff; }
    .nav-pills > li.pg-off { opacity:.3; pointer-events:none; }
    .pg-panel { background:#fff; border:1px solid #e4e3df; border-radius:8px; padding:14px 16px; margin-top:12px; }
    .pg-panel h4 { font-size:13px; text-transform:uppercase; letter-spacing:.04em; color:#6b6b66; margin:0 0 10px; }
    .pg-plot-title { font-size:16px; font-weight:650; } .pg-plot-sub { font-size:12px; color:#6b6b66; margin-bottom:4px; }
    .pg-grade { display:flex; align-items:center; gap:12px; margin-bottom:12px; }
    .pg-sw { width:16px; height:40px; border-radius:4px; flex:none; border:1px solid rgba(0,0,0,.15); }
    .pg-num { font-size:34px; font-weight:650; line-height:1; font-variant-numeric:tabular-nums; }
    .pg-num.sm { font-size:24px; } .pg-sw.sm { height:28px; }
    .pg-lab { font-size:12px; color:#6b6b66; } .pg-kv { font-size:13px; margin:3px 0; font-variant-numeric:tabular-nums; }
    .pg-kv b { font-weight:600; } .pg-note { font-size:12px; color:#6b6b66; margin-top:8px; }
    .irs--shiny .irs-bar { background:#1a1a19; border-color:#1a1a19; } .irs--shiny .irs-single { background:#1a1a19; }
    .control-label { font-size:13px; font-weight:600; } .checkbox label { font-size:12.5px; }
    .pg-tilt .irs-min, .pg-tilt .irs-max { visibility:hidden !important; }
  ")), tags$script(HTML(JS))),
  h2(class = "pg-title", "Pitch Shaper"),
  div(class = "pg-sub", "Set a pitch's spin and release traits. The dot shows its expected movement, colored by grade against all MLB pitches."),
  div(class = "pg-top",
      if (!is.null(pitcher_choices))
        selectizeInput("pitcher", sprintf("Start from a pitcher (%d)", YEAR), c("League average" = "", pitcher_choices), width = "280px",
                       options = list(placeholder = "League average - type a name to search"))
      else div(class = "pg-note", "Add season_table_regraded.csv to app_artifacts to load real pitchers."),
      actionButton("league", "Back to league average", class = "btn-default btn-sm"),
      radioButtons("hand", "Pitcher throws", c("Right" = "R", "Left" = "L"), inline = TRUE),
      radioButtons("fb", "Primary fastball", setNames(FB_TYPES, PT_NAME[FB_TYPES]), inline = TRUE)),
  do.call(tabsetPanel, c(list(id = "pitch", type = "pills"), lapply(PT, function(p) tabPanel(PT_NAME[[p]], value = p)))),
  fluidRow(
    column(3, div(class = "pg-panel", h4(textOutput("slider_title", inline = TRUE)),
                  lapply(PT, function(p) conditionalPanel(sprintf("input.pitch == '%s'", p), slider_ui(p))),
                  checkboxInput("link", "Moving arm angle also shifts spin direction and release height", TRUE),
                  actionButton("reset", "Reset this pitch", class = "btn-default btn-sm"))),
    column(6, div(class = "pg-panel", div(class = "pg-plot-title", textOutput("plot_title", inline = TRUE)),
                  div(class = "pg-plot-sub", textOutput("plot_sub", inline = TRUE)),
                  plotlyOutput("plot", height = "560px"))),
    column(3, div(class = "pg-panel", h4("Grade"), uiOutput("grades")),
              div(class = "pg-panel", h4("Expected shape"), uiOutput("shape"))))
)

# server
server <- function(input, output, session) {
  lhp <- reactive(as.numeric(identical(input$hand, "L")))
  sg  <- reactive(if (lhp() == 1) -1 else 1)
  st  <- reactiveValues(id = "", off = list(), act = list())
  vals <- function(p) { v <- lapply(setNames(names(SPEC), names(SPEC)), function(k) input[[paste0(k, "_", p)]])
                        req(!any(vapply(v, is.null, logical(1)))); v }
  shape_of <- function(p) {
    v <- vals(p); mv <- move_from(p, v); o <- st$off[[p]]; if (is.null(o)) o <- c(hb = 0, ivb = 0)
    list(pt = p, release_speed = v$velo, release_extension = v$ext, arm_angle = v$arm, release_pos_z = v$relz,
         hb_arm = mv[["hb"]] + o[["hb"]], ivb = mv[["ivb"]] + o[["ivb"]])
  }
  cur <- reactive({ req(input$pitch); shape_of(input$pitch) })
  fbs <- reactive(shape_of(input$fb))
  grd <- reactive({
    s <- cur(); is_fb <- identical(input$pitch, input$fb); x <- grade_xrv(s, fbs(), is_fb, lhp())
    vsR <- if (lhp() == 1) x[["opp"]] else x[["same"]]; vsL <- if (lhp() == 1) x[["same"]] else x[["opp"]]
    list(xR = vsR, xL = vsL, gR = to_plus(vsR), gL = to_plus(vsL), g = to_plus((vsR + vsL) / 2), is_fb = is_fb)
  })

  # laoding pitchers
  set_pitch <- function(p, row = NULL, hand_lhp = 0) {
    v <- list()
    for (k in names(SPEC)) { s <- slider_spec(p, k, hand_lhp, row); v[[k]] <- s$value
      updateSliderInput(session, paste0(k, "_", p), min = s$min, max = s$max, value = s$value) }
    if (is.null(row)) { st$off[[p]] <- NULL; st$act[[p]] <- NULL }
    else { mv <- move_from(p, v)
      st$off[[p]] <- if (ANCHOR_TO_ACTUAL) c(hb = row$hb_arm - mv[["hb"]], ivb = row$ivb - mv[["ivb"]]) else NULL
      st$act[[p]] <- row }
  }
  load_pitcher <- function(id) {
    rows <- if (is.null(ARS) || !nzchar(id)) NULL else ARS[ARS$pitcher == as.integer(id), ]
    if (is.null(rows) || !nrow(rows)) {
      st$id <- ""; h <- isolate(lhp())
      for (p in PT) set_pitch(p, NULL, h)
      updateRadioButtons(session, "fb", choices = setNames(FB_TYPES, PT_NAME[FB_TYPES]), selected = FB_TYPES[1], inline = TRUE)
      session$sendCustomMessage("pgTabs", as.list(PT))
      return(invisible())
    }
    st$id <- id; h <- as.numeric(rows$p_throws[1] == "L"); have <- intersect(PT, rows$pt)
    updateRadioButtons(session, "hand", selected = rows$p_throws[1])
    for (p in PT) set_pitch(p, if (p %in% have) rows[rows$pt == p, ][1, ] else NULL, h)
    fbh <- intersect(FB_TYPES, have); prim <- intersect(rows$pt[rows$is_fb == 1], fbh)
    if (!length(fbh)) fbh <- FB_TYPES
    updateRadioButtons(session, "fb", choices = setNames(fbh, PT_NAME[fbh]), selected = if (length(prim)) prim[1] else fbh[1], inline = TRUE)
    session$sendCustomMessage("pgTabs", as.list(have))
    if (!isolate(input$pitch) %in% have) updateTabsetPanel(session, "pitch", selected = if (length(prim)) prim[1] else have[1])
  }
  observeEvent(input$pitcher, load_pitcher(input$pitcher), ignoreInit = TRUE)
  observeEvent(input$league, { if (nzchar(st$id)) updateSelectizeInput(session, "pitcher", selected = "") else load_pitcher("") })
  observeEvent(input$reset, { p <- input$pitch; a <- st$act[[p]]
    set_pitch(p, a, if (is.null(a)) lhp() else as.numeric(a$p_throws == "L")) })
  observeEvent(input$hand, session$sendCustomMessage("pgHand", input$hand))

  # plot
  cloud <- reactive({ d <- CLOUD[CLOUD$pt == input$pitch, ]; d$x <- d$hb_arm * sg(); d })
  marks <- reactive({ s <- cur(); g <- grd(); f <- fbs()
    list(x = s$hb_arm * sg(), y = s$ivb, col = grade_col(g$g), fx = f$hb_arm * sg(), fy = f$ivb, show_fb = !g$is_fb) })
  output$plot <- renderPlotly({
    cl <- cloud(); m <- isolate(marks()); a <- st$act[[input$pitch]]; arm <- if (lhp() == 1) c("arm side", "glove side") else c("glove side", "arm side")
    tip <- sprintf("%s %d<br>%.1f mph, HB %.1f, IVB %.1f<br>Grade %.0f (%d pitches)", cl$player_name, cl$game_year, cl$release_speed,
                   cl$x, cl$ivb, cl$grade, cl$n)
    plot_ly() %>%
      add_trace(type = "scatter", mode = "markers", x = cl$x, y = cl$ivb, text = tip, hoverinfo = "text", name = "MLB",
                marker = list(size = 6, opacity = 0.6, color = cl$grade, cmin = LIM[1], cmax = LIM[2],
                              colorscale = list(c(0, COL_BAD), c(0.5, COL_MID), c(1, COL_GOOD)),
                              colorbar = list(title = "Grade", tickvals = c(70, 85, 100, 115, 130), ticktext = c("70", "85", "100", "115", "130+"),
                                              len = 0.45, thickness = 14, outlinewidth = 0))) %>%
      add_trace(type = "scatter", x = m$fx, y = m$fy, mode = "markers+text", text = "fastball", textposition = "top center", hoverinfo = "skip",
                visible = m$show_fb, textfont = list(color = INK, size = 12),
                marker = list(symbol = "diamond-open", size = 13, color = INK, line = list(width = 2, color = INK))) %>%
      add_trace(type = "scatter", mode = "markers", x = if (is.null(a)) 0 else a$hb_arm * sg(), y = if (is.null(a)) 0 else a$ivb, hoverinfo = "text",
                text = if (is.null(a)) "" else sprintf("%s %d, measured<br>HB %.1f, IVB %.1f", a$player_name, a$game_year, a$hb_arm * sg(), a$ivb),
                visible = !is.null(a), marker = list(symbol = "circle-open", size = 30, color = INK, line = list(width = 1.5, color = INK))) %>%
      add_trace(type = "scatter", mode = "markers", x = m$x, y = m$y, hoverinfo = "skip",
                marker = list(size = 22, color = m$col, opacity = 1, line = list(width = 2, color = INK))) %>%
      layout(showlegend = FALSE, plot_bgcolor = "white", paper_bgcolor = "white", margin = list(l = 55, r = 10, t = 10, b = 50),
             xaxis = list(title = "Horizontal break (in, pitcher's view)", range = c(-25, 25), dtick = 10, zeroline = TRUE, zerolinecolor = "#b9b8b3",
                          gridcolor = "#ecebe7", fixedrange = TRUE, constrain = "domain"),
             yaxis = list(title = "Induced vertical break (in)", range = c(-25, 25), dtick = 10, zeroline = TRUE, zerolinecolor = "#b9b8b3",
                          gridcolor = "#ecebe7", scaleanchor = "x", scaleratio = 1, fixedrange = TRUE, constrain = "domain"),
             annotations = list(list(x = -24, y = -23.5, text = paste0("← ", arm[1]), showarrow = FALSE, xanchor = "left", font = list(color = "#6b6b66", size = 12)),
                                list(x = 24, y = -23.5, text = paste0(arm[2], " →"), showarrow = FALSE, xanchor = "right", font = list(color = "#6b6b66", size = 12)))) %>%
      config(displayModeBar = FALSE)
  })
  observe({ m <- marks()
    plotlyProxy("plot", session) %>%
      plotlyProxyInvoke("restyle", list(x = list(list(m$fx)), y = list(list(m$fy)), visible = m$show_fb), list(1)) %>%
      plotlyProxyInvoke("restyle", list(x = list(list(m$x)), y = list(list(m$y)), `marker.color` = m$col), list(3))
  })

  # text
  who <- reactive(if (nzchar(st$id) && !is.null(st$act[[input$pitch]])) st$act[[input$pitch]]$player_name else NULL)
  output$slider_title <- renderText(paste(PT_NAME[[input$pitch]], "traits"))
  output$plot_title <- renderText(paste0(PT_NAME[[input$pitch]], ", ", if (lhp() == 1) "LHP" else "RHP", if (!is.null(who())) paste0("  -  ", who(), " ", YEAR)))
  output$plot_sub <- renderText(sprintf("Small dots: measured average movement of MLB pitchers' %ss in %d (%d+ pitches), same grade scale.%s",
    tolower(PT_NAME[[input$pitch]]), YEAR, CLOUD_MIN_N, if (!is.null(who())) " Ring: this pitcher's measured movement." else ""))
  card <- function(val, lab, small = FALSE) div(class = "pg-grade",
    div(class = paste("pg-sw", if (small) "sm"), style = paste0("background:", grade_col(val), ";")),
    div(div(class = paste("pg-num", if (small) "sm"), sprintf("%.0f", val)), div(class = "pg-lab", lab)))
  output$grades <- renderUI({ g <- grd()
    tagList(card(g$g, "Overall (average of both sides)"),
            card(g$gR, sprintf("vs right-handed batters (%+.2f runs/100)", g$xR), TRUE),
            card(g$gL, sprintf("vs left-handed batters (%+.2f runs/100)", g$xL), TRUE),
            div(class = "pg-note", "100 is the average MLB pitch; 10 points is one standard deviation. Runs per 100 pitches: negative favours the pitcher."))
  })
  output$shape <- renderUI({ s <- cur(); g <- grd(); f <- fbs(); a <- st$act[[input$pitch]]
    side <- function(x) sprintf("%.1f in %s", abs(x), if (x >= 0) "arm side" else "glove side")
    tagList(div(class = "pg-kv", HTML(sprintf("<b>Horizontal break</b> %s", side(s$hb_arm)))),
            div(class = "pg-kv", HTML(sprintf("<b>Induced vertical break</b> %.1f in", s$ivb))),
            div(class = "pg-kv", HTML(sprintf("<b>Velocity</b> %.1f mph", s$release_speed))),
            if (!is.null(a)) div(class = "pg-kv", HTML(sprintf("<b>Change from measured</b> %+.1f in HB, %+.1f in IVB", s$hb_arm - a$hb_arm, s$ivb - a$ivb))),
            if (g$is_fb) div(class = "pg-note", "This is the primary fastball. Other pitches are graded relative to it.")
            else tagList(
              div(class = "pg-kv", HTML(sprintf("<b>vs %s</b> %+.1f mph, %+.1f in HB, %+.1f in IVB", tolower(PT_NAME[[input$fb]]),
                                                s$release_speed - f$release_speed, s$hb_arm - f$hb_arm, s$ivb - f$ivb))),
              div(class = "pg-note", sprintf("Graded off the %s set on its own tab.", tolower(PT_NAME[[input$fb]])))))
  })
}

shinyApp(ui, server)
