# 03_regrade.R

PREP_FILE <- "model_out/prep.rds"
ART_DIR   <- "app_artifacts"
OUT_DIR   <- "model_out"
K_APP     <- 1000
N_BIG     <- 40000
MIN_CELL  <- 50
SEED      <- 42

suppressPackageStartupMessages({ library(data.table); library(lightgbm) })
options(width = 200, scipen = 999, datatable.print.class = FALSE, datatable.showProgress = FALSE)
set.seed(SEED); t_start <- Sys.time()
RPT  <- file.path(OUT_DIR, "regrade_report.txt"); if (file.exists(RPT)) file.remove(RPT)
emit <- function(txt) { cat(txt, sep = "\n"); cat(txt, file = RPT, sep = "\n", append = TRUE) }
say  <- function(...) emit(paste0(...))
hdr  <- function(x) emit(c("", strrep("=", 100), x, strrep("=", 100)))
rnd  <- function(d, k = 3) d[, lapply(.SD, function(x) if (is.double(x)) round(x, k) else x)]
shw  <- function(x, k = 3) emit(capture.output(print(rnd(as.data.table(x), k), nrows = 500L, topn = 500L, row.names = FALSE)))
tick <- function(...) message(format(Sys.time(), "%H:%M:%S"), "  ", ...)
wcor <- function(a, b, w) { ok <- is.finite(a) & is.finite(b) & is.finite(w) & w > 0
                            if (sum(ok) < 5) return(NA_real_); cov.wt(cbind(a[ok], b[ok]), wt = w[ok], cor = TRUE)$cor[1, 2] }
for (f in c(PREP_FILE, file.path(ART_DIR, c("grader.txt", "meta.rds", "season_table.csv", "movement_lm.rds"))))
  if (!file.exists(f)) stop("Missing ", f, " (working directory ", getwd(), ")")

meta <- readRDS(file.path(ART_DIR, "meta.rds"))
G    <- lgb.load(file.path(ART_DIR, "grader.txt"))
GF   <- meta$grader$feats; BM <- meta$grader$bat_means; PT <- meta$pitch_types
ST   <- fread(file.path(ART_DIR, "season_table.csv"))
STUFF <- c("release_speed", "hb_arm", "ivb", "release_extension", "arm_angle", "release_pos_z"); DN <- paste0("d_", STUFF)
supported <- c(STUFF, "velo_diff", "hb_diff", "ivb_diff", "is_fb", "same_hand", "is_lhp", "balls", "strikes", "px_bat", "pz_norm", names(BM))
if (!all(GF %in% supported)) stop("Grader uses features this script does not rebuild: ", paste(setdiff(GF, supported), collapse = ", "))

# reference pitches
tick("loading ", PREP_FILE)
sc <- readRDS(PREP_FILE)$sc; setDT(sc)
sc <- sc[, c("pitcher", "game_year", "pt", "same_hand", "px_bat", "pz_norm", "balls", "strikes", STUFF), with = FALSE]; gc()
sc[, cell_n := .N, by = .(pitcher, game_year, pt)]
sc[, (DN) := lapply(.SD, function(x) x - mean(x, na.rm = TRUE)), by = .(pitcher, game_year, pt), .SDcols = STUFF]   # deviation from own average
for (v in DN) sc[, (v) := { q <- quantile(get(v), c(.01, .99), na.rm = TRUE); pmin(pmax(get(v), q[1]), q[2]) }, by = pt]
dev_sd <- sc[cell_n >= MIN_CELL, lapply(.SD, sd, na.rm = TRUE), by = pt, .SDcols = DN]
el <- sc$cell_n >= MIN_CELL & complete.cases(sc[, DN, with = FALSE])
P  <- sc[el, c("pt", "same_hand", "px_bat", "pz_norm", "balls", "strikes", DN), with = FALSE][,
         .SD[sample(.N, min(.N, N_BIG))], by = .(pt, same_hand)]
rm(sc, el); gc()
P[, k := seq_len(.N), by = .(pt, same_hand)]
ctr <- function(d) { d[, (DN) := lapply(.SD, function(x) x - mean(x)), by = .(pt, same_hand), .SDcols = DN]; d }   # mean deviation exactly 0
PA <- ctr(copy(P[k <= K_APP])); PB <- ctr(copy(P))
key  <- function(p, h) paste(p, h)
keys <- CJ(pt = PT, same_hand = 0:1)[, key(pt, same_hand)]
split_pool <- function(d) { l <- lapply(keys, function(kk) { x <- d[key(pt, same_hand) == kk]
  list(ctx = as.matrix(x[, .(px_bat, pz_norm, balls, strikes)]), dev = as.matrix(setnames(x[, DN, with = FALSE], STUFF))) }); names(l) <- keys; l }
poolA <- split_pool(PA); poolB <- split_pool(PB)

# xRV/100 of each shape against one reference pool (no offset). shapes: STUFF + fb_velo, fb_hb, fb_ivb, is_fb, is_lhp
grade_block <- function(shapes, pool, same_hand, chunk = 150L) {
  K <- nrow(pool$ctx); out <- numeric(nrow(shapes))
  for (ix in split(seq_len(nrow(shapes)), ceiling(seq_len(nrow(shapes)) / chunk))) {
    s <- rep(ix, each = K); r <- rep(seq_len(K), length(ix))
    X <- matrix(NA_real_, length(s), length(GF), dimnames = list(NULL, GF))
    for (v in STUFF) if (v %in% GF) X[, v] <- shapes[[v]][s] + pool$dev[r, v]
    fbm <- shapes$is_fb[s] == 1
    dif <- function(v, ref) ifelse(fbm, 0, shapes[[v]][s] + pool$dev[r, v] - shapes[[ref]][s])
    if ("velo_diff" %in% GF) X[, "velo_diff"] <- dif("release_speed", "fb_velo")
    if ("hb_diff" %in% GF)   X[, "hb_diff"]   <- dif("hb_arm", "fb_hb")
    if ("ivb_diff" %in% GF)  X[, "ivb_diff"]  <- dif("ivb", "fb_ivb")
    if ("is_fb" %in% GF)     X[, "is_fb"]     <- shapes$is_fb[s]
    if ("is_lhp" %in% GF)    X[, "is_lhp"]    <- shapes$is_lhp[s]
    if ("same_hand" %in% GF) X[, "same_hand"] <- same_hand
    for (v in intersect(c("px_bat", "pz_norm", "balls", "strikes"), GF)) X[, v] <- pool$ctx[r, v]
    for (v in intersect(names(BM), GF)) X[, v] <- BM[[v]]
    out[ix] <- 100 * colMeans(matrix(predict(G, X), nrow = K))
  }
  out
}

# defaults & ranges
ST[, is_lhp := as.numeric(p_throws == "L")]
cmean <- function(a, w) (atan2(sum(w * sin(a * pi / 180)), sum(w * cos(a * pi / 180))) * 180 / pi) %% 360
D0 <- ST[n >= 100 & !is.na(active_spin) & is.finite(arm_angle)]
defaults <- D0[, c(lapply(.SD, weighted.mean, w = n, na.rm = TRUE), .(axis_m = cmean(axis_m, n), rows = .N)), by = .(pt, is_lhp),
               .SDcols = c("release_speed", "release_spin_rate", "active_spin", "arm_angle", "release_extension", "release_pos_z", "hb_arm", "ivb")]
QR <- c(.005, .995)
rng <- rbindlist(lapply(c("release_speed", "release_spin_rate", "active_spin", "arm_angle", "release_extension", "release_pos_z"),
                        function(v) D0[, .(var = v, lo = quantile(get(v), QR[1]), hi = quantile(get(v), QR[2])), by = pt]))
ax  <- D0[, { c0 <- cmean(axis_m, n); u <- ((axis_m - c0 + 180) %% 360) - 180 + c0      # unwrapped around the type's centre (may leave 0-360)
              .(var = "axis_m", lo = quantile(u, QR[1]), hi = quantile(u, QR[2])) }, by = pt]
ranges <- rbind(rng, ax)

base_shape <- function(p, lhp) {
  d <- defaults[pt == p & is_lhp == lhp]; f <- defaults[pt == "FF" & is_lhp == lhp]
  if (!nrow(d)) d <- defaults[pt == p][1]
  data.table(pt = p, is_lhp = lhp, d[, STUFF, with = FALSE], fb_velo = f$release_speed, fb_hb = f$hb_arm, fb_ivb = f$ivb, is_fb = as.numeric(p == "FF"))
}

# offsets & checks
tick("calibrating the ", K_APP, "-pitch reference sets")
off <- CJ(pt = PT, same_hand = 0:1, is_lhp = 0:1)
off[, c("big", "app") := { b <- base_shape(pt, is_lhp); kk <- key(pt, same_hand)
                           .(grade_block(b, poolB[[kk]], same_hand), grade_block(b, poolA[[kk]], same_hand)) }, by = .(pt, same_hand, is_lhp)]
off[, offset := big - app]
grade_app <- function(shapes, h) {
  o <- off[.(shapes$pt, h, shapes$is_lhp), on = .(pt, same_hand, is_lhp), offset]; g <- numeric(nrow(shapes))
  for (p in unique(shapes$pt)) { i <- which(shapes$pt == p); g[i] <- grade_block(shapes[i], poolA[[key(p, h)]], h) }
  g + o
}
grade_big <- function(shapes, h) { g <- numeric(nrow(shapes))
  for (p in unique(shapes$pt)) { i <- which(shapes$pt == p); g[i] <- grade_block(shapes[i], poolB[[key(p, h)]], h, chunk = 4L) }; g }

R <- ST[n >= MIN_CELL & is.finite(release_speed) & is.finite(hb_arm) & is.finite(ivb) & is.finite(release_extension) &
          is.finite(arm_angle) & is.finite(release_pos_z)]
R[is.na(is_fb), is_fb := 0]
chk <- R[n >= 200][, .SD[sample(.N, min(.N, 8L))], by = pt]
tick("checking the approximation on ", nrow(chk), " real shapes")
chk[, `:=`(a0 = grade_app(chk, 0), a1 = grade_app(chk, 1), b0 = grade_big(chk, 0), b1 = grade_big(chk, 1))]

tick("regrading ", nrow(R), " pitcher-season-pitch shapes")
R[, `:=`(g_opp = grade_app(R, 0), g_same = grade_app(R, 1))]
R[, g_avg := (g_opp + g_same) / 2]
sc_all <- R[n >= 100, .(mu = weighted.mean(g_avg, n), sd = sqrt(sum(n * (g_avg - weighted.mean(g_avg, n))^2) / sum(n)))]
plus <- function(x) 100 - 10 * (x - sc_all$mu) / sc_all$sd
R[, `:=`(plus_new = plus(g_avg), plus_vsR = plus(fifelse(is_lhp == 1, g_opp, g_same)), plus_vsL = plus(fifelse(is_lhp == 1, g_same, g_opp)))]

# report
hdr("1. REFERENCE SETS")
say("Reference pitches per pitch type x platoon side: app ", K_APP, ", calibration up to ", N_BIG, ". Grader features: ", length(GF))
emit("Pitch-to-pitch SD around a pitcher's own average (what the scatter adds):"); shw(dev_sd, 2)
emit("Offset removed from each app reference set (runs/100, at the league-average shape):")
shw(dcast(off, pt ~ paste0("same", same_hand, "_lhp", is_lhp), value.var = "offset"), 3)
say("Offset size: SD ", round(sd(off$offset), 3), ", max |offset| ", round(max(abs(off$offset)), 3))
hdr("2. APP GRADE vs LARGE-SAMPLE GRADE ON REAL SHAPES (runs/100)")
e <- c(chk$a0 - chk$b0, chk$a1 - chk$b1)
say("Shapes: ", nrow(chk), " | RMSE ", round(sqrt(mean(e^2)), 3), " | mean error ", round(mean(e), 3), " | max |error| ", round(max(abs(e)), 3),
    " | grade SD for scale: ", round(sc_all$sd, 3))
shw(chk[, .(shapes = .N, rmse = sqrt(mean(c(a0 - b0, a1 - b1)^2)), max_abs = max(abs(c(a0 - b0, a1 - b1)))), by = pt], 3)
hdr("3. NEW SCALE AND GRADES")
say("Scale (n >= 100, pitch-weighted, straight average of the two batter sides): mean ", round(sc_all$mu, 3), ", SD ", round(sc_all$sd, 3),
    " runs/100. Grade = 100 - 10 z; blue above 100.")
shw(R[n >= 100, .(rows = .N, grade = weighted.mean(plus_new, n), sd = sd(plus_new), vs_same_xrv100 = weighted.mean(g_same, n),
                  vs_opp_xrv100 = weighted.mean(g_opp, n), actual_xrv100 = weighted.mean(xrv100, n)), by = pt][order(-grade)], 2)
cols <- c("player_name", "game_year", "pt", "p_throws", "n", "release_speed", "hb_arm", "ivb", "plus_new", "plus_vsR", "plus_vsL", "grade_plus", "xrv100")
emit("Best 15 (n >= 300); grade_plus is the old grade:");  shw(head(R[n >= 300][order(-plus_new), cols, with = FALSE], 15), 1)
emit("Worst 10 (n >= 300):");                              shw(head(R[n >= 300][order(plus_new), cols, with = FALSE], 10), 1)
say("New vs old grade (n >= 200): r = ", round(R[n >= 200, wcor(plus_new, grade_plus, n)], 3),
    " | new grade vs actual xRV/100: r = ", round(R[n >= 200, wcor(g_avg, xrv100, n)], 3),
    " | old: ", round(R[n >= 200, wcor(g_xrv100, xrv100, n)], 3))
nx <- R[n >= 200, .(pitcher, pt, game_year = game_year - 1L, x_next = xrv100, n_next = n)]
G2 <- merge(R[n >= 200], nx, by = c("pitcher", "pt", "game_year"))
say("Next-season xRV/100: new grade r = ", round(G2[, wcor(g_avg, x_next, pmin(n, n_next))], 3),
    " | held-out pitchers ", round(G2[is_heldout == TRUE, wcor(g_avg, x_next, pmin(n, n_next))], 3),
    " | this season's xRV r = ", round(G2[, wcor(xrv100, x_next, pmin(n, n_next))], 3))
hdr("4. SMOOTHNESS AND SPEED")
sw <- function(p, v, grid) { b <- base_shape(p, 0)[rep(1L, length(grid))]; set(b, j = v, value = as.numeric(grid)); (grade_app(b, 0) + grade_app(b, 1)) / 2 }
for (z in list(list("ST", "hb_arm", seq(-22, -6, .5)), list("FF", "release_speed", seq(90, 100, .25)), list("FF", "ivb", seq(10, 20, .5)),
               list("CH", "ivb", seq(-4, 12, .5)))) {
  g <- plus(sw(z[[1]], z[[2]], z[[3]]))
  say(z[[1]], " ", z[[2]], " ", min(z[[3]]), " to ", max(z[[3]]), ": grade ", round(g[1], 1), " -> ", round(g[length(g)], 1),
      " | largest single step ", round(max(abs(diff(g))), 2), " grade points (step ", z[[3]][2] - z[[3]][1], ")")
}
b <- base_shape("SL", 0); tm <- system.time(for (i in 1:10) { grade_block(b, poolA[["SL 0"]], 0); grade_block(b, poolA[["SL 1"]], 1) })[["elapsed"]] / 10
say("One app update (both batter sides): ", round(1000 * tm), " ms")

# app bundle
L  <- readRDS(file.path(ART_DIR, "movement_lm.rds")); tt <- delete.response(terms(L$hb))
cf <- function(m) { b <- coef(m); b[is.na(b)] <- 0; b }
dfl <- function(d) as.data.frame(d)
app <- list(
  created = Sys.time(), pitch_types = PT, grader_feats = GF, bat_means = BM, stuff = STUFF, k_app = K_APP,
  pool    = poolA,
  offset  = setNames(off$offset, paste(off$pt, off$same_hand, off$is_lhp)),
  scale   = c(mu = sc_all$mu, sd = sc_all$sd),
  defaults = dfl(defaults), ranges = dfl(ranges),
  move    = list(terms = tt, xlevels = L$hb$xlevels, contrasts = L$hb$contrasts, hb = cf(L$hb), ivb = cf(L$ivb),
                 col_share = meta$movement$col_share_default),
  cloud   = dfl(R[n >= 200, .(pt, is_lhp, player_name, game_year, n, release_speed, hb_arm, ivb, grade = plus_new)]))
saveRDS(app, file.path(ART_DIR, "app_data.rds"))
fwrite(R, file.path(ART_DIR, "season_table_regraded.csv"))
hdr("5. DONE")
say("Wrote ", file.path(ART_DIR, "app_data.rds"), " (", round(file.size(file.path(ART_DIR, "app_data.rds")) / 1e6, 1), " MB) and season_table_regraded.csv")
say("Elapsed (min): ", round(as.numeric(difftime(Sys.time(), t_start, units = "mins")), 1))
