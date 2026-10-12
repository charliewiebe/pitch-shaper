# 02_build_models.R

# config
QUICK      <- FALSE
SEASONS    <- 2023:2026
RAW_FILE   <- "statcast_data/statcast_raw_2023_2026.rds"
CSV_DIR    <- c(".", "Downloads", "~/Downloads")
OUT_DIR    <- "model_out"
ART_DIR    <- "app_artifacts"
USE_CACHE  <- TRUE
SEED       <- 42
NTHREAD    <- max(1L, parallel::detectCores() - 1L)

PT_KEEP    <- c("FF", "SI", "FC", "SL", "ST", "SV", "CU", "CH", "FS")
MIN_N_MOVE <- 50
FB_MIN     <- 30 
VAL_FRAC   <- 0.15
K_REF      <- 300
MOVE_SET   <- "M3"
GRADER_VARIANT <- "auto"

SCALE <- if (QUICK) 1 / 8 else 1
if (QUICK) { OUT_DIR <- paste0(OUT_DIR, "_quick"); ART_DIR <- paste0(ART_DIR, "_quick") }
thr   <- function(x) max(5, round(x * SCALE))

# setup
pkgs <- c("data.table", "lightgbm")
new  <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(new)) install.packages(new)
suppressPackageStartupMessages({ library(data.table); library(lightgbm) })
dir.create(OUT_DIR, showWarnings = FALSE); dir.create(ART_DIR, showWarnings = FALSE)
options(width = 220, scipen = 999, datatable.print.class = FALSE, datatable.showProgress = FALSE)
set.seed(SEED)
t_start <- Sys.time()

RPT  <- file.path(OUT_DIR, "model_report.txt"); if (file.exists(RPT)) file.remove(RPT)
emit <- function(txt) { cat(txt, sep = "\n"); cat(txt, file = RPT, sep = "\n", append = TRUE) }
say  <- function(...) emit(paste0(...))
hdr  <- function(x) emit(c("", "", strrep("=", 110), x, strrep("=", 110)))
sub_ <- function(x) emit(c("", paste0("--- ", x, " ---")))
rnd  <- function(d, k = 3) d[, lapply(.SD, function(x) if (is.double(x)) round(x, k) else x)]
shw  <- function(x, k = 3) emit(capture.output(print(rnd(as.data.table(x), k), nrows = 1000L, topn = 1000L, row.names = FALSE)))
sec  <- function(title, expr) {                       # report-only blocks: a failure is logged, not fatal
  hdr(title)
  tryCatch(expr, error = function(e) say("!! SECTION FAILED: ", conditionMessage(e)))
  invisible(NULL)
}
tick <- function(...) message(format(Sys.time(), "%H:%M:%S"), "  ", ...)

wrmse <- function(y, p, w) { ok <- is.finite(y) & is.finite(p); sqrt(sum(w[ok] * (y[ok] - p[ok])^2) / sum(w[ok])) }
wr2   <- function(y, p, w) { ok <- is.finite(y) & is.finite(p); y <- y[ok]; p <- p[ok]; w <- w[ok]
                             1 - sum(w * (y - p)^2) / sum(w * (y - sum(w * y) / sum(w))^2) }
wcor  <- function(a, b, w) { ok <- is.finite(a) & is.finite(b) & is.finite(w) & w > 0
                             if (sum(ok) < 5) return(NA_real_)
                             cov.wt(cbind(a[ok], b[ok]), wt = w[ok], cor = TRUE)$cor[1, 2] }
mat   <- function(d, feats, rows = NULL) {
  x <- as.matrix(if (is.null(rows)) d[, feats, with = FALSE] else d[rows, feats, with = FALSE])
  storage.mode(x) <- "double"; x
}

# LightGBM wrappers (the only places the lightgbm API is touched)
lgb_fit <- function(X, y, w = NULL, Xv = NULL, yv = NULL, wv = NULL, params, nrounds, esr = NULL) {
  dtr <- lgb.Dataset(X, label = y, weight = w)
  if (is.null(Xv)) {
    bst <- lgb.train(params = params, data = dtr, nrounds = nrounds, verbose = -1L)
    return(list(bst = bst, best_iter = nrounds, val_loss = NA_real_))
  }
  dva <- lgb.Dataset.create.valid(dtr, Xv, label = yv, weight = wv)
  bst <- lgb.train(params = params, data = dtr, nrounds = nrounds, valids = list(valid = dva),
                   early_stopping_rounds = esr, verbose = -1L)
  ev  <- unlist(bst$record_evals[["valid"]][[1]][["eval"]])
  list(bst = bst, best_iter = if (length(ev)) which.min(ev) else nrounds, val_loss = if (length(ev)) min(ev) else NA_real_)
}
lgb_pred <- function(m, X) predict(m$bst, X, num_iteration = m$best_iter)
P_BASE   <- list(objective = "regression", metric = "l2", verbosity = -1L, seed = SEED, force_col_wise = TRUE)

local({                                              # 5-second API smoke test before anything expensive
  X <- matrix(rnorm(8000), 2000, 4, dimnames = list(NULL, paste0("f", 1:4))); y <- X[, 1] + rnorm(2000)
  m <- lgb_fit(X[1:1500, ], y[1:1500], w = rep(1, 1500), Xv = X[1501:2000, ], yv = y[1501:2000], wv = rep(1, 500),
               params = c(P_BASE, list(learning_rate = 0.1, num_leaves = 7L, num_threads = 2L)), nrounds = 30L, esr = 5L)
  p <- lgb_pred(m, X[1501:2000, ]); f <- tempfile(fileext = ".txt")
  lgb.save(m$bst, f, num_iteration = m$best_iter); p2 <- predict(lgb.load(f), X[1501:2000, ])
  stopifnot(length(p) == 500, m$best_iter >= 1, max(abs(p - p2)) < 1e-6, nrow(lgb.importance(m$bst)) >= 1)
  m0 <- lgb_fit(X, y, params = c(P_BASE, list(num_threads = 2L)), nrounds = 5L); stopifnot(length(lgb_pred(m0, X)) == 2000)
})
tick("lightgbm ", as.character(packageVersion("lightgbm")), " smoke test passed")

# pitch table
BAT_FEATS <- c("b_xwoba", "b_whiff", "b_chase", "b_xwobacon")
PREP_FILE <- file.path(OUT_DIR, if (QUICK) "prep_quick.rds" else "prep.rds")

add_eng <- function(d) {      # arsenal differentials, league-relative z, pitch-type dummies (pitch rows and shapes alike)
  d[, `:=`(velo_diff = fifelse(is_fb == 1, 0, release_speed - fb_velo),
           hb_diff   = fifelse(is_fb == 1, 0, hb_arm - fb_hb),
           ivb_diff  = fifelse(is_fb == 1, 0, ivb - fb_ivb))]
  d[lgz, on = .(game_year, pt), `:=`(velo_z = (release_speed - i.m_velo) / i.s_velo,
                                     hb_z   = (hb_arm - i.m_hb) / i.s_hb,
                                     ivb_z  = (ivb - i.m_ivb) / i.s_ivb)]
  for (p in PT_KEEP) set(d, j = paste0("pt_", p), value = as.numeric(d$pt == p))
  invisible(d)
}

if (USE_CACHE && file.exists(PREP_FILE)) {
  tick("loading cached pitch table ", PREP_FILE)
  prep <- readRDS(PREP_FILE)
  sc <- prep$sc; setDT(sc); crv <- prep$crv; lgz <- prep$lgz; bat_means <- prep$bat_means
  agg <- prep$agg; fb <- prep$fb; info <- prep$info; rm(prep)
} else {
  tick("loading ", RAW_FILE)
  sc <- readRDS(RAW_FILE); setDT(sc)
  keep <- c("game_pk", "game_date", "game_year", "at_bat_number", "pitch_number", "pitcher", "batter", "player_name",
            "p_throws", "stand", "pitch_type", "release_speed", "release_spin_rate", "spin_axis", "release_extension",
            "release_pos_x", "release_pos_z", "arm_angle", "pfx_x", "pfx_z", "plate_x", "plate_z", "sz_top", "sz_bot",
            "zone", "balls", "strikes", "description", "launch_speed", "launch_angle",
            "estimated_woba_using_speedangle", "woba_value", "woba_denom", "delta_run_exp", "home_team")
  miss <- setdiff(keep, names(sc)); if (length(miss)) stop("Missing columns in raw file: ", paste(miss, collapse = ", "))
  sc <- sc[, keep, with = FALSE]; gc()
  if (QUICK) { g <- unique(sc$game_pk); sc <- sc[game_pk %in% sample(g, length(g) %/% 8L)] }
  for (cc in c("release_speed", "release_spin_rate", "spin_axis", "release_extension", "release_pos_x", "release_pos_z",
               "arm_angle", "pfx_x", "pfx_z", "plate_x", "plate_z", "sz_top", "sz_bot", "balls", "strikes",
               "launch_speed", "launch_angle", "estimated_woba_using_speedangle", "woba_value", "woba_denom", "delta_run_exp"))
    if (!is.double(sc[[cc]])) set(sc, j = cc, value = suppressWarnings(as.numeric(sc[[cc]])))
  for (cc in c("game_year", "zone", "pitcher", "batter"))
    if (!is.integer(sc[[cc]])) set(sc, j = cc, value = suppressWarnings(as.integer(sc[[cc]])))
  info <- list(n_raw = nrow(sc))

  # outcome classes
  oc_map <- c(ball = "ball", blocked_ball = "ball", pitchout = "ball", called_strike = "cs",
              swinging_strike = "whiff", swinging_strike_blocked = "whiff", foul_tip = "whiff", swinging_pitchout = "whiff",
              foul = "foul", foul_pitchout = "foul", foul_bunt = "bunt", bunt_foul_tip = "bunt", missed_bunt = "bunt",
              hit_by_pitch = "hbp", hit_into_play = "bip")
  sc[, oc := unname(oc_map[description])]
  sc <- sc[!is.na(oc) & !is.na(delta_run_exp) & balls <= 3 & strikes <= 2]
  info$n_outcome <- nrow(sc)


  crv <- sc[oc != "bip", .(rv = mean(delta_run_exp), n = .N), by = .(balls, strikes, oc)]
  sc[crv, on = .(balls, strikes, oc), xrv := i.rv]
  tick("fitting batted-ball run value model (5-fold out-of-fold)")
  ok   <- which(sc$oc == "bip" & !is.na(sc$launch_speed) & !is.na(sc$launch_angle))
  Xb   <- cbind(launch_speed = sc$launch_speed[ok], launch_angle = sc$launch_angle[ok], balls = sc$balls[ok], strikes = sc$strikes[ok])
  yb   <- sc$delta_run_exp[ok]
  fold <- sample(rep_len(1:5, length(ok))); oof <- numeric(length(ok))
  P_XRV <- c(P_BASE, list(learning_rate = 0.1, num_leaves = 63L, min_data_in_leaf = 100L, lambda_l2 = 5, num_threads = NTHREAD))
  for (k in 1:5) {
    m <- lgb_fit(Xb[fold != k, ], yb[fold != k], params = P_XRV, nrounds = if (QUICK) 80L else 300L)
    oof[fold == k] <- lgb_pred(m, Xb[fold == k, , drop = FALSE])
  }
  set(sc, i = ok, j = "xrv", value = oof)
  info$xrv_bip_r2  <- 1 - sum((yb - oof)^2) / sum((yb - mean(yb))^2)
  info$xrv_bip_cor_xwoba <- cor(oof, sc$estimated_woba_using_speedangle[ok], use = "pairwise.complete.obs")
  bm <- sc[oc == "bip", .(m = mean(delta_run_exp)), by = .(balls, strikes)]
  sc[bm, on = .(balls, strikes), bip_m := i.m]
  sc[oc == "bip" & is.na(xrv), xrv := bip_m]; sc[, bip_m := NULL]
  rm(Xb, yb, oof, fold, ok); gc()

  # geometry & flags
  sc[, sgn  := fifelse(p_throws == "R", 1, -1)]
  sc[, bsgn := fifelse(stand == "R", 1, -1)]
  sc[, `:=`(hb_arm    = -12 * pfx_x * sgn,
            ivb       = 12 * pfx_z,
            rel_x_arm = -release_pos_x * sgn,
            axis_m    = 180 + sgn * (spin_axis - 180),
            same_hand = as.numeric(stand == p_throws),
            is_lhp    = as.numeric(p_throws == "L"),
            px_bat    = plate_x * bsgn,
            pz_norm   = (plate_z - sz_bot) / (sz_top - sz_bot),
            swing     = description %in% c("swinging_strike", "swinging_strike_blocked", "foul", "foul_tip", "hit_into_play"),
            whiff     = oc == "whiff",
            in_zone   = fifelse(is.na(zone), NA, zone <= 9L))]
  sc[(sz_top - sz_bot) < 0.5, pz_norm := NA_real_]
  sc[, pt := pitch_type]; sc[pitch_type %in% c("KC", "CS"), pt := "CU"]

  # batter quality
  sc[, xw := fifelse(is.na(estimated_woba_using_speedangle), woba_value, estimated_woba_using_speedangle)]
  bat <- sc[, .(pa = sum(woba_denom == 1, na.rm = TRUE), xw_sum = sum(xw[woba_denom == 1], na.rm = TRUE),
                sw = sum(swing), wh = sum(whiff),
                ooz = sum(!in_zone, na.rm = TRUE), ch = sum(swing & !in_zone, na.rm = TRUE),
                bbe = sum(oc == "bip" & !is.na(estimated_woba_using_speedangle)),
                xc_sum = sum(estimated_woba_using_speedangle[oc == "bip"], na.rm = TRUE)), by = .(batter, game_year)]
  lgb_ <- bat[, .(l_xw = sum(xw_sum) / sum(pa), l_wh = sum(wh) / sum(sw), l_ch = sum(ch) / sum(ooz), l_xc = sum(xc_sum) / sum(bbe)), by = game_year]
  bat <- merge(bat, lgb_, by = "game_year")
  kk <- c(pa = 150, sw = 150, ooz = 200, bbe = 100) * SCALE
  bat[, `:=`(b_xwoba    = (xw_sum + kk[["pa"]] * l_xw) / (pa + kk[["pa"]]),
             b_whiff    = (wh + kk[["sw"]] * l_wh) / (sw + kk[["sw"]]),
             b_chase    = (ch + kk[["ooz"]] * l_ch) / (ooz + kk[["ooz"]]),
             b_xwobacon = (xc_sum + kk[["bbe"]] * l_xc) / (bbe + kk[["bbe"]]))]
  sc[bat, on = .(batter, game_year), (BAT_FEATS) := mget(paste0("i.", BAT_FEATS))]
  sc[, xw := NULL]

  # modeling rows
  sc <- sc[pt %in% PT_KEEP & !is.na(release_speed) & !is.na(hb_arm) & !is.na(ivb) & !is.na(px_bat) & !is.na(pz_norm)]
  info$n_model <- nrow(sc)
  bat_means <- as.list(sc[, lapply(.SD, mean, na.rm = TRUE), .SDcols = BAT_FEATS])


  agg <- sc[, .(n = .N, player_name = player_name[1], p_throws = p_throws[1], is_lhp = is_lhp[1],
                release_speed = mean(release_speed), release_spin_rate = mean(release_spin_rate, na.rm = TRUE),
                hb_arm = mean(hb_arm), ivb = mean(ivb),
                release_extension = mean(release_extension, na.rm = TRUE), rel_x_arm = mean(rel_x_arm, na.rm = TRUE),
                release_pos_z = mean(release_pos_z, na.rm = TRUE), arm_angle = mean(arm_angle, na.rm = TRUE),
                ax_s = mean(sin(axis_m * pi / 180), na.rm = TRUE), ax_c = mean(cos(axis_m * pi / 180), na.rm = TRUE),
                col_share = mean(home_team == "COL"), same_share = mean(same_hand),
                rv100 = 100 * mean(delta_run_exp), xrv100 = 100 * mean(xrv),
                whiff_sw = sum(whiff) / sum(swing), zone_pct = mean(in_zone, na.rm = TRUE)),
            by = .(pitcher, game_year, pt)]
  agg[, `:=`(axis_m = (atan2(ax_s, ax_c) * 180 / pi) %% 360, axis_R = sqrt(ax_s^2 + ax_c^2))]
  fb <- agg[pt %in% c("FF", "SI", "FC") & n >= thr(FB_MIN)][order(-n),
            .(fb_type = pt[1], fb_velo = release_speed[1], fb_hb = hb_arm[1], fb_ivb = ivb[1]), by = .(pitcher, game_year)]
  lgz <- sc[, .(m_velo = mean(release_speed), s_velo = sd(release_speed), m_hb = mean(hb_arm), s_hb = sd(hb_arm),
                m_ivb = mean(ivb), s_ivb = sd(ivb)), by = .(game_year, pt)]
  sc[fb, on = .(pitcher, game_year), `:=`(fb_type = i.fb_type, fb_velo = i.fb_velo, fb_hb = i.fb_hb, fb_ivb = i.fb_ivb)]
  sc[, is_fb := as.numeric(!is.na(fb_type) & pt == fb_type)]
  add_eng(sc)
  drop <- intersect(c("pfx_x", "pfx_z", "release_pos_x", "plate_x", "plate_z", "sz_top", "sz_bot", "zone", "sgn", "bsgn",
                      "launch_speed", "launch_angle", "woba_value", "woba_denom", "estimated_woba_using_speedangle",
                      "pitch_type", "spin_axis", "description", "game_date", "at_bat_number", "pitch_number", "fb_type"), names(sc))
  sc[, (drop) := NULL]; gc()
  if (USE_CACHE) { tick("caching prepared pitch table")
    saveRDS(list(sc = sc, crv = crv, lgz = lgz, bat_means = bat_means, agg = agg, fb = fb, info = info), PREP_FILE, compress = FALSE) }
}
tick("pitch table ready: ", format(nrow(sc), big.mark = ","), " rows")

sec("0. RUN INFO", {
  say("Run: ", format(Sys.time()), " | ", R.version.string, " | data.table ", as.character(packageVersion("data.table")),
      " | lightgbm ", as.character(packageVersion("lightgbm")), " | QUICK = ", QUICK, " | threads = ", NTHREAD)
  say("Rows: raw ", info$n_raw, " -> with outcome ", info$n_outcome, " -> modelling ", info$n_model)
})
sec("1. EXPECTED RUN VALUE (xRV) TARGET", {
  say("Batted-ball model (EV, LA, count -> run value), out-of-fold R2 vs delta_run_exp: ", round(info$xrv_bip_r2, 4),
      " | r with Savant xwOBA: ", round(info$xrv_bip_cor_xwoba, 4))
  sub_("count-level run value of non-BIP outcomes (batting side, + = good for hitter)")
  shw(dcast(crv[oc != "bunt"], balls + strikes ~ oc, value.var = "rv"), 4)
  sub_("noise removed: SD of actual run value vs xRV, and mean check")
  shw(sc[, .(n = .N, sd_actual = sd(delta_run_exp), sd_xrv = sd(xrv), mean_actual = mean(delta_run_exp), mean_xrv = mean(xrv)), by = oc][order(-n)], 4)
  shw(sc[, .(oc = "ALL", n = .N, sd_actual = sd(delta_run_exp), sd_xrv = sd(xrv), mean_actual = mean(delta_run_exp), mean_xrv = mean(xrv))], 4)
  sub_("batter-quality features (pitch-weighted)")
  shw(sc[, lapply(.SD, function(x) c(mean(x, na.rm = TRUE), sd(x, na.rm = TRUE), min(x, na.rm = TRUE), max(x, na.rm = TRUE))), .SDcols = BAT_FEATS][, stat := c("mean", "sd", "min", "max")], 4)
})

# season table + active spin + cross-check
get_csv <- function(name) {
  if (exists(name, envir = globalenv()) && is.data.frame(get(name, envir = globalenv()))) return(as.data.table(get(name, envir = globalenv())))
  f <- file.path(CSV_DIR, paste0(name, ".csv")); f <- f[file.exists(f)]
  if (!length(f)) stop(name, ".csv not found in: ", paste(CSV_DIR, collapse = ", "), " (working directory ", getwd(), ")")
  fread(f[1])
}
AS_MAP <- c(fourseam = "FF", sinker = "SI", cutter = "FC", changeup = "CH", splitter = "FS", curve = "CU",
            slider = "SL", sweeper = "ST", slurve = "SV")
asp <- rbindlist(lapply(SEASONS, function(y) {
  a <- get_csv(sprintf("active_spin_%d", y))
  rbindlist(lapply(names(AS_MAP), function(k) data.table(pitcher = as.integer(a$entity_id), game_year = as.integer(y), pt = AS_MAP[[k]],
                                                          active_spin = suppressWarnings(as.numeric(a[[paste0("active_spin_", k)]])))))
}))[!is.na(active_spin)]
trw <- get_csv("pitch_traits_2023_through_2026")
csv <- rbindlist(lapply(PT_KEEP, function(p) { lo <- tolower(p)
  data.table(pitcher = as.integer(trw$player_id), game_year = as.integer(trw$year), pt = p, csv_hand = trw$pitch_hand,
             csv_n = as.numeric(trw$n) * as.numeric(trw[[paste0("n_", lo, "_formatted")]]) / 100, csv_arm_angle = as.numeric(trw$arm_angle),
             csv_velo = as.numeric(trw[[paste0(lo, "_avg_speed")]]), csv_spin = as.numeric(trw[[paste0(lo, "_avg_spin")]]),
             csv_bx = as.numeric(trw[[paste0(lo, "_avg_break_x")]]), csv_ivb = as.numeric(trw[[paste0(lo, "_avg_break_z_induced")]]))
}))[!is.na(csv_velo)]
csv[, csv_hb_arm := -csv_bx * fifelse(csv_hand == "R", 1, -1)]

S <- copy(agg)
S[asp, on = .(pitcher, game_year, pt), active_spin := i.active_spin]
S[csv, on = .(pitcher, game_year, pt), `:=`(csv_n = i.csv_n, csv_velo = i.csv_velo, csv_spin = i.csv_spin, csv_hb_arm = i.csv_hb_arm,
                                           csv_ivb = i.csv_ivb, csv_arm_angle = i.csv_arm_angle)]
S[fb, on = .(pitcher, game_year), `:=`(fb_type = i.fb_type, fb_velo = i.fb_velo, fb_hb = i.fb_hb, fb_ivb = i.fb_ivb)]
S[, is_fb := as.numeric(!is.na(fb_type) & pt == fb_type)]
add_eng(S)

sec("2. SEASON TABLE AND CSV CROSS-CHECK", {
  say("Pitcher-season-pitch rows: ", nrow(S), " | with active spin: ", S[!is.na(active_spin), .N],
      " | with active spin and n >= ", thr(MIN_N_MOVE), ": ", S[!is.na(active_spin) & n >= thr(MIN_N_MOVE), .N])
  shw(S[, .(rows = .N, with_active_spin = sum(!is.na(active_spin)), usable = sum(!is.na(active_spin) & n >= thr(MIN_N_MOVE)),
            in_traits_csv = sum(!is.na(csv_velo)), mean_active_spin = mean(active_spin, na.rm = TRUE)), by = pt][order(-rows)], 1)
  sub_("pitch-level aggregate minus pitch_traits CSV (rows with n >= 100): same definition if mean ~ 0 and r ~ 1")
  cc <- S[!is.na(csv_velo) & n >= thr(100)]
  cmp <- function(a, b, lab) data.table(trait = lab, n = sum(is.finite(a - b)), r = cor(a, b, use = "pairwise.complete.obs"),
                                        mean_diff = mean(a - b, na.rm = TRUE), median_abs_diff = median(abs(a - b), na.rm = TRUE),
                                        p95_abs_diff = quantile(abs(a - b), .95, na.rm = TRUE))
  shw(rbind(cmp(cc$n, cc$csv_n, "pitches"), cmp(cc$release_speed, cc$csv_velo, "velo"), cmp(cc$release_spin_rate, cc$csv_spin, "spin"),
            cmp(cc$hb_arm, cc$csv_hb_arm, "hb_arm (in)"), cmp(cc$ivb, cc$csv_ivb, "ivb (in)"),
            cmp(cc$arm_angle, cc$csv_arm_angle, "arm_angle (pitch-type mean vs season)")), 3)
  sub_("same check for IVB by pitch type")
  shw(cc[, .(n = .N, mean_diff_ivb = mean(ivb - csv_ivb), mean_diff_hb = mean(hb_arm - csv_hb_arm)), by = pt][order(-n)], 3)
})

# movement model
M <- S[!is.na(active_spin) & n >= thr(MIN_N_MOVE) & is.finite(axis_m) & is.finite(release_spin_rate) & is.finite(arm_angle) &
         is.finite(release_extension) & is.finite(release_pos_z) & is.finite(rel_x_arm)]
add_move_feats <- function(d) {           # everything here is a deterministic function of the sliders
  d[, eff := active_spin / 100]
  d[, `:=`(sin_ax = sin(axis_m * pi / 180), cos_ax = cos(axis_m * pi / 180))]
  d[, `:=`(spin_x = -release_spin_rate * eff * sin_ax / 1000,     # transverse spin toward arm side (k rpm)
           spin_z = -release_spin_rate * eff * cos_ax / 1000)]    # transverse spin producing lift (k rpm)
  invisible(d)
}
add_move_feats(M)
M[, w := pmin(n, 500)]
pf <- data.table(pitcher = unique(M$pitcher)); pf[, fold := sample(rep_len(1:5, .N))]
M[pf, on = "pitcher", fold := i.fold]
M[, pt_f := factor(pt, levels = PT_KEEP)]

F_SPIN <- c("release_speed", "release_spin_rate", "eff", "sin_ax", "cos_ax", "spin_x", "spin_z", "col_share")
F_PT   <- paste0("pt_", PT_KEEP)
MOVE_SETS <- list(
  M1 = c(F_SPIN, F_PT),
  M2 = c(F_SPIN, F_PT, "arm_angle", "release_extension"),
  M3 = c(F_SPIN, F_PT, "arm_angle", "release_extension", "release_pos_z"),
  M4 = c(F_SPIN, F_PT, "arm_angle", "release_extension", "release_pos_z", "rel_x_arm"),
  M5 = c(F_SPIN, F_PT, "release_extension", "release_pos_z", "rel_x_arm"),
  M3_no_type = c(F_SPIN, "arm_angle", "release_extension", "release_pos_z"),
  M3_no_active_spin = setdiff(c(F_SPIN, F_PT, "arm_angle", "release_extension", "release_pos_z"), c("eff", "spin_x", "spin_z")))
P_MOVE  <- c(P_BASE, list(learning_rate = 0.03, num_leaves = 15L, min_data_in_leaf = 15L, feature_fraction = 0.9,
                          bagging_fraction = 0.8, bagging_freq = 1L, lambda_l2 = 1, num_threads = min(4L, NTHREAD)))
NR_MOVE <- if (QUICK) 400L else 3000L

cv_move <- function(feats, target) {
  oof <- rep(NA_real_, nrow(M)); it <- integer(5)
  for (k in 1:5) {
    tr <- M$fold != k
    m <- lgb_fit(mat(M, feats, tr), M[[target]][tr], w = M$w[tr], Xv = mat(M, feats, !tr), yv = M[[target]][!tr], wv = M$w[!tr],
                 params = P_MOVE, nrounds = NR_MOVE, esr = 100L)
    oof[!tr] <- lgb_pred(m, mat(M, feats, !tr)); it[k] <- m$best_iter
  }
  list(oof = oof, iters = it)
}
tick("movement model: cross-validating ", length(MOVE_SETS), " feature sets on ", nrow(M), " rows")
move_cv <- list(); move_tab <- list()
for (s in names(MOVE_SETS)) for (tg in c("hb_arm", "ivb")) {
  r <- cv_move(MOVE_SETS[[s]], tg); move_cv[[paste(s, tg)]] <- r
  move_tab[[length(move_tab) + 1L]] <- data.table(set = s, target = tg, n_feats = length(MOVE_SETS[[s]]), rmse_in = wrmse(M[[tg]], r$oof, M$w),
                                                   r2 = wr2(M[[tg]], r$oof, M$w), mean_best_iter = round(mean(r$iters)))
}
move_tab <- rbindlist(move_tab)
lm_form <- function(y) as.formula(paste(y, "~ pt_f * (spin_x + spin_z + release_speed + release_spin_rate + eff + arm_angle +",
                                        "release_extension + release_pos_z) + col_share"))
lm_oof <- sapply(c("hb_arm", "ivb"), function(tg) { o <- rep(NA_real_, nrow(M))
  for (k in 1:5) o[M$fold == k] <- suppressWarnings(predict(lm(lm_form(tg), data = M[fold != k], weights = w), M[fold == k])); o })

MOVE_FEATS <- MOVE_SETS[[MOVE_SET]]
M[, `:=`(hb_oof = move_cv[[paste(MOVE_SET, "hb_arm")]]$oof, ivb_oof = move_cv[[paste(MOVE_SET, "ivb")]]$oof)]
nr_hb  <- max(50L, as.integer(round(mean(move_cv[[paste(MOVE_SET, "hb_arm")]]$iters))))
nr_ivb <- max(50L, as.integer(round(mean(move_cv[[paste(MOVE_SET, "ivb")]]$iters))))
move_hb  <- lgb_fit(mat(M, MOVE_FEATS), M$hb_arm, w = M$w, params = P_MOVE, nrounds = nr_hb)
move_ivb <- lgb_fit(mat(M, MOVE_FEATS), M$ivb,    w = M$w, params = P_MOVE, nrounds = nr_ivb)
lgb.save(move_hb$bst,  file.path(ART_DIR, "movement_hb.txt"))
lgb.save(move_ivb$bst, file.path(ART_DIR, "movement_ivb.txt"))
saveRDS(list(hb = lm(lm_form("hb_arm"), data = M, weights = w, model = FALSE), ivb = lm(lm_form("ivb"), data = M, weights = w, model = FALSE)),
        file.path(ART_DIR, "movement_lm.rds"))

sec("3. MOVEMENT MODEL (pitcher-grouped 5-fold CV, weights = min(n, 500))", {
  say("Rows: ", nrow(M), " | pitchers: ", uniqueN(M$pitcher), " | saved set: ", MOVE_SET, " (", paste(MOVE_FEATS, collapse = ", "), ")")
  sub_("GBM by feature set (rmse in inches)")
  shw(dcast(move_tab, set + n_feats ~ target, value.var = c("rmse_in", "r2", "mean_best_iter")), 3)
  sub_("linear baseline with pitch-type interactions (physics features, same folds)")
  shw(data.table(target = c("hb_arm", "ivb"), rmse_in = c(wrmse(M$hb_arm, lm_oof[, 1], M$w), wrmse(M$ivb, lm_oof[, 2], M$w)),
                 r2 = c(wr2(M$hb_arm, lm_oof[, 1], M$w), wr2(M$ivb, lm_oof[, 2], M$w))), 3)
  sub_(paste0("saved set ", MOVE_SET, " by pitch type (out-of-fold)"))
  shw(M[, .(n = .N, sd_hb = sd(hb_arm), rmse_hb = wrmse(hb_arm, hb_oof, w), r2_hb = wr2(hb_arm, hb_oof, w),
            sd_ivb = sd(ivb), rmse_ivb = wrmse(ivb, ivb_oof, w), r2_ivb = wr2(ivb, ivb_oof, w),
            bias_hb = mean(hb_oof - hb_arm), bias_ivb = mean(ivb_oof - ivb)), by = pt][order(-n)], 3)
  sub_("out-of-fold error by sample size")
  shw(M[, .(n = .N, rmse_hb = sqrt(mean((hb_arm - hb_oof)^2)), rmse_ivb = sqrt(mean((ivb - ivb_oof)^2))),
        by = .(pitches = cut(n, c(0, 100, 200, 400, 800, Inf), right = FALSE))][order(pitches)], 3)
  sub_("feature importance (gain share), HB then IVB")
  shw(head(lgb.importance(move_hb$bst)[, .(Feature, Gain)], 15), 4)
  shw(head(lgb.importance(move_ivb$bst)[, .(Feature, Gain)], 15), 4)
  sub_("largest out-of-fold misses (n >= 300)")
  shw(head(M[n >= thr(300)][order(-((hb_arm - hb_oof)^2 + (ivb - ivb_oof)^2)),
             .(player_name, game_year, pt, n, active_spin, axis_m, hb_arm, hb_oof, ivb, ivb_oof, col_share)], 15), 1)
})

# pitch grader (pitch level)
F_STUFF <- c("release_speed", "hb_arm", "ivb", "release_extension", "arm_angle", "release_pos_z")
F_ARS   <- c("velo_diff", "hb_diff", "ivb_diff", "is_fb")
F_CTX   <- c("same_hand", "is_lhp", "balls", "strikes", "px_bat", "pz_norm", BAT_FEATS)
F_Z     <- c("velo_z", "hb_z", "ivb_z")
VARIANTS <- list(B_context_only = F_CTX,
                 V0_raw    = c(F_STUFF, F_ARS, F_CTX),
                 V1_type   = c(F_STUFF, F_ARS, F_CTX, F_PT),
                 V2_z      = c(F_STUFF, F_ARS, F_CTX, F_Z),
                 V3_type_z = c(F_STUFF, F_ARS, F_CTX, F_PT, F_Z))
P_GRADE  <- c(P_BASE, list(learning_rate = 0.05, num_leaves = 63L, min_data_in_leaf = 500L, feature_fraction = 0.8,
                           bagging_fraction = 0.6, bagging_freq = 1L, lambda_l2 = 20, max_bin = 255L, num_threads = NTHREAD))
NR_GRADE <- if (QUICK) 150L else 3000L
ESR_GRADE <- if (QUICK) 15L else 60L

vp     <- sample(unique(sc$pitcher), round(VAL_FRAC * uniqueN(sc$pitcher)))
is_val <- sc$pitcher %in% vp
y_all  <- sc$xrv; yv <- y_all[is_val]; mu_tr <- mean(y_all[!is_val]); mse0 <- mean((yv - mu_tr)^2)
val_id <- sc[is_val, .(pitcher, game_year, pt)]

graders <- list(); gtab <- list(); se_val <- list()
train_grader <- function(tag, feats) {
  t0 <- Sys.time(); tick("grader ", tag, ": ", length(feats), " features")
  Xv <- mat(sc, feats, is_val)
  m  <- lgb_fit(mat(sc, feats, !is_val), y_all[!is_val], Xv = Xv, yv = yv, params = P_GRADE, nrounds = NR_GRADE, esr = ESR_GRADE)
  pv <- lgb_pred(m, Xv); rm(Xv); gc()
  va <- copy(val_id)[, `:=`(y = yv, p = pv)][, .(n = .N, y = mean(y), p = mean(p)), by = .(pitcher, game_year, pt)][n >= thr(200)]
  graders[[tag]] <<- m; se_val[[tag]] <<- (yv - pv)^2
  gtab[[tag]] <<- data.table(variant = tag, n_feats = length(feats), best_iter = m$best_iter, val_mse = mean((yv - pv)^2),
                             skill_pct = 100 * (1 - mean((yv - pv)^2) / mse0), cell_r = wcor(va$p, va$y, va$n),
                             minutes = as.numeric(difftime(Sys.time(), t0, units = "mins")))
  invisible(pv)
}
for (v in names(VARIANTS)) train_grader(v, VARIANTS[[v]])
cand <- c("V0_raw", "V1_type", "V2_z", "V3_type_z")
d_mse <- function(v) mean(se_val[[v]] - se_val[["V0_raw"]])
d_se  <- function(v) sd(se_val[[v]] - se_val[["V0_raw"]]) / sqrt(length(yv))
best  <- "V0_raw"
if (GRADER_VARIANT == "auto") {
  dz <- sapply(setdiff(cand, "V0_raw"), function(v) d_mse(v) / d_se(v))
  if (any(dz < -2)) best <- names(dz)[which.min(sapply(names(dz), d_mse) + ifelse(dz < -2, 0, Inf))]
} else best <- GRADER_VARIANT
VARIANTS[["V4_best_plus_release_side"]] <- c(VARIANTS[[best]], "rel_x_arm")
train_grader("V4_best_plus_release_side", VARIANTS[["V4_best_plus_release_side"]])
GM <- graders[[best]]; GF <- VARIANTS[[best]]
pv_best <- lgb_pred(GM, mat(sc, GF, is_val))
lgb.save(GM$bst, file.path(ART_DIR, "grader.txt"), num_iteration = GM$best_iter)
gtab <- rbindlist(gtab)
gtab[, `:=`(d_mse_vs_V0_x1e6 = 1e6 * sapply(variant, d_mse), se_x1e6 = 1e6 * sapply(variant, d_se))]

sec("4. PITCH GRADER (target = xRV per pitch; 15% of pitchers held out)", {
  say("Train pitches: ", sum(!is_val), " | held-out pitches: ", sum(is_val), " | held-out pitchers: ", length(vp),
      " | held-out MSE of constant prediction: ", signif(mse0, 6))
  say("skill_pct = % of held-out MSE removed vs a constant. cell_r = weighted r between mean prediction and mean xRV by pitcher-season-pitch (n >= ",
      thr(200), ") on held-out pitchers.")
  shw(gtab, 4)
  say("Selected variant: ", best, " (", length(GF), " features). Rule: V0_raw unless another of V1-V3 beats it by more than 2 SE; V4 is reported only.")
  sub_("feature importance of selected variant (gain share)")
  shw(lgb.importance(GM$bst)[, .(Feature, Gain, Frequency)], 4)
  sub_("held-out skill by pitch type (selected variant vs context-only)")
  shw(data.table(pt = sc$pt[is_val], se = se_val[[best]], se_b = se_val[["B_context_only"]], se0 = (yv - mu_tr)^2)[,
        .(n = .N, skill_pct = 100 * (1 - sum(se) / sum(se0)), context_only_skill_pct = 100 * (1 - sum(se_b) / sum(se0))), by = pt][order(-n)], 3)
  sub_("held-out calibration: deciles of predicted xRV (per 100 pitches)")
  shw(data.table(p = pv_best, y = yv, a = sc$delta_run_exp[is_val])[, dec := cut(p, quantile(p, 0:10 / 10), include.lowest = TRUE, labels = FALSE)][,
        .(n = .N, pred_xrv100 = 100 * mean(p), mean_xrv100 = 100 * mean(y), mean_actual_rv100 = 100 * mean(a)), by = dec][order(dec)], 3)
})

# location-neutral grades
loc_ref <- sc[, .SD[sample(.N, min(.N, K_REF))], by = .(pt, same_hand), .SDcols = c("px_bat", "pz_norm", "balls", "strikes")]
plat    <- sc[, .(same_share = mean(same_hand)), by = .(pt, is_lhp)]

grade_shapes <- function(d, model = GM, feats = GF) {   # d: one row per pitch shape; returns xRV/100 vs opposite- and same-hand batters
  d <- copy(d); out <- matrix(NA_real_, nrow(d), 2, dimnames = list(NULL, c("opp", "same")))
  for (p in unique(d$pt)) for (h in 0:1) {
    ref <- loc_ref[pt == p & same_hand == h]; rid <- which(d$pt == p); K <- nrow(ref)
    if (!length(rid) || !K) next
    for (ix in split(rid, ceiling(seq_along(rid) / 400))) {
      a <- d[rep(ix, each = K)]
      a[, `:=`(px_bat = rep(ref$px_bat, length(ix)), pz_norm = rep(ref$pz_norm, length(ix)),
               balls = rep(ref$balls, length(ix)), strikes = rep(ref$strikes, length(ix)), same_hand = as.numeric(h))]
      for (b in BAT_FEATS) set(a, j = b, value = bat_means[[b]])
      out[ix, h + 1L] <- 100 * colMeans(matrix(lgb_pred(model, mat(a, feats)), nrow = K))
    }
  }
  out
}
overall <- function(d, g) { w <- plat[d, on = .(pt, is_lhp), same_share]; w * g[, "same"] + (1 - w) * g[, "opp"] }

tick("grading ", nrow(S), " season shapes over reference locations")
g <- grade_shapes(S)
S[, `:=`(g_opp = g[, "opp"], g_same = g[, "same"])]
S[, g_xrv100 := overall(S, g)]
ref_rows  <- S[n >= thr(100)]
scale_all <- ref_rows[, .(mu = weighted.mean(g_xrv100, n), sd = sqrt(sum(n * (g_xrv100 - weighted.mean(g_xrv100, n))^2) / sum(n)))]
scale_pt  <- ref_rows[, .(mu = weighted.mean(g_xrv100, n), sd = sqrt(sum(n * (g_xrv100 - weighted.mean(g_xrv100, n))^2) / sum(n)), rows = .N), by = pt]
S[, grade_plus := 100 - 10 * (g_xrv100 - scale_all$mu) / scale_all$sd]       # 100 = average pitch, +10 = 1 SD better
S[, is_heldout := pitcher %in% vp]


C <- M[, c("pitcher", "game_year", "pt", "is_lhp", "n", F_STUFF, "rel_x_arm", "fb_velo", "fb_hb", "fb_ivb", "is_fb", "hb_oof", "ivb_oof"), with = FALSE]
C[, `:=`(hb_arm = hb_oof, ivb = ivb_oof)]; add_eng(C)
gc_ <- grade_shapes(C); C[, g_pred_move := overall(C, gc_)]
C[S, on = .(pitcher, game_year, pt), g_actual_move := i.g_xrv100]

base_shape <- function(p, lhp = 0) {
  r <- S[pt == p & is_lhp == lhp & n >= thr(100), lapply(.SD, weighted.mean, w = n, na.rm = TRUE),
         .SDcols = c(F_STUFF, "rel_x_arm", "fb_velo", "fb_hb", "fb_ivb")]
  r[, `:=`(pt = p, is_lhp = lhp, game_year = max(SEASONS), is_fb = as.numeric(p %in% c("FF", "SI")))]
  r
}
sweep1 <- function(p, var, grid) {
  d <- base_shape(p)[rep(1L, length(grid))]; set(d, j = var, value = as.numeric(grid)); add_eng(d)
  gg <- grade_shapes(d)
  data.table(pt = p, var = var, value = grid, xrv100 = overall(d, gg), vs_same = gg[, "same"], vs_opp = gg[, "opp"])
}

sec("5. LOCATION-NEUTRAL GRADES (xRV per 100 pitches at reference locations; lower = better for the pitcher)", {
  say("Scale (n >= ", thr(100), ", pitch-weighted): mean ", round(scale_all$mu, 3), ", SD ", round(scale_all$sd, 3),
      " runs/100. grade_plus = 100 - 10 * z.")
  sub_("grade distribution by pitch type")
  shw(ref_rows[, .(rows = .N, mean_xrv100 = weighted.mean(g_xrv100, n), sd = sd(g_xrv100), p05 = quantile(g_xrv100, .05), p95 = quantile(g_xrv100, .95),
                   vs_same = weighted.mean(g_same, n), vs_opp = weighted.mean(g_opp, n),
                   actual_xrv100 = weighted.mean(xrv100, n), actual_rv100 = weighted.mean(rv100, n)), by = pt][order(mean_xrv100)], 3)
  cols <- c("player_name", "game_year", "pt", "n", "release_speed", "hb_arm", "ivb", "g_xrv100", "grade_plus", "xrv100", "rv100", "whiff_sw")
  sub_("best 20 pitches (n >= 300)");  shw(head(S[n >= thr(300)][order(g_xrv100), cols, with = FALSE], 20), 2)
  sub_("worst 20 pitches (n >= 300)"); shw(head(S[n >= thr(300)][order(-g_xrv100), cols, with = FALSE], 20), 2)
  sub_("best 3 per pitch type (n >= 200)")
  shw(S[n >= thr(200)][order(g_xrv100), head(.SD, 3), by = pt, .SDcols = setdiff(cols, "pt")], 2)
  sub_("grade vs actual results by pitcher-season-pitch (weighted r; grade is location-neutral, actuals are not)")
  rr <- function(d, lab) data.table(rows = lab, n = nrow(d), r_xrv100 = wcor(d$g_xrv100, d$xrv100, d$n), r_rv100 = wcor(d$g_xrv100, d$rv100, d$n),
                                    r_whiff = wcor(d$g_xrv100, d$whiff_sw, d$n))
  shw(rbind(rr(S[n >= thr(200)], "all, n>=200"), rr(S[n >= thr(200) & is_heldout == TRUE], "held-out pitchers, n>=200"),
            rr(S[n >= thr(500)], "all, n>=500")), 3)
  sub_("predicting NEXT season's results for the same pitcher and pitch (n >= 200 both years)")
  nx <- S[n >= thr(200), .(pitcher, pt, game_year = game_year - 1L, xrv_next = xrv100, rv_next = rv100, n_next = n)]
  G2 <- merge(S[n >= thr(200)], nx, by = c("pitcher", "pt", "game_year")); G2[, ww := pmin(n, n_next)]
  pr <- function(d, lab) data.table(rows = lab, n = nrow(d), grade_to_next_xrv = wcor(d$g_xrv100, d$xrv_next, d$ww),
                                    this_xrv_to_next_xrv = wcor(d$xrv100, d$xrv_next, d$ww), grade_to_next_rv = wcor(d$g_xrv100, d$rv_next, d$ww),
                                    this_rv_to_next_rv = wcor(d$rv100, d$rv_next, d$ww))
  shw(rbind(pr(G2, "all"), pr(G2[is_heldout == TRUE], "held-out pitchers")), 3)
  sub_("chain check: grade using PREDICTED movement (out-of-fold) vs grade using actual movement")
  shw(C[, .(rows = .N, r = wcor(g_pred_move, g_actual_move, n), rmse_xrv100 = sqrt(mean((g_pred_move - g_actual_move)^2)),
            sd_grade = sd(g_actual_move)), by = pt][order(-rows)], 3)
  shw(C[, .(pt = "ALL", rows = .N, r = wcor(g_pred_move, g_actual_move, n), rmse_xrv100 = sqrt(mean((g_pred_move - g_actual_move)^2)), sd_grade = sd(g_actual_move))], 3)
  sub_("one-variable sweeps from the average RHP shape of each pitch (does the grade move the right way?)")
  shw(rbind(sweep1("FF", "release_speed", seq(90, 100, 2)), sweep1("FF", "ivb", seq(10, 20, 2)), sweep1("FF", "hb_arm", seq(0, 15, 3)),
            sweep1("FF", "release_pos_z", seq(5, 6.5, 0.5)), sweep1("FF", "release_extension", seq(5.5, 7.5, 0.5)),
            sweep1("SI", "ivb", seq(0, 15, 3)), sweep1("SI", "hb_arm", seq(8, 20, 3)),
            sweep1("SL", "release_speed", seq(80, 90, 2)), sweep1("SL", "ivb", seq(-6, 9, 3)), sweep1("ST", "hb_arm", seq(-22, -6, 4)),
            sweep1("CU", "ivb", seq(-18, -2, 4)), sweep1("CH", "release_speed", seq(80, 90, 2)), sweep1("CH", "ivb", seq(-4, 12, 4)),
            sweep1("FS", "ivb", seq(-6, 9, 3))), 3)
})

# artifacts
cq <- function(x, pr) { c0 <- (atan2(mean(sin(x * pi / 180)), mean(cos(x * pi / 180))) * 180 / pi) %% 360   # quantiles of a circular variable
                         (quantile(((x - c0 + 180) %% 360) - 180, pr, names = FALSE) + c0) %% 360 }
sl_vars <- c("release_speed", "release_spin_rate", "active_spin", "arm_angle", "release_extension", "release_pos_z", "rel_x_arm", "hb_arm", "ivb")
sliders <- rbindlist(lapply(c(sl_vars, "axis_m"), function(v) M[, {
  q <- if (v == "axis_m") cq(get(v), c(.01, .5, .99)) else quantile(get(v), c(.01, .5, .99), names = FALSE)
  .(var = v, lo = q[1], mid = q[2], hi = q[3]) }, by = pt]))
fb_default <- S[is_fb == 1 & n >= thr(100), .(fb_velo = weighted.mean(release_speed, n), fb_hb = weighted.mean(hb_arm, n),
                                              fb_ivb = weighted.mean(ivb, n), rows = .N), by = .(is_lhp, pt)]
meta <- list(created = Sys.time(), seasons = SEASONS, pitch_types = PT_KEEP,
             movement = list(set = MOVE_SET, feats = MOVE_FEATS, nrounds = c(hb = nr_hb, ivb = nr_ivb), cv = move_tab, col_share_default = 1 / 30),
             grader = list(variant = best, feats = GF, best_iter = GM$best_iter, table = gtab, bat_feats = BAT_FEATS, bat_means = bat_means,
                           lgz = lgz[game_year == max(game_year)], loc_ref = loc_ref, platoon = plat, fb_default = fb_default,
                           scale_all = scale_all, scale_pt = scale_pt, count_rv = crv),
             sliders = sliders)
saveRDS(meta, file.path(ART_DIR, "meta.rds"))
out_cols <- intersect(c("pitcher", "player_name", "game_year", "pt", "p_throws", "n", "release_speed", "release_spin_rate", "active_spin", "axis_m",
                        "axis_R", "arm_angle", "release_extension", "release_pos_z", "rel_x_arm", "hb_arm", "ivb", "col_share", "fb_velo", "fb_hb",
                        "fb_ivb", "is_fb", "g_xrv100", "g_same", "g_opp", "grade_plus", "xrv100", "rv100", "whiff_sw", "zone_pct", "is_heldout"), names(S))
ST <- S[, out_cols, with = FALSE]
ST[M, on = .(pitcher, game_year, pt), `:=`(hb_pred_oof = i.hb_oof, ivb_pred_oof = i.ivb_oof)]
fwrite(ST, file.path(ART_DIR, "season_table.csv"))

sec("6. ARTIFACTS AND SLIDER RANGES", {
  say("Saved to ", ART_DIR, ": ", paste(list.files(ART_DIR), collapse = ", "))
  sub_("slider ranges by pitch type (1% / median / 99% of pitcher-season averages)")
  for (st in c("lo", "mid", "hi")) { say(st, ":"); shw(dcast(sliders, var ~ pt, value.var = st), 1) }
  sub_("default primary fastball by hand"); shw(fb_default, 2)
  sub_("platoon exposure used for the overall grade"); shw(dcast(plat, pt ~ is_lhp, value.var = "same_share"), 3)
  say("")
  say("Elapsed (min): ", round(as.numeric(difftime(Sys.time(), t_start, units = "mins")), 1))
})
tick("Done. Report: ", RPT)
