# =====================================================================================
# 01_statcast_pull_explore.R
# Step 1 of the pitch-grade / movement Shiny pipeline:
#   (A) pull pitch-level Statcast from Baseball Savant (chunked, cached, resumable)
#   (B) derive a handful of exploration-only features
#   (C) write a text EDA report + a few PNGs to eda_out/
# Paste eda_out/statcast_eda_report.txt back (and the PNGs if convenient).
# =====================================================================================

# ----------------------------------- CONFIG -----------------------------------------
SEASONS     <- 2023:2026
GAME_TYPES  <- c("R")            # R = regular season; add "F","D","L","W" for postseason
CHUNK_DAYS  <- 3                 # days per Savant request (auto-splits if the row cap is hit)
ROW_CAP     <- 25000             # Savant truncates a single CSV at this many rows
SLEEP_SEC   <- 1                 # pause between requests
REFRESH     <- FALSE             # TRUE = ignore the combined file and rebuild from chunks/web
DATA_DIR    <- "statcast_data"
OUT_DIR     <- "eda_out"
SEED        <- 42

# ----------------------------------- SETUP ------------------------------------------
pkgs <- c("data.table", "httr", "ggplot2")
new  <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(new)) install.packages(new)
suppressPackageStartupMessages({ library(data.table); library(httr); library(ggplot2) })

CHUNK_DIR <- file.path(DATA_DIR, "chunks")
RAW_FILE  <- file.path(DATA_DIR, sprintf("statcast_raw_%d_%d.rds", min(SEASONS), max(SEASONS)))
dir.create(CHUNK_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_DIR,   recursive = TRUE, showWarnings = FALSE)
set.seed(SEED)
t_start <- Sys.time()

# =====================================================================================
# (A) PULL
# =====================================================================================
UA <- "Mozilla/5.0 (statcast research pull; R httr)"

savant_url <- function(start, end, season, game_types = GAME_TYPES) {
  paste0(
    "https://baseballsavant.mlb.com/statcast_search/csv?all=true",
    "&hfPT=&hfAB=&hfBBT=&hfPR=&hfZ=&stadium=&hfBBL=&hfNewZones=",
    "&hfGT=", paste0(game_types, "%7C", collapse = ""),
    "&hfC=&hfSea=", season, "%7C",
    "&hfSit=&hfOuts=&opponent=&pitcher_throws=&batter_stands=&hfSA=",
    "&player_type=pitcher&hfInfield=&team=&position=&hfOutfield=&hfRO=&home_road=",
    "&game_date_gt=", format(start), "&game_date_lt=", format(end),
    "&hfFlag=&hfPull=&metric_1=&hfInn=&min_pitches=0&min_results=0",
    "&group_by=name&sort_col=pitches&player_event_sort=h_launch_speed&sort_order=desc",
    "&min_abs=0&type=details"
  )
}

fetch_chunk <- function(start, end, season) {
  resp <- httr::RETRY("GET", savant_url(start, end, season),
                      httr::user_agent(UA), httr::timeout(300),
                      times = 6, pause_base = 3, pause_cap = 90, quiet = TRUE)
  httr::stop_for_status(resp)
  txt <- httr::content(resp, as = "text", encoding = "UTF-8")
  if (is.null(txt) || is.na(txt) || !nzchar(trimws(txt))) return(data.table())
  txt <- sub("^﻿", "", txt)
  if (!grepl("pitch_type", substr(txt, 1, 300), fixed = TRUE))
    stop("Non-CSV response from Savant for ", format(start), " to ", format(end))
  fread(text = txt, na.strings = c("", "NA", "null"), showProgress = FALSE, encoding = "UTF-8", integer64 = "double")
}

# Deprecated / heavy free-text columns not worth carrying.
DROP_COLS <- c("spin_dir", "spin_rate_deprecated", "break_angle_deprecated",
               "break_length_deprecated", "tfs_deprecated", "tfs_zulu_deprecated",
               "umpire", "sv_id", "des")

pull_range <- function(start, end, season) {
  f <- file.path(CHUNK_DIR, sprintf("sc_%s_%s.rds", format(start), format(end)))
  if (file.exists(f)) return(readRDS(f))
  dt <- fetch_chunk(start, end, season)
  Sys.sleep(SLEEP_SEC)
  if (nrow(dt) >= ROW_CAP && end > start) {            # truncated -> split and retry
    mid <- start + as.integer(end - start) %/% 2L
    return(rbindlist(list(pull_range(start, mid, season), pull_range(mid + 1L, end, season)),
                     use.names = TRUE, fill = TRUE))
  }
  if (nrow(dt) >= ROW_CAP) warning("Single-day pull hit the row cap: ", format(start))
  drop <- intersect(DROP_COLS, names(dt))
  if (length(drop)) dt[, (drop) := NULL]
  if (end < Sys.Date() - 2L) saveRDS(dt, f)            # don't cache days that may still be filling in
  dt
}

pull_season <- function(season) {
  post  <- any(GAME_TYPES %in% c("F", "D", "L", "W"))
  first <- as.Date(sprintf("%d-03-15", season))
  last  <- min(as.Date(sprintf(if (post) "%d-11-10" else "%d-10-05", season)), Sys.Date() - 1L)
  if (last < first) return(data.table())
  starts <- seq(first, last, by = CHUNK_DAYS)
  ends   <- pmin(starts + CHUNK_DAYS - 1L, last)
  out <- vector("list", length(starts))
  for (i in seq_along(starts)) {
    out[[i]] <- pull_range(starts[i], ends[i], season)
    message(sprintf("[%d] %s to %s : %6d rows  (%d/%d)", season, format(starts[i]), format(ends[i]),
                    nrow(out[[i]]), i, length(starts)))
  }
  rbindlist(out, use.names = TRUE, fill = TRUE)
}

if (file.exists(RAW_FILE) && !REFRESH) {
  message("Loading ", RAW_FILE)
  sc <- readRDS(RAW_FILE)
  n_dupes <- NA_integer_
} else {
  sc <- rbindlist(lapply(SEASONS, pull_season), use.names = TRUE, fill = TRUE)
  n0 <- nrow(sc)
  sc <- unique(sc, by = c("game_pk", "at_bat_number", "pitch_number"))
  n_dupes <- n0 - nrow(sc)
  setorder(sc, game_date, game_pk, at_bat_number, pitch_number)
  saveRDS(sc, RAW_FILE)
  message("Saved ", RAW_FILE, " (", format(nrow(sc), big.mark = ","), " rows)")
}
setDT(sc)

# =====================================================================================
# (B) TYPES + EXPLORATION-ONLY FEATURES
# =====================================================================================
REQUIRED <- c("pitch_type", "pitch_name", "game_date", "game_year", "game_pk", "at_bat_number", "pitch_number",
              "pitcher", "batter", "p_throws", "stand", "release_speed", "release_spin_rate",
              "spin_axis", "release_extension", "release_pos_x", "release_pos_y", "release_pos_z",
              "pfx_x", "pfx_z", "plate_x", "plate_z", "sz_top", "sz_bot", "zone",
              "vx0", "vy0", "vz0", "ax", "ay", "az", "balls", "strikes", "description", "events",
              "type", "bb_type", "launch_speed", "launch_angle", "estimated_woba_using_speedangle",
              "woba_value", "woba_denom", "delta_run_exp", "home_team", "game_type")
OPTIONAL <- c("arm_angle", "api_break_x_arm", "api_break_x_batter_in", "api_break_z_with_gravity",
              "delta_pitcher_run_exp", "effective_speed", "bat_speed", "swing_length",
              "attack_angle", "attack_direction", "swing_path_tilt",
              "estimated_ba_using_speedangle", "estimated_slg_using_speedangle",
              "launch_speed_angle", "delta_home_win_exp", "n_thruorder_pitcher",
              "pitcher_days_since_prev_game", "age_pit")
miss_req <- setdiff(REQUIRED, names(sc))
if (length(miss_req)) stop("Savant schema changed; missing required columns: ", paste(miss_req, collapse = ", "))
absent_optional <- setdiff(OPTIONAL, names(sc))
for (cc in absent_optional) set(sc, j = cc, value = NA_real_)

NUM_COLS <- intersect(c("release_speed", "release_spin_rate", "spin_axis", "release_extension",
                        "release_pos_x", "release_pos_y", "release_pos_z", "pfx_x", "pfx_z",
                        "plate_x", "plate_z", "sz_top", "sz_bot", "vx0", "vy0", "vz0", "ax", "ay", "az",
                        "launch_speed", "launch_angle", "estimated_woba_using_speedangle",
                        "woba_value", "woba_denom", "delta_run_exp", OPTIONAL), names(sc))
for (cc in NUM_COLS) if (!is.double(sc[[cc]])) set(sc, j = cc, value = suppressWarnings(as.numeric(sc[[cc]])))
for (cc in c("zone", "balls", "strikes", "game_year"))
  if (!is.integer(sc[[cc]])) set(sc, j = cc, value = suppressWarnings(as.integer(sc[[cc]])))

# Transverse (active) spin from the Magnus acceleration (Nathan). Sea-level air density assumed,
# so altitude parks will read low.
spin_physics <- function(vx0, vy0, vz0, ax, ay, az, K = 5.383e-3, g = 32.174) {
  vyf <- -sqrt(pmax(vy0^2 - 2 * ay * (50 - 17 / 12), 0))
  tf  <- (vyf - vy0) / ay
  vxb <- vx0 + ax * tf / 2; vyb <- vy0 + ay * tf / 2; vzb <- vz0 + az * tf / 2
  vb  <- sqrt(vxb^2 + vyb^2 + vzb^2)
  adrag <- -(ax * vxb + ay * vyb + (az + g) * vzb) / vb
  amx <- ax + adrag * vxb / vb
  amy <- ay + adrag * vyb / vb
  amz <- az + g + adrag * vzb / vb
  cl  <- sqrt(amx^2 + amy^2 + amz^2) / (K * vb^2)
  s   <- 0.4 * cl / (1 - 2.32 * cl)
  s[!is.na(cl) & cl >= 0.42] <- NA_real_
  vxf <- vx0 + ax * tf; vzf <- vz0 + az * tf
  list(spin_t = 78.92 * s * vb,
       vaa    = -atan(vzf / vyf) * 180 / pi,
       haa_x  = atan(vxf / -vyf) * 180 / pi)       # + = toward catcher's right
}

sc[, sgn := fifelse(p_throws == "R", 1, -1)]
ph <- sc[, spin_physics(vx0, vy0, vz0, ax, ay, az)]
sc[, `:=`(
  hb          = -12 * pfx_x,                         # pitcher's view, inches (+ = RHP arm side)
  ivb         =  12 * pfx_z,
  hb_arm      = -12 * pfx_x * sgn,                   # + = arm side for both hands
  rel_x_arm   = -release_pos_x * sgn,                # + = arm side
  spin_axis_m = 180 + sgn * (spin_axis - 180),       # LHP mirrored onto RHP convention
  spin_t_est   = ph$spin_t,
  spin_eff_est = ph$spin_t / release_spin_rate,
  vaa          = ph$vaa,
  haa_arm      = -sgn * ph$haa_x
)]
rm(ph)
sc[!is.finite(spin_eff_est) | spin_eff_est > 2, spin_eff_est := NA_real_]
sc[, axis_inf_m := 180 + sgn * (((atan2(-pfx_x, pfx_z) * 180 / pi + 180) %% 360) - 180)]  # movement-implied axis
sc[, axis_dev   := ((spin_axis_m - axis_inf_m + 180) %% 360) - 180]                       # measured - implied
sc[, `:=`(sin_ax = sin(spin_axis_m * pi / 180), cos_ax = cos(spin_axis_m * pi / 180))]

swing_desc <- c("swinging_strike", "swinging_strike_blocked", "foul", "foul_tip", "hit_into_play")
whiff_desc <- c("swinging_strike", "swinging_strike_blocked", "foul_tip")
sc[, `:=`(
  swing         = description %in% swing_desc,
  whiff         = description %in% whiff_desc,
  called_strike = description %in% "called_strike",
  bip           = description %in% "hit_into_play",
  in_zone       = fifelse(is.na(zone), NA, zone <= 9L),
  geom_zone     = abs(plate_x) <= 0.83 & plate_z >= sz_bot - 0.121 & plate_z <= sz_top + 0.121,
  count         = paste0(balls, "-", strikes),
  same_hand     = stand == p_throws
)]

# =====================================================================================
# (C) REPORT
# =====================================================================================
options(width = 220, scipen = 999, datatable.print.class = FALSE)
hdr  <- function(x) cat("\n\n", strrep("=", 110), "\n", x, "\n", strrep("=", 110), "\n", sep = "")
sub_ <- function(x) cat("\n--- ", x, " ---\n", sep = "")
shw  <- function(x) print(x, nrows = 1000L, topn = 1000L, row.names = FALSE)
mn   <- function(x, d = 2) round(mean(x, na.rm = TRUE), d)
sdv  <- function(x, d = 2) round(sd(x, na.rm = TRUE), d)
pct  <- function(x, d = 1) round(100 * mean(x, na.rm = TRUE), d)
circ_mean <- function(deg) {
  r <- deg[!is.na(deg)] * pi / 180
  if (!length(r)) return(NA_real_)
  round((atan2(mean(sin(r)), mean(cos(r))) * 180 / pi) %% 360, 1)
}
QS <- c(.01, .05, .25, .5, .75, .95, .99)
qtab <- function(vars, by, rows = TRUE) {
  rbindlist(lapply(vars, function(v) {
    x <- sc[[v]]; keep <- rows & !is.na(x)
    out <- data.table(g = sc[[by]][keep], x = x[keep])[, as.list(round(quantile(x, QS), 2)), by = g]
    setnames(out, "g", by)
    out[, var := v]
    out
  }), use.names = TRUE)
}
sec <- function(title, expr) {                      # one failing section never kills the report
  hdr(title)
  tryCatch(expr, error = function(e) cat("!! SECTION FAILED:", conditionMessage(e), "\n"))
  invisible(NULL)
}

pt_n <- sc[!is.na(pitch_type), .N, by = pitch_type][order(-N)]
main_types <- pt_n[N >= 0.003 * sum(N) & !pitch_type %in% c("PO", "FA", "EP", "UN", "IN", "AB"), pitch_type]
is_main <- sc$pitch_type %in% main_types

run_report <- function() {
  sink(file.path(OUT_DIR, "statcast_eda_report.txt"), split = TRUE)
  on.exit(sink(), add = TRUE)

  sec("0. RUN INFO", {
  cat("Run:", format(Sys.time()), "| ", R.version.string, "| data.table", as.character(packageVersion("data.table")), "\n")
  cat("Seasons:", paste(SEASONS, collapse = ","), "| game types:", paste(GAME_TYPES, collapse = ","), "\n")
  cat("Rows:", format(nrow(sc), big.mark = ","), "| raw cols:", ncol(sc), "(incl. derived)\n")
  cat("Duplicate rows removed at pull:", n_dupes, "\n")
  cat("Optional columns ABSENT from Savant CSV:", if (length(absent_optional)) paste(absent_optional, collapse = ", ") else "none", "\n")
  cat("Main pitch types (>=0.3% share):", paste(main_types, collapse = ", "), "\n")
  cat("Object size (GB):", round(as.numeric(object.size(sc)) / 1e9, 2), "\n")

  })
  sec("1. SCHEMA (class, % NA, n unique, example)", {
  shw(data.table(
    col      = names(sc),
    class    = vapply(sc, function(x) class(x)[1], ""),
    pct_na   = round(100 * vapply(sc, function(x) mean(is.na(x)), 0), 2),
    n_unique = vapply(sc, uniqueN, 0L),
    example  = vapply(sc, function(x) { v <- x[!is.na(x)]; if (length(v)) substr(as.character(v[1]), 1, 28) else NA_character_ }, "")
  ))

  })
  sec("2. INTEGRITY", {
  sub_("rows / games / pitchers / date range by season")
  shw(sc[, .(rows = .N, games = uniqueN(game_pk), pitchers = uniqueN(pitcher), batters = uniqueN(batter),
              first = min(game_date), last = max(game_date)), by = game_year][order(game_year)])
  sub_("game_type")
  shw(sc[, .N, by = game_type][order(-N)])
  sub_("pitches per game (quantiles) -- low values flag truncated pulls")
  ppg <- sc[, .N, by = .(game_year, game_pk)]
  shw(ppg[, as.list(round(quantile(N, c(0, .01, .05, .5, .95, 1)))), by = game_year][order(game_year)])
  cat("Games with < 150 pitches:", ppg[N < 150, .N], "\n")
  sub_("implausible values (counts)")
  shw(sc[, .(velo_lt40_gt106 = sum(release_speed < 40 | release_speed > 106, na.rm = TRUE),
              spin_lt300_gt3800 = sum(release_spin_rate < 300 | release_spin_rate > 3800, na.rm = TRUE),
              abs_pfx_gt_2.5ft = sum(abs(pfx_x) > 2.5 | abs(pfx_z) > 2.5, na.rm = TRUE),
              ext_lt4_gt8.5 = sum(release_extension < 4 | release_extension > 8.5, na.rm = TRUE),
              relz_lt1_gt8 = sum(release_pos_z < 1 | release_pos_z > 8, na.rm = TRUE),
              bad_count = sum(balls > 3 | strikes > 2, na.rm = TRUE),
              pitch_type_na = sum(is.na(pitch_type)))])

  })
  sec("3. % MISSING BY SEASON (key fields)", {
  key_cols <- intersect(c("pitch_type", "release_speed", "release_spin_rate", "spin_axis", "release_extension",
                          "release_pos_x", "release_pos_z", "arm_angle", "pfx_x", "pfx_z", "api_break_x_arm",
                          "api_break_z_with_gravity", "plate_x", "plate_z", "sz_top", "zone", "vx0", "ax",
                          "effective_speed", "delta_run_exp", "delta_pitcher_run_exp", "delta_home_win_exp",
                          "bat_speed", "swing_length", "attack_angle", "spin_eff_est", "axis_dev", "vaa"), names(sc))
  na_tab <- sc[, lapply(.SD, function(x) round(100 * mean(is.na(x)), 2)), by = game_year, .SDcols = key_cols][order(game_year)]
  print(t(as.matrix(na_tab, rownames = "game_year")))
  sub_("swing-only / BIP-only fields: % missing among swings and BIP")
  shw(sc[, .(swings = sum(swing), bat_speed_na_sw = pct(is.na(bat_speed[swing])), swing_len_na_sw = pct(is.na(swing_length[swing])),
              bip = sum(bip), ev_na_bip = pct(is.na(launch_speed[bip])), la_na_bip = pct(is.na(launch_angle[bip])),
              xwoba_na_bip = pct(is.na(estimated_woba_using_speedangle[bip]))), by = game_year][order(game_year)])

  })
  sec("4. PITCH TYPE COUNTS BY SEASON", {
  ptc <- dcast(sc[, .N, by = .(pitch_type, pitch_name, game_year)], pitch_type + pitch_name ~ game_year, value.var = "N", fill = 0L)
  ptc[, total := rowSums(.SD), .SDcols = setdiff(names(ptc), c("pitch_type", "pitch_name"))]
  ptc[, share_pct := round(100 * total / sum(total), 2)]
  shw(ptc[order(-total)])

  })
  sec("5. PHYSICAL TRAIT MEANS BY PITCH TYPE x HAND (arm-side-normalised; axis mirrored for LHP)", {
  sv <- c("release_speed", "release_spin_rate", "hb_arm", "ivb", "release_extension", "rel_x_arm", "release_pos_z",
          "arm_angle", "spin_eff_est", "axis_dev", "vaa", "haa_arm")
  shw(sc[is_main, c(.(n = .N, axis_m_circ = circ_mean(spin_axis_m), axis_inf_circ = circ_mean(axis_inf_m)),
                     lapply(.SD, mn)), by = .(pitch_type, p_throws), .SDcols = sv][order(pitch_type, p_throws)])
  sub_("SDs (hands pooled)")
  shw(sc[is_main, c(.(n = .N), lapply(.SD, sdv)), by = pitch_type, .SDcols = sv][order(-n)])

  })
  sec("6. QUANTILES OF SLIDER-CANDIDATE TRAITS BY PITCH TYPE (hands pooled, arm-normalised)", {
  slv <- c("release_speed", "release_spin_rate", "spin_axis_m", "spin_eff_est", "release_extension",
           "rel_x_arm", "release_pos_z", "arm_angle", "hb_arm", "ivb")
  qt <- qtab(slv, "pitch_type", rows = is_main)
  setcolorder(qt, c("var", "pitch_type"))
  shw(qt[order(match(var, slv), pitch_type)])

  })
  sec("7. CONVENTION + PHYSICS CHECKS", {
  sub_("hb_arm (= -12*pfx_x*sgn) vs Savant api_break_x_arm : expect r ~ +1")
  shw(sc[, .(n = sum(!is.na(api_break_x_arm) & !is.na(hb_arm)),
              r = round(cor(hb_arm, api_break_x_arm, use = "pairwise.complete.obs"), 4),
              mean_diff_in = mn(hb_arm - api_break_x_arm), sd_diff_in = sdv(hb_arm - api_break_x_arm)), by = game_year][order(game_year)])
  sub_("estimated spin efficiency by pitch type (quantiles) + % > 1.05 + % NA")
  se <- qtab("spin_eff_est", "pitch_type", rows = is_main)
  se <- merge(se, sc[is_main, .(pct_gt_1.05 = pct(spin_eff_est > 1.05), pct_na = pct(is.na(spin_eff_est))), by = pitch_type], by = "pitch_type")
  shw(se[, var := NULL][])
  sub_("axis deviation (measured spin axis - movement-implied axis, deg; SSW proxy) by pitch type")
  shw(qtab("axis_dev", "pitch_type", rows = is_main)[, var := NULL][])
  sub_("pitcher-season level: between-pitcher SD of mean traits (>=100 pitches of the type)")
  pps <- sc[is_main, .(n = .N, velo = mean(release_speed, na.rm = TRUE), spin = mean(release_spin_rate, na.rm = TRUE),
                       hb_arm = mean(hb_arm, na.rm = TRUE), ivb = mean(ivb, na.rm = TRUE),
                       eff = mean(spin_eff_est, na.rm = TRUE), axis_dev = mean(axis_dev, na.rm = TRUE)),
            by = .(pitcher, game_year, pitch_type)][n >= 100]
  shw(pps[, c(.(pitcher_seasons = .N), lapply(.SD, sdv)), by = pitch_type, .SDcols = c("velo", "spin", "hb_arm", "ivb", "eff", "axis_dev")][order(-pitcher_seasons)])

  })
  sec("8. DRIFT: FOUR-SEAM BY SEASON, AND BY PARK", {
  dv <- c("release_speed", "release_spin_rate", "ivb", "hb_arm", "release_extension", "release_pos_z", "arm_angle", "spin_eff_est", "axis_dev")
  shw(sc[pitch_type %in% "FF", c(.(n = .N), lapply(.SD, mn)), by = game_year, .SDcols = dv][order(game_year)])
  sub_("FF by home park (sorted by IVB) -- environment effect on movement")
  shw(sc[pitch_type %in% "FF", .(n = .N, velo = mn(release_speed), spin = mn(release_spin_rate), ivb = mn(ivb),
                                   hb_arm = mn(hb_arm), spin_eff_est = mn(spin_eff_est, 3)), by = home_team][order(ivb)])

  })
  sec("9. OUTCOMES", {
  sub_("description"); shw(sc[, .(n = .N, pct = round(100 * .N / nrow(sc), 2)), by = description][order(-n)])
  sub_("type");        shw(sc[, .N, by = type][order(-N)])
  sub_("events (PA-ending)"); shw(sc[!is.na(events), .(n = .N, woba_value = mn(woba_value, 3), woba_denom = mn(woba_denom, 2)), by = events][order(-n)])
  sub_("bb_type on BIP"); shw(sc[bip == TRUE, .(n = .N, ev = mn(launch_speed, 1), la = mn(launch_angle, 1), woba = mn(woba_value, 3),
                                                 xwoba = mn(estimated_woba_using_speedangle, 3)), by = bb_type][order(-n)])
  sub_("rates by pitch type (rv100_bat: delta_run_exp per 100 pitches, + = good for hitter)")
  shw(sc[is_main, .(n = .N,
                     swing = pct(swing), whiff_per_sw = round(100 * sum(whiff) / sum(swing), 1), csw = pct(whiff | called_strike),
                     zone = pct(in_zone), chase = round(100 * sum(swing & !in_zone, na.rm = TRUE) / sum(!in_zone, na.rm = TRUE), 1),
                     bip = pct(bip), gb_per_bip = round(100 * sum(bb_type %in% "ground_ball") / sum(bip), 1),
                     ev_bip = mn(launch_speed[bip], 1), xwobacon = mn(estimated_woba_using_speedangle[bip], 3),
                     rv100_bat = round(100 * mean(delta_run_exp, na.rm = TRUE), 3)), by = pitch_type][order(-n)])
  sub_("platoon split by pitch type")
  shw(sc[is_main & !is.na(same_hand), .(n = .N, whiff_per_sw = round(100 * sum(whiff) / sum(swing), 1),
                                         xwobacon = mn(estimated_woba_using_speedangle[bip], 3),
                                         rv100_bat = round(100 * mean(delta_run_exp, na.rm = TRUE), 3)),
          by = .(pitch_type, same_hand)][order(pitch_type, same_hand)])

  })
  sec("10. TARGET CANDIDATES", {
  sub_("delta_run_exp distribution")
  print(round(quantile(sc$delta_run_exp, c(0, .001, .01, .05, .25, .5, .75, .95, .99, .999, 1), na.rm = TRUE), 3))
  cat("mean:", mn(sc$delta_run_exp, 5), " sd:", sdv(sc$delta_run_exp, 4), " %NA:", pct(is.na(sc$delta_run_exp), 3), "\n")
  sub_("delta_pitcher_run_exp vs delta_run_exp (if present)")
  if (!"delta_pitcher_run_exp" %in% absent_optional)
    cat("r =", round(cor(sc$delta_run_exp, sc$delta_pitcher_run_exp, use = "pairwise.complete.obs"), 4),
        "| mean(sum) =", mn(sc$delta_run_exp + sc$delta_pitcher_run_exp, 5), "\n") else cat("absent\n")
  sub_("delta_run_exp by description")
  shw(sc[, .(n = .N, mean = mn(delta_run_exp, 4), sd = sdv(delta_run_exp, 4)), by = description][order(-n)])
  sub_("by count: n, zone%, swing%, mean delta_run_exp, and RV of ball / called strike / whiff")
  shw(sc[balls <= 3 & strikes <= 2, .(n = .N, zone = pct(in_zone), swing = pct(swing),
                                        rv_mean = mn(delta_run_exp, 4),
                                        rv_ball = mn(delta_run_exp[description %in% c("ball", "blocked_ball")], 4),
                                        rv_called_strike = mn(delta_run_exp[called_strike], 4),
                                        rv_whiff = mn(delta_run_exp[whiff], 4),
                                        rv_foul = mn(delta_run_exp[description %in% "foul"], 4),
                                        rv_bip = mn(delta_run_exp[bip], 4)), by = count][order(count)])
  sub_("coverage of result fields by pitch outcome class")
  shw(sc[, .(n = .N, xwoba = pct(!is.na(estimated_woba_using_speedangle)), woba_value = pct(!is.na(woba_value)),
              woba_denom = pct(!is.na(woba_denom)), delta_run_exp = pct(!is.na(delta_run_exp)),
              launch_speed = pct(!is.na(launch_speed)), launch_speed_angle = pct(!is.na(launch_speed_angle))),
          by = .(pa_ending = !is.na(events), bip)][order(pa_ending, bip)])
  sub_("BIP: wOBA vs xwOBA vs run value by season")
  shw(sc[bip == TRUE, .(n = .N, ev = mn(launch_speed, 1), la = mn(launch_angle, 1), woba = mn(woba_value, 3),
                         xwoba = mn(estimated_woba_using_speedangle, 3), rv_bip = mn(delta_run_exp, 4),
                         r_xwoba_rv = round(cor(estimated_woba_using_speedangle, delta_run_exp, use = "pairwise.complete.obs"), 3)),
          by = game_year][order(game_year)])

  })
  sec("11. LOCATION / STRIKE ZONE BY SEASON (2026: plate_x/z at mid-plate, sz_top/bot ABS-defined)", {
  shw(sc[, .(n = .N, plate_x = mn(plate_x, 3), plate_x_sd = sdv(plate_x, 3), plate_z = mn(plate_z, 3), plate_z_sd = sdv(plate_z, 3),
              sz_top = mn(sz_top, 3), sz_top_sd = sdv(sz_top, 3), sz_bot = mn(sz_bot, 3), sz_bot_sd = sdv(sz_bot, 3),
              zone_savant = pct(in_zone), zone_geom = pct(geom_zone),
              cs_rate_in_geom = round(100 * sum(called_strike & geom_zone, na.rm = TRUE) / sum(!swing & geom_zone, na.rm = TRUE), 1),
              cs_rate_out_geom = round(100 * sum(called_strike & !geom_zone, na.rm = TRUE) / sum(!swing & !geom_zone, na.rm = TRUE), 1)),
          by = game_year][order(game_year)])
  sub_("median within-batter-season SD of sz_top / sz_bot")
  shw(sc[, .(n = .N, s_top = sd(sz_top, na.rm = TRUE), s_bot = sd(sz_bot, na.rm = TRUE)), by = .(batter, game_year)][n >= 200,
          .(batters = .N, med_sd_sz_top = round(median(s_top, na.rm = TRUE), 4), med_sd_sz_bot = round(median(s_bot, na.rm = TRUE), 4)), by = game_year][order(game_year)])
  sub_("mean location by pitch type x platoon (plate_x arm-normalised: + = pitcher's arm side)")
  shw(sc[is_main & !is.na(same_hand), .(n = .N, plate_x_arm = mn(-plate_x * sgn, 3), plate_z = mn(plate_z, 3),
                                         plate_x_sd = sdv(plate_x, 3), plate_z_sd = sdv(plate_z, 3), zone = pct(in_zone)),
          by = .(pitch_type, same_hand)][order(pitch_type, same_hand)])

  })
  sec("12. ARSENAL STRUCTURE (pitcher-season)", {
  ars <- sc[!is.na(pitch_type), .N, by = .(pitcher, game_year, pitch_type)]
  ars[, share := N / sum(N), by = .(pitcher, game_year)]
  fb <- ars[pitch_type %in% c("FF", "SI", "FC")][order(-N), .(primary_fb = pitch_type[1], primary_fb_n = N[1]), by = .(pitcher, game_year)]
  ps <- ars[, .(pitches = sum(N), n_types = .N, n_types_5pct = sum(share >= .05), primary = pitch_type[which.max(N)]), by = .(pitcher, game_year)]
  ps <- merge(ps, fb, by = c("pitcher", "game_year"), all.x = TRUE)
  sub_("pitches per pitcher-season (quantiles)")
  print(round(quantile(ps$pitches, c(0, .1, .25, .5, .75, .9, 1))))
  sub_("pitch types with >=5% usage per pitcher-season"); shw(ps[, .(pitcher_seasons = .N, pitches = sum(pitches)), by = n_types_5pct][order(n_types_5pct)])
  sub_("primary pitch");    shw(ps[, .(pitcher_seasons = .N, pitches = sum(pitches)), by = primary][order(-pitches)])
  sub_("primary fastball (most-thrown of FF/SI/FC)"); shw(ps[, .(pitcher_seasons = .N, pitches = sum(pitches)), by = primary_fb][order(-pitches)])
  cat("Share of pitches from pitcher-seasons with <50 primary-FB pitches (or none):",
      round(100 * ps[is.na(primary_fb_n) | primary_fb_n < 50, sum(pitches)] / ps[, sum(pitches)], 2), "%\n")
  sub_("pitcher-season-pitch-type cells by sample size")
  shw(ars[pitch_type %in% main_types, .(cells = .N, ge50 = sum(N >= 50), ge100 = sum(N >= 100), ge300 = sum(N >= 300)), by = pitch_type][order(-cells)])

  })
  sec("13. CORRELATIONS (150k-row samples)", {
  cv <- c("release_speed", "release_spin_rate", "hb_arm", "ivb", "release_extension", "rel_x_arm", "release_pos_z",
          "arm_angle", "spin_eff_est", "axis_dev", "vaa", "haa_arm", "delta_run_exp")
  for (lab in c("ALL", "FF", "SL", "CH")) {
    idx <- if (lab == "ALL") which(is_main) else which(sc$pitch_type %in% lab)
    if (length(idx) < 5000) next
    idx <- idx[sample.int(length(idx), min(length(idx), 150000L))]
    sub_(lab)
    print(round(suppressWarnings(cor(sc[idx, cv, with = FALSE], use = "pairwise.complete.obs")), 2))
  }

  })
  sec("14. BASELINE LINEAR FITS: MOVEMENT ~ SLIDER TRAITS (per pitch type, <=100k rows)", {
  cat("A: velo + spin*(sin,cos axis) + rel_x + rel_z + ext\nB: A with est. spin efficiency interacted (efficiency is itself movement-derived)\nC: A + arm_angle\n")
  rhsA <- "release_speed + release_spin_rate*(sin_ax + cos_ax) + rel_x_arm + release_pos_z + release_extension"
  rhsB <- "release_speed + spin_eff_est*release_spin_rate*(sin_ax + cos_ax) + rel_x_arm + release_pos_z + release_extension"
  rhsC <- paste(rhsA, "+ arm_angle")
  fit1 <- function(d, y, rhs) {
    m <- lm(as.formula(paste(y, "~", rhs)), data = d)
    c(round(summary(m)$r.squared, 3), round(sqrt(mean(residuals(m)^2)), 2))
  }
  need <- c("hb_arm", "ivb", "release_speed", "release_spin_rate", "sin_ax", "cos_ax", "rel_x_arm", "release_pos_z",
            "release_extension", "spin_eff_est", "arm_angle")
  has_arm <- !"arm_angle" %in% absent_optional && mean(is.na(sc$arm_angle)) < 0.5
  if (!has_arm) need <- setdiff(need, "arm_angle")
  shw(rbindlist(lapply(main_types, function(pt) {
    d <- sc[pitch_type %in% pt, need, with = FALSE]
    d <- d[complete.cases(d)]
    if (nrow(d) < 3000) return(NULL)
    if (nrow(d) > 100000L) d <- d[sample.int(nrow(d), 100000L)]
    a1 <- fit1(d, "hb_arm", rhsA); a2 <- fit1(d, "ivb", rhsA)
    b1 <- fit1(d, "hb_arm", rhsB); b2 <- fit1(d, "ivb", rhsB)
    c1 <- if (has_arm) fit1(d, "hb_arm", rhsC) else c(NA, NA)
    c2 <- if (has_arm) fit1(d, "ivb", rhsC) else c(NA, NA)
    data.table(pitch_type = pt, n = nrow(d), sd_hb = sdv(d$hb_arm), sd_ivb = sdv(d$ivb),
               hb_r2_A = a1[1], hb_rmse_A = a1[2], ivb_r2_A = a2[1], ivb_rmse_A = a2[2],
               hb_r2_B = b1[1], hb_rmse_B = b1[2], ivb_r2_B = b2[1], ivb_rmse_B = b2[2],
               hb_r2_C = c1[1], hb_rmse_C = c1[2], ivb_r2_C = c2[1], ivb_rmse_C = c2[2])
  })))

  })
  sec("15. COMPLETE-CASE ROWS BY PITCH TYPE FOR CANDIDATE FEATURE SETS", {
  core <- c("release_speed", "release_spin_rate", "spin_axis", "release_extension", "release_pos_x", "release_pos_z",
            "pfx_x", "pfx_z", "plate_x", "plate_z", "sz_top", "sz_bot", "vx0", "ax", "delta_run_exp")
  cc_core <- complete.cases(sc[, core, with = FALSE])
  shw(data.table(pitch_type = sc$pitch_type, core = cc_core, core_arm = cc_core & !is.na(sc$arm_angle),
                  core_eff = cc_core & !is.na(sc$spin_eff_est))[pitch_type %in% main_types,
       .(n = .N, core = sum(core), core_plus_arm_angle = sum(core_arm), core_plus_spin_eff = sum(core_eff)), by = pitch_type][order(-n)])

  })
  cat("\n\nElapsed (min):", round(as.numeric(difftime(Sys.time(), t_start, units = "mins")), 1), "\n")
  invisible(NULL)
}
run_report()

# ----------------------------------- PLOTS ------------------------------------------
tryCatch({
  idx <- which(is_main & !is.na(sc$hb_arm) & !is.na(sc$ivb))
  idx <- idx[sample.int(length(idx), min(length(idx), 400000L))]
  pd  <- sc[idx, .(pitch_type, hb_arm, ivb, spin_axis_m, axis_inf_m, spin_eff_est)]

  ggsave(file.path(OUT_DIR, "01_movement_by_pitch_type.png"), width = 13, height = 9, dpi = 110,
    plot = ggplot(pd, aes(hb_arm, ivb)) + geom_bin2d(bins = 70) + facet_wrap(~pitch_type) +
      scale_fill_viridis_c(trans = "log10") + coord_equal(xlim = c(-28, 28), ylim = c(-28, 28)) +
      geom_hline(yintercept = 0, linewidth = .2) + geom_vline(xintercept = 0, linewidth = .2) +
      labs(x = "HB (in, + = arm side)", y = "IVB (in)", title = "Movement by pitch type") + theme_minimal())

  ggsave(file.path(OUT_DIR, "02_measured_vs_implied_axis.png"), width = 13, height = 9, dpi = 110,
    plot = ggplot(pd[!is.na(spin_axis_m)], aes(axis_inf_m, spin_axis_m)) + geom_bin2d(bins = 72) + facet_wrap(~pitch_type) +
      scale_fill_viridis_c(trans = "log10") + geom_abline(slope = 1, intercept = 0, linewidth = .2) +
      labs(x = "Movement-implied axis (deg, LHP mirrored)", y = "Measured spin axis (deg, LHP mirrored)",
           title = "Measured vs movement-implied spin axis") + theme_minimal())

  ggsave(file.path(OUT_DIR, "03_est_spin_efficiency.png"), width = 11, height = 6, dpi = 110,
    plot = ggplot(pd[!is.na(spin_eff_est) & spin_eff_est < 1.3], aes(spin_eff_est, colour = pitch_type)) +
      geom_density(linewidth = .7) + labs(x = "Estimated spin efficiency", y = NULL,
                                          title = "Estimated spin efficiency by pitch type") + theme_minimal())
}, error = function(e) message("Plots failed: ", conditionMessage(e)))

message("Done. Report: ", file.path(OUT_DIR, "statcast_eda_report.txt"))
