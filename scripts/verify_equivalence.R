## =============================================================================
## verify_equivalence.R
## Checks that the model in this repository (Irrigation_model_for_paper/) gives the
## same results as the model used for the simulations of the paper.
##
## Both models are run, one process each, on exactly the same 168 h past + 168 h
## forecast windows built from data/<FARM>/ (same selection of iterations as the
## simulation launcher: last negative step of the irrigation series), with the same
## config, crop table, kc_correction = 1 and sensor extremes of the whole series.
## Outputs are compared with the same rule as examples/run_example.R
## (numeric: all.equal, tolerance 1e-6; text: identical).
##
## Two comparisons (MODES):
##   "kc_fixed" : repo model with kc_correction fixed to 1  vs  simulation `model_template`
##                (the configuration of the paper: kc fixed, no dynamic correction)
##   "kc_dynamic": repo model as it is (dynamic kc correction)  vs  the same simulation model
##                with the original kc_corr_function restored
##                (checks that the rewrite/commenting of the repo code changed nothing)
##
## USAGE (R >= 4.0, internet connection as in the README):
##   Rscript scripts/verify_equivalence.R "<path to SWB_model_farm_FULL_RUN/Run Claude/model_template>"
##   optional: add  N_ITER=4   FARMS=AM,JD,NR,SG   YEARS=2024,2025   MODES=kc_fixed,kc_dynamic   FULL=1
## Default: 4 iterations per farm and year spread over the season. FULL=1 runs them all.
## Results: console + verification_report.csv (in the repo root; ignored by git if added to .gitignore).
## =============================================================================

suppressPackageStartupMessages({ library(jsonlite); library(data.table) })

args <- commandArgs(trailingOnly = TRUE)
kv   <- strsplit(args[grepl("=", args)], "=", fixed = TRUE)
opt  <- setNames(lapply(kv, `[`, 2), sapply(kv, `[`, 1))
pos  <- args[!grepl("=", args)]
getopt <- function(k, d) if (!is.null(opt[[k]])) opt[[k]] else d

REF_TEMPLATE <- normalizePath(if (length(pos)) pos[1] else stop("pass the path of the simulation model_template"), winslash = "/")
ARGV <- commandArgs(FALSE); f <- sub("^--file=", "", ARGV[grep("^--file=", ARGV)])
REPO <- normalizePath(file.path(if (length(f)) dirname(f[1]) else "scripts", ".."), winslash = "/")
FARMS <- strsplit(getopt("FARMS", "AM,JD,NR,SG"), ",")[[1]]
YEARS <- strsplit(getopt("YEARS", "2024,2025"), ",")[[1]]
MODES <- strsplit(getopt("MODES", "kc_fixed,kc_dynamic"), ",")[[1]]
N_ITER <- as.integer(getopt("N_ITER", "4")); FULL <- getopt("FULL", "0") == "1"
WORK <- file.path(REPO, "_tmp_replay"); week_size <- 168L
RSCRIPT <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")

stopifnot(dir.exists(REF_TEMPLATE), file.exists(file.path(REF_TEMPLATE, "crop_water_req_model_farm_multilayer.R")))
REPO_MODEL <- file.path(REPO, "Irrigation_model_for_paper")

## ---- the same crop table for both models (the one of the simulations) -------
REF_CROP <- file.path(REF_TEMPLATE, "Input_data", "crop_data", "crop_table_REVIEWED.csv")
norm <- function(p) gsub("\r", "", readLines(p, warn = FALSE))
crop_diff <- which(norm(REF_CROP) != norm(file.path(REPO_MODEL, "Input_data/crop_data/crop_table_REVIEWED.csv")))
cat(sprintf("Crop table repo vs simulation: %s\n", if (length(crop_diff)) paste0("DIFFERENT in line(s) ", paste(crop_diff, collapse = ", "), " -> the simulation table is used for both models") else "identical"))

## ---- helpers ------------------------------------------------------------------
rd_txt <- function(p) paste(readLines(p, warn = FALSE), collapse = "\n")
sub1 <- function(txt, pattern, repl) {
  m <- gregexpr(pattern, txt, perl = TRUE)[[1]]
  if (m[1] == -1 || length(m) != 1L) stop("pattern must match exactly once: ", pattern)
  sub(pattern, repl, txt, perl = TRUE)
}
make_ref <- function(dir, mode) {        # simulation model, as is (kc fixed) or with the original kc function
  unlink(dir, recursive = TRUE); dir.create(dir, recursive = TRUE)
  file.copy(list.files(REF_TEMPLATE, full.names = TRUE), dir, recursive = TRUE)
  unlink(file.path(dir, "Output_data", "*")); dir.create(file.path(dir, "Output_data"), showWarnings = FALSE)
  if (mode == "kc_dynamic") {
    sc <- file.path(dir, "crop_water_req_model_farm_multilayer.R")
    txt <- sub1(rd_txt(sc), "kc_correction=1  # KC FIXED[^\n]*",
                "kc_correction=kc_corr_function(a,kc_corr,kc_correction,irr_given_vol,moisture_LOW,duration_past,moisture_sensor,sensor_levels)")
    writeLines(txt, sc)
  }
  list(dir = dir, script = "runner_base.R")
}
make_repo <- function(dir, mode) {       # repo model, kc fixed to 1 only in the temporary copy
  unlink(dir, recursive = TRUE); dir.create(dir, recursive = TRUE)
  file.copy(list.files(REPO_MODEL, full.names = TRUE, all.files = FALSE), dir, recursive = TRUE)
  unlink(file.path(dir, "Output_data"), recursive = TRUE); dir.create(file.path(dir, "Output_data"))
  file.copy(REF_CROP, file.path(dir, "Input_data/crop_data/crop_table_REVIEWED.csv"), overwrite = TRUE)
  sc <- file.path(dir, "ACQUAOUNT_farm.R")
  if (mode == "kc_fixed") {
    writeLines(sub1(rd_txt(sc), "kc_correction\\s*<-\\s*kc_corr_function\\([^)]*\\)", "kc_correction <- 1"), sc)
  }
  list(dir = dir, script = "ACQUAOUNT_farm.R")
}
write_inputs <- function(dir, past, fut, cfg, ext) {
  inp <- function(...) file.path(dir, "Input_data", ...); wc <- function(x, f) write.csv(x, f, row.names = FALSE)
  for (d in c("climate_data", "irrigation_data", "soil_moisture_data")) { dir.create(inp(d), showWarnings = FALSE, recursive = TRUE); }
  file.copy(cfg, file.path(dir, "config.input.json"), overwrite = TRUE)
  write.table(ext, inp("soil_moisture_data", "SM_extremes_previous_run.csv"), row.names = FALSE, col.names = FALSE)
  write.table(1, inp("crop_data", "kc_correction.csv"), row.names = FALSE, col.names = FALSE)
  for (nm in c("past", "fut")) {
    x <- get(nm)
    wc(x$data, inp("climate_data", paste0("date_", nm, ".csv"))); wc(x$ora, inp("climate_data", paste0("hour_", nm, ".csv")))
    wc(x$RH, inp("climate_data", paste0("humidity_", nm, ".csv"))); wc(x$precipitation, inp("climate_data", paste0("precipitation_", nm, ".csv")))
    wc(x$sw_rad, inp("climate_data", paste0("solar_rad_", nm, ".csv"))); wc(x$temp, inp("climate_data", paste0("temperature_", nm, ".csv")))
    wc(x$wind, inp("climate_data", paste0("wind_", nm, ".csv")))
  }
  smc <- c("moisture_sensor_raw15cm", "moisture_sensor_raw30cm", "moisture_sensor_raw45cm")
  wc(past$irr_given_vol, inp("irrigation_data", "water_flow_past.csv"))
  wc(past[, smc], inp("soil_moisture_data", "SM_past.csv")); wc(fut[, smc], inp("soil_moisture_data", "SM_fut.csv"))
}
run_model <- function(m, tag) {
  old <- setwd(m$dir); on.exit(setwd(old))
  log <- file.path(m$dir, paste0("run_", tag, ".log"))
  st <- system2(RSCRIPT, m$script, stdout = log, stderr = log)
  list(status = st, log = log)
}
OUTS <- c("daily_variables_final.csv", "hourly_variables_final.csv", "Irrigation_dates.csv", "summary_table.csv")
rd_out <- function(p) read.table(p, header = TRUE, sep = ";", quote = "\"", check.names = FALSE, stringsAsFactors = FALSE)
cmp_out <- function(a, b) {       # same rule as examples/run_example.R; returns "" if equal, else the reason
  if (!identical(dim(a), dim(b))) return(sprintf("dim %s vs %s", paste(dim(a), collapse = "x"), paste(dim(b), collapse = "x")))
  if (!identical(names(a), names(b))) return("column names differ")
  for (j in seq_along(a)) {
    ok <- if (is.numeric(a[[j]]) && is.numeric(b[[j]])) isTRUE(all.equal(a[[j]], b[[j]], tolerance = 1e-6)) else identical(as.character(a[[j]]), as.character(b[[j]]))
    if (!ok) {
      d <- if (is.numeric(a[[j]]) && is.numeric(b[[j]])) sprintf(", max|diff|=%.4g", max(abs(a[[j]] - b[[j]]), na.rm = TRUE)) else ""
      return(sprintf("column '%s' differs%s", names(a)[j], d))
    }
  }
  ""
}
last_negative_positions_dt <- function(x, index_array) {       # as in the simulation launcher
  dx <- diff(x); DT <- data.table(i = seq_along(dx), x = dx); DT[, run := rleid(x != 0)]
  res <- DT[x != 0 & x < 0, .(pos = i[.N]), by = run]$pos
  if (!length(index_array)) return(integer())
  res <- res[res > index_array[1] & res < index_array[length(index_array)]]
  res + 1L
}
data_file <- function(farm, year) {
  d <- file.path(REPO, "data", farm)
  cand <- c(if (year == "2025") file.path(d, paste0(farm, "_SM1_2025.csv")), file.path(d, paste0(farm, "_SM1.csv")))
  cand[file.exists(cand)][1]
}

## ---- main loop ----------------------------------------------------------------
dir.create(WORK, showWarnings = FALSE); report <- list()
for (mode in MODES) {
  cat(sprintf("\n=========== MODE %s ===========\n", mode))
  ref <- make_ref(file.path(WORK, paste0("ref_", mode)), mode)
  rep <- make_repo(file.path(WORK, paste0("repo_", mode)), mode)
  for (farm in FARMS) for (year in YEARS) {
    cfg <- file.path(REPO, "data", farm, sprintf("config.input_%s_%s.json", farm, year))
    full <- read.csv(data_file(farm, year)); js <- fromJSON(cfg)
    s0 <- as.POSIXct(js$season_start, format = "%d/%m/%Y", tz = "UTC"); s1 <- as.POSIXct(js$season_end, format = "%d/%m/%Y", tz = "UTC")
    da <- as.POSIXct(full$data, format = "%d/%m/%Y", tz = "UTC")
    ia <- which(da >= max(min(da, na.rm = TRUE), s0) & da <= min(max(da, na.rm = TRUE), s1))
    il <- last_negative_positions_dt(full$irr_given_vol, ia); il <- il[il >= week_size & il < (nrow(full) - week_size)]
    sel <- if (FULL) seq_along(il) else unique(round(seq(1, length(il), length.out = min(N_ITER, length(il)))))
    ext <- c(max(full[, 8:10], na.rm = TRUE), min(full[, 8:10], na.rm = TRUE))
    for (i in sel) {
      rng <- (il[i] - week_size + 1):(il[i] + week_size); dp <- full[rng, ]
      past <- dp[1:week_size, ]; fut <- dp[(week_size + 1):nrow(dp), ]
      for (m in list(ref, rep)) {
        write_inputs(m$dir, past, fut, cfg, ext); unlink(file.path(m$dir, "Output_data", "*"))
      }
      r1 <- run_model(ref, "ref"); r2 <- run_model(rep, "repo")
      res <- c(); ok <- TRUE
      if (r1$status != 0 || r2$status != 0) { res <- sprintf("RUN FAILED (ref=%s repo=%s) see %s / %s", r1$status, r2$status, r1$log, r2$log); ok <- FALSE
      } else for (o in OUTS) {
        pa <- file.path(ref$dir, "Output_data", o); pb <- file.path(rep$dir, "Output_data", o)
        if (!file.exists(pa) || !file.exists(pb)) { res <- c(res, paste(o, "missing")); ok <- FALSE; next }
        d <- cmp_out(rd_out(pa), rd_out(pb)); if (nzchar(d)) { res <- c(res, paste(o, d)); ok <- FALSE }
      }
      cat(sprintf("[%s] %s %s iter %d/%d (%s): %s\n", mode, farm, year, i, length(il), as.character(full$data[il[i]]),
                  if (ok) "IDENTICAL" else paste("DIFFERENT -", paste(res, collapse = "; "))))
      report[[length(report) + 1L]] <- data.frame(mode = mode, farm = farm, year = year, iter = i, n_iter_total = length(il),
                                                  date = as.character(full$data[il[i]]), identical = ok, detail = paste(res, collapse = "; "))
    }
  }
}
rep_df <- do.call(rbind, report); write.csv(rep_df, file.path(REPO, "verification_report.csv"), row.names = FALSE)
cat(sprintf("\n%d/%d iterations identical. Report: verification_report.csv\n", sum(rep_df$identical), nrow(rep_df)))
for (m in unique(rep_df$mode)) cat(sprintf("  %-10s %d/%d identical\n", m, sum(rep_df$identical[rep_df$mode == m]), sum(rep_df$mode == m)))
