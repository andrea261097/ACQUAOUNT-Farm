# Run one of the worked examples and compare the results with the reference outputs.
#
# Usage (from the root of the repository):
#   Rscript examples/run_example.R NR_potato
#   Rscript examples/run_example.R AM_citrus
#
# The script copies the model to a temporary folder, replaces its inputs with those of
# the chosen example, runs the model, stores the new outputs in
# examples/<example>/Output_data_new/ and compares them with examples/<example>/reference_output/.
# Note: the pedotransfer step queries the ROSETTA web service (package soilDB), so an
# internet connection is required.

args <- commandArgs(trailingOnly = TRUE)
example <- if (length(args) >= 1) args[1] else "NR_potato"

root      <- normalizePath(getwd())
model_dir <- file.path(root, "Irrigation_model_for_paper")
ex_dir    <- file.path(root, "examples", example)
if (!dir.exists(model_dir) || !dir.exists(ex_dir)) {
  stop("Run this script from the repository root and use an existing example name ",
       "(NR_potato or AM_citrus).")
}

# 1. temporary working copy of the model (the repository is left untouched)
work <- file.path(tempdir(), "ACQUAOUNT_farm_example")
dir.create(work, recursive = TRUE)
file.copy(file.path(model_dir, c("ACQUAOUNT_farm.R", "Functions")), work, recursive = TRUE)
dir.create(file.path(work, "Input_data", "crop_data"), recursive = TRUE)
file.copy(file.path(model_dir, "Input_data", "crop_data", "crop_table_REVIEWED.csv"),
          file.path(work, "Input_data", "crop_data"))

# 2. example inputs (configuration, climate, soil moisture, irrigation, model state)
file.copy(file.path(ex_dir, "config.input.json"), work)
invisible(file.copy(list.files(file.path(ex_dir, "Input_data"), full.names = TRUE),
          file.path(work, "Input_data"), recursive = TRUE))

# 3. run
# (the model script clears the R workspace, so the paths are passed through environment variables)
Sys.setenv(ACQ_ROOT = root, ACQ_EXAMPLE = example, ACQ_WORK = work)
setwd(work)
source("ACQUAOUNT_farm.R")

root    <- Sys.getenv("ACQ_ROOT")
example <- Sys.getenv("ACQ_EXAMPLE")
work    <- Sys.getenv("ACQ_WORK")
ex_dir  <- file.path(root, "examples", example)
setwd(root)

# 4. keep the new outputs and compare with the reference
new_dir <- file.path(ex_dir, "Output_data_new")
dir.create(new_dir, showWarnings = FALSE)
invisible(file.copy(list.files(file.path(work, "Output_data"), full.names = TRUE), new_dir, overwrite = TRUE))

read_out <- function(p) read.table(p, header = TRUE, sep = ";", quote = "\"", check.names = FALSE,
                                   stringsAsFactors = FALSE)
ok <- TRUE
for (f in list.files(file.path(ex_dir, "reference_output"))) {
  ref <- read_out(file.path(ex_dir, "reference_output", f))
  new <- read_out(file.path(new_dir, f))
  same <- identical(dim(ref), dim(new)) && identical(names(ref), names(new))
  if (same) {
    for (j in seq_along(ref)) {
      same <- same && if (is.numeric(ref[[j]]) && is.numeric(new[[j]]))
        isTRUE(all.equal(ref[[j]], new[[j]], tolerance = 1e-6)) else
        identical(as.character(ref[[j]]), as.character(new[[j]]))
    }
  }
  cat(sprintf("%-28s %s\n", f, if (same) "OK" else "DIFFERENT"))
  ok <- ok && same
}
cat(if (ok) "\nAll outputs match the reference.\n" else "\nSome outputs differ from the reference.\n")
