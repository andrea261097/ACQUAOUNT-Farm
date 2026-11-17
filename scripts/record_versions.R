# Records the R version and the versions of the packages used by the model.
# Run once from the repository root, in the same R installation used for the paper runs:
#   Rscript scripts/record_versions.R
# It writes SOFTWARE_VERSIONS.txt, to be committed together with the release.
pkgs <- c("aqp", "dplyr", "jsonlite", "lubridate", "outliers", "randomForest",
          "ranger", "readxl", "soilDB", "tictoc", "units")
v <- vapply(pkgs, function(p) if (requireNamespace(p, quietly = TRUE))
  as.character(utils::packageVersion(p)) else "NOT INSTALLED", character(1))
out <- c(R.version.string,
         paste("Platform:", R.version$platform),
         paste("Date:", Sys.Date()),
         "",
         paste0(pkgs, " ", v))
writeLines(out, "SOFTWARE_VERSIONS.txt")
cat(out, sep = "\n")
