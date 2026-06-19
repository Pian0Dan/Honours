#!/usr/bin/env Rscript

script_args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", script_args, value = TRUE)
script_dir <- if (length(file_arg) == 1) dirname(normalizePath(sub("^--file=", "", file_arg))) else getwd()
source(file.path(script_dir, "R", "leximin.R"))

args <- commandArgs(trailingOnly = TRUE)

usage <- paste(
  "Usage:",
  "  Rscript run_leximin.R DATA_DIR [OUTPUT_CSV] [PANEL_SIZE] [ROUNDS] [PANEL_OUTPUT_CSV]",
  "",
  "Examples:",
  "  Rscript run_leximin.R data/example_small_20",
  "  Rscript run_leximin.R data/example_small_20 output/example_small_probs.csv",
  "  Rscript run_leximin.R data/my_instance_12 output/my_probs.csv 12 36",
  sep = "\n"
)

if (length(args) < 1 || args[1] %in% c("-h", "--help")) {
  cat(usage, "\n")
  quit(status = ifelse(length(args) < 1, 1, 0))
}

data_dir <- args[1]
output_csv <- if (length(args) >= 2 && nzchar(args[2])) args[2] else NULL
k <- if (length(args) >= 3 && nzchar(args[3])) as.integer(args[3]) else NULL
rounds <- if (length(args) >= 4 && nzchar(args[4])) as.integer(args[4]) else NULL
panel_output_csv <- if (length(args) >= 5 && nzchar(args[5])) {
  args[5]
} else if (!is.null(output_csv)) {
  if (grepl("\\.csv$", output_csv)) sub("\\.csv$", "_panels.csv", output_csv) else paste0(output_csv, "_panels.csv")
} else {
  NULL
}

if (!is.null(output_csv)) {
  dir.create(dirname(output_csv), recursive = TRUE, showWarnings = FALSE)
}
if (!is.null(panel_output_csv)) {
  dir.create(dirname(panel_output_csv), recursive = TRUE, showWarnings = FALSE)
}

probabilities <- run_leximin(data_dir, output_csv = output_csv,
                             panel_output_csv = panel_output_csv,
                             k = k, rounds = rounds)

print(head(probabilities, 20))
cat("\nTotal probability:", sum(probabilities$selection_probability), "\n")
cat("Minimum probability:", min(probabilities$selection_probability), "\n")
cat("Maximum probability:", max(probabilities$selection_probability), "\n")
if (!is.null(output_csv)) {
  cat("Wrote:", output_csv, "\n")
}
if (!is.null(panel_output_csv)) {
  cat("Wrote panels:", panel_output_csv, "\n")
}
