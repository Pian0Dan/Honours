# ---------------------------------------------------------------
# Shared helpers for reading Panelot lottery.csv files: matching a
# lottery to the respondent pool it was drawn from, and parsing a
# lottery.csv into a tidy one-row-per-panel structure. Used by
# scripts/performance_evaluator.R and scripts/panel_visualiser.R.
# ---------------------------------------------------------------

source("scripts/simdata2.R") # only needed for age_levels / tenure_levels

# Absolute path, not relative -- shiny::runApp() changes the working
# directory for every reactive evaluation, not just at startup, so a
# relative path here would break as soon as a panel is requested.
lottery_dir <- normalizePath("data/panelot_output", mustWork = FALSE)

# Respondent pools are read from the CSVs actually fed to Panelot,
# rather than re-simulated, since the simulation code has moved on
# since those lotteries were generated and no longer reproduces the
# same individuals.
read_survey_sample <- function(path) {
  raw <- read.csv(path)
  data.frame(
    ID = raw$pool_member_identifier,
    Age = factor(raw$age, levels = age_levels),
    Tenure = factor(raw$tenure, levels = tenure_levels)
  )
}

respondent_csv_dir <- normalizePath(
  "data/panelot_input",
  mustWork = FALSE
)
survey_samples <- list(
  ind_eq = read_survey_sample(
    file.path(respondent_csv_dir, "sim_data2_ind_eq.csv")
  ),
  ind_ass = read_survey_sample(
    file.path(respondent_csv_dir, "sim_data2_ind_ass.csv")
  ),
  cov_eq = read_survey_sample(
    file.path(respondent_csv_dir, "sim_data2_cov_eq.csv")
  ),
  cov_ass = read_survey_sample(
    file.path(respondent_csv_dir, "sim_data2_cov_ass.csv")
  )
)

# Maps "lottery_<ind|cov>_<eq|ass...>.csv" to the matching survey
# sample above.
get_survey_sample <- function(lottery_file) {
  name <- sub("^lottery_", "", sub("\\.csv$", "", basename(lottery_file)))
  parts <- strsplit(name, "_")[[1]]

  if (length(parts) != 2 || !parts[1] %in% c("ind", "cov")) {
    stop(
      "Can't tell which population/case ",
      lottery_file,
      " was drawn from -- expected a name like ",
      "'lottery_<ind|cov>_<eq|ass>.csv'."
    )
  }

  case <- if (grepl("^eq", parts[2])) {
    "eq"
  } else if (grepl("^ass", parts[2])) {
    "ass"
  } else {
    stop(
      "Unrecognised response-case code '",
      parts[2],
      "' in ",
      lottery_file,
      " -- expected something starting with 'eq' or 'ass'."
    )
  }

  key <- paste(parts[1], case, sep = "_")
  survey_samples[[key]][, c("ID", "Age", "Tenure")]
}

# Each row of a lottery.csv is one possible panel -- collapse its
# "panel member 1".."panel member k" columns into a single list
# column so membership is easier to work with.
read_lottery <- function(path) {
  raw <- read.csv(path, check.names = FALSE)
  member_cols <- grep("^panel member", names(raw))
  data.frame(
    panel_id = seq_len(nrow(raw)),
    return_probability = raw[["return probability of panel"]],
    member_ids = I(lapply(seq_len(nrow(raw)), function(i) {
      as.integer(raw[i, member_cols])
    }))
  )
}
