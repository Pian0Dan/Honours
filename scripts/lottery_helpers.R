# ---------------------------------------------------------------
# Shared helpers for working with Panelot lottery.csv outputs
# (panelot_output/<n>var/lottery_*.csv): mapping a lottery file to
# the respondent pool ("survey sample") it was drawn from, and
# reading a lottery.csv into a tidy one-row-per-panel structure.
# Used by scripts/performance_evaluator.R and
# scripts/panel_visualiser.R.
# ---------------------------------------------------------------

source("scripts/simdata2.R") # only needed for age_levels / tenure_levels

# Resolved to an absolute path immediately, rather than kept
# relative -- shiny::runApp() re-anchors the working directory to
# this app's own directory (scripts/) for every reactive evaluation
# triggered later by user interaction, not just at startup, so a
# relative path here would resolve fine when this file is first
# sourced (while the working directory is briefly the project root)
# but fail every time a panel is actually requested afterwards.
lottery_dir <- normalizePath("data/panelot_output", mustWork = FALSE)

# --- Map a lottery.csv's filename to the survey sample (the
# respondent pool) it was generated from ---
# File names follow "lottery_<population>_<case>.csv", where
# population is "ind" (sim_data2) or "cov" (sim_data2_cov), and case
# is "eq" (Responded1, equal chance) or "ass" (Responded2,
# age-associated) -- the same "<ind|cov>_<eq|ass>" convention used
# for the respondent-pool CSVs written by scripts/csv_helper.R
# (sim_data2_ind_eq.csv etc.). The case token is still matched by
# prefix below (rather than requiring an exact "eq"/"ass" match), in
# case a longer variant (e.g. "assoc") is ever used for a file name
# in future. Deliberately read the respondent-pool CSVs written by
# scripts/csv_helper.R (the exact files fed to Panelot to generate
# each lottery.csv) from disk, rather than re-deriving the respondent
# pool by re-running simsamp2.R/simsamp2yes.R in this session --
# those scripts have since been edited (e.g. the population is now
# sliced to 10,000 before sampling respondents), so re-running them
# no longer reproduces the same individuals, and most panel member
# IDs would fail to match a re-simulated pool.
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

# --- Read a lottery.csv into a tidy one-row-per-panel structure ---
# Columns "panel member 1".."panel member k" hold the selected pool
# IDs for that panel; collapse them into a single list-column so
# each row is still one panel, but membership is easy to work with.
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
