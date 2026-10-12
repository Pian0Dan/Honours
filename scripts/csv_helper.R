# Example matrix
sim_cat2 <- matrix(
  c(
    "age",
    "18-34",
    7,
    8,
    "age",
    "35-44",
    0.20 * 50,
    0.20 * 50,
    "age",
    "45-54",
    12,
    13,
    "age",
    "55-64",
    12,
    13,
    "age",
    "65+",
    7,
    8,
    "tenure",
    "Rent",
    0.3 * 50,
    0.3 * 50,
    "tenure",
    "Own",
    0.5 * 50,
    0.5 * 50,
    "tenure",
    "Other",
    0.2 * 50,
    0.2 * 50
  ),
  nrow = 8,
  byrow = TRUE
)

colnames(sim_cat2) <- c("category", "name", "min", "max")

# Save as CSV
write.csv(sim_cat2, "sim_cat2.csv", row.names = FALSE)

# -------

# ---------------------------------------------------------------
# Respondent CSV exports for simsamp2.R: for each of the two
# populations (independent "ind" = `sim_data2`, covaried/associated
# "cov" = `sim_data2_cov`) and each of the two response-rate cases
# ("eq" = equal chance, "ass" = age-associated), write a CSV
# containing only the individuals who actually responded to the
# survey, with:
#
#   - pool_member_identifier: the individual's ID, carried over from
#     the population (see simdata2.R, where ID is first assigned)
#   - age
#   - tenure
#
# File names follow the same "<ind|cov>_<eq|ass>" convention used for
# the lottery files (lottery_ind_eq.csv etc.) further down the
# pipeline, so a file name alone identifies both the population and
# response case it was drawn from.
# ---------------------------------------------------------------

source("simsamp2yes.R") # also sources simsamp2.R and simdata2.R

# --- Build and write a respondents-only CSV from a "*_yes" table ---
build_respondent_csv <- function(data, file) {
  respondents <- data.frame(
    pool_member_identifier = data$ID,
    age = data$Age,
    tenure = data$Tenure
  )
  write.csv(respondents, file, row.names = FALSE)
  respondents
}

sim_data2_ind_eq_csv <- build_respondent_csv(
  sim_data2_ind_eq_yes,
  "sim_data2_ind_eq.csv"
)
sim_data2_ind_ass_csv <- build_respondent_csv(
  sim_data2_ind_ass_yes,
  "sim_data2_ind_ass.csv"
)
sim_data2_cov_eq_csv <- build_respondent_csv(
  sim_data2_cov_eq_yes,
  "sim_data2_cov_eq.csv"
)
sim_data2_cov_ass_csv <- build_respondent_csv(
  sim_data2_cov_ass_yes,
  "sim_data2_cov_ass.csv"
)

# --- Quick checks ---
nrow(sim_data2_ind_eq_csv)
nrow(sim_data2_ind_ass_csv)
nrow(sim_data2_cov_eq_csv)
nrow(sim_data2_cov_ass_csv)
head(sim_data2_ind_eq_csv)
