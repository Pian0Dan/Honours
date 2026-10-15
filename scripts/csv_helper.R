# Quota file for Panelot: min/max count per age bracket and tenure
# category at k = 50.
sim_cat2 <- matrix(
  c(
    "age", "18-34", 7, 8,
    "age", "35-44", 0.20 * 50, 0.20 * 50,
    "age", "45-54", 12, 13,
    "age", "55-64", 12, 13,
    "age", "65+", 7, 8,
    "tenure", "Rent", 0.3 * 50, 0.3 * 50,
    "tenure", "Own", 0.5 * 50, 0.5 * 50,
    "tenure", "Other", 0.2 * 50, 0.2 * 50
  ),
  nrow = 8,
  byrow = TRUE
)

colnames(sim_cat2) <- c("category", "name", "min", "max")
write.csv(sim_cat2, "sim_cat2.csv", row.names = FALSE)

# ---------------------------------------------------------------
# Writes each population/response-case respondent pool out to its
# own CSV, ready to upload to Panelot alongside sim_cat2.csv above.
# File names follow the same ind|cov_eq|ass convention as the
# lottery files these feed into.
# ---------------------------------------------------------------

source("simsamp2yes.R") # also sources simsamp2.R and simdata2.R

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

nrow(sim_data2_ind_eq_csv)
nrow(sim_data2_ind_ass_csv)
nrow(sim_data2_cov_eq_csv)
nrow(sim_data2_cov_ass_csv)
head(sim_data2_ind_eq_csv)
