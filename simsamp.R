# ---------------------------------------------------------------
# Build response rate matrices: P(respond | traits), used to decide
# which individuals sampled from the simulated population (see
# simdata.R) actually respond once invited to the survey.
#
# Structure mirrors the population joint probability arrays in
# simdata.R: an array indexed by age x gender x tenure. Unlike those
# population arrays, response rate matrices do not need to sum to 1
# -- each cell is simply a probability in [0, 1], used to draw one
# Bernoulli response trial per individual with that trait
# combination.
# ---------------------------------------------------------------

source("simdata.R")

# --- Independent case: everyone is equally likely to respond ---
# A single flat response rate applied uniformly across every trait
# combination. This is the baseline case described in the
# methodology, against which the effects of differential response
# (covarying with observed or latent traits) can later be compared.
response_rate_indep <- 0.30

response_matrix_indep <- array(
  response_rate_indep,
  dim = c(length(age_levels), length(gender_levels), length(tenure_levels)),
  dimnames = list(
    age = age_levels,
    gender = gender_levels,
    tenure = tenure_levels
  )
)

response_matrix_indep

# --- Apply a response rate matrix to a sampled population ---
# For each individual, look up their trait combination's response
# rate in the matrix and draw a single Bernoulli trial to decide
# whether they respond.
apply_response_matrix <- function(data, response_matrix) {
  idx <- cbind(
    match(data$age, dimnames(response_matrix)$age),
    match(data$gender, dimnames(response_matrix)$gender),
    match(data$tenure, dimnames(response_matrix)$tenure)
  )
  rate <- response_matrix[idx]
  rbinom(nrow(data), 1, rate)
}

set.seed(21)
sim_data_omo$responded <- apply_response_matrix(
  sim_data_omo,
  response_matrix_indep
)

# --- Quick checks ---
# The realized response rate should be close to response_rate_indep,
# with no meaningful association between responding and any trait,
# since the matrix is flat (independent case).
mean(sim_data_omo$responded)
chisq.test(table(sim_data_omo$age, sim_data_omo$responded))
chisq.test(table(sim_data_omo$gender, sim_data_omo$responded))
chisq.test(table(sim_data_omo$tenure, sim_data_omo$responded))
