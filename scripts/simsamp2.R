# ---------------------------------------------------------------
# Builds the 10,000-person slice of each population (simdata2.R),
# then simulates survey responses on it. Output is one row per
# sliced individual: ID, Age, Tenure, Responded1 (equal-chance case),
# Responded2 (age-associated case).
# ---------------------------------------------------------------

source("scripts/simdata2.R")

set.seed(13)

# Flag 10,000 individuals as included rather than indexing the
# sample directly, so the slice keeps the population's row order.
# Response probability only depends on age/tenure, not row position,
# so this doesn't change who's eligible to respond -- but it does
# keep sample.int()'s output lined up the same way run to run, which
# matters here since the respondent-pool CSVs and Panelot lotteries
# this feeds into were generated against that ordering.
slice_population <- function(population, n = 10000) {
  included <- logical(nrow(population))
  included[sample.int(nrow(population), n)] <- TRUE
  population[included, , drop = FALSE]
}

# Response rate by age x tenure cell, for each case. Equal-chance is
# just a flat rate; the associated case scales up with age the same
# way the age-renting association was seeded in simdata2.R.
build_response_matrix_indep <- function(rate = 0.30) {
  array(
    rate,
    dim = c(length(age_levels), length(tenure_levels)),
    dimnames = list(age = age_levels, tenure = tenure_levels)
  )
}

build_response_matrix_age <- function(base_rate = 0.20, alpha = 1.0) {
  age_score <- seq(0, 1, length.out = length(age_levels))
  rate_by_age <- base_rate * exp(alpha * age_score)
  matrix(
    rate_by_age,
    nrow = length(age_levels),
    ncol = length(tenure_levels),
    dimnames = list(age = age_levels, tenure = tenure_levels)
  )
}

# Looks up each row's own age/tenure response rate and draws a
# single Bernoulli trial against it.
apply_matrix_response <- function(data, response_matrix) {
  idx <- cbind(
    match(data$age, dimnames(response_matrix)$age),
    match(data$tenure, dimnames(response_matrix)$tenure)
  )
  rbinom(nrow(data), 1, response_matrix[idx])
}

response_matrix_indep <- build_response_matrix_indep()
response_matrix_age <- build_response_matrix_age()
response_matrix_indep
response_matrix_age

build_population_structure <- function(population_slice) {
  data.frame(
    ID = population_slice$ID,
    Age = population_slice$age,
    Tenure = population_slice$tenure,
    Responded1 = apply_matrix_response(population_slice, response_matrix_indep),
    Responded2 = apply_matrix_response(population_slice, response_matrix_age)
  )
}

# Slice and simulate responses one population at a time (rather than
# both slices followed by both response draws) so the two populations
# don't shift each other's random draws -- this needs to stay in sync
# with however the original respondent-pool CSVs were generated.
sim_data2_slice <- slice_population(sim_data2)
sim_data2_structure <- build_population_structure(sim_data2_slice)

sim_data2_cov_slice <- slice_population(sim_data2_cov)
sim_data2_cov_structure <- build_population_structure(sim_data2_cov_slice)

nrow(sim_data2_slice)
nrow(sim_data2_cov_slice)

# Mosaic plots of each slice, matching simdata2.R's population-level
# plots so the two can be compared side by side.
pop2_ind_slice_plot <-
  ggplot(sim_data2_slice) +
  geom_mosaic(aes(x = product(age), fill = tenure)) +
  scale_x_productlist(name = "Age Bracket") +
  scale_y_productlist(name = "Tenure") +
  labs(title = "sim_data2: 10,000-person slice", fill = "Tenure") +
  theme_minimal()

pop2_cov_slice_plot <-
  ggplot(sim_data2_cov_slice) +
  geom_mosaic(aes(x = product(age), fill = tenure)) +
  scale_x_productlist(name = "Age Bracket") +
  scale_y_productlist(name = "Tenure") +
  labs(title = "sim_data2_cov: 10,000-person slice", fill = "Tenure") +
  theme_minimal()

pop2_ind_slice_plot
pop2_cov_slice_plot

# Quick checks: every row here responded to something, by construction.
str(sim_data2_structure)
str(sim_data2_cov_structure)
nrow(sim_data2_structure)
nrow(sim_data2_cov_structure)

# For sim_data2 (independent population): Responded1 should show no
# real association with age, Responded2 should rise with age.
chisq.test(table(sim_data2_structure$Age, sim_data2_structure$Responded1))
chisq.test(table(sim_data2_structure$Age, sim_data2_structure$Responded2))
tapply(sim_data2_structure$Responded2, sim_data2_structure$Age, mean)

# Same checks for sim_data2_cov -- the response pattern should hold
# regardless of the population's own age-tenure association.
chisq.test(table(sim_data2_cov_structure$Age, sim_data2_cov_structure$Responded1))
chisq.test(table(sim_data2_cov_structure$Age, sim_data2_cov_structure$Responded2))
tapply(sim_data2_cov_structure$Responded2, sim_data2_cov_structure$Age, mean)

# Recode Responded1/2 as Yes/No factors for the mosaic plots below,
# giving one tidy table per population.
build_response_table <- function(structure_df) {
  response_table <- structure_df
  response_table$Responded1 <- factor(
    response_table$Responded1,
    levels = c(0, 1),
    labels = c("No", "Yes")
  )
  response_table$Responded2 <- factor(
    response_table$Responded2,
    levels = c(0, 1),
    labels = c("No", "Yes")
  )
  rownames(response_table) <- NULL
  response_table
}

sim_data2_response_table <- build_response_table(sim_data2_structure)
sim_data2_cov_response_table <- build_response_table(sim_data2_cov_structure)

str(sim_data2_response_table)
str(sim_data2_cov_response_table)
nrow(sim_data2_response_table)
nrow(sim_data2_cov_response_table)
table(sim_data2_response_table$Responded1, sim_data2_response_table$Responded2)
table(
  sim_data2_cov_response_table$Responded1,
  sim_data2_cov_response_table$Responded2
)

# The double-decker response-rate plots now live in
# scripts/simsamp2_doubledecker.R -- they weren't used anywhere in
# the thesis and dragged in legendry::guide_axis_nested(), which
# clashes with ggmosaic2. That script isn't sourced from anywhere.
