# ---------------------------------------------------------------
# Individual-level data structures for simdata2.R: for each of the
# two populations (independent `sim_data2`, associated
# `sim_data2_cov`), build a single tidy data frame with one row per
# individual, tracking:
#
#   - ID:         unique individual identifier
#   - Age:        the individual's age bracket
#   - Tenure:     the individual's tenure status
#   - Included:   whether this individual was drawn into the 10%
#                 survey sample (the "invited" pool)
#   - Responded1: whether this individual would respond under the
#                 independent ("equal chance") response-rate case
#   - Responded2: whether this individual would respond under the
#                 associated (age-associated) response-rate case
#
# Responded1/Responded2 are only meaningful (non-NA) for included
# individuals, since responses are only ever drawn from those
# actually invited to the survey.
# ---------------------------------------------------------------

source("simdata2.R")
# legendry provides guide_axis_nested(), used below to split each
# double decker plot's combined "Age:Tenure" x-axis label into two
# clean, bracketed rows -- one per variable.
library(legendry)

set.seed(13)

# --- 1. Response rate case builders ---
# Same exponential-adjustment construction used to seed the
# association in simdata2.R itself.

# Case 1 ("equal chance" / independent): a flat response rate
# applied uniformly across every age x tenure combination.
build_response_matrix_indep <- function(rate = 0.30) {
  array(
    rate,
    dim = c(length(age_levels), length(tenure_levels)),
    dimnames = list(age = age_levels, tenure = tenure_levels)
  )
}

# Case 2 ("associated"): response rate increases with age. `alpha`
# controls the strength of the association -- 0 reduces to a flat
# rate; larger values create a steeper increase with age.
build_response_matrix_age <- function(base_rate = 0.20, alpha = 1.0) {
  age_score <- seq(0, 1, length.out = length(age_levels)) # 0 (youngest) to 1 (oldest)
  rate_by_age <- base_rate * exp(alpha * age_score)
  matrix(
    rate_by_age,
    nrow = length(age_levels),
    ncol = length(tenure_levels),
    dimnames = list(age = age_levels, tenure = tenure_levels)
  )
}

# --- 2. Apply a response rate case to a data frame ---
# For each individual, look up their age x tenure response rate
# under the given case and draw a single Bernoulli trial to decide
# whether they respond.
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

# --- 3. Build the ID / Age / Tenure / Included / Responded1 /
# Responded2 structure for a population ---
# A random 10% of individuals are marked Included (the survey
# sample); Responded1 and Responded2 are then simulated only for
# those included individuals, leaving NA for everyone else. ID is
# carried over from the population itself (assigned in simdata2.R
# when each individual was generated), so the same individual can be
# followed through both the population and this sampling/response
# structure by ID.
build_population_structure <- function(population, frac = 0.10) {
  n <- nrow(population)
  n_included <- round(n * frac)
  included <- logical(n)
  included[sample.int(n, n_included)] <- TRUE

  structure_df <- data.frame(
    ID = population$ID,
    Age = population$age,
    Tenure = population$tenure,
    Included = included,
    Responded1 = NA_integer_,
    Responded2 = NA_integer_
  )

  structure_df$Responded1[included] <- apply_matrix_response(
    population[included, , drop = FALSE],
    response_matrix_indep
  )
  structure_df$Responded2[included] <- apply_matrix_response(
    population[included, , drop = FALSE],
    response_matrix_age
  )

  structure_df
}

sim_data2_structure <- build_population_structure(sim_data2)
sim_data2_cov_structure <- build_population_structure(sim_data2_cov)

# --- 4. Quick checks ---
str(sim_data2_structure)
str(sim_data2_cov_structure)

# Included should be ~10% TRUE in both populations.
mean(sim_data2_structure$Included)
mean(sim_data2_cov_structure$Included)

# Among included individuals in the independent population
# (sim_data2): Responded1 (equal chance) should show no meaningful
# association with Age; Responded2 (associated) should show a
# significant, increasing-with-age association.
chisq.test(table(
  sim_data2_structure$Age[sim_data2_structure$Included],
  sim_data2_structure$Responded1[sim_data2_structure$Included]
))
chisq.test(table(
  sim_data2_structure$Age[sim_data2_structure$Included],
  sim_data2_structure$Responded2[sim_data2_structure$Included]
))
tapply(
  sim_data2_structure$Responded2[sim_data2_structure$Included],
  sim_data2_structure$Age[sim_data2_structure$Included],
  mean
)

# Same checks for the associated population (sim_data2_cov) -- the
# Responded1/Responded2 pattern should hold regardless of the
# underlying population's own age-tenure association.
chisq.test(table(
  sim_data2_cov_structure$Age[sim_data2_cov_structure$Included],
  sim_data2_cov_structure$Responded1[sim_data2_cov_structure$Included]
))
chisq.test(table(
  sim_data2_cov_structure$Age[sim_data2_cov_structure$Included],
  sim_data2_cov_structure$Responded2[sim_data2_cov_structure$Included]
))
tapply(
  sim_data2_cov_structure$Responded2[sim_data2_cov_structure$Included],
  sim_data2_cov_structure$Age[sim_data2_cov_structure$Included],
  mean
)

# --- 5. Tidy tables for mosaic plots of response rate by category
# ---
# Reshape each population's structure down to just the individuals
# sliced into the survey sample (Included == TRUE) -- one row per
# ID, with Responded1 and Responded2 as separate Yes/No columns
# (rather than long format), giving exactly 10,000 rows per
# population. This is the shape geom_mosaic() needs to plot response
# rate by category -- x = product(Age, Tenure), fill = Responded1 or
# Responded2. Two separate tables are built (one per population).
build_response_table <- function(structure_df) {
  response_table <- structure_df[
    structure_df$Included,
    c("ID", "Age", "Tenure", "Responded1", "Responded2")
  ]
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

# --- 6. Double decker plots of response rate by category, for each
# population and each response case separately ---
# Age and tenure are both nested on the x-axis (via divider =
# ddecker(), the double decker template: a sequence of spines in the
# same direction -- here age nested within tenure -- with the final
# spine, response status, laid out in the opposite direction),
# giving the y-axis its usual double decker role as the conditional
# proportion of Yes/No responses within every age x tenure
# combination. Response status is shown both as that y-axis split
# and as fill colour, matching the classic double decker
# presentation. guide_axis_nested() splits the combined "Age:Tenure"
# x-axis label into two clean, bracketed rows -- one per variable.
ggplot(sim_data2_response_table) +
  geom_mosaic(
    aes(x = product(Age, Tenure), fill = Responded1),
    divider = ddecker()
  ) +
  labs(fill = "Responded", title = "sim_data2: Responded1 (equal chance)") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 7)) +
  guides(x = guide_axis_nested(key = key_range_auto(sep = ":")))

ggplot(sim_data2_response_table) +
  geom_mosaic(
    aes(x = product(Age, Tenure), fill = Responded2),
    divider = ddecker()
  ) +
  labs(fill = "Responded", title = "sim_data2: Responded2 (associated)") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 7)) +
  guides(x = guide_axis_nested(key = key_range_auto(sep = ":")))

ggplot(sim_data2_cov_response_table) +
  geom_mosaic(
    aes(x = product(Age, Tenure), fill = Responded1),
    divider = ddecker()
  ) +
  labs(
    fill = "Responded",
    title = "sim_data2_cov: Responded1 (equal chance)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 7)) +
  guides(x = guide_axis_nested(key = key_range_auto(sep = ":")))

ggplot(sim_data2_cov_response_table) +
  geom_mosaic(
    aes(x = product(Age, Tenure), fill = Responded2),
    divider = ddecker()
  ) +
  labs(
    fill = "Responded",
    title = "sim_data2_cov: Responded2 (associated)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 7)) +
  guides(x = guide_axis_nested(key = key_range_auto(sep = ":")))
