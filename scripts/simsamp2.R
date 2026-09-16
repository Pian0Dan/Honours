# ---------------------------------------------------------------
# Individual-level data structures for simdata2.R: for each of the
# two populations (independent `sim_data2`, associated
# `sim_data2_cov`), first randomly slice the population down to
# 10,000 individuals (the survey sample), visualise that slice with
# a mosaic plot matching the structure of simdata2.R's own
# population-level plots, and then build a single tidy data frame
# with one row per sliced individual, tracking:
#
#   - ID:         unique individual identifier (carried over from
#                 the population)
#   - Age:        the individual's age bracket
#   - Tenure:     the individual's tenure status
#   - Responded1: whether this individual would respond under the
#                 independent ("equal chance") response-rate case
#   - Responded2: whether this individual would respond under the
#                 associated (age-associated) response-rate case
#
# Every individual in this file's data frames belongs to the 10,000-
# person slice (there is no separate "Included" flag any more, since
# slicing to 10,000 *is* the act of inclusion), so Responded1/
# Responded2 are always simulated.
# ---------------------------------------------------------------

source("scripts/simdata2.R")
# legendry provides guide_axis_nested(), used below to split each
# double decker plot's combined "Age:Tenure" x-axis label into two
# clean, bracketed rows -- one per variable.
library(legendry)

set.seed(13)

# --- 1. Randomly slice each population down to 10,000 individuals
# ---
# Each slice is a simple random sample (without replacement) of
# 10,000 rows from the full 100,000-row population -- this is the
# survey sample that response rates will be simulated against below.
slice_population <- function(population, n = 10000) {
  population[sample.int(nrow(population), n), , drop = FALSE]
}

sim_data2_slice <- slice_population(sim_data2)
sim_data2_cov_slice <- slice_population(sim_data2_cov)

nrow(sim_data2_slice)
nrow(sim_data2_cov_slice)

# --- 2. Mosaic plots of each slice ---
# Structured identically to simdata2.R's pop2_ind_plot / pop2_cov_plot
# (age on the x-axis, tenure as fill), so the sliced sample can be
# visually compared against the full population it was drawn from.
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

# --- 3. Response rate case builders ---
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

# --- 4. Apply a response rate case to a data frame ---
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

# --- 5. Build the ID / Age / Tenure / Responded1 / Responded2
# structure for a 10,000-person slice ---
# Every individual in the slice is, by construction, part of the
# survey sample, so Responded1 and Responded2 are simulated for all
# of them. ID is carried over from the population itself (assigned
# in simdata2.R when each individual was generated), so the same
# individual can be followed through both the population and this
# sampling/response structure by ID.
build_population_structure <- function(population_slice) {
  data.frame(
    ID = population_slice$ID,
    Age = population_slice$age,
    Tenure = population_slice$tenure,
    Responded1 = apply_matrix_response(population_slice, response_matrix_indep),
    Responded2 = apply_matrix_response(population_slice, response_matrix_age)
  )
}

sim_data2_structure <- build_population_structure(sim_data2_slice)
sim_data2_cov_structure <- build_population_structure(sim_data2_cov_slice)

# --- 6. Quick checks ---
str(sim_data2_structure)
str(sim_data2_cov_structure)

# Every row here is included by construction (the slice itself is
# the 10,000-person survey sample).
nrow(sim_data2_structure)
nrow(sim_data2_cov_structure)

# Among individuals in the independent population's slice
# (sim_data2): Responded1 (equal chance) should show no meaningful
# association with Age; Responded2 (associated) should show a
# significant, increasing-with-age association.
chisq.test(table(
  sim_data2_structure$Age,
  sim_data2_structure$Responded1
))
chisq.test(table(
  sim_data2_structure$Age,
  sim_data2_structure$Responded2
))
tapply(
  sim_data2_structure$Responded2,
  sim_data2_structure$Age,
  mean
)

# Same checks for the associated population's slice (sim_data2_cov)
# -- the Responded1/Responded2 pattern should hold regardless of the
# underlying population's own age-tenure association.
chisq.test(table(
  sim_data2_cov_structure$Age,
  sim_data2_cov_structure$Responded1
))
chisq.test(table(
  sim_data2_cov_structure$Age,
  sim_data2_cov_structure$Responded2
))
tapply(
  sim_data2_cov_structure$Responded2,
  sim_data2_cov_structure$Age,
  mean
)

# --- 7. Tidy tables for mosaic plots of response rate by category
# ---
# Reshape each population's structure with Responded1 and Responded2
# as separate Yes/No factor columns (rather than 0/1 integers),
# giving exactly 10,000 rows per population -- the shape geom_mosaic()
# needs to plot response rate by category -- x = product(Age,
# Tenure), fill = Responded1 or Responded2. Two separate tables are
# built (one per population).
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

# --- 8. Double decker plots of response rate by category, for each
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
pop2_ind_eq_plot <- ggplot(sim_data2_response_table) +
  geom_mosaic(
    aes(x = product(Age, Tenure), fill = Responded1),
    divider = ddecker()
  ) +
  labs(fill = "Responded", title = "sim_data2: Responded1 (equal chance)") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 7)) +
  guides(x = guide_axis_nested(key = key_range_auto(sep = ":")))

pop2_ind_ass_plot <- ggplot(sim_data2_response_table) +
  geom_mosaic(
    aes(x = product(Age, Tenure), fill = Responded2),
    divider = ddecker()
  ) +
  labs(fill = "Responded", title = "sim_data2: Responded2 (associated)") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 7)) +
  guides(x = guide_axis_nested(key = key_range_auto(sep = ":")))

pop2_cov_eq_plot <- ggplot(sim_data2_cov_response_table) +
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

pop2_cov_ass_plot <- ggplot(sim_data2_cov_response_table) +
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
