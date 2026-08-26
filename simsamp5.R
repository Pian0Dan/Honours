# ---------------------------------------------------------------
# Sampling process for simdata5.R: from each simulated population
# (the independent `sim_data5` and covaried `sim_data5_oeh`
# age x gender x tenure x education x location populations), first
# draw a random 10% survey sample (the "invited" pool), then
# simulate which of those invited individuals actually respond,
# under three response-rate cases:
#
#  1. Independent: everyone in the sample is equally likely to
#     respond, regardless of any trait.
#  2. Age-covaried: response likelihood increases with age (an
#     observed trait).
#  3. Latent-covaried: response likelihood increases with the
#     unobserved binary `Latent` trait, which is not accounted for
#     in the population's demographic structure -- illustrating how
#     an unobserved variable can bias who responds (and therefore
#     panel composition) without ever showing up as an association
#     with age or any other observed trait.
#
# Response rate "matrices" (or lookup vectors, for the latent case)
# do not need to sum to 1 -- each cell/lookup value is simply a
# probability in [0, 1], used to draw one Bernoulli response trial
# per sampled individual.
# ---------------------------------------------------------------

source("simdata5.R")

set.seed(13)

# --- 1. Draw a random 10% survey sample from a population ---
# This is the "invited" pool from which responses are then drawn,
# taken before any response-rate case is applied.
draw_survey_sample <- function(data, frac = 0.10) {
  n_sample <- round(nrow(data) * frac)
  data[sample.int(nrow(data), n_sample), , drop = FALSE]
}

# --- 2. Response rate case builders ---
# Response rate arrays are indexed by age x gender x tenure x
# education x location, mirroring the shape of simdata5.R's
# population joint probability arrays, even where a case only
# varies with one of the five dimensions (the other dimensions are
# simply broadcast).

# Case 1 (independent): a flat response rate applied uniformly
# across every combination of the five traits, providing a baseline
# against which the effects of differential response (cases 2 and
# 3) can be compared.
build_response_matrix_indep <- function(rate = 0.30) {
  array(
    rate,
    dim = c(
      length(age_levels),
      length(gender_levels),
      length(tenure_levels),
      length(education_levels),
      length(location_levels)
    ),
    dimnames = list(
      age = age_levels,
      gender = gender_levels,
      tenure = tenure_levels,
      education = education_levels,
      location = location_levels
    )
  )
}

# Case 2 (age-covaried): response rate increases with age, using
# the same ordinal-score-and-exponential-adjustment construction
# used to seed covariance in simdata5.R itself. `alpha` controls the
# strength of the association -- 0 reduces to a flat rate; larger
# values create a steeper increase with age. The same rate is
# broadcast across every gender x tenure x education x location
# combination (age varies fastest in the array, matching how the
# population arrays in simdata5.R are laid out).
build_response_matrix_age <- function(base_rate = 0.20, alpha = 1.0) {
  age_score <- seq(0, 1, length.out = length(age_levels)) # 0 (youngest) to 1 (oldest)
  rate_by_age <- base_rate * exp(alpha * age_score)
  n_other <- length(gender_levels) *
    length(tenure_levels) *
    length(education_levels) *
    length(location_levels)
  array(
    rep(rate_by_age, times = n_other),
    dim = c(
      length(age_levels),
      length(gender_levels),
      length(tenure_levels),
      length(education_levels),
      length(location_levels)
    ),
    dimnames = list(
      age = age_levels,
      gender = gender_levels,
      tenure = tenure_levels,
      education = education_levels,
      location = location_levels
    )
  )
}

# Case 3 (latent-covaried): response rate increases with the
# unobserved `Latent` trait rather than with any observed trait.
# Same exponential construction as case 2, but indexed by Latent
# (0/1) instead of age.
build_response_rate_latent <- function(base_rate = 0.20, alpha = 0.8) {
  rate <- base_rate * exp(alpha * c(0, 1))
  names(rate) <- c("0", "1")
  rate
}

# --- 3. Apply a response rate case to a sampled population ---
# For each individual in the sample, look up their response rate
# under the given case and draw a single Bernoulli trial to decide
# whether they respond.
apply_matrix_response <- function(data, response_matrix) {
  idx <- cbind(
    match(data$age, dimnames(response_matrix)$age),
    match(data$gender, dimnames(response_matrix)$gender),
    match(data$tenure, dimnames(response_matrix)$tenure),
    match(data$education, dimnames(response_matrix)$education),
    match(data$location, dimnames(response_matrix)$location)
  )
  rbinom(nrow(data), 1, response_matrix[idx])
}

apply_latent_response <- function(data, rate_by_latent) {
  rbinom(nrow(data), 1, rate_by_latent[as.character(data$Latent)])
}

# --- 4. Build the three response rate cases ---
response_matrix_indep <- build_response_matrix_indep()
response_matrix_age <- build_response_matrix_age()
response_rate_latent <- build_response_rate_latent()

response_rate_latent

# --- 5. Draw survey samples and simulate responses for each
# population ---
# Both the independent (sim_data5) and covaried (sim_data5_oeh)
# populations from simdata5.R are put through the same sampling and
# response process, so the effect of each response-rate case can be
# examined independently of which population it is applied to.
sim_data5_sample <- draw_survey_sample(sim_data5)
sim_data5_oeh_sample <- draw_survey_sample(sim_data5_oeh)

# Mosaic plots of the survey sample itself (the 10% invited pool,
# before any response is simulated) -- distinct from the
# population-level mosaic plots in simdata5.R, this shows whether
# the random draw preserved each population's full five-variable
# structure at the smaller sample size. This replicates simdata5.R's
# own mosaic plot structure exactly: age purely on the y-axis (via
# the same explicit divider spec), tenure/education/location on the
# x-axis, and gender used purely as fill colour.
# strip_mosaic_fill_labels() removes gender's segment from the
# composite "gender:tenure:education:location" label before
# guide_axis_nested() splits the remainder into three clean,
# bracketed rows. Recode tenure/education/location to first-letter
# labels for this plot only, so the dense nested x-axis text stays
# readable; gender is left unabbreviated since it only ever appears
# via fill colour and the legend.
sim_data5_sample_plot <- abbreviate_for_plot(
  sim_data5_sample,
  c("tenure", "education", "location")
)

sim_data5_sample_base <- ggplot(sim_data5_sample_plot) +
  geom_mosaic(
    aes(x = product(age, tenure, education, location), fill = gender),
    divider = c("hspine", "vspine", "hspine", "hspine", "hspine")
  )
sim_data5_sample_labels <- strip_mosaic_fill_labels(sim_data5_sample_base)

sim_data5_sample_base +
  scale_x_productlist(
    name = NULL,
    breaks = sim_data5_sample_labels$breaks,
    labels = sim_data5_sample_labels$labels
  ) +
  scale_y_productlist(name = "Age Brackets") +
  labs(fill = "Gender", title = "Survey sample: sim_data5_sample") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 5)) +
  guides(x = guide_axis_nested(key = key_range_auto(sep = ":")))

sim_data5_oeh_sample_plot <- abbreviate_for_plot(
  sim_data5_oeh_sample,
  c("tenure", "education", "location")
)

sim_data5_oeh_sample_base <- ggplot(sim_data5_oeh_sample_plot) +
  geom_mosaic(
    aes(x = product(age, tenure, education, location), fill = gender),
    divider = c("hspine", "vspine", "hspine", "hspine", "hspine")
  )
sim_data5_oeh_sample_labels <- strip_mosaic_fill_labels(
  sim_data5_oeh_sample_base
)

sim_data5_oeh_sample_base +
  scale_x_productlist(
    name = NULL,
    breaks = sim_data5_oeh_sample_labels$breaks,
    labels = sim_data5_oeh_sample_labels$labels
  ) +
  scale_y_productlist(name = "Age Brackets") +
  labs(fill = "Gender", title = "Survey sample: sim_data5_oeh_sample") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 5)) +
  guides(x = guide_axis_nested(key = key_range_auto(sep = ":")))

for (sample_name in c("sim_data5_sample", "sim_data5_oeh_sample")) {
  sample_df <- get(sample_name)

  sample_df$responded_indep <- apply_matrix_response(
    sample_df,
    response_matrix_indep
  )
  sample_df$responded_age <- apply_matrix_response(
    sample_df,
    response_matrix_age
  )
  sample_df$responded_latent <- apply_latent_response(
    sample_df,
    response_rate_latent
  )

  assign(sample_name, sample_df)
}

# --- 6. Quick checks ---
# Sample sizes should be ~10% of each population.
nrow(sim_data5_sample)
nrow(sim_data5_oeh_sample)

# Case 1 (independent): realized response rate should be close to
# 0.30, with no meaningful association between responding and age
# in either sample, since the matrix is flat.
mean(sim_data5_sample$responded_indep)
chisq.test(table(sim_data5_sample$age, sim_data5_sample$responded_indep))
mean(sim_data5_oeh_sample$responded_indep)
chisq.test(table(
  sim_data5_oeh_sample$age,
  sim_data5_oeh_sample$responded_indep
))

# Case 2 (age-covaried): response rate should rise with age, and
# the association between age and responding should now be
# significant in both samples.
tapply(sim_data5_sample$responded_age, sim_data5_sample$age, mean)
chisq.test(table(sim_data5_sample$age, sim_data5_sample$responded_age))
tapply(sim_data5_oeh_sample$responded_age, sim_data5_oeh_sample$age, mean)
chisq.test(table(sim_data5_oeh_sample$age, sim_data5_oeh_sample$responded_age))

# Case 3 (latent-covaried): response rate should be higher for
# Latent = 1 than Latent = 0, with a significant association between
# Latent and responding, but -- crucially -- no significant
# association between age (or any other observed trait) and
# responding, since the bias is tied entirely to the unobserved
# trait rather than to any variable an algorithm could actually see.
tapply(sim_data5_sample$responded_latent, sim_data5_sample$Latent, mean)
chisq.test(table(sim_data5_sample$Latent, sim_data5_sample$responded_latent))
chisq.test(table(sim_data5_sample$age, sim_data5_sample$responded_latent))

tapply(sim_data5_oeh_sample$responded_latent, sim_data5_oeh_sample$Latent, mean)
chisq.test(table(
  sim_data5_oeh_sample$Latent,
  sim_data5_oeh_sample$responded_latent
))
chisq.test(table(
  sim_data5_oeh_sample$age,
  sim_data5_oeh_sample$responded_latent
))

# --- 7. Visualise how the sample's age composition shifts under
# each response-rate case ---
# For a given sample, compute the age composition of (a) everyone
# invited (the full 10% sample, before any response is simulated),
# and (b) just those who responded under each of the three cases.
# Comparing these age distributions side by side shows whether -- and
# how much -- each response-rate case skews who ends up responding,
# relative to who was originally invited.
age_composition <- function(sample_df) {
  rbind(
    data.frame(
      case = "Sample (invited)",
      prop.table(table(age = sample_df$age))
    ),
    data.frame(
      case = "Independent response",
      prop.table(table(age = sample_df$age[sample_df$responded_indep == 1]))
    ),
    data.frame(
      case = "Age-covaried response",
      prop.table(table(age = sample_df$age[sample_df$responded_age == 1]))
    ),
    data.frame(
      case = "Latent-covaried response",
      prop.table(table(age = sample_df$age[sample_df$responded_latent == 1]))
    )
  )
}

age_composition_long <- rbind(
  cbind(
    population = "sim_data5 (independent)",
    age_composition(sim_data5_sample)
  ),
  cbind(
    population = "sim_data5_oeh (covaried)",
    age_composition(sim_data5_oeh_sample)
  )
)
age_composition_long$case <- factor(
  age_composition_long$case,
  levels = c(
    "Sample (invited)",
    "Independent response",
    "Age-covaried response",
    "Latent-covaried response"
  ),
  labels = c("Sample", "Independent", "Age-covaried", "Latent-covaried")
)

# Rendered as pie charts (one per population x response-rate case)
# rather than grouped bars, since pie charts are the more common
# convention for showing a single categorical variable's composition
# as a share of a whole -- each slice is age's share of that
# particular sample/case, and the grid of pies lets the shift in age
# composition be compared case by case.
ggplot(age_composition_long, aes(x = "", y = Freq, fill = age)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  facet_grid(population ~ case) +
  labs(
    fill = "Age bracket",
    title = "Age composition of the invited sample vs. respondents under each response-rate case"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )
