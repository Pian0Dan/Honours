# Code from Claude
# ---------------------------------------------------------------
# Simulate categorical data (age x gender x tenure x education x
# location) with NO association between variables, using
# stats::rmultinom()
#
# Key idea: independence means the joint probability of any
# combination of levels equals the PRODUCT of the marginal
# probabilities. So we build the joint probability table as an
# outer product of five marginal vectors, flatten it into a
# vector, draw counts for every cell with one call to rmultinom(),
# then expand those counts into individual rows.
# ---------------------------------------------------------------

library(ggplot2)
library(ggmosaic)
# legendry provides guide_axis_nested(), used below to render each
# mosaic's composite x-axis label ("age:tenure:education:location")
# as separate, bracketed rows -- one row per variable -- which is how
# published double decker plots typically label their axis.
library(legendry)
# first_letter_labels()/abbreviate_for_plot() recode categorical
# levels down to their first letter (extending colliding levels by
# an extra letter, e.g. Regional -> R, Remote -> Re) for use in
# plots only -- the underlying analysis data keeps full level names.
source("scripts/label_helpers.R")

set.seed(13)

# --- 1. Define levels for each variable ---
age_levels <- c("18-34", "35-44", "45-54", "55-64", "65+") # 5 levels
gender_levels <- c("Male", "Female", "Non-binary") # 3 levels
tenure_levels <- c("Rent", "Own", "Other") # 3 levels
education_levels <- c(
  "No Qualification",
  "Secondary",
  "Bachelor",
  "Postgraduate"
) # 4 levels
location_levels <- c("Metro", "Regional", "Remote") # 3 levels

# --- 2. Define marginal probability vectors (each must sum to 1) ---
p_age <- c(0.20, 0.20, 0.20, 0.20, 0.20) # uniform across age groups
p_gender <- c(0.45, 0.45, 0.10) # e.g. Male/Female/Non-binary
p_tenure <- c(0.30, 0.40, 0.30) # Rent/Own/Other
p_education <- c(0.20, 0.35, 0.30, 0.15) # No Qualification/Secondary/Bachelor/Postgraduate
p_location <- c(0.60, 0.30, 0.10) # Metro/Regional/Remote

stopifnot(
  all.equal(sum(p_age), 1),
  all.equal(sum(p_gender), 1),
  all.equal(sum(p_tenure), 1),
  all.equal(sum(p_education), 1),
  all.equal(sum(p_location), 1)
)

# --- 3. Build the joint probability array as an outer product ---
# Because it's a pure outer product, P(age,gender,tenure,education,location) =
# P(age) * P(gender) * P(tenure) * P(education) * P(location) for
# every cell -> independence by construction in the DGP
# (data-generating process).
joint_array <- outer(
  outer(outer(outer(p_age, p_gender), p_tenure), p_education),
  p_location
)
dim(joint_array) <- c(
  length(age_levels),
  length(gender_levels),
  length(tenure_levels),
  length(education_levels),
  length(location_levels)
)
dimnames(joint_array) <- list(
  age = age_levels,
  gender = gender_levels,
  tenure = tenure_levels,
  education = education_levels,
  location = location_levels
)

# Flatten to a probability vector for rmultinom().
# as.vector() on an N-D array varies dim 1 (age) fastest, then dim 2
# (gender), dim 3 (tenure), dim 4 (education), then dim 5 (location)
# slowest -- expand.grid() below uses the same ordering, so the two
# align exactly.
prob_vec <- as.vector(joint_array)
stopifnot(all.equal(sum(prob_vec), 1))

# --- 4. Draw cell counts with a single multinomial draw ---
n_total <- 100000 # total number of simulated records
cell_counts <- rmultinom(n = 1, size = n_total, prob = prob_vec)[, 1]

# --- 5. Expand cell counts into an individual-level data frame ---
combos <- expand.grid(
  age = age_levels,
  gender = gender_levels,
  tenure = tenure_levels,
  education = education_levels,
  location = location_levels
)
combos$count <- cell_counts

sim_data5 <- combos[
  rep(seq_len(nrow(combos)), combos$count),
  c("age", "gender", "tenure", "education", "location")
]
rownames(sim_data5) <- NULL

# Convert to factors with the intended level order
sim_data5$age <- factor(sim_data5$age, levels = age_levels)
sim_data5$gender <- factor(sim_data5$gender, levels = gender_levels)
sim_data5$tenure <- factor(sim_data5$tenure, levels = tenure_levels)
sim_data5$education <- factor(sim_data5$education, levels = education_levels)
sim_data5$location <- factor(sim_data5$location, levels = location_levels)

# Add a random binary variable, independent of every other column
# (a simple Bernoulli(0.5) draw per row).
sim_data5$Latent <- rbinom(nrow(sim_data5), 1, 0.5)

# --- 6. Quick checks ---
str(sim_data5)
prop.table(table(sim_data5$age))
prop.table(table(sim_data5$gender))
prop.table(table(sim_data5$tenure))
prop.table(table(sim_data5$education))
prop.table(table(sim_data5$location))

# Chi-square tests should show no significant association
# (occasional significant p-values are expected by chance,
# especially with large n -- this only reflects sampling noise,
# not a real relationship, since independence is baked into prob_vec)
chisq.test(table(sim_data5$age, sim_data5$gender))
chisq.test(table(sim_data5$age, sim_data5$tenure))
chisq.test(table(sim_data5$gender, sim_data5$tenure))
chisq.test(table(sim_data5$education, sim_data5$location))

# Mosaic plot with age purely on the y-axis (name = "age", 5 clean
# levels, via the explicit divider spec below -- ggmosaic's divider
# argument resolves axis placement through non-obvious recursive
# geometry rather than a simple per-variable rule, so this exact
# sequence was found by testing combinations rather than derived
# analytically), tenure/education/location on the x-axis, and gender
# used purely as fill colour. ggmosaic's own x tick labels would
# normally be a "gender:tenure:education:location" composite (fill
# variables are always folded into the axis text once 3+ variables
# are combined); strip_mosaic_fill_labels() removes gender's segment
# from that composite before guide_axis_nested() splits the
# remainder into three clean, bracketed rows -- one per x-product
# variable -- so gender's categories never appear as text, only as
# colour and in the legend.
# Recode tenure/education/location to first-letter labels for this
# plot only (e.g. Own -> O, Other -> Ot, Regional -> R, Remote ->
# Re), so the dense nested x-axis text stays readable. Gender is
# left unabbreviated since it only ever appears via fill colour and
# the legend, never as axis text.
sim_data5_plot <- abbreviate_for_plot(
  sim_data5,
  c("tenure", "education", "location")
)

sim_data5_base <- ggplot(sim_data5_plot) +
  geom_mosaic(
    aes(x = product(age, tenure, education, location), fill = gender),
    divider = c("hspine", "vspine", "hspine", "hspine", "hspine")
  )
sim_data5_labels <- strip_mosaic_fill_labels(sim_data5_base)

sim_data5_base +
  scale_x_productlist(
    name = NULL,
    breaks = sim_data5_labels$breaks,
    labels = sim_data5_labels$labels
  ) +
  scale_y_productlist(name = "Age Brackets") +
  labs(fill = "Gender") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 5)) +
  guides(x = guide_axis_nested(key = key_range_auto(sep = ":")))


# --- Old, Educated, Homeowner Skew w/ positive cov between age and homeownership, positive cov between non-binary and renting, positive cov between age and education, and positive cov between education and homeownership ---

set.seed(13)

# --- 1. Define levels for each variable ---
age_levels <- c("18-34", "35-44", "45-54", "55-64", "65+") # 5 levels
gender_levels <- c("Male", "Female", "Non-binary") # 3 levels
tenure_levels <- c("Rent", "Own", "Other") # 3 levels
education_levels <- c(
  "No Qualification",
  "Secondary",
  "Bachelor",
  "Postgraduate"
) # 4 levels
location_levels <- c("Metro", "Regional", "Remote") # 3 levels

# --- 2. Define marginal probability vectors (each must sum to 1) ---
p_age <- c(0.10, 0.15, 0.20, 0.25, 0.30) # Heavy older skew
p_gender <- c(0.45, 0.45, 0.10) # e.g. Male/Female/Non-binary (skew to Non-binary)
p_tenure <- c(0.30, 0.60, 0.10) # Rent/Own/Other (homeowner skew)
p_education <- c(0.10, 0.20, 0.30, 0.40) # No Qualification/Secondary/Bachelor/Postgraduate (educated skew)
p_location <- c(0.60, 0.30, 0.10) # Metro/Regional/Remote

stopifnot(
  all.equal(sum(p_age), 1),
  all.equal(sum(p_gender), 1),
  all.equal(sum(p_tenure), 1),
  all.equal(sum(p_education), 1),
  all.equal(sum(p_location), 1)
)

# --- 3. Build a joint array with the desired association, then rake it
# so every margin matches p_age / p_gender / p_tenure / p_education /
# p_location exactly ---

# 3a. Seed array: start from independence, then multiply in an
# association pattern with:
#  - POSITIVE covariance between age and homeownership (older people
#    own more than chance; younger people own less than chance)
#  - POSITIVE covariance between "Non-binary" gender and renting (renters
#    skew toward the "Non-binary" gender category)
#  - POSITIVE covariance between age and education (older people skew
#    toward higher education levels than chance), implemented as a
#    linear-by-linear ordinal association: the boost applied to each
#    (age, education) combination scales with the product of their
#    ordinal scores, so higher age and higher education reinforce
#    each other smoothly
#  - POSITIVE covariance between education and homeownership (people
#    with higher education levels own more than chance)
# Location is not targeted by any adjustment, so it stays independent
# of every other variable once raked.
# `alpha` controls the strength of each association -- 0 reduces to
# independence; larger values create stronger covariance.
seed_array <- outer(
  outer(outer(outer(p_age, p_gender), p_tenure), p_education),
  p_location
)
dim(seed_array) <- c(
  length(age_levels),
  length(gender_levels),
  length(tenure_levels),
  length(education_levels),
  length(location_levels)
)

alpha_age_own <- 1.2
alpha_gender_rent <- 1.2
alpha_age_education <- 1.2
alpha_education_own <- 1.2
age_score <- seq(0, 1, length.out = length(age_levels)) # 0 (youngest) to 1 (oldest)
education_score <- seq(0, 1, length.out = length(education_levels)) # 0 (least) to 1 (most educated)
nonbinary_idx <- which(gender_levels == "Non-binary")
own_idx <- which(tenure_levels == "Own")
rent_idx <- which(tenure_levels == "Rent")

for (i in seq_along(age_levels)) {
  # Older ages own more -> positive covariance between age and
  # homeownership
  seed_array[i, , own_idx, , ] <- seed_array[i, , own_idx, , ] *
    exp(alpha_age_own * age_score[i])
}

# "Non-binary" gender rents more -> positive covariance between
# Non-binary and renting
seed_array[, nonbinary_idx, rent_idx, , ] <- seed_array[,
  nonbinary_idx,
  rent_idx,
  ,
] *
  exp(alpha_gender_rent)

for (i in seq_along(age_levels)) {
  for (j in seq_along(education_levels)) {
    # Older ages skew toward higher education -> positive covariance
    # between age and education (score product keeps the boost
    # ordinal, rather than targeting a single category of either
    # variable)
    seed_array[i, , , j, ] <- seed_array[i, , , j, ] *
      exp(alpha_age_education * age_score[i] * education_score[j])
  }
}

for (j in seq_along(education_levels)) {
  # Higher education levels own more -> positive covariance between
  # education and homeownership
  seed_array[,, own_idx, j, ] <- seed_array[,, own_idx, j, ] *
    exp(alpha_education_own * education_score[j])
}

# 3b. Iterative proportional fitting (raking): alternately rescale each
# margin to match the target, until convergence. This preserves the
# association pattern above while forcing the marginals to match
# p_age, p_gender, p_tenure, p_education, p_location exactly.
joint_array <- seed_array
for (iter in seq_len(100)) {
  joint_array <- sweep(joint_array, 1, p_age / apply(joint_array, 1, sum), "*")
  joint_array <- sweep(
    joint_array,
    2,
    p_gender / apply(joint_array, 2, sum),
    "*"
  )
  joint_array <- sweep(
    joint_array,
    3,
    p_tenure / apply(joint_array, 3, sum),
    "*"
  )
  joint_array <- sweep(
    joint_array,
    4,
    p_education / apply(joint_array, 4, sum),
    "*"
  )
  joint_array <- sweep(
    joint_array,
    5,
    p_location / apply(joint_array, 5, sum),
    "*"
  )
}

stopifnot(
  isTRUE(all.equal(
    apply(joint_array, 1, sum),
    p_age,
    tolerance = 1e-8,
    check.attributes = FALSE
  )),
  isTRUE(all.equal(
    apply(joint_array, 2, sum),
    p_gender,
    tolerance = 1e-8,
    check.attributes = FALSE
  )),
  isTRUE(all.equal(
    apply(joint_array, 3, sum),
    p_tenure,
    tolerance = 1e-8,
    check.attributes = FALSE
  )),
  isTRUE(all.equal(
    apply(joint_array, 4, sum),
    p_education,
    tolerance = 1e-8,
    check.attributes = FALSE
  )),
  isTRUE(all.equal(
    apply(joint_array, 5, sum),
    p_location,
    tolerance = 1e-8,
    check.attributes = FALSE
  )),
  isTRUE(all.equal(sum(joint_array), 1))
)

dimnames(joint_array) <- list(
  age = age_levels,
  gender = gender_levels,
  tenure = tenure_levels,
  education = education_levels,
  location = location_levels
)

# Flatten to a probability vector for rmultinom().
# as.vector() on an N-D array varies dim 1 (age) fastest, then dim 2
# (gender), dim 3 (tenure), dim 4 (education), then dim 5 (location)
# slowest -- expand.grid() below uses the same ordering, so the two
# align exactly.
prob_vec <- as.vector(joint_array)
stopifnot(all.equal(sum(prob_vec), 1))

# --- 4. Draw cell counts with a single multinomial draw ---
n_total <- 100000 # total number of simulated records
cell_counts <- rmultinom(n = 1, size = n_total, prob = prob_vec)[, 1]

# --- 5. Expand cell counts into an individual-level data frame ---
combos <- expand.grid(
  age = age_levels,
  gender = gender_levels,
  tenure = tenure_levels,
  education = education_levels,
  location = location_levels
)
combos$count <- cell_counts

sim_data5_oeh <- combos[
  rep(seq_len(nrow(combos)), combos$count),
  c("age", "gender", "tenure", "education", "location")
]
rownames(sim_data5_oeh) <- NULL

# Convert to factors with the intended level order
sim_data5_oeh$age <- factor(sim_data5_oeh$age, levels = age_levels)
sim_data5_oeh$gender <- factor(sim_data5_oeh$gender, levels = gender_levels)
sim_data5_oeh$tenure <- factor(sim_data5_oeh$tenure, levels = tenure_levels)
sim_data5_oeh$education <- factor(
  sim_data5_oeh$education,
  levels = education_levels
)
sim_data5_oeh$location <- factor(
  sim_data5_oeh$location,
  levels = location_levels
)

# Add a random binary variable, independent of every other column
# (a simple Bernoulli(0.5) draw per row).
sim_data5_oeh$Latent <- rbinom(nrow(sim_data5_oeh), 1, 0.5)

# --- 6. Quick checks ---
str(sim_data5_oeh)
prop.table(table(sim_data5_oeh$age))
prop.table(table(sim_data5_oeh$gender))
prop.table(table(sim_data5_oeh$tenure))
prop.table(table(sim_data5_oeh$education))
prop.table(table(sim_data5_oeh$location))

# Chi-square tests should now show significant association for the
# pairs the seed array targeted directly: age-tenure (positive, via
# homeownership increasing with age), gender-tenure (positive, via
# "Non-binary" renting more often), age-education (positive, via
# higher education levels becoming more likely with age), and
# tenure-education (positive, via homeownership increasing with
# education). Raking preserves the marginals exactly but can also
# leak association into pairs that were not directly targeted (e.g.
# age-gender, since both are tied to tenure). Location was never
# targeted, so it should remain unassociated with everything else
# (aside from sampling noise).
chisq.test(table(sim_data5_oeh$age, sim_data5_oeh$gender))
chisq.test(table(sim_data5_oeh$age, sim_data5_oeh$tenure))
chisq.test(table(sim_data5_oeh$gender, sim_data5_oeh$tenure))
chisq.test(table(sim_data5_oeh$age, sim_data5_oeh$education))
chisq.test(table(sim_data5_oeh$tenure, sim_data5_oeh$education))
chisq.test(table(sim_data5_oeh$education, sim_data5_oeh$location))

# Mosaic plot with age purely on the y-axis, tenure/education/
# location on the x-axis, and gender used purely as fill colour (see
# the note on the sim_data5 plot above for why
# strip_mosaic_fill_labels() is used instead of ggmosaic's automatic
# tick labels -- it removes gender's segment from the composite
# "gender:tenure:education:location" label before guide_axis_nested()
# splits the remainder into three clean, bracketed rows).
# Recode tenure/education/location to first-letter labels for this
# plot only. Gender is left unabbreviated (see note on the sim_data5
# plot above).
sim_data5_oeh_plot <- abbreviate_for_plot(
  sim_data5_oeh,
  c("tenure", "education", "location")
)

sim_data5_oeh_base <- ggplot(sim_data5_oeh_plot) +
  geom_mosaic(
    aes(x = product(age, tenure, education, location), fill = gender),
    divider = c("hspine", "vspine", "hspine", "hspine", "hspine")
  )
sim_data5_oeh_labels <- strip_mosaic_fill_labels(sim_data5_oeh_base)

sim_data5_oeh_base +
  scale_x_productlist(
    name = NULL,
    breaks = sim_data5_oeh_labels$breaks,
    labels = sim_data5_oeh_labels$labels
  ) +
  scale_y_productlist(name = "Age Brackets") +
  labs(fill = "Gender") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 5)) +
  guides(x = guide_axis_nested(key = key_range_auto(sep = ":")))
