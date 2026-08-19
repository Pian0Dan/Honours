# Code from Claude
# ---------------------------------------------------------------
# Simulate categorical data (age x gender x tenure) with NO
# association between variables, using stats::rmultinom()
#
# Key idea: independence means the joint probability of any
# combination of levels equals the PRODUCT of the marginal
# probabilities. So we build the joint probability table as an
# outer product of three marginal vectors, flatten it into a
# vector, draw counts for every cell with one call to rmultinom(),
# then expand those counts into individual rows.
# ---------------------------------------------------------------

library(ggplot2)
library(ggmosaic)
# legendry provides guide_axis_nested(), used below to split each
# mosaic's composite x-axis label (e.g. "Male:Rent") into separate,
# bracketed rows -- one row per variable -- so the labels stay
# readable instead of overlapping into illegible text.
library(legendry)
# first_letter_labels()/abbreviate_for_plot() recode categorical
# levels down to their first letter (extending colliding levels by
# an extra letter) for use in plots only -- the underlying analysis
# data keeps full level names.
source("label_helpers.R")

set.seed(13)

# --- 1. Define levels for each variable ---
age_levels <- c("18-34", "35-44", "45-54", "55-64", "65+") # 5 levels
gender_levels <- c("Male", "Female", "Non-binary") # 3 levels
tenure_levels <- c("Rent", "Own", "Other") # 3 levels

# --- 2. Define marginal probability vectors (each must sum to 1) ---
p_age <- c(0.20, 0.20, 0.20, 0.20, 0.20) # uniform across age groups
p_gender <- c(0.45, 0.45, 0.10) # e.g. Male/Female/Non-binary
p_tenure <- c(0.30, 0.40, 0.30) # Rent/Own/Other

stopifnot(
  all.equal(sum(p_age), 1),
  all.equal(sum(p_gender), 1),
  all.equal(sum(p_tenure), 1)
)

# --- 3. Build the joint probability array as an outer product ---
# Because it's a pure outer product, P(age,gender,tenure) =
# P(age) * P(gender) * P(tenure) for every cell -> independence
# by construction in the DGP (data-generating process).
joint_array <- outer(outer(p_age, p_gender), p_tenure)
dim(joint_array) <- c(
  length(age_levels),
  length(gender_levels),
  length(tenure_levels)
)
dimnames(joint_array) <- list(
  age = age_levels,
  gender = gender_levels,
  tenure = tenure_levels
)

# Flatten to a probability vector for rmultinom().
# as.vector() on a 3-D array varies dim 1 (age) fastest, then dim 2
# (gender), then dim 3 (tenure) -- expand.grid() below uses the
# same ordering, so the two align exactly.
prob_vec <- as.vector(joint_array)
stopifnot(all.equal(sum(prob_vec), 1))

# --- 4. Draw cell counts with a single multinomial draw ---
n_total <- 100000 # total number of simulated records
cell_counts <- rmultinom(n = 1, size = n_total, prob = prob_vec)[, 1]

# --- 5. Expand cell counts into an individual-level data frame ---
combos <- expand.grid(
  age = age_levels,
  gender = gender_levels,
  tenure = tenure_levels
)
combos$count <- cell_counts

sim_data <- combos[
  rep(seq_len(nrow(combos)), combos$count),
  c("age", "gender", "tenure")
]
rownames(sim_data) <- NULL

# Convert to factors with the intended level order
sim_data$age <- factor(sim_data$age, levels = age_levels)
sim_data$gender <- factor(sim_data$gender, levels = gender_levels)
sim_data$tenure <- factor(sim_data$tenure, levels = tenure_levels)

# Add a random binary variable, independent of every other column
# (a simple Bernoulli(0.5) draw per row).
sim_data$Latent <- rbinom(nrow(sim_data), 1, 0.5)

# --- 6. Quick checks ---
str(sim_data)
prop.table(table(sim_data$age))
prop.table(table(sim_data$gender))
prop.table(table(sim_data$tenure))

# Chi-square tests should show no significant association
# (occasional significant p-values are expected by chance,
# especially with large n -- this only reflects sampling noise,
# not a real relationship, since independence is baked into prob_vec)
chisq.test(table(sim_data$age, sim_data$gender))
chisq.test(table(sim_data$age, sim_data$tenure))
chisq.test(table(sim_data$gender, sim_data$tenure))

# Faceting by tenure was redundant with including tenure in the x
# product (both encode the same variable), and produced illegible,
# overlapping composite tick labels. guide_axis_nested() instead
# splits the "gender:tenure" leaf label into two clean, bracketed
# rows -- one per variable.

# Recode gender/tenure to first-letter labels for this plot only
# (e.g. Male -> M, Rent -> R, Own -> O, Other -> Ot).
sim_data_plot <- abbreviate_for_plot(sim_data, c("gender", "tenure"))

ggplot(sim_data_plot) +
  geom_mosaic(aes(x = product(age, tenure), fill = gender)) +
  scale_x_productlist(name = NULL) +
  scale_y_productlist(name = "Age Bracket") +
  labs(fill = "Gender") +
  theme_minimal() +
  guides(x = guide_axis_nested(key = key_range_auto(sep = ":")))

# --- Older, Male, Owner Skew w/ positive cov between age and ownership ---

set.seed(13)

# --- 1. Define levels for each variable ---
age_levels <- c("18-34", "35-44", "45-54", "55-64", "65+") # 5 levels
gender_levels <- c("Male", "Female", "Non-binary") # 3 levels
tenure_levels <- c("Rent", "Own", "Other") # 3 levels

# --- 2. Define marginal probability vectors (each must sum to 1) ---
p_age <- c(0.05, 0.10, 0.20, 0.40, 0.25) # Heavy older skew
p_gender <- c(0.60, 0.30, 0.10) # e.g. Male/Female/Non-binary (skew to male)
p_tenure <- c(0.30, 0.60, 0.10) # Rent/Own/Other (owner skew)

stopifnot(
  all.equal(sum(p_age), 1),
  all.equal(sum(p_gender), 1),
  all.equal(sum(p_tenure), 1)
)

# --- 3. Build a joint array with the desired association, then rake it
# so every margin matches p_age / p_gender / p_tenure exactly ---

# 3a. Seed array: start from independence, then multiply in an
# association pattern (older + Male + Own co-occur more than chance).
# `alpha` controls the strength of the association -- 0 reduces to
# independence; larger values create stronger covariance.
seed_array <- outer(outer(p_age, p_gender), p_tenure)
dim(seed_array) <- c(
  length(age_levels),
  length(gender_levels),
  length(tenure_levels)
)

alpha <- 1.2
age_score <- seq(0, 1, length.out = length(age_levels)) # 0 (youngest) to 1 (oldest)
male_idx <- which(gender_levels == "Male")
own_idx <- which(tenure_levels == "Own")

for (i in seq_along(age_levels)) {
  seed_array[i, male_idx, ] <- seed_array[i, male_idx, ] *
    exp(alpha * age_score[i])
  seed_array[i, , own_idx] <- seed_array[i, , own_idx] *
    exp(alpha * age_score[i])
}

# 3b. Iterative proportional fitting (raking): alternately rescale each
# margin to match the target, until convergence. This preserves the
# association pattern above while forcing the marginals to match
# p_age, p_gender, p_tenure exactly.
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
  isTRUE(all.equal(sum(joint_array), 1))
)

dimnames(joint_array) <- list(
  age = age_levels,
  gender = gender_levels,
  tenure = tenure_levels
)

# Flatten to a probability vector for rmultinom().
# as.vector() on a 3-D array varies dim 1 (age) fastest, then dim 2
# (gender), then dim 3 (tenure) -- expand.grid() below uses the
# same ordering, so the two align exactly.
prob_vec <- as.vector(joint_array)
stopifnot(all.equal(sum(prob_vec), 1))

# --- 4. Draw cell counts with a single multinomial draw ---
n_total <- 100000 # total number of simulated records
cell_counts <- rmultinom(n = 1, size = n_total, prob = prob_vec)[, 1]

# --- 5. Expand cell counts into an individual-level data frame ---
combos <- expand.grid(
  age = age_levels,
  gender = gender_levels,
  tenure = tenure_levels
)
combos$count <- cell_counts

sim_data_omo <- combos[
  rep(seq_len(nrow(combos)), combos$count),
  c("age", "gender", "tenure")
]
rownames(sim_data_omo) <- NULL

# Convert to factors with the intended level order
sim_data_omo$age <- factor(sim_data_omo$age, levels = age_levels)
sim_data_omo$gender <- factor(sim_data_omo$gender, levels = gender_levels)
sim_data_omo$tenure <- factor(sim_data_omo$tenure, levels = tenure_levels)

# Add a random binary variable, independent of every other column
# (a simple Bernoulli(0.5) draw per row).
sim_data_omo$Latent <- rbinom(nrow(sim_data_omo), 1, 0.5)

# --- 6. Quick checks ---
str(sim_data_omo)
prop.table(table(sim_data_omo$age))
prop.table(table(sim_data_omo$gender))
prop.table(table(sim_data_omo$tenure))

# Chi-square tests should now show significant association for the
# pairs the seed array targeted (age-gender, age-tenure), since the
# association was baked into joint_array before raking. Raking
# preserves the marginals exactly but can also leak some association
# into gender-tenure, since both were tied to age.
chisq.test(table(sim_data_omo$age, sim_data_omo$gender))
chisq.test(table(sim_data_omo$age, sim_data_omo$tenure))
chisq.test(table(sim_data_omo$gender, sim_data_omo$tenure))

# Faceting by tenure was redundant with including tenure in the x
# product, and produced illegible, overlapping composite tick
# labels. guide_axis_nested() instead splits the "gender:tenure"
# leaf label into two clean, bracketed rows -- one per variable.

# Recode gender/tenure to first-letter labels for these plots only.
sim_data_omo_plot <- abbreviate_for_plot(sim_data_omo, c("gender", "tenure"))

ggplot(sim_data_omo_plot) +
  geom_mosaic(aes(x = product(age, tenure), fill = gender)) +
  scale_x_productlist(name = NULL) +
  scale_y_productlist(name = "Age Bracket") +
  labs(fill = "Gender") +
  theme_minimal() +
  guides(x = guide_axis_nested(key = key_range_auto(sep = ":")))

# Check without gender: only gender is on the x-axis, so its labels
# are already legible on their own (no nested guide needed). Axis
# titles below were previously mislabeled (leftover from copying the
# plot above) -- x actually shows gender, y actually shows tenure.
# Note: labs(x=, y=) does not override geom_mosaic's product-based
# axis titles, so scale_x_productlist()/scale_y_productlist() must
# be used instead.
ggplot(sim_data_omo_plot) +
  geom_mosaic(aes(x = product(gender), fill = tenure)) +
  scale_x_productlist(name = "Gender") +
  scale_y_productlist(name = "Tenure") +
  labs(fill = "Tenure") +
  theme_minimal()


# --- Young, Renter Skew w/ negative cov between age and renting and positive cov between non-binary and renting ---

set.seed(13)

# --- 1. Define levels for each variable ---
age_levels <- c("18-34", "35-44", "45-54", "55-64", "65+") # 5 levels
gender_levels <- c("Male", "Female", "Non-binary") # 3 levels
tenure_levels <- c("Rent", "Own", "Other") # 3 levels

# --- 2. Define marginal probability vectors (each must sum to 1) ---
p_age <- c(0.30, 0.25, 0.20, 0.15, 0.10) # Heavy older skew
p_gender <- c(0.45, 0.45, 0.10) # e.g. Male/Female/Non-binary (skew to Non-binary)
p_tenure <- c(0.60, 0.30, 0.10) # Rent/Own/Other (renter skew)

stopifnot(
  all.equal(sum(p_age), 1),
  all.equal(sum(p_gender), 1),
  all.equal(sum(p_tenure), 1)
)

# --- 3. Build a joint array with the desired association, then rake it
# so every margin matches p_age / p_gender / p_tenure exactly ---

# 3a. Seed array: start from independence, then multiply in an
# association pattern with:
#  - NEGATIVE covariance between age and renting (younger people rent
#    more than chance; older people rent less than chance)
#  - POSITIVE covariance between "Non-binary" gender and renting (renters
#    skew toward the "Non-binary" gender category)
# `alpha` controls the strength of each association -- 0 reduces to
# independence; larger values create stronger covariance.
seed_array <- outer(outer(p_age, p_gender), p_tenure)
dim(seed_array) <- c(
  length(age_levels),
  length(gender_levels),
  length(tenure_levels)
)

alpha_age_rent <- 1.2
alpha_gender_rent <- 1.2
age_score <- seq(0, 1, length.out = length(age_levels)) # 0 (youngest) to 1 (oldest)
nonbinary_idx <- which(gender_levels == "Non-binary")
rent_idx <- which(tenure_levels == "Rent")

for (i in seq_along(age_levels)) {
  # Older ages rent less -> negative covariance between age and renting
  seed_array[i, , rent_idx] <- seed_array[i, , rent_idx] *
    exp(-alpha_age_rent * age_score[i])
}

# "Non-binary" gender rents more -> positive covariance between
# Non-binary and renting
seed_array[, nonbinary_idx, rent_idx] <- seed_array[, nonbinary_idx, rent_idx] *
  exp(alpha_gender_rent)

# 3b. Iterative proportional fitting (raking): alternately rescale each
# margin to match the target, until convergence. This preserves the
# association pattern above while forcing the marginals to match
# p_age, p_gender, p_tenure exactly.
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
  isTRUE(all.equal(sum(joint_array), 1))
)

dimnames(joint_array) <- list(
  age = age_levels,
  gender = gender_levels,
  tenure = tenure_levels
)

# Flatten to a probability vector for rmultinom().
# as.vector() on a 3-D array varies dim 1 (age) fastest, then dim 2
# (gender), then dim 3 (tenure) -- expand.grid() below uses the
# same ordering, so the two align exactly.
prob_vec <- as.vector(joint_array)
stopifnot(all.equal(sum(prob_vec), 1))

# --- 4. Draw cell counts with a single multinomial draw ---
n_total <- 100000 # total number of simulated records
cell_counts <- rmultinom(n = 1, size = n_total, prob = prob_vec)[, 1]

# --- 5. Expand cell counts into an individual-level data frame ---
combos <- expand.grid(
  age = age_levels,
  gender = gender_levels,
  tenure = tenure_levels
)
combos$count <- cell_counts

sim_data_yor <- combos[
  rep(seq_len(nrow(combos)), combos$count),
  c("age", "gender", "tenure")
]
rownames(sim_data_yor) <- NULL

# Convert to factors with the intended level order
sim_data_yor$age <- factor(sim_data_yor$age, levels = age_levels)
sim_data_yor$gender <- factor(sim_data_yor$gender, levels = gender_levels)
sim_data_yor$tenure <- factor(sim_data_yor$tenure, levels = tenure_levels)

# Add a random binary variable, independent of every other column
# (a simple Bernoulli(0.5) draw per row).
sim_data_yor$Latent <- rbinom(nrow(sim_data_yor), 1, 0.5)

# --- 6. Quick checks ---
str(sim_data_yor)
prop.table(table(sim_data_yor$age))
prop.table(table(sim_data_yor$gender))
prop.table(table(sim_data_yor$tenure))

# Chi-square tests should now show significant association for the
# pairs the seed array targeted directly (age-tenure: negative, via
# renting more often at younger ages; gender-tenure: positive, via
# "Non-binary" renting more often). Raking preserves the marginals exactly
# but can also leak some association into age-gender, since both are
# tied to tenure.
chisq.test(table(sim_data_yor$age, sim_data_yor$gender))
chisq.test(table(sim_data_yor$age, sim_data_yor$tenure))
chisq.test(table(sim_data_yor$gender, sim_data_yor$tenure))

# guide_axis_nested() splits the "gender:tenure" leaf label into two
# clean, bracketed rows -- one per variable -- instead of one
# overlapping composite label.

# Recode gender/tenure to first-letter labels for these plots only.
sim_data_yor_plot <- abbreviate_for_plot(sim_data_yor, c("gender", "tenure"))

ggplot(sim_data_yor_plot) +
  geom_mosaic(aes(x = product(age, tenure), fill = gender)) +
  scale_x_productlist(name = NULL) +
  scale_y_productlist(name = "Age Bracket") +
  labs(fill = "Gender") +
  theme_minimal() +
  guides(x = guide_axis_nested(key = key_range_auto(sep = ":")))

# Check without gender: only gender is on the x-axis, so its labels
# are already legible on their own (no nested guide needed). Axis
# titles below were previously mislabeled (leftover from copying the
# plot above) -- x actually shows gender, y actually shows tenure.
ggplot(sim_data_yor_plot) +
  geom_mosaic(aes(x = product(gender), fill = tenure)) +
  scale_x_productlist(name = "Gender") +
  scale_y_productlist(name = "Tenure") +
  labs(fill = "Tenure") +
  theme_minimal()
