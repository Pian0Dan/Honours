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
library(ggmosaic2)
# legendry provides guide_axis_nested(), used below to split each
# mosaic's composite x-axis label (e.g. "Male:Rent") into separate,
# bracketed rows -- one row per variable -- so the labels stay
# readable instead of overlapping into illegible text.
library(legendry)
# first_letter_labels()/abbreviate_for_plot() recode categorical
# levels down to their first letter (extending colliding levels by
# an extra letter) for use in plots only -- the underlying analysis
# data keeps full level names.
source("scripts/label_helpers.R")

set.seed(13)

# --- 1. Define levels for each variable ---
age_levels <- c("18-34", "35-44", "45-54", "55-64", "65+") # 5 levels
gender_levels <- c("Male", "Female", "Non-binary") # 3 levels
tenure_levels <- c("Rent", "Own", "Other") # 3 levels

# --- 2. Define marginal probability vectors (each must sum to 1) ---
p_age <- c(0.15, 0.175, 0.225, 0.25, 0.20) # uniform across age groups
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

# Mosaic plot with age on the y-axis, tenure as the only labelled
# x-axis variable, and gender used purely as fill colour. ggmosaic
# still internally folds gender's proportions into the x-axis
# geometry (an unavoidable quirk once 3+ variables are combined), so
# rather than let its automatic tick labels expose that as
# "gender:tenure" text, mosaic_axis_breaks() computes the midpoint
# of each tenure block directly from the rendered layer, and those
# midpoints are supplied as explicit breaks/labels -- showing only
# "Rent"/"Own"/"Other" on the x-axis, with gender visible solely via
# fill colour and the legend.
sim_data_base <- ggplot(sim_data) +
  geom_mosaic(aes(x = product(age, tenure), fill = gender))
tenure_breaks <- mosaic_axis_breaks(sim_data_base, "tenure")

sim_data_base +
  scale_x_productlist(
    name = "Tenure",
    breaks = tenure_breaks$x,
    labels = tenure_breaks$label
  ) +
  scale_y_productlist(name = "Age Bracket") +
  labs(fill = "Gender") +
  theme_minimal()


# --- Old, Homeowner Skew w/ positive cov between age and homeownership and positive cov between non-binary and renting ---

set.seed(13)

# --- 1. Define levels for each variable ---
age_levels <- c("18-34", "35-44", "45-54", "55-64", "65+") # 5 levels
gender_levels <- c("Male", "Female", "Non-binary") # 3 levels
tenure_levels <- c("Rent", "Own", "Other") # 3 levels

# --- 2. Define marginal probability vectors (each must sum to 1) ---
p_age <- c(0.10, 0.15, 0.20, 0.25, 0.30) # Heavy older skew
p_gender <- c(0.45, 0.45, 0.10) # e.g. Male/Female/Non-binary (skew to Non-binary)
p_tenure <- c(0.30, 0.60, 0.10) # Rent/Own/Other (homeowner skew)

stopifnot(
  all.equal(sum(p_age), 1),
  all.equal(sum(p_gender), 1),
  all.equal(sum(p_tenure), 1)
)

# --- 3. Build a joint array with the desired association, then rake it
# so every margin matches p_age / p_gender / p_tenure exactly ---

# 3a. Seed array: start from independence, then multiply in an
# association pattern with:
#  - POSITIVE covariance between age and homeownership (older people
#    own more than chance; younger people own less than chance)
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

alpha_age_own <- 1.2
alpha_gender_rent <- 1.2
age_score <- seq(0, 1, length.out = length(age_levels)) # 0 (youngest) to 1 (oldest)
nonbinary_idx <- which(gender_levels == "Non-binary")
own_idx <- which(tenure_levels == "Own")
rent_idx <- which(tenure_levels == "Rent")

for (i in seq_along(age_levels)) {
  # Older ages own more -> positive covariance between age and
  # homeownership
  seed_array[i, , own_idx] <- seed_array[i, , own_idx] *
    exp(alpha_age_own * age_score[i])
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

sim_data_oho <- combos[
  rep(seq_len(nrow(combos)), combos$count),
  c("age", "gender", "tenure")
]
rownames(sim_data_oho) <- NULL

# Convert to factors with the intended level order
sim_data_oho$age <- factor(sim_data_oho$age, levels = age_levels)
sim_data_oho$gender <- factor(sim_data_oho$gender, levels = gender_levels)
sim_data_oho$tenure <- factor(sim_data_oho$tenure, levels = tenure_levels)

# Add a random binary variable, independent of every other column
# (a simple Bernoulli(0.5) draw per row).
sim_data_oho$Latent <- rbinom(nrow(sim_data_oho), 1, 0.5)

# --- 6. Quick checks ---
str(sim_data_oho)
prop.table(table(sim_data_oho$age))
prop.table(table(sim_data_oho$gender))
prop.table(table(sim_data_oho$tenure))

# Chi-square tests should now show significant association for the
# pairs the seed array targeted directly (age-tenure: positive, via
# homeownership increasing with age; gender-tenure: positive, via
# "Non-binary" renting more often). Raking preserves the marginals exactly
# but can also leak some association into age-gender, since both are
# tied to tenure.
chisq.test(table(sim_data_oho$age, sim_data_oho$gender))
chisq.test(table(sim_data_oho$age, sim_data_oho$tenure))
chisq.test(table(sim_data_oho$gender, sim_data_oho$tenure))

# Mosaic plot with age on the y-axis, tenure as the only labelled
# x-axis variable, and gender used purely as fill colour (see note
# on the sim_data plot above for why mosaic_axis_breaks() is used
# instead of ggmosaic's automatic tick labels).
sim_data_oho_base <- ggplot(sim_data_oho) +
  geom_mosaic(aes(x = product(age, tenure), fill = gender))
tenure_breaks_oho <- mosaic_axis_breaks(sim_data_oho_base, "tenure")

sim_data_oho_base +
  scale_x_productlist(
    name = "Tenure",
    breaks = tenure_breaks_oho$x,
    labels = tenure_breaks_oho$label
  ) +
  scale_y_productlist(name = "Age Bracket") +
  labs(fill = "Gender") +
  theme_minimal()
