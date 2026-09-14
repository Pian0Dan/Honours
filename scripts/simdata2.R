# ---------------------------------------------------------------
# Simulate categorical data (age x tenure) with two scenarios:
#  - an independent case (no association between age and tenure)
#  - a covaried case (age and tenure are associated)
# using stats::rmultinom(), following the same joint-probability-
# array + raking approach used in simdata.R / simdata5.R.
# ---------------------------------------------------------------

library(ggplot2)
library(ggmosaic)

set.seed(13)

# --- 1. Define levels for each variable ---
age_levels <- c("18-34", "35-44", "45-54", "55-64", "65+") # 5 levels
tenure_levels <- c("Rent", "Own", "Other") # 3 levels

# --- 2. Define marginal probability vectors (each must sum to 1) ---
p_age <- c(0.15, 0.20, 0.25, 0.25, 0.15) # uniform across age groups
p_tenure <- c(0.30, 0.50, 0.20) # Rent/Own/Other

stopifnot(
  all.equal(sum(p_age), 1),
  all.equal(sum(p_tenure), 1)
)

# --- 3. Build the joint probability array as an outer product ---
# Because it's a pure outer product, P(age,tenure) = P(age) *
# P(tenure) for every cell -> independence by construction in the
# DGP (data-generating process).
joint_array <- outer(p_age, p_tenure)
dim(joint_array) <- c(length(age_levels), length(tenure_levels))
dimnames(joint_array) <- list(
  age = age_levels,
  tenure = tenure_levels
)

# Flatten to a probability vector for rmultinom().
# as.vector() on a 2-D array varies dim 1 (age) fastest, then dim 2
# (tenure) -- expand.grid() below uses the same ordering, so the two
# align exactly.
prob_vec <- as.vector(joint_array)
stopifnot(all.equal(sum(prob_vec), 1))

# --- 4. Draw cell counts with a single multinomial draw ---
n_total <- 100000 # total number of simulated records
cell_counts <- rmultinom(n = 1, size = n_total, prob = prob_vec)[, 1]

# --- 5. Expand cell counts into an individual-level data frame ---
combos <- expand.grid(
  age = age_levels,
  tenure = tenure_levels
)
combos$count <- cell_counts

sim_data2 <- combos[
  rep(seq_len(nrow(combos)), combos$count),
  c("age", "tenure")
]
rownames(sim_data2) <- NULL

# Assign each population member a unique ID as soon as they are
# generated, so downstream scripts (e.g. simsamp2.R) can track the
# same individual through sampling and response simulation, rather
# than re-deriving an ID from row position later on.
sim_data2$ID <- seq_len(nrow(sim_data2))

# Convert to factors with the intended level order
sim_data2$age <- factor(sim_data2$age, levels = age_levels)
sim_data2$tenure <- factor(sim_data2$tenure, levels = tenure_levels)

# Add a random binary variable, independent of every other column
# (a simple Bernoulli(0.5) draw per row). Not included in any plot.
sim_data2$Latent <- rbinom(nrow(sim_data2), 1, 0.5)

# --- 6. Quick checks ---
str(sim_data2)
prop.table(table(sim_data2$age))
prop.table(table(sim_data2$tenure))

# Chi-square test should show no significant association
# (occasional significant p-values are expected by chance,
# especially with large n -- this only reflects sampling noise,
# not a real relationship, since independence is baked into prob_vec)
chisq.test(table(sim_data2$age, sim_data2$tenure))

# ggplot(sim_data2) +
#geom_mosaic(aes(x = product(age, tenure))) +
#scale_x_productlist(name = "Tenure") +
#scale_y_productlist(name = "Age Bracket") +
#theme_minimal()

pop2_ind_plot <-
  ggplot(sim_data2) +
  geom_mosaic(aes(x = product(age), fill = tenure)) +
  scale_x_productlist(name = "Age Bracket") +
  scale_y_productlist(name = "Tenure") +
  theme_minimal()

# --- Covaried case: negative covariance between age and renting ---
# (younger people rent more than chance; older people rent less
# than chance)

set.seed(13)

# --- 1. Define levels for each variable ---
age_levels <- c("18-34", "35-44", "45-54", "55-64", "65+") # 5 levels
tenure_levels <- c("Rent", "Own", "Other") # 3 levels

# --- 2. Define marginal probability vectors (each must sum to 1) ---
p_age <- c(0.15, 0.20, 0.25, 0.25, 0.15) # uniform across age groups
p_tenure <- c(0.30, 0.50, 0.20) # Rent/Own/Other

stopifnot(
  all.equal(sum(p_age), 1),
  all.equal(sum(p_tenure), 1)
)

# --- 3. Build a joint array with the desired association, then rake
# it so every margin matches p_age / p_tenure exactly ---

# 3a. Seed array: start from independence, then multiply in an
# association pattern -- older ages rent less than chance, younger
# ages rent more than chance. `alpha` controls the strength of the
# association -- 0 reduces to independence; larger values create
# stronger covariance.
seed_array <- outer(p_age, p_tenure)
dim(seed_array) <- c(length(age_levels), length(tenure_levels))

alpha <- 1.2
age_score <- seq(0, 1, length.out = length(age_levels)) # 0 (youngest) to 1 (oldest)
rent_idx <- which(tenure_levels == "Rent")

for (i in seq_along(age_levels)) {
  # Older ages rent less -> negative covariance between age and renting
  seed_array[i, rent_idx] <- seed_array[i, rent_idx] *
    exp(-alpha * age_score[i])
}

# 3b. Iterative proportional fitting (raking): alternately rescale
# each margin to match the target, until convergence. This preserves
# the association pattern above while forcing the marginals to
# match p_age, p_tenure exactly.
joint_array <- seed_array
for (iter in seq_len(100)) {
  joint_array <- sweep(joint_array, 1, p_age / apply(joint_array, 1, sum), "*")
  joint_array <- sweep(
    joint_array,
    2,
    p_tenure / apply(joint_array, 2, sum),
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
    p_tenure,
    tolerance = 1e-8,
    check.attributes = FALSE
  )),
  isTRUE(all.equal(sum(joint_array), 1))
)

dimnames(joint_array) <- list(
  age = age_levels,
  tenure = tenure_levels
)

# Flatten to a probability vector for rmultinom().
prob_vec <- as.vector(joint_array)
stopifnot(all.equal(sum(prob_vec), 1))

# --- 4. Draw cell counts with a single multinomial draw ---
n_total <- 100000 # total number of simulated records
cell_counts <- rmultinom(n = 1, size = n_total, prob = prob_vec)[, 1]

# --- 5. Expand cell counts into an individual-level data frame ---
combos <- expand.grid(
  age = age_levels,
  tenure = tenure_levels
)
combos$count <- cell_counts

sim_data2_cov <- combos[
  rep(seq_len(nrow(combos)), combos$count),
  c("age", "tenure")
]
rownames(sim_data2_cov) <- NULL

# Assign each population member a unique ID as soon as they are
# generated (see the note on sim_data2 above).
sim_data2_cov$ID <- seq_len(nrow(sim_data2_cov))

# Convert to factors with the intended level order
sim_data2_cov$age <- factor(sim_data2_cov$age, levels = age_levels)
sim_data2_cov$tenure <- factor(sim_data2_cov$tenure, levels = tenure_levels)

# Add a random binary variable, independent of every other column
# (a simple Bernoulli(0.5) draw per row). Not included in any plot.
sim_data2_cov$Latent <- rbinom(nrow(sim_data2_cov), 1, 0.5)

# --- 6. Quick checks ---
str(sim_data2_cov)
prop.table(table(sim_data2_cov$age))
prop.table(table(sim_data2_cov$tenure))

# Chi-square test should now show significant association between
# age and tenure, since the association was baked into joint_array
# before raking. Raking preserves the marginals exactly.
chisq.test(table(sim_data2_cov$age, sim_data2_cov$tenure))

pop2_cov_plot <-
  ggplot(sim_data2_cov) +
  geom_mosaic(aes(x = product(age), fill = tenure)) +
  scale_x_productlist(name = "Age Bracket") +
  scale_y_productlist(name = "Tenure") +
  theme_minimal()
