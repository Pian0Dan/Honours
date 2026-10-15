# ---------------------------------------------------------------
# Simulates two age x tenure populations via stats::rmultinom(): an
# independent case, and a covaried case built by seeding an
# association then raking the marginals back to match.
# ---------------------------------------------------------------

library(ggplot2)
library(ggmosaic2)

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

# as.vector() varies age fastest then tenure -- expand.grid() below
# uses the same ordering, so the two line up.
prob_vec <- as.vector(joint_array)
stopifnot(all.equal(sum(prob_vec), 1))

n_total <- 100000
cell_counts <- rmultinom(n = 1, size = n_total, prob = prob_vec)[, 1]

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

# Assign IDs now so downstream scripts can track the same individual
# through sampling and response simulation.
sim_data2$ID <- seq_len(nrow(sim_data2))

# Convert to factors with the intended level order
sim_data2$age <- factor(sim_data2$age, levels = age_levels)
sim_data2$tenure <- factor(sim_data2$tenure, levels = tenure_levels)

# Unused placeholder variable, kept around from an earlier draft.
sim_data2$Latent <- rbinom(nrow(sim_data2), 1, 0.5)

str(sim_data2)
prop.table(table(sim_data2$age))
prop.table(table(sim_data2$tenure))

# Should come back non-significant -- independence is baked into
# prob_vec, so any significant p-value here is just sampling noise.
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
  labs(fill = "Tenure") +
  theme_minimal()

# --- Covaried case: younger people rent more than chance, older
# people rent less ---

set.seed(13)

age_levels <- c("18-34", "35-44", "45-54", "55-64", "65+") # 5 levels
tenure_levels <- c("Rent", "Own", "Other") # 3 levels

p_age <- c(0.15, 0.20, 0.25, 0.25, 0.15) # uniform across age groups
p_tenure <- c(0.30, 0.50, 0.20) # Rent/Own/Other

stopifnot(
  all.equal(sum(p_age), 1),
  all.equal(sum(p_tenure), 1)
)

# Start from independence, then scale down renting by age (alpha
# controls the strength -- 0 reduces to independence), before raking
# back to the target marginals below.
seed_array <- outer(p_age, p_tenure)
dim(seed_array) <- c(length(age_levels), length(tenure_levels))

alpha <- 1.2
age_score <- seq(0, 1, length.out = length(age_levels)) # 0 (youngest) to 1 (oldest)
rent_idx <- which(tenure_levels == "Rent")

for (i in seq_along(age_levels)) {
  seed_array[i, rent_idx] <- seed_array[i, rent_idx] *
    exp(-alpha * age_score[i])
}

# Raking: alternately rescale rows then columns to the target
# marginals until convergence, keeping the association seeded above.
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

prob_vec <- as.vector(joint_array)
stopifnot(all.equal(sum(prob_vec), 1))

n_total <- 100000
cell_counts <- rmultinom(n = 1, size = n_total, prob = prob_vec)[, 1]

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

sim_data2_cov$ID <- seq_len(nrow(sim_data2_cov))

# Convert to factors with the intended level order
sim_data2_cov$age <- factor(sim_data2_cov$age, levels = age_levels)
sim_data2_cov$tenure <- factor(sim_data2_cov$tenure, levels = tenure_levels)

sim_data2_cov$Latent <- rbinom(nrow(sim_data2_cov), 1, 0.5)

str(sim_data2_cov)
prop.table(table(sim_data2_cov$age))
prop.table(table(sim_data2_cov$tenure))

# Should now come back significant, since raking preserves the
# association seeded above while matching the marginals exactly.
chisq.test(table(sim_data2_cov$age, sim_data2_cov$tenure))

pop2_cov_plot <-
  ggplot(sim_data2_cov) +
  geom_mosaic(aes(x = product(age), fill = tenure)) +
  scale_x_productlist(name = "Age Bracket") +
  scale_y_productlist(name = "Tenure") +
  labs(fill = "Tenure") +
  theme_minimal()
