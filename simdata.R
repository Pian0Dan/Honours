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

set.seed(13)

# --- 1. Define levels for each variable ---
age_levels <- c("18-34", "35-44", "45-54", "55-64", "65+") # 5 levels
gender_levels <- c("Male", "Female", "Other") # 3 levels
tenure_levels <- c("Rent", "Own", "Other") # 3 levels

# --- 2. Define marginal probability vectors (each must sum to 1) ---
p_age <- c(0.20, 0.20, 0.20, 0.20, 0.20) # uniform across age groups
p_gender <- c(0.45, 0.45, 0.10) # e.g. Male/Female/Other
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

ggplot(sim_data) +
  geom_mosaic(aes(x = product(age, tenure), fill = gender)) +
  facet_grid(~tenure) +
  #labs(title = "Mosaic Plot by var3",
  #     x = "var1", y = "var2") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
