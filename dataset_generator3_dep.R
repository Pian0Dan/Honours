library(dplyr)
library(purrr)
library(tidyr)
library(MASS)
library(mvtnorm)

# 2. Setup population parameters
set.seed(13)
n_population <- 100000

# 3. Define the covariance structure (3 variables)
sigma_matrix <- matrix(
  c(
    1.0,
    0.0,
    -0.5,
    0.6,
    1.0,
    0.1,
    -0.5,
    0.1,
    1.0
  ),
  nrow = 3,
  ncol = 3,
  byrow = TRUE
)

# 4. Generate the continuous population foundation
latent_population <- rmvnorm(
  n = n_population,
  mean = c(0, 0, 0),
  sigma = sigma_matrix
)

# 5. GENERATE THE ACTUAL CATEGORICAL POPULATION DATA FRAME
categorical_population <- data.frame(
  # Column 1: Age
  Age = cut(
    latent_population[, 1],
    breaks = c(-Inf, qnorm(0.20), qnorm(0.40), qnorm(0.60), qnorm(0.8), Inf),
    labels = c("18-34", "35-44", "45-54", "55-64", "65+")
  ),

  # Column 2: Gender
  Gender = cut(
    latent_population[, 2],
    breaks = c(-Inf, qnorm(0.333), qnorm(0.667), Inf),
    labels = c("Male", "Female", "Non-binary")
  ),

  # Column 3: Tenure
  Tenure = cut(
    latent_population[, 3],
    breaks = c(-Inf, qnorm(0.333), qnorm(0.667), Inf),
    labels = c("Homeowner", "Renter", "Other")
  )
)

# 6. View the generated population
print(head(categorical_population, 10))

# 7. Verify proportions

categorical_population |>
  pivot_longer(everything(), names_to = "trait", values_to = "level") |>
  count(trait, level) |>
  group_by(trait) |>
  mutate(prop = n / sum(n)) |>
  arrange(trait, level)
