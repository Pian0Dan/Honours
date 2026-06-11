library(dplyr)
library(purrr)
library(tidyr)
library(MASS)

# Generate correlation matrices
independent_cor <- matrix(
  c(
    1.0,
    0.0,
    0.0,
    0.0,
    1.0,
    0.0,
    0.0,
    0.0,
    1.0
  ),
  nrow = 3,
  byrow = TRUE
)

weak_cor <- matrix(
  c(
    1.0,
    0.0,
    0.2,
    0.0,
    1.0,
    0.1,
    0.2,
    0.1,
    1.0
  ),
  nrow = 3,
  byrow = TRUE
)

moderate_cor <- matrix(
  c(
    1.0,
    0.0,
    0.4,
    0.0,
    1.0,
    0.2,
    0.4,
    0.2,
    1.0
  ),
  nrow = 3,
  byrow = TRUE
)

strong_cor <- matrix(
  c(
    1.0,
    0.0,
    0.7,
    0.0,
    1.0,
    0.4,
    0.7,
    0.4,
    1.0
  ),
  nrow = 3,
  byrow = TRUE
)

extreme_cor <- matrix(
  c(
    1.0,
    0.0,
    0.9,
    0.0,
    1.0,
    0.7,
    0.9,
    0.7,
    1.0
  ),
  nrow = 3,
  byrow = TRUE
)

negative_cor <- matrix(
  c(
    1.0,
    0.0,
    -0.5,
    0.0,
    1.0,
    0.0,
    -0.5,
    0.0,
    1.0
  ),
  nrow = 3,
  byrow = TRUE
)

# Put correlation matrices into a named list
correlation_scenarios <- list(
  independent = independent_cor,
  weak = weak_cor,
  moderate = moderate_cor,
  strong = strong_cor
)

# Define representation scenarios

representation_scenarios <- list(
  balanced = list(
    age_group = c(
      "18-29" = 0.25,
      "30-44" = 0.25,
      "45-59" = 0.25,
      "60+" = 0.25
    ),

    gender = c(
      Female = 0.50,
      Male = 0.48,
      Non_binary = 0.02
    ),

    tenure = c(
      Homeowner = 0.33,
      Renter = 0.33,
      Other = 0.34
    )
  ),

  mildly_skewed = list(
    age_group = c(
      "18-29" = 0.35,
      "30-44" = 0.30,
      "45-59" = 0.20,
      "60+" = 0.15
    ),

    gender = c(
      Female = 0.55,
      Male = 0.43,
      Non_binary = 0.02
    ),

    tenure = c(
      Homeowner = 0.40,
      Renter = 0.45,
      Other = 0.15
    )
  ),

  highly_skewed = list(
    age_group = c(
      "18-29" = 0.50,
      "30-44" = 0.30,
      "45-59" = 0.15,
      "60+" = 0.05
    ),

    gender = c(
      Female = 0.65,
      Male = 0.33,
      Non_binary = 0.02
    ),

    tenure = c(
      Homeowner = 0.15,
      Renter = 0.75,
      Other = 0.10
    )
  ),

  extreme_underrepresentation = list(
    age_group = c(
      "18-29" = 0.70,
      "30-44" = 0.20,
      "45-59" = 0.08,
      "60+" = 0.02
    ),

    gender = c(
      Female = 0.80,
      Male = 0.19,
      Non_binary = 0.01
    ),

    tenure = c(
      Homeowner = 0.05,
      Renter = 0.90,
      Other = 0.05
    )
  )
)

# Create all combinations of representation × correlation
scenario_grid <- expand_grid(
  representation_name = names(representation_scenarios),
  correlation_name = names(correlation_scenarios)
) %>%
  mutate(
    probs = map(representation_name, ~ representation_scenarios[[.x]]),
    cor_mat = map(correlation_name, ~ correlation_scenarios[[.x]]),
    scenario_name = paste(representation_name, correlation_name, sep = "_")
  )

make_category <- function(score, probs) {
  cut(
    pnorm(score),
    breaks = c(0, cumsum(probs)),
    labels = names(probs),
    include.lowest = TRUE
  )
}

# Define population generation function

generate_population <- function(
  N,
  scenario_name,
  probs,
  cor_mat,
  seed = 123
) {
  set.seed(seed)

  latent <- MASS::mvrnorm(
    n = N,
    mu = rep(0, length(probs)),
    Sigma = cor_mat
  )

  latent <- as.data.frame(latent)
  names(latent) <- names(probs)

  tibble(
    scenario = scenario_name,
    id = 1:N,

    age_group = make_category(
      latent$age_group,
      probs$age_group
    ),

    gender = make_category(
      latent$gender,
      probs$gender
    ),

    tenure = make_category(
      latent$tenure,
      probs$tenure
    )
  )
}

# Generate populations as a list of data frames
populations <- pmap(
  list(
    N = 10000,
    scenario_name = scenario_grid$scenario_name,
    probs = scenario_grid$probs,
    cor_mat = scenario_grid$cor_mat
  ),
  generate_population
)

# Give each population a useful name
names(populations) <- scenario_grid$scenario_name

# View output populations
summary(populations)

populations$balanced_independent
populations$extreme_underrepresentation_strong
