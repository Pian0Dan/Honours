library(dplyr)
library(tidyr)
library(purrr)

# Function to generate a synthetic population based on specified demographic distributions

generate_population3 <- function(
  N,
  scenario_name,
  age_shape,
  gender_probs,
  tenure_probs
) {
  age_raw <- rbeta(N, shape1 = age_shape[1], shape2 = age_shape[2])

  tibble(
    scenario = scenario_name,
    id = 1:N,

    age = round(18 + age_raw * (90 - 18)),

    age_group = cut(
      age,
      breaks = c(17, 29, 44, 59, 74, 90),
      labels = c("18-29", "30-44", "45-59", "60-74", "75+"),
      include.lowest = TRUE
    ),

    gender = sample(
      names(gender_probs),
      size = N,
      replace = TRUE,
      prob = gender_probs
    ),

    tenure = sample(
      names(tenure_probs),
      size = N,
      replace = TRUE,
      prob = tenure_probs
    )
  )
}

scenarios <- list(
  balanced = list(
    age_shape = c(3, 3),
    gender_probs = c(Female = 0.48, Male = 0.48, Non_binary = 0.04),
    tenure_probs = c(Homeowner = 0.35, Renter = 0.45, Other = 0.20)
  ),

  young_renter = list(
    age_shape = c(2, 6),
    gender_probs = c(Female = 0.48, Male = 0.48, Non_binary = 0.04),
    tenure_probs = c(Homeowner = 0.15, Renter = 0.70, Other = 0.15)
  ),

  older_homeowner = list(
    age_shape = c(6, 2),
    gender_probs = c(Female = 0.48, Male = 0.48, Non_binary = 0.04),
    tenure_probs = c(Homeowner = 0.75, Renter = 0.15, Other = 0.10)
  )
)

# Generate the population dataset for all scenarios

set.seed(13)

populations3 <- imap(
  scenarios,
  ~ generate_population3(
    N = 100000,
    scenario_name = .y,
    age_shape = .x$age_shape,
    gender_probs = .x$gender_probs,
    tenure_probs = .x$tenure_probs
  )
)

# Assess representation within the populations

population_summary3 <- map_dfr(
  populations3,
  ~ .x %>%
    pivot_longer(
      cols = c(age_group, gender, tenure),
      names_to = "trait",
      values_to = "category"
    ) %>%
    count(scenario, trait, category) %>%
    group_by(scenario, trait) %>%
    mutate(
      proportion = n / sum(n),
      percentage = round(proportion * 100, 1)
    ) %>%
    ungroup()
)

population_summary3
