library(dplyr)
library(tidyr)
library(purrr)

# Function to generate a synthetic population based on specified demographic distributions

generate_population5 <- function(
  N,
  scenario_name,
  age_shape,
  gender_probs,
  tenure_probs,
  location_probs,
  education_probs
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
    ),

    location = sample(
      names(location_probs),
      size = N,
      replace = TRUE,
      prob = location_probs
    ),

    education = sample(
      names(education_probs),
      size = N,
      replace = TRUE,
      prob = education_probs
    )
  )
}

scenarios <- list(
  balanced = list(
    age_shape = c(3, 3),
    gender_probs = c(Female = 0.48, Male = 0.48, Non_binary = 0.04),
    tenure_probs = c(Homeowner = 0.35, Renter = 0.45, Other = 0.20),
    location_probs = c(Metro = 0.50, Regional = 0.40, Remote = 0.10),
    education_probs = c(
      High_school = 0.30,
      TAFE = 0.30,
      Bachelor = 0.30,
      Postgraduate = 0.10
    )
  ),

  young_metro_renter = list(
    age_shape = c(2, 6),
    gender_probs = c(Female = 0.48, Male = 0.48, Non_binary = 0.04),
    tenure_probs = c(Homeowner = 0.15, Renter = 0.70, Other = 0.15),
    location_probs = c(Metro = 0.80, Regional = 0.18, Remote = 0.02),
    education_probs = c(
      High_school = 0.20,
      TAFE = 0.25,
      Bachelor = 0.40,
      Postgraduate = 0.15
    )
  ),

  older_homeowner_regional = list(
    age_shape = c(6, 2),
    gender_probs = c(Female = 0.48, Male = 0.48, Non_binary = 0.04),
    tenure_probs = c(Homeowner = 0.75, Renter = 0.15, Other = 0.10),
    location_probs = c(Metro = 0.35, Regional = 0.55, Remote = 0.10),
    education_probs = c(
      High_school = 0.45,
      TAFE = 0.30,
      Bachelor = 0.20,
      Postgraduate = 0.05
    )
  )
)

# Generate the population dataset for all scenarios

set.seed(13)

populations5 <- imap(
  scenarios,
  ~ generate_population5(
    N = 100000,
    scenario_name = .y,
    age_shape = .x$age_shape,
    gender_probs = .x$gender_probs,
    tenure_probs = .x$tenure_probs,
    education_probs = .x$education_probs,
    location_probs = .x$location_probs
  )
)

# Assess representation within the populations

population_summary5 <- map_dfr(
  populations,
  ~ .x %>%
    pivot_longer(
      cols = c(age_group, gender, tenure, education, location),
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

population_summary5
