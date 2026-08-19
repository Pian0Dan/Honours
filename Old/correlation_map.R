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

cor_matrices <- list(
  independent = independent_cor,
  weak = weak_cor,
  moderate = moderate_cor,
  strong = strong_cor
)
