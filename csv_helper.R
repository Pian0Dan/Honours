# Example matrix
sim_cat2 <- matrix(
  c(
    "age",
    "18-34",
    7,
    8,
    "age",
    "35-44",
    0.20 * 50,
    0.20 * 50,
    "age",
    "45-54",
    12,
    13,
    "age",
    "55-64",
    12,
    13,
    "age",
    "65+",
    7,
    8,
    "tenure",
    "Rent",
    0.3 * 50,
    0.3 * 50,
    "tenure",
    "Own",
    0.5 * 50,
    0.5 * 50,
    "tenure",
    "Other",
    0.2 * 50,
    0.2 * 50
  ),
  nrow = 8,
  byrow = TRUE
)

colnames(sim_cat2) <- c("category", "feature", "min", "max")

# Save as CSV
write.csv(sim_cat2, "sim_cat2.csv", row.names = FALSE)

# -------

# Population matrix

sim_data2

write.csv(sim_data2, "sim_data2.csv", row.names = FALSE)
