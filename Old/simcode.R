# Simulation
n <- 1000

probs <- c(
  0.20, # A, X, Yes
  0.05, # B, X, Yes
  0.05, # C, X, Yes
  0.15, # A, Y, Yes
  0.05, # B, Y, Yes
  0.10, # C, Y, Yes
  0.10, # A, X, No
  0.10, # B, X, No
  0.08, # C, X, No
  0.05, # A, Y, No
  0.02, # B, Y, No
  0.05 # C, Y, No
)

no_cov_probs <- c(
  0.20, # A, X, Yes
  0.05, # B, X, Yes
  0.05, # C, X, Yes
  0.2, # A, Y, Yes
  0.05, # B, Y, Yes
  0.05, # C, Y, Yes
  0.20, # A, X, No
  0.05, # B, X, No
  0.05, # C, X, No
  0.2, # A, Y, No
  0.05, # B, Y, No
  0.05 # C, Y, No
) /
  1.2

cells <- expand.grid(
  var1 = c("A", "B", "C"),
  var2 = c("X", "Y"),
  var3 = c("Yes", "No")
)

counts <- rmultinom(1, size = 100000, prob = no_cov_probs)
df <- cells[rep(1:nrow(cells), times = counts), ]

chisq.test(table(df$var1, df$var2))

# Cramér's V
library(vcd)
assocstats(table(df$var1, df$var2))

library(ggmosaic)
library(ggplot2)
ggplot(df) +
  geom_mosaic(aes(x = product(var2, var1), fill = var2)) +
  labs(title = "Mosaic Plot", x = "var1", y = "var2") +
  theme_minimal()

ggplot(df) +
  geom_mosaic(aes(x = product(var3, var2, var1), fill = var3)) +
  labs(title = "Mosaic Plot: var1, var2, var3", x = "var1 × var2", y = "var3") +
  theme_minimal()

ggplot(df) +
  geom_mosaic(aes(x = product(var2, var1), fill = var2)) +
  facet_grid(~var3) +
  labs(title = "Mosaic Plot by var3", x = "var1", y = "var2") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
