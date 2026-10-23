library(tibble)
library(ggmosaic2)
library(ggplot2)

# Built deterministically (no shuffling/seed) so the association is
# exact and reproducible: 25 Apple-lovers and 25 Orange-lovers, each
# split 80/20 in opposite directions on ice cream flavour, giving a
# clear association for illustration (chisq.test(): X-squared =
# 15.68, df = 1, p < 0.001).
fruit <- c(rep("Apple", 25), rep("Orange", 25))
ice_cream <- c(
  rep("Vanilla", 20),
  rep("Strawberry", 5), # Apple: 80% Vanilla
  rep("Strawberry", 20),
  rep("Vanilla", 5) # Orange: 80% Strawberry
)

combos <- tibble(fruit, ice_cream)

combos

ice_cream_colours <- c(
  "Vanilla" = "#E8C547", # Deeper gold, enough contrast against white
  "Strawberry" = "#E8537A" # Deeper pink/red
)

fruit_and_cream <- combos |>
  ggplot() +
  geom_mosaic(aes(x = product(fruit), fill = ice_cream), colour = "white") +
  scale_x_productlist(name = "Favourite Fruit") +
  scale_y_productlist(name = "Favourite Ice Cream") +
  scale_fill_manual(
    values = ice_cream_colours,
    guide = guide_legend(reverse = TRUE)
  ) +
  labs(
    fill = "Favourite Ice Cream"
  ) +
  # Matches the theme_minimal() used by every other mosaic plot in
  # this thesis, rather than a one-off custom background.
  theme_minimal() +
  theme(aspect.ratio = 1)
