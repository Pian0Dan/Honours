library(tibble)
library(ggmosaic2)
library(ggplot2)

set.seed(13)

fruit <- c(rep("Apple", 25), rep("Orange", 25))
ice_cream <- c(rep("Vanilla", 25), rep("Strawberry", 25))

combos <- tibble(fruit, ice_cream)

combos$ice_cream <- sample(combos$ice_cream)

combos

ice_cream_colours <- c(
  "Vanilla" = "#FFFDD0", # Cream/pale yellow
  "Strawberry" = "#FFB6C1" # Light pink
)

bg_colour <- "#E0E0E0" # Light grey

fruit_and_cream <- combos |>
  ggplot() +
  geom_mosaic(aes(x = product(fruit), fill = ice_cream)) +
  scale_x_productlist(name = "Favourite Fruit") +
  scale_y_productlist(name = "Favourite Ice Cream") +
  scale_fill_manual(
    values = ice_cream_colours,
    guide = guide_legend(reverse = TRUE)
  ) +
  labs(
    title = "Intersections of Favourite Fruits and Ice Creams",
    fill = "Favourite Ice Cream"
  ) +
  theme(
    # Applies the light grey to the entire canvas
    panel.background = element_rect(fill = bg_colour, colour = NA),
    plot.background = element_rect(fill = bg_colour, colour = NA),
    legend.background = element_rect(fill = bg_colour, colour = NA),
    legend.key = element_rect(fill = bg_colour, colour = NA),

    # Removes the white grid lines
    panel.grid = element_blank(),

    # Make square
    aspect_ratio = 1
  )
