# ---------------------------------------------------------------
# Compare the simdata5.R population-level mosaic plots against the
# simsamp5.R survey-sample-level mosaic plots, side by side, for
# both the independent (sim_data5) and covaried (sim_data5_oeh)
# scenarios. This checks visually whether the 10% random sampling
# step in simsamp5.R preserved each population's full five-variable
# (age x gender x tenure x education x location) structure.
# ---------------------------------------------------------------

source("simsamp5.R") # also sources simdata5.R

library(patchwork)

# simdata5.R / simsamp5.R only print their final, fully-labelled
# mosaic plots at top level -- the labelled version itself isn't
# saved to a variable, just the "base" geom_mosaic() layer and the
# breaks/labels used to label it. This helper rebuilds the exact
# same final plot (matching the layers used in both scripts) from
# those stored pieces, so it can be reused for direct comparison.
build_final_mosaic <- function(base_plot, labels, title) {
  base_plot +
    scale_x_productlist(
      name = NULL,
      breaks = labels$breaks,
      labels = labels$labels
    ) +
    scale_y_productlist(name = "Age Brackets") +
    labs(fill = "Gender", title = title) +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 5)) +
    guides(x = guide_axis_nested(key = key_range_auto(sep = ":")))
}

# --- Independent scenario: population (sim_data5) vs. survey
# sample (sim_data5_sample) ---
population_plot_indep <- build_final_mosaic(
  sim_data5_base,
  sim_data5_labels,
  "Population: sim_data5"
)
sample_plot_indep <- build_final_mosaic(
  sim_data5_sample_base,
  sim_data5_sample_labels,
  "Sample: sim_data5_sample"
)

population_plot_indep + sample_plot_indep +
  plot_layout(guides = "collect")

# --- Covaried scenario: population (sim_data5_oeh) vs. survey
# sample (sim_data5_oeh_sample) ---
population_plot_oeh <- build_final_mosaic(
  sim_data5_oeh_base,
  sim_data5_oeh_labels,
  "Population: sim_data5_oeh"
)
sample_plot_oeh <- build_final_mosaic(
  sim_data5_oeh_sample_base,
  sim_data5_oeh_sample_labels,
  "Sample: sim_data5_oeh_sample"
)

population_plot_oeh + sample_plot_oeh +
  plot_layout(guides = "collect")
