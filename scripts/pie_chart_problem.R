# ---------------------------------------------------------------
# Demonstrates why a pie chart (or, equivalently, a simple bar
# chart) cannot reveal whether two categorical variables are
# associated, even though it is a common way of communicating a
# sample or panel's composition in current sortition practice.
#
# Builds an age pie chart and a tenure pie chart for each of this
# thesis's two two-variable populations:
#   - sim_data2:     the independent population (no true age-tenure
#                     association)
#   - sim_data2_cov: the associated population (a genuine negative
#                     age-renting association, introduced via the
#                     seed-and-rake procedure -- see scripts/simdata2.R)
#
# Both populations were constructed by design to share (almost)
# identical age and tenure marginal distributions -- the raking step
# used to build sim_data2_cov exists specifically to restore the same
# marginals as sim_data2 while retaining the association introduced
# beforehand (@sec-methodology). The two populations' pie charts are
# therefore expected to look the same, variable by variable, despite
# their joint, intersectional structure being fundamentally
# different: this is precisely the property that makes a pie chart a
# poor diagnostic of representativeness at the intersectional level,
# and motivates the mosaic plots used throughout the rest of this
# thesis instead (@sec-methodology's "Visualisation methods").
# ---------------------------------------------------------------

source("scripts/simdata2.R") # sim_data2, sim_data2_cov

library(ggplot2)
library(patchwork)
library(scales)

# --- Pie chart of one categorical variable's marginal distribution
# ---
# Short per-chart titles ("Age" / "Tenure") deliberately avoid
# repeating the population name here -- that is instead carried by
# each row's own plot_annotation() title below, so long titles don't
# collide with the neighbouring chart's legend.
plot_pie <- function(data, variable, title) {
  tab <- as.data.frame(prop.table(table(data[[variable]])))
  names(tab) <- c("category", "prop")
  tab$label <- percent(tab$prop, accuracy = 1)

  ggplot(tab, aes(x = "", y = prop, fill = category)) +
    geom_col(width = 1, colour = "white") +
    geom_text(
      aes(label = label),
      position = position_stack(vjust = 0.5),
      size = 3,
      colour = "white",
      fontface = "bold"
    ) +
    coord_polar(theta = "y") +
    labs(title = title, fill = tools::toTitleCase(variable)) +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5))
}

pie_age_ind <- plot_pie(sim_data2, "age", "Age")
pie_tenure_ind <- plot_pie(sim_data2, "tenure", "Tenure")
pie_age_cov <- plot_pie(sim_data2_cov, "age", "Age")
pie_tenure_cov <- plot_pie(sim_data2_cov, "tenure", "Tenure")

# Each row is its own small patchwork with its own title, combined
# via wrap_elements() so that title survives being placed inside the
# larger two-row layout below (a title attached directly to a patch
# that is then itself patched together into a bigger one is
# otherwise dropped).
pie_row_ind <- wrap_elements(
  (pie_age_ind + pie_tenure_ind) +
    plot_annotation(title = "Independent population")
)
pie_row_cov <- wrap_elements(
  (pie_age_cov + pie_tenure_cov) +
    plot_annotation(title = "Associated population")
)

pie_chart_patchwork <-
  pie_row_ind /
  pie_row_cov +
  plot_annotation(title = "Age and tenure marginal proportions, by population")

# --- Quantify just how close the two populations' marginals are, for
# reporting alongside the figure: the largest percentage-point gap,
# across all age brackets or all tenure categories, between the
# independent and associated population's marginal proportions. ---
age_marginal_gap <- max(abs(
  prop.table(table(sim_data2$age)) - prop.table(table(sim_data2_cov$age))
)) *
  100

tenure_marginal_gap <- max(abs(
  prop.table(table(sim_data2$tenure)) - prop.table(table(sim_data2_cov$tenure))
)) *
  100

pie_chart_patchwork
age_marginal_gap
tenure_marginal_gap
