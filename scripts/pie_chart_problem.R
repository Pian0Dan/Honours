# ---------------------------------------------------------------
# Shows why a pie chart (or bar chart) can't reveal whether two
# categorical variables are associated: builds an age and a tenure
# pie chart for both the independent (sim_data2) and associated
# (sim_data2_cov) populations, which share near-identical marginals
# by design despite one having a genuine age-tenure association and
# the other none.
# ---------------------------------------------------------------

source("scripts/simdata2.R") # sim_data2, sim_data2_cov

library(ggplot2)
library(patchwork)
library(scales)

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

# wrap_elements() so each row keeps its own title once nested inside
# the combined two-row layout below (a bare title here would
# otherwise get dropped).
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

# Largest percentage-point gap between the two populations' marginals,
# for reporting alongside the figure.
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
