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

plot_pie <- function(
  data,
  variable,
  title,
  legend_position = "none",
  legend_ncol = 1
) {
  tab <- as.data.frame(prop.table(table(data[[variable]])))
  names(tab) <- c("category", "prop")
  tab$label <- percent(tab$prop, accuracy = 1)

  ggplot(tab, aes(x = "", y = prop, fill = category)) +
    geom_col(width = 1, colour = "white") +
    geom_text(
      aes(label = label),
      position = position_stack(vjust = 0.5),
      size = 5.5,
      colour = "white",
      fontface = "bold"
    ) +
    coord_polar(theta = "y") +
    guides(fill = guide_legend(ncol = legend_ncol)) +
    labs(title = title, fill = tools::toTitleCase(variable)) +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 20),
      legend.position = legend_position,
      legend.title = element_text(size = 15, face = "bold"),
      legend.text = element_text(size = 13)
    )
}

# Age and tenure categories (and colours) are identical across the two
# populations, so each variable only needs one shared legend rather than
# one per pie.
extract_legend <- function(gg_plot) {
  g <- ggplotGrob(gg_plot)
  g$grobs[[which(sapply(g$grobs, function(x) x$name) == "guide-box")]]
}

pie_age_ind <- plot_pie(sim_data2, "age", "Age")
pie_tenure_ind <- plot_pie(sim_data2, "tenure", "Tenure")
pie_age_cov <- plot_pie(sim_data2_cov, "age", "Age")
pie_tenure_cov <- plot_pie(sim_data2_cov, "tenure", "Tenure")

legend_age <- extract_legend(plot_pie(
  sim_data2,
  "age",
  "Age",
  legend_position = "right",
  legend_ncol = 1
))
legend_tenure <- extract_legend(plot_pie(
  sim_data2,
  "tenure",
  "Tenure",
  legend_position = "right",
  legend_ncol = 1
))

legends_middle <- wrap_elements(grid::nullGrob()) /
  wrap_elements(legend_age) /
  wrap_elements(legend_tenure) /
  wrap_elements(grid::nullGrob()) +
  plot_layout(heights = c(0.3, 1, 1, 0.3))

# wrap_elements() so each row keeps its own title once nested inside
# the combined two-row layout below (a bare title here would
# otherwise get dropped).
pie_row_ind <- wrap_elements(
  (pie_age_ind + pie_tenure_ind) +
    plot_annotation(
      title = "Independent population",
      theme = theme(
        plot.title = element_text(hjust = 0.5, size = 24, face = "bold")
      )
    )
)
pie_row_cov <- wrap_elements(
  (pie_age_cov + pie_tenure_cov) +
    plot_annotation(
      title = "Associated population",
      theme = theme(
        plot.title = element_text(hjust = 0.5, size = 24, face = "bold")
      )
    )
)

pie_chart_patchwork <-
  (pie_row_ind | legends_middle | pie_row_cov) +
  plot_layout(widths = c(2, 0.5, 2))

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
