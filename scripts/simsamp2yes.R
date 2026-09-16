# ---------------------------------------------------------------
# Mosaic plots of respondents only: for each of the two populations
# from simdata2.R (independent `sim_data2`, associated
# `sim_data2_cov`) and each of the two response-rate cases from
# simsamp2.R (Responded1 "equal chance", Responded2 "associated"),
# restrict the response table to just the individuals who actually
# responded ("Yes"), and plot their age x tenure composition as a
# mosaic plot -- following the same age (y-axis) / tenure (x-axis)
# convention used for simdata2.R's own population-level mosaic
# plots. No fill is needed here, since every row in each plot's data
# already responded (there is no "No" category left to colour by).
# ---------------------------------------------------------------

source("scripts/simsamp2.R")

# --- 1. Restrict each response table to respondents only ---
sim_data2_responded1_yes <- sim_data2_response_table[
  sim_data2_response_table$Responded1 == "Yes",
]
sim_data2_responded2_yes <- sim_data2_response_table[
  sim_data2_response_table$Responded2 == "Yes",
]
sim_data2_cov_responded1_yes <- sim_data2_cov_response_table[
  sim_data2_cov_response_table$Responded1 == "Yes",
]
sim_data2_cov_responded2_yes <- sim_data2_cov_response_table[
  sim_data2_cov_response_table$Responded2 == "Yes",
]

nrow(sim_data2_responded1_yes)
nrow(sim_data2_responded2_yes)
nrow(sim_data2_cov_responded1_yes)
nrow(sim_data2_cov_responded2_yes)

# --- 2. Mosaic plots of respondents only ---
# Structured like the new simdata2.R mosaic plots: age on the
# x-axis, tenure shown purely as fill colour (rather than the older
# age-on-y/tenure-on-x, no-fill convention used previously here).
plot_respondents_only <- function(data, title) {
  ggplot(data) +
    geom_mosaic(aes(x = product(Age), fill = Tenure)) +
    scale_x_productlist(name = "Age Bracket") +
    scale_y_productlist(name = "Tenure") +
    labs(title = title, fill = "Tenure") +
    theme_minimal()
}

pop2_ind_eq_plot <- plot_respondents_only(
  sim_data2_responded1_yes,
  "sim_data2: respondents under Responded1 (equal chance)"
)

pop2_ind_ass_plot <- plot_respondents_only(
  sim_data2_responded2_yes,
  "sim_data2: respondents under Responded2 (associated)"
)

pop2_cov_eq_plot <- plot_respondents_only(
  sim_data2_cov_responded1_yes,
  "sim_data2_cov: respondents under Responded1 (equal chance)"
)

pop2_cov_ass_plot <- plot_respondents_only(
  sim_data2_cov_responded2_yes,
  "sim_data2_cov: respondents under Responded2 (associated)"
)
