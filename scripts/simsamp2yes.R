# ---------------------------------------------------------------
# Filters each population/response-case pair down to respondents
# only, and plots their age x tenure composition as a mosaic plot
# (same age-on-x, tenure-as-fill convention used elsewhere).
# ---------------------------------------------------------------

source("scripts/simsamp2.R")

sim_data2_ind_eq_yes <- sim_data2_response_table[
  sim_data2_response_table$Responded1 == "Yes",
]
sim_data2_ind_ass_yes <- sim_data2_response_table[
  sim_data2_response_table$Responded2 == "Yes",
]
sim_data2_cov_eq_yes <- sim_data2_cov_response_table[
  sim_data2_cov_response_table$Responded1 == "Yes",
]
sim_data2_cov_ass_yes <- sim_data2_cov_response_table[
  sim_data2_cov_response_table$Responded2 == "Yes",
]

nrow(sim_data2_ind_eq_yes)
nrow(sim_data2_ind_ass_yes)
nrow(sim_data2_cov_eq_yes)
nrow(sim_data2_cov_ass_yes)

# No fill needed here -- every row already responded, so there's no
# "No" category left to colour by.
plot_respondents_only <- function(data, title) {
  ggplot(data) +
    geom_mosaic(aes(x = product(Age), fill = Tenure)) +
    scale_x_productlist(name = "Age Bracket") +
    scale_y_productlist(name = "Tenure") +
    labs(title = title, fill = "Tenure") +
    theme_minimal()
}

pop2_ind_eq_respondents_plot <- plot_respondents_only(
  sim_data2_ind_eq_yes,
  "Respondents: ind_eq"
)

pop2_ind_ass_respondents_plot <- plot_respondents_only(
  sim_data2_ind_ass_yes,
  "Respondents: ind_ass"
)

pop2_cov_eq_respondents_plot <- plot_respondents_only(
  sim_data2_cov_eq_yes,
  "Respondents: cov_eq"
)

pop2_cov_ass_respondents_plot <- plot_respondents_only(
  sim_data2_cov_ass_yes,
  "Respondents: cov_ass"
)
