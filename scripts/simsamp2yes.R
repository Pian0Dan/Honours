# ---------------------------------------------------------------
# Mosaic plots of respondents only: for each of the two populations
# from simdata2.R (independent "ind" = `sim_data2`, covaried/
# associated "cov" = `sim_data2_cov`) and each of the two
# response-rate cases from simsamp2.R (Responded1 = "eq" equal
# chance, Responded2 = "ass" age-associated), restrict the response
# table to just the individuals who actually responded ("Yes"), and
# plot their age x tenure composition as a mosaic plot -- following
# the same age (y-axis) / tenure (x-axis) convention used for
# simdata2.R's own population-level mosaic plots. No fill is needed
# here, since every row in each plot's data already responded (there
# is no "No" category left to colour by).
#
# Object names below use the same "<ind|cov>_<eq|ass>" convention as
# the lottery files (lottery_ind_eq.csv etc.) and the respondent-pool
# CSVs written by csv_helper.R. The plot objects are additionally
# suffixed "_respondents_plot" (rather than reusing simsamp2.R's
# "pop2_<ind|cov>_<eq|ass>_plot" names) since those names already
# belong to a different plot in simsamp2.R (the full-slice,
# double-decker response-rate mosaic, sourced immediately above as
# part of simsamp2.R) -- reusing them here would silently shadow that
# earlier plot rather than extend it.
# ---------------------------------------------------------------

source("scripts/simsamp2.R")

# --- 1. Restrict each response table to respondents only ---
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
