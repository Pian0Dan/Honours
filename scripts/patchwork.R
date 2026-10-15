# ---------------------------------------------------------------
# Patchworks tracing each population's age x tenure structure across
# population -> slice -> respondents, under both response cases.
# pop1 = independent population (sim_data2), pop2 = associated
# population (sim_data2_cov) -- not to be confused with the "pop2_*"
# object names from simdata2.R/simsamp2.R/simsamp2yes.R, where "pop2"
# just labels the two-variable pipeline generally.
# ---------------------------------------------------------------

library(patchwork)

# simsamp2yes.R sources simsamp2.R and simdata2.R, so this builds
# every plot referenced below. capture.output() just swallows the
# str()/chisq.test() console noise those scripts print along the way.
invisible(capture.output(source("scripts/simsamp2yes.R")))

# Angling the age labels keeps adjacent brackets from overlapping at
# this three-panel width.
rotate_age_labels <- theme(
  axis.text.x = element_text(angle = 45, hjust = 1)
)

pop1_pipeline_eq <-
  (pop2_ind_plot + labs(title = "Population")) +
  (pop2_ind_slice_plot + labs(title = "Slice (10,000)")) +
  (pop2_ind_eq_respondents_plot + labs(title = "Respondents (eq)")) +
  plot_layout(ncol = 3, guides = "collect") +
  plot_annotation(
    title = "Independent population (pop1): population \u2192 slice \u2192 eq response"
  ) &
  rotate_age_labels

pop2_pipeline_eq <-
  (pop2_cov_plot + labs(title = "Population")) +
  (pop2_cov_slice_plot + labs(title = "Slice (10,000)")) +
  (pop2_cov_eq_respondents_plot + labs(title = "Respondents (eq)")) +
  plot_layout(ncol = 3, guides = "collect") +
  plot_annotation(
    title = "Associated population (pop2): population \u2192 slice \u2192 eq response"
  ) &
  rotate_age_labels

# Same population/slice panels as above -- response case only
# affects who responds, not the population or slice -- with the
# age-associated respondents plot swapped in as the final stage.
pop1_pipeline_ass <-
  (pop2_ind_plot + labs(title = "Population")) +
  (pop2_ind_slice_plot + labs(title = "Slice (10,000)")) +
  (pop2_ind_ass_respondents_plot + labs(title = "Respondents (ass)")) +
  plot_layout(ncol = 3, guides = "collect") +
  plot_annotation(
    title = "Independent population (pop1): population \u2192 slice \u2192 ass response"
  ) &
  rotate_age_labels

pop2_pipeline_ass <-
  (pop2_cov_plot + labs(title = "Population")) +
  (pop2_cov_slice_plot + labs(title = "Slice (10,000)")) +
  (pop2_cov_ass_respondents_plot + labs(title = "Respondents (ass)")) +
  plot_layout(ncol = 3, guides = "collect") +
  plot_annotation(
    title = "Associated population (pop2): population \u2192 slice \u2192 ass response"
  ) &
  rotate_age_labels

pop1_pipeline_eq
pop2_pipeline_eq
pop1_pipeline_ass
pop2_pipeline_ass
