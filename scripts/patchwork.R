# ---------------------------------------------------------------
# Patchwork panels illustrating how each population's age x tenure
# structure evolves across the sortition pipeline -- population ->
# slice -> respondents -- for the independent population ("pop1",
# sim_data2) and the associated population ("pop2", sim_data2_cov)
# separately, matching the pop1/pop2 naming used in the planning
# notes (README.qmd, Week 11 plan). Two response cases are built for
# each population: equal-chance ("eq") and age-associated ("ass"),
# so the "eq" and "ass" patchworks can be contrasted directly, as
# planned in README.qmd's Week 11 notes.
#
# Note: this is a different use of "pop2" to the "pop2_*" object
# names below -- those come from simdata2.R/simsamp2.R/simsamp2yes.R,
# where "pop2" labels the *two-variable* (age x tenure) pipeline's
# plot objects, for both the independent and associated populations.
# Here, pop1/pop2 instead distinguish the independent population
# (pop1) from the associated population (pop2), per README.qmd.
#
# Draws on the mosaic plots already built by:
#   - scripts/simdata2.R:    pop2_ind_plot, pop2_cov_plot
#     (population-level, age on x-axis, tenure as fill)
#   - scripts/simsamp2.R:    pop2_ind_slice_plot, pop2_cov_slice_plot
#     (10,000-person slice, same age/tenure convention)
#   - scripts/simsamp2yes.R: pop2_ind_eq_respondents_plot,
#     pop2_cov_eq_respondents_plot (respondents under the equal-
#     chance response case), and pop2_ind_ass_respondents_plot,
#     pop2_cov_ass_respondents_plot (respondents under the
#     age-associated response case), all in the same age/tenure
#     convention.
# ---------------------------------------------------------------

library(patchwork)

# simsamp2yes.R sources simsamp2.R, which in turn sources
# simdata2.R, so this one source() call builds every plot object
# referenced below. capture.output() discards the console noise
# (str(), chisq.test() printouts, etc.) those scripts print as a
# side effect of sourcing.
invisible(capture.output(source("scripts/simsamp2yes.R")))

# --- Relabel each stage with a short, consistent title (rather than
# the mix of blank and dataset-specific titles each plot was
# originally given), so the three panels read as a clean sequence,
# and collect the repeated Tenure legend into a single shared one. ---

pop1_pipeline_eq <-
  (pop2_ind_plot + labs(title = "Population")) +
  (pop2_ind_slice_plot + labs(title = "Slice (10,000)")) +
  (pop2_ind_eq_respondents_plot + labs(title = "Respondents (eq)")) +
  plot_layout(ncol = 3, guides = "collect") +
  plot_annotation(
    title = "Independent population (pop1): population \u2192 slice \u2192 eq response"
  )

pop2_pipeline_eq <-
  (pop2_cov_plot + labs(title = "Population")) +
  (pop2_cov_slice_plot + labs(title = "Slice (10,000)")) +
  (pop2_cov_eq_respondents_plot + labs(title = "Respondents (eq)")) +
  plot_layout(ncol = 3, guides = "collect") +
  plot_annotation(
    title = "Associated population (pop2): population \u2192 slice \u2192 eq response"
  )

# --- Age-associated ("ass") response case: same population and
# slice panels as above (response case only affects who responds,
# not the population or the slice drawn from it), but swapping in
# the age-associated respondents plot as the final stage, so this
# patchwork can be compared directly against pop1_pipeline_eq /
# pop2_pipeline_eq to see how a response rate that rises with age
# distorts the respondent stage relative to the equal-chance case. ---

pop1_pipeline_ass <-
  (pop2_ind_plot + labs(title = "Population")) +
  (pop2_ind_slice_plot + labs(title = "Slice (10,000)")) +
  (pop2_ind_ass_respondents_plot + labs(title = "Respondents (ass)")) +
  plot_layout(ncol = 3, guides = "collect") +
  plot_annotation(
    title = "Independent population (pop1): population \u2192 slice \u2192 ass response"
  )

pop2_pipeline_ass <-
  (pop2_cov_plot + labs(title = "Population")) +
  (pop2_cov_slice_plot + labs(title = "Slice (10,000)")) +
  (pop2_cov_ass_respondents_plot + labs(title = "Respondents (ass)")) +
  plot_layout(ncol = 3, guides = "collect") +
  plot_annotation(
    title = "Associated population (pop2): population \u2192 slice \u2192 ass response"
  )

pop1_pipeline_eq
pop2_pipeline_eq
pop1_pipeline_ass
pop2_pipeline_ass
