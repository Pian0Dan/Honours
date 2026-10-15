# ---------------------------------------------------------------
# Chi-squared test + Cramer's V for the age-tenure association at
# every stage of the two-variable pipeline (population, slice,
# respondents under both response cases), for both populations.
# Quantifies what the patchwork mosaics in scripts/patchwork.R show
# visually.
# ---------------------------------------------------------------

invisible(capture.output(source("scripts/patchwork.R")))

# Population/slice objects use lower-case age/tenure columns; the
# respondent objects use capitalised Age/Tenure -- pass column names
# explicitly so one helper handles both.
association_stats <- function(data, age_col, tenure_col) {
  tbl <- table(data[[age_col]], data[[tenure_col]])
  test <- chisq.test(tbl)
  n <- sum(tbl)
  cramers_v <- sqrt(unname(test$statistic) / n / (min(dim(tbl)) - 1))

  data.frame(
    n = n,
    chi_sq = unname(test$statistic),
    df = unname(test$parameter),
    p_value = test$p.value,
    cramers_v = cramers_v
  )
}

association_summary_2var <- rbind(
  cbind(
    population = "Independent (pop1)",
    case = "--",
    stage = "Population",
    association_stats(sim_data2, "age", "tenure")
  ),
  cbind(
    population = "Independent (pop1)",
    case = "--",
    stage = "Slice (10,000)",
    association_stats(sim_data2_slice, "age", "tenure")
  ),
  cbind(
    population = "Independent (pop1)",
    case = "eq",
    stage = "Respondents",
    association_stats(sim_data2_ind_eq_yes, "Age", "Tenure")
  ),
  cbind(
    population = "Independent (pop1)",
    case = "ass",
    stage = "Respondents",
    association_stats(sim_data2_ind_ass_yes, "Age", "Tenure")
  ),
  cbind(
    population = "Associated (pop2)",
    case = "--",
    stage = "Population",
    association_stats(sim_data2_cov, "age", "tenure")
  ),
  cbind(
    population = "Associated (pop2)",
    case = "--",
    stage = "Slice (10,000)",
    association_stats(sim_data2_cov_slice, "age", "tenure")
  ),
  cbind(
    population = "Associated (pop2)",
    case = "eq",
    stage = "Respondents",
    association_stats(sim_data2_cov_eq_yes, "Age", "Tenure")
  ),
  cbind(
    population = "Associated (pop2)",
    case = "ass",
    stage = "Respondents",
    association_stats(sim_data2_cov_ass_yes, "Age", "Tenure")
  )
)
rownames(association_summary_2var) <- NULL

# "0.000" reads as exactly zero -- show "< .001" instead wherever
# that would happen, keeping the raw p_value column for anything else.
association_summary_2var$p_value_fmt <- ifelse(
  association_summary_2var$p_value < 0.001,
  "< .001",
  sprintf("%.3f", association_summary_2var$p_value)
)

association_summary_2var
