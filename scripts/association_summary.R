# ---------------------------------------------------------------
# Numerical summary of age-tenure association strength at each
# stage of the two-variable (age x tenure) simulation pipeline --
# population, slice, and respondents (under both the equal-chance
# "eq" and age-associated "ass" response cases) -- for each of the
# independent ("pop1") and associated ("pop2") populations.
#
# Quantifies, via a chi-squared test of independence and Cramer's V
# effect size, exactly what the patchwork mosaic plots in
# scripts/patchwork.R show visually: whether each stage's age x
# tenure association is distinguishable from independence, and how
# strong it is. Population- and slice-level objects use lower-case
# "age"/"tenure" columns (from simdata2.R/simsamp2.R); respondent
# objects use capitalised "Age"/"Tenure" columns (from
# simsamp2yes.R) -- association_stats() takes the column names
# explicitly so both conventions can be handled by the same helper.
# ---------------------------------------------------------------

# Builds every stage's data frame used below (sim_data2, sim_data2_cov,
# sim_data2_slice, sim_data2_cov_slice, sim_data2_ind_eq_yes,
# sim_data2_ind_ass_yes, sim_data2_cov_eq_yes, sim_data2_cov_ass_yes).
invisible(capture.output(source("scripts/patchwork.R")))

# --- Chi-squared test of independence + Cramer's V for a single
# age x tenure table ---
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

# A rounded p-value of 0.000 could be misread as exactly zero rather
# than merely very small, so format p-values below .001 as "< .001"
# for display, while keeping the raw numeric p_value column intact
# for any other use.
association_summary_2var$p_value_fmt <- ifelse(
  association_summary_2var$p_value < 0.001,
  "< .001",
  sprintf("%.3f", association_summary_2var$p_value)
)

association_summary_2var
