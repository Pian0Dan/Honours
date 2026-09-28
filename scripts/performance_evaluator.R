# ---------------------------------------------------------------
# Panel-level representativeness check: for every lottery.csv found
# under panelot_output/2var (each row of which is one *possible*
# panel the sortition lottery could return, together with its exact
# selection probability -- see Panelot's column-generation output),
# this script:
#
#   1. Compiles the actual age/tenure of the panel members named in
#      every row (every possible panel combination) by joining their
#      pool IDs back onto the respondent pool the lottery was run on
#      (the "survey sample" -- the people who actually responded and
#      were eligible to be selected).
#   2. Runs an age x tenure association test *within* each panel
#      (its own, standalone chi-squared test of independence).
#   3. Runs a homogeneity test comparing each panel's age x tenure
#      association against the association observed in the survey
#      sample as a whole, via the age:tenure:in-panel three-way
#      interaction term in a Poisson log-linear model -- a
#      significant interaction means the panel's association differs
#      from the sample's.
#   4. Reports the proportion of panels whose association is
#      significantly different from the survey sample's, at both an
#      uncorrected alpha = 0.05 and a Benjamini-Hochberg-adjusted
#      threshold (since every lottery.csv involves running the same
#      test hundreds of times -- see caveats below).
#
# Caveat: each panel has only 50 members spread across 5 x 3 = 15
# age x tenure cells (~3-4 per cell on average), so both the within-
# panel chi-squared test and the interaction term's asymptotic
# chi-squared approximation are on shaky ground for any individual
# panel (expected counts well below the usual rule-of-thumb of 5).
# Within-panel tests use a Monte Carlo p-value (chisq.test's
# simulate.p.value) to sidestep this; the homogeneity/interaction
# tests are reported with this caveat in mind and are best read in
# aggregate (i.e. the proportion of panels flagged) rather than
# trusting any single panel's p-value.
# ---------------------------------------------------------------

source("scripts/lottery_helpers.R") # lottery_dir, read_lottery, get_survey_sample

set.seed(2469)

lottery_files <- list.files(
  lottery_dir,
  pattern = "^lottery.*\\.csv$",
  full.names = TRUE
)
lottery_files

# --- 1. Cramer's V, for reporting association strength alongside
# each p-value ---
cramers_v <- function(chisq_stat, n, table_dim) {
  sqrt((chisq_stat / n) / (min(table_dim) - 1))
}

# --- 2. Within-panel association test ---
# A Monte Carlo p-value avoids relying on the (poor, at n = 50 over
# 15 cells) chi-squared asymptotic approximation.
test_within_panel <- function(panel_members) {
  tbl <- table(panel_members$Age, panel_members$Tenure)
  test <- chisq.test(tbl, simulate.p.value = TRUE, B = 2000)
  list(
    p_value = test$p.value,
    cramers_v = cramers_v(test$statistic, sum(tbl), dim(tbl))
  )
}

# --- 3. Panel-vs-survey-sample homogeneity test ---
# Split the full survey sample into "Panel" (this panel's members)
# vs "Rest" (everyone else who responded but wasn't selected onto
# this particular panel), then test whether the age:tenure
# association differs between those two groups via the three-way
# age:tenure:group interaction in a Poisson log-linear model. A
# significant interaction means this panel's age x tenure
# association is significantly different from the survey sample's.
test_panel_vs_sample <- function(panel_ids, survey_sample) {
  grouped <- survey_sample
  grouped$Group <- factor(
    ifelse(grouped$ID %in% panel_ids, "Panel", "Rest"),
    levels = c("Rest", "Panel")
  )
  counts <- as.data.frame(table(
    Age = grouped$Age,
    Tenure = grouped$Tenure,
    Group = grouped$Group
  ))

  m_no_interaction <- glm(
    Freq ~ Age * Tenure + Age * Group + Tenure * Group,
    data = counts,
    family = poisson
  )
  m_interaction <- glm(
    Freq ~ Age * Tenure * Group,
    data = counts,
    family = poisson
  )
  lrt <- anova(m_no_interaction, m_interaction, test = "Chisq")
  lrt[["Pr(>Chi)"]][2]
}

# --- 4. Run the full comparison for one lottery.csv ---
evaluate_lottery <- function(path) {
  survey_sample <- get_survey_sample(path)
  lottery <- read_lottery(path)

  # Reference association observed in the survey sample as a whole,
  # for context alongside the per-panel results below.
  sample_tbl <- table(survey_sample$Age, survey_sample$Tenure)
  sample_test <- chisq.test(sample_tbl)

  results <- do.call(
    rbind,
    lapply(seq_len(nrow(lottery)), function(i) {
      panel_ids <- lottery$member_ids[[i]]
      panel_members <- survey_sample[survey_sample$ID %in% panel_ids, ]

      within_panel <- test_within_panel(panel_members)
      homogeneity_p <- test_panel_vs_sample(panel_ids, survey_sample)

      data.frame(
        panel_id = lottery$panel_id[i],
        return_probability = lottery$return_probability[i],
        n_members = nrow(panel_members),
        within_panel_p = within_panel$p_value,
        within_panel_cramers_v = within_panel$cramers_v,
        vs_sample_p = homogeneity_p
      )
    })
  )

  results$vs_sample_p_bh <- p.adjust(results$vs_sample_p, method = "BH")
  results$sig_different <- results$vs_sample_p < 0.05
  results$sig_different_bh <- results$vs_sample_p_bh < 0.05

  list(
    file = path,
    sample_p_value = sample_test$p.value,
    sample_cramers_v = cramers_v(
      sample_test$statistic,
      sum(sample_tbl),
      dim(sample_tbl)
    ),
    results = results
  )
}

# --- 5. Run across every lottery.csv found, and report the headline
# proportion ---
lottery_evaluations <- lapply(lottery_files, evaluate_lottery)
names(lottery_evaluations) <- basename(lottery_files)

summarise_evaluation <- function(evaluation) {
  data.frame(
    file = basename(evaluation$file),
    n_panels = nrow(evaluation$results),
    sample_association_p = evaluation$sample_p_value,
    sample_cramers_v = evaluation$sample_cramers_v,
    prop_sig_different = mean(evaluation$results$sig_different),
    prop_sig_different_bh = mean(evaluation$results$sig_different_bh)
  )
}

lottery_summary <- do.call(
  rbind,
  lapply(lottery_evaluations, summarise_evaluation)
)
rownames(lottery_summary) <- NULL

# The headline result: proportion of panels (out of every possible
# panel the lottery could return) whose age x tenure association is
# significantly different from the survey sample's, per lottery.csv.
lottery_summary

# Per-panel detail (p-values, Cramer's V, and significance flags for
# every panel), for the first lottery file evaluated -- inspect
# lottery_evaluations[[<file>]]$results for any file.
str(lottery_evaluations[[1]]$results)
head(lottery_evaluations[[1]]$results)
