# ---------------------------------------------------------------
# For every lottery.csv in data/panelot_output: joins each possible
# panel's members back onto the survey sample, tests the age x
# tenure association within the panel, tests whether that
# association differs from the sample's, and reports the proportion
# of panels flagged as significantly different (alpha = 0.05,
# uncorrected).
#
# Caveat: each panel has only 50 members spread across 15 age x
# tenure cells (~3-4 per cell), so the chi-squared asymptotics are on
# shaky ground for any one panel. Within-panel tests use a Monte
# Carlo p-value to sidestep this; the homogeneity test is best read
# in aggregate (the proportion flagged) rather than trusting any
# single panel's p-value.
# ---------------------------------------------------------------

source("scripts/lottery_helpers.R") # lottery_dir, read_lottery, get_survey_sample

set.seed(2469)

lottery_files <- list.files(
  lottery_dir,
  pattern = "^lottery.*\\.csv$",
  full.names = TRUE
)
lottery_files

cramers_v <- function(chisq_stat, n, table_dim) {
  sqrt((chisq_stat / n) / (min(table_dim) - 1))
}

# Monte Carlo p-value avoids relying on the chi-squared asymptotic
# approximation, which is weak at this sample size.
test_within_panel <- function(panel_members) {
  tbl <- table(panel_members$Age, panel_members$Tenure)
  test <- chisq.test(tbl, simulate.p.value = TRUE, B = 2000)
  list(
    p_value = test$p.value,
    cramers_v = cramers_v(test$statistic, sum(tbl), dim(tbl))
  )
}

# Splits the sample into this panel's members vs everyone else, then
# tests whether the age:tenure association differs between the two
# groups via the three-way interaction in a Poisson log-linear model.
# A significant interaction means the panel's association differs
# from the sample's.
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

evaluate_lottery <- function(path) {
  survey_sample <- get_survey_sample(path)
  lottery <- read_lottery(path)

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

  results$sig_different <- results$vs_sample_p < 0.05

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

lottery_evaluations <- lapply(lottery_files, evaluate_lottery)
names(lottery_evaluations) <- basename(lottery_files)

summarise_evaluation <- function(evaluation) {
  data.frame(
    file = basename(evaluation$file),
    n_panels = nrow(evaluation$results),
    sample_association_p = evaluation$sample_p_value,
    sample_cramers_v = evaluation$sample_cramers_v,
    prop_sig_different = mean(evaluation$results$sig_different)
  )
}

lottery_summary <- do.call(
  rbind,
  lapply(lottery_evaluations, summarise_evaluation)
)
rownames(lottery_summary) <- NULL

# Headline result: proportion of possible panels per lottery.csv
# whose age x tenure association differs significantly from the
# survey sample's.
lottery_summary

# Per-panel detail for the first lottery file -- inspect
# lottery_evaluations[[<file>]]$results for any file.
str(lottery_evaluations[[1]]$results)
head(lottery_evaluations[[1]]$results)
