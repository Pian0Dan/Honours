# ---------------------------------------------------------------
# Panel-level representativeness check -- extended from
# performance_evaluator.R to also benchmark every panel against a
# matched simple random sample (SRS) of the same size, drawn with no
# quota constraints at all. For every lottery.csv found under
# panelot_output/2var (each row of which is one *possible* panel the
# sortition lottery could return, together with its exact selection
# probability -- see Panelot's column-generation output), this
# script:
#
#   1. Compiles the actual age/tenure of the panel members named in
#      every row (every possible panel combination) by joining their
#      pool IDs back onto the respondent pool the lottery was run on
#      (the "survey sample" -- the people who actually responded and
#      were eligible to be selected).
#   2. Draws a matched SRS of the same size for every panel, via
#      draw_random_sample_for_panel() (see scripts/srs_visualiser.R)
#      -- deterministically paired to that specific lottery file and
#      panel_id, so every panel has exactly one random-sample
#      counterpart, reproducible across runs.
#   3. Runs an age x tenure association test *within* each panel and
#      within its matched random sample (a standalone chi-squared
#      test of independence for each).
#   4. Runs a homogeneity test comparing each panel's (and each
#      random sample's) age x tenure association against the
#      association observed in the survey sample as a whole, via the
#      age:tenure:in-group three-way interaction term in a Poisson
#      log-linear model -- a significant interaction means the
#      panel's (or random sample's) association differs from the
#      sample's.
#   5. Reports the proportion of panels, and the proportion of
#      matched random samples, whose association is significantly
#      different from the survey sample's. For panels, this is
#      reported at both an uncorrected alpha = 0.05 and a
#      Benjamini-Hochberg-adjusted threshold, as a robustness check
#      that the near-ubiquitous "different" finding survives
#      correcting for running the same test hundreds of times per
#      lottery.csv. No BH-adjusted figure is reported for the random
#      samples: they're close to a true-null scenario by
#      construction, so a BH-adjusted proportion there is
#      mathematically expected to collapse toward 0 as the number of
#      tests grows regardless of the specific draws -- it would
#      confirm the null rather than convey anything about the random
#      samples themselves. Comparing the *raw* proportions for panels
#      vs. random samples directly is what separates a genuine effect
#      of quota-constrained panel selection from ordinary small-
#      sample variability: if panels were no more divergent from the
#      sample than equivalently sized random draws, the two raw
#      proportions should be similar (close to the nominal alpha).
#
# Caveat: each panel/sample has only 50 members spread across 5 x 3
# = 15 age x tenure cells (~3-4 per cell on average), so both the
# within-group chi-squared test and the interaction term's
# asymptotic chi-squared approximation are on shaky ground for any
# individual panel or sample (expected counts well below the usual
# rule-of-thumb of 5). Within-group tests use a Monte Carlo p-value
# (chisq.test's simulate.p.value) to sidestep this; the homogeneity/
# interaction tests are reported with this caveat in mind and are
# best read in aggregate (i.e. the proportion flagged) rather than
# trusting any single panel's or sample's p-value.
#
# A further consequence of n = 50 random samples specifically (not
# quota-constrained panels, which are guaranteed some representation
# in every category): purely by chance, an unconstrained random
# sample can occasionally end up with zero members in one age
# bracket, which makes the within-group chi-squared statistic (and
# therefore its Cramer's V) undefined (NaN) for that draw. This does
# not affect the panel-vs-sample homogeneity test used for the
# headline proportions below (a Poisson model over the full survey
# sample, which handles zero counts natively) -- only the
# within-group Cramer's V summaries, which are averaged with
# na.rm = TRUE and reported alongside a count of how many draws were
# affected.
# ---------------------------------------------------------------

source("scripts/lottery_helpers.R") # lottery_dir, read_lottery, get_survey_sample
source("scripts/srs_visualiser.R") # draw_random_sample_for_panel

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

# --- 2. Within-group association test ---
# A Monte Carlo p-value avoids relying on the (poor, at n = 50 over
# 15 cells) chi-squared asymptotic approximation. Used for both
# panels and their matched random samples.
#
# An unconstrained random sample of 50 (unlike a quota-constrained
# panel, which is guaranteed some representation in every category)
# can, purely by chance, end up with zero members in an age bracket
# -- this is the same edge case documented in the header comment,
# which makes the resulting Cramer's V undefined (NaN) and is
# already handled downstream (na.rm = TRUE, reported transparently
# via n_srs_cramers_v_undefined). When it happens, chisq.test()
# additionally emits two warnings before returning NaN ("cannot
# compute simulated p-value with zero marginals" and "Chi-squared
# approximation may be incorrect"); since this is anticipated and
# already accounted for, those two specific messages are muffled
# below rather than left to spam the console -- any other, genuinely
# unexpected warning from chisq.test() still surfaces normally.
expected_zero_margin_warnings <- c(
  "cannot compute simulated p-value with zero marginals",
  "Chi-squared approximation may be incorrect"
)

test_within_group <- function(group_members) {
  tbl <- table(group_members$Age, group_members$Tenure)
  test <- withCallingHandlers(
    chisq.test(tbl, simulate.p.value = TRUE, B = 2000),
    warning = function(w) {
      if (conditionMessage(w) %in% expected_zero_margin_warnings) {
        invokeRestart("muffleWarning")
      }
    }
  )
  list(
    p_value = test$p.value,
    cramers_v = cramers_v(test$statistic, sum(tbl), dim(tbl))
  )
}

# --- 3. Group-vs-survey-sample homogeneity test ---
# Split the full survey sample into "Group" (this panel's or random
# sample's members) vs "Rest" (everyone else in the sample), then
# test whether the age:tenure association differs between those two
# groups via the three-way age:tenure:group interaction in a Poisson
# log-linear model. A significant interaction means this group's age
# x tenure association is significantly different from the survey
# sample's. Used for both panels and their matched random samples.
#
# This is called twice per panel (once for the panel, once for its
# matched random sample) across thousands of panels per lottery.csv,
# and profiling showed it -- specifically glm()'s per-call overhead
# of re-parsing the formula and rebuilding the model matrix from
# scratch every time -- accounted for roughly 80% of this script's
# total runtime. Two things are exploited here to speed it up by
# about 5x with floating-point-identical results:
#
#   (a) The Age x Tenure x Group combinations (5 x 3 x 2 = 30 cells)
#       are always the same, in the same order, for every panel --
#       only the cell counts (Freq) differ. So the design matrix for
#       the no-three-way-interaction model is built *once* below
#       (X_reduced), and reused via the low-level glm.fit() interface
#       instead of calling glm() (which would rebuild it from the
#       formula on every call) -- the counts vector alone changes.
#   (b) The saturated (Age*Tenure*Group) model has exactly as many
#       parameters as cells (30), so it always fits the data exactly
#       by construction: its deviance and residual df are always
#       precisely 0. The likelihood-ratio test for the three-way
#       interaction (comparing the reduced model to the saturated
#       one) therefore reduces to the *reduced* model's own residual
#       deviance goodness-of-fit test -- a standard equivalence in
#       log-linear model theory -- so the saturated model never
#       actually needs to be fitted at all.
homogeneity_template <- expand.grid(
  Age = age_levels,
  Tenure = tenure_levels,
  Group = c("Rest", "Group")
)
X_reduced <- model.matrix(
  ~ Age * Tenure + Age * Group + Tenure * Group,
  data = homogeneity_template
)
stopifnot(
  # confirms the saturated-model shortcut in (b) above applies
  qr(model.matrix(~ Age * Tenure * Group, data = homogeneity_template))$rank ==
    nrow(homogeneity_template)
)
poisson_family <- poisson()

test_group_vs_sample <- function(group_ids, survey_sample) {
  Group <- factor(
    ifelse(survey_sample$ID %in% group_ids, "Group", "Rest"),
    levels = c("Rest", "Group")
  )
  # table()'s cells vary in the same order as homogeneity_template
  # (first argument fastest), matching X_reduced's rows exactly.
  y <- as.vector(table(
    Age = survey_sample$Age,
    Tenure = survey_sample$Tenure,
    Group = Group
  ))
  fit <- glm.fit(X_reduced, y, family = poisson_family)
  pchisq(fit$deviance, df = fit$df.residual, lower.tail = FALSE)
}

# --- 4. Run the full comparison for one lottery.csv ---
# For every panel, also draws and evaluates its matched random
# sample, so the two can be compared row-for-row below.
evaluate_lottery <- function(path) {
  survey_sample <- get_survey_sample(path)
  lottery <- read_lottery(path)

  # Reference association observed in the survey sample as a whole,
  # for context alongside the per-panel/per-sample results below.
  sample_tbl <- table(survey_sample$Age, survey_sample$Tenure)
  sample_test <- chisq.test(sample_tbl)

  results <- do.call(
    rbind,
    lapply(seq_len(nrow(lottery)), function(i) {
      panel_id <- lottery$panel_id[i]
      panel_ids <- lottery$member_ids[[i]]
      panel_members <- survey_sample[survey_sample$ID %in% panel_ids, ]

      srs_members <- draw_random_sample_for_panel(
        survey_sample,
        path,
        panel_id,
        n = nrow(panel_members)
      )

      within_panel <- test_within_group(panel_members)
      within_srs <- test_within_group(srs_members)
      panel_vs_sample_p <- test_group_vs_sample(panel_ids, survey_sample)
      srs_vs_sample_p <- test_group_vs_sample(srs_members$ID, survey_sample)

      data.frame(
        panel_id = panel_id,
        return_probability = lottery$return_probability[i],
        n_members = nrow(panel_members),
        within_panel_p = within_panel$p_value,
        within_panel_cramers_v = within_panel$cramers_v,
        vs_sample_p = panel_vs_sample_p,
        within_srs_p = within_srs$p_value,
        within_srs_cramers_v = within_srs$cramers_v,
        srs_vs_sample_p = srs_vs_sample_p
      )
    })
  )

  results$vs_sample_p_bh <- p.adjust(results$vs_sample_p, method = "BH")
  results$sig_different <- results$vs_sample_p < 0.05
  results$sig_different_bh <- results$vs_sample_p_bh < 0.05

  # No BH-adjusted column for the random samples -- see the note on
  # this in the header comment above.
  results$srs_sig_different <- results$srs_vs_sample_p < 0.05

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
# proportions for panels alongside their matched random samples ---
lottery_evaluations <- lapply(lottery_files, evaluate_lottery)
names(lottery_evaluations) <- basename(lottery_files)

summarise_evaluation <- function(evaluation) {
  data.frame(
    file = basename(evaluation$file),
    n_panels = nrow(evaluation$results),
    sample_association_p = evaluation$sample_p_value,
    sample_cramers_v = evaluation$sample_cramers_v,
    prop_sig_different = mean(evaluation$results$sig_different, na.rm = TRUE),
    prop_sig_different_bh = mean(
      evaluation$results$sig_different_bh,
      na.rm = TRUE
    ),
    prop_sig_different_srs = mean(
      evaluation$results$srs_sig_different,
      na.rm = TRUE
    ),
    median_cramers_v_panel = median(
      evaluation$results$within_panel_cramers_v,
      na.rm = TRUE
    ),
    median_cramers_v_srs = median(
      evaluation$results$within_srs_cramers_v,
      na.rm = TRUE
    ),
    # How many of the matched random-sample draws ended up with an
    # undefined within-group Cramer's V (see the caveat above) --
    # reported for transparency, not excluded silently.
    n_srs_cramers_v_undefined = sum(
      is.na(evaluation$results$within_srs_cramers_v)
    )
  )
}

lottery_summary <- do.call(
  rbind,
  lapply(lottery_evaluations, summarise_evaluation)
)
rownames(lottery_summary) <- NULL

# The headline result: proportion of panels (out of every possible
# panel the lottery could return), and the proportion of their
# matched random samples, whose age x tenure association is
# significantly different from the survey sample's, per
# lottery.csv. A much higher proportion for panels than for random
# samples indicates that the pattern is a real effect of
# quota-constrained selection, not just small-sample variability.
lottery_summary

# Per-panel detail (p-values, Cramer's V, and significance flags for
# every panel and its matched random sample), for the first lottery
# file evaluated -- inspect lottery_evaluations[[<file>]]$results
# for any file.
str(lottery_evaluations[[1]]$results)
head(lottery_evaluations[[1]]$results)
