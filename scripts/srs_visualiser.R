# ---------------------------------------------------------------
# Visualise the age x tenure composition of a simple random sample
# (SRS) -- drawn with no quota constraints at all -- alongside the
# survey sample it was drawn from, as a mosaic plot. Structured
# identically to scripts/panel_visualiser.R's plot_panel_vs_sample()
# (age on the x-axis, tenure as fill colour, "Tenure" as the fill
# legend title, companion plots placed side by side via patchwork),
# so an SRS draw can be compared visually against both the survey
# sample and against an actual sortition panel of the same size.
#
# This is the visual counterpart to the random-sampling benchmark in
# 05-panel-association-skeleton.qmd (the "Comparison against the
# random-sampling benchmark" section), which draws 500 SRS samples
# of size 50 from the cov/eq survey sample to show that sortition
# panels are flagged as significantly different from their sample
# far more often than equivalently sized random draws are. The first
# two examples below reproduce (via the same seed and draw order)
# the first two of those 500 draws, so the plots shown here are
# drawn from literally the same samples already analysed there.
# ---------------------------------------------------------------

source("scripts/lottery_helpers.R") # survey_samples, age_levels, tenure_levels
library(ggplot2)
library(ggmosaic)
library(patchwork)

# --- 1. Draw a simple random sample from a survey sample ---
# No quota constraints of any kind -- a plain sample.int() draw of
# n IDs, used purely as a benchmark against quota-constrained
# sortition panels of the same size.
draw_random_sample <- function(survey_sample, n = 50) {
  ids <- sample(survey_sample$ID, n)
  survey_sample[survey_sample$ID %in% ids, ]
}

# --- 2. Deterministic, panel-id-specific random sample ---
# Draws a reproducible simple random sample tied to one specific
# lottery file and panel_id, rather than reshuffling independently
# every time a plot is drawn -- so every unique Leximin panel has
# exactly one "paired" random sample of the same size to be compared
# against, and revisiting the same panel_id later reproduces the
# same paired random sample rather than a fresh one. The global RNG
# stream is saved and restored around the seeded draw, so this
# doesn't disturb randomness used elsewhere in a session (e.g.
# unrelated sample() calls, or draw_random_sample() itself when
# called without a fixed seed).
seed_for_panel <- function(lottery_file, panel_id) {
  file_component <- sum(utf8ToInt(basename(lottery_file)))
  as.integer(file_component * 100000 + panel_id)
}

draw_random_sample_for_panel <- function(
  survey_sample,
  lottery_file,
  panel_id,
  n = 50
) {
  old_seed <- if (exists(".Random.seed", envir = .GlobalEnv)) {
    .Random.seed
  } else {
    NULL
  }
  on.exit({
    if (!is.null(old_seed)) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  })

  set.seed(seed_for_panel(lottery_file, panel_id))
  draw_random_sample(survey_sample, n)
}

# --- 3. Mosaic plot of one random sample's age x tenure composition
# ---
plot_srs_mosaic <- function(members) {
  ggplot(members) +
    geom_mosaic(aes(x = product(Age), fill = Tenure)) +
    scale_x_productlist(name = "Age Bracket") +
    scale_y_productlist(name = "Tenure") +
    labs(
      fill = "Tenure",
      title = "Random sample",
      subtitle = paste0("n = ", nrow(members))
    ) +
    theme_minimal()
}

# --- 4. Mosaic plot of the full survey sample a random sample was
# drawn from ---
plot_survey_sample_mosaic <- function(survey_sample) {
  ggplot(survey_sample) +
    geom_mosaic(aes(x = product(Age), fill = Tenure)) +
    scale_x_productlist(name = "Age Bracket") +
    scale_y_productlist(name = "Tenure") +
    labs(
      fill = "Tenure",
      title = "Survey sample",
      subtitle = paste0("n = ", nrow(survey_sample))
    ) +
    theme_minimal()
}

# --- 5. Random-sample mosaic plot with a companion survey-sample
# mosaic plot placed alongside it, for direct visual comparison ---
plot_srs_vs_sample <- function(members, survey_sample) {
  plot_srs_mosaic(members) +
    plot_survey_sample_mosaic(survey_sample) +
    plot_layout(guides = "collect")
}

# --- 6. Example usage ---
# Reproduce (via the same seed, pool and draw order) the first two
# of the 500 random samples drawn in 05-panel-association-skeleton.qmd's
# random-sampling benchmark, so these plots show literally the same
# draws already analysed numerically there.
set.seed(6821)
cov_eq_pool <- survey_samples$cov_eq

srs_example_1 <- draw_random_sample(cov_eq_pool, 50)
plot_srs_vs_sample(srs_example_1, cov_eq_pool)

srs_example_2 <- draw_random_sample(cov_eq_pool, 50)
plot_srs_vs_sample(srs_example_2, cov_eq_pool)
