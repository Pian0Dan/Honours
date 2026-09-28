# ---------------------------------------------------------------
# Visualise the age x tenure composition of any given panel from a
# Panelot lottery.csv (panelot_output/2var), as a mosaic plot --
# structured identically to simdata2.R's population-level mosaic
# plots and simsamp2yes.R's respondents-only plots (age on the
# x-axis, tenure as fill colour, "Tenure" as the fill legend title),
# so a panel's composition can be compared directly against the
# population, survey sample, or respondents plots elsewhere in this
# project. A companion mosaic plot of the survey sample the panel
# was drawn from is placed alongside every panel plot (via
# patchwork), so the two can be compared directly, side by side.
# ---------------------------------------------------------------

source("scripts/lottery_helpers.R") # lottery_dir, read_lottery, get_survey_sample
library(ggplot2)
library(ggmosaic)
library(patchwork)

# --- 1. Available lotteries under panelot_output/2var ---
lottery_files <- list.files(
  lottery_dir,
  pattern = "^lottery.*\\.csv$",
  full.names = TRUE
)
lottery_files

# --- 2. Load one lottery.csv together with the survey sample (the
# respondent pool) it was drawn from ---
# The returned "panel source" bundles everything plot_panel_mosaic()
# below needs to pull out and plot any specific panel by ID.
load_panel_source <- function(lottery_file) {
  list(
    file = lottery_file,
    lottery = read_lottery(lottery_file),
    survey_sample = get_survey_sample(lottery_file)
  )
}

# --- 3. Compile the actual selected members of one specific panel
# ---
# panel_id indexes rows of the loaded lottery.csv -- i.e. it selects
# one of the panels the lottery could possibly return.
get_panel_members <- function(panel_source, panel_id) {
  n_panels <- nrow(panel_source$lottery)
  if (panel_id < 1 || panel_id > n_panels || panel_id != round(panel_id)) {
    stop("panel_id must be a whole number between 1 and ", n_panels, ".")
  }
  member_ids <- panel_source$lottery$member_ids[[panel_id]]
  panel_source$survey_sample[
    panel_source$survey_sample$ID %in% member_ids,
  ]
}

# --- 4. Mosaic plot of one panel's age x tenure composition ---
plot_panel_mosaic <- function(panel_source, panel_id) {
  members <- get_panel_members(panel_source, panel_id)
  probability <- panel_source$lottery$return_probability[panel_id]

  ggplot(members) +
    geom_mosaic(aes(x = product(Age), fill = Tenure)) +
    scale_x_productlist(name = "Age Bracket") +
    scale_y_productlist(name = "Tenure") +
    labs(
      fill = "Tenure",
      title = paste0("Panel ", panel_id),
      subtitle = paste0(
        "n = ",
        nrow(members),
        ", return probability = ",
        signif(probability, 3)
      )
    ) +
    theme_minimal()
}

# --- 5. Mosaic plot of the survey sample (the full respondent pool)
# a panel source's lottery was drawn from ---
plot_sample_mosaic <- function(panel_source) {
  survey_sample <- panel_source$survey_sample

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

# --- 6. Panel mosaic plot with a companion survey-sample mosaic
# plot placed alongside it, for direct visual comparison ---
plot_panel_vs_sample <- function(panel_source, panel_id) {
  plot_panel_mosaic(panel_source, panel_id) +
    plot_sample_mosaic(panel_source) +
    plot_layout(guides = "collect") +
    plot_annotation(title = basename(panel_source$file))
}

# --- 7. Example usage ---
# Load the (currently only) available lottery and its survey sample.
cov_eq_source <- load_panel_source(
  file.path(lottery_dir, "lottery_cov_eq.csv")
)
nrow(cov_eq_source$lottery)

# The panel the lottery is most likely to actually return, next to
# the survey sample it was drawn from...
most_likely_panel_id <- which.max(cov_eq_source$lottery$return_probability)
plot_panel_vs_sample(cov_eq_source, most_likely_panel_id)

# ...versus an arbitrary, randomly chosen panel -- change
# random_panel_id (or pass any panel_id from 1 to
# nrow(cov_eq_source$lottery)) to inspect a different panel.
set.seed(8348)
random_panel_id <- sample(nrow(cov_eq_source$lottery), 1)
plot_panel_vs_sample(cov_eq_source, random_panel_id)
