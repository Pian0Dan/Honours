# ---------------------------------------------------------------
# Mosaic plot of any panel from a Panelot lottery.csv, alongside the
# survey sample it was drawn from, for direct comparison.
# ---------------------------------------------------------------

source("scripts/lottery_helpers.R") # lottery_dir, read_lottery, get_survey_sample
library(ggplot2)
library(ggmosaic2)
library(patchwork)

lottery_files <- list.files(
  lottery_dir,
  pattern = "^lottery.*\\.csv$",
  full.names = TRUE
)
lottery_files

# Bundles a lottery with the survey sample it was drawn from, so a
# specific panel can be looked up and plotted by ID.
load_panel_source <- function(lottery_file) {
  list(
    file = lottery_file,
    lottery = read_lottery(lottery_file),
    survey_sample = get_survey_sample(lottery_file)
  )
}

# panel_id indexes rows of the lottery -- i.e. picks one of the
# panels the lottery could possibly return.
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

plot_panel_vs_sample <- function(panel_source, panel_id) {
  plot_sample_mosaic(panel_source) +
    plot_panel_mosaic(panel_source, panel_id) +
    plot_layout(guides = "collect") +
    plot_annotation(title = basename(panel_source$file))
}

# Example usage: the panel the lottery is most likely to actually
# return, next to its survey sample...
cov_eq_source <- load_panel_source(
  file.path(lottery_dir, "lottery_cov_eq.csv")
)
nrow(cov_eq_source$lottery)

most_likely_panel_id <- which.max(cov_eq_source$lottery$return_probability)
plot_panel_vs_sample(cov_eq_source, most_likely_panel_id)

# ...versus an arbitrary panel (change random_panel_id, or pass any
# ID from 1 to nrow(cov_eq_source$lottery), to inspect a different one).
set.seed(8348)
random_panel_id <- sample(nrow(cov_eq_source$lottery), 1)
plot_panel_vs_sample(cov_eq_source, random_panel_id)
