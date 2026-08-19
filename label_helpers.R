# ---------------------------------------------------------------
# Shared helper for abbreviating categorical factor levels down to
# their first letter for use in plots, while keeping the underlying
# analysis data (chi-square tests, proportions, etc.) on full level
# names.
#
# Levels are processed in order: the first level to claim a given
# starting letter keeps the single-letter abbreviation; any later
# level that would collide with an already-used abbreviation is
# extended by one more letter (repeating until it's unique). e.g.
# c("Rent", "Own", "Other") -> c("R", "O", "Ot"), since Own claims
# "O" first and Other must be extended to stay distinguishable.
# ---------------------------------------------------------------

first_letter_labels <- function(x) {
  out <- character(length(x))
  seen <- character(0)
  for (i in seq_along(x)) {
    n <- 1
    candidate <- substr(x[i], 1, n)
    while (candidate %in% seen) {
      n <- n + 1
      candidate <- substr(x[i], 1, n)
    }
    out[i] <- candidate
    seen <- c(seen, candidate)
  }
  out
}

# Returns a copy of `data` with the levels of each factor column
# named in `cols` recoded via first_letter_labels(). The underlying
# category assignment per row is unchanged -- only the level labels
# are shortened, which is all that's needed for plotting.
abbreviate_for_plot <- function(data, cols) {
  for (col in cols) {
    levels(data[[col]]) <- first_letter_labels(levels(data[[col]]))
  }
  data
}
