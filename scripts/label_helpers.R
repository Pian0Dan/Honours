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

# ---------------------------------------------------------------
# For a geom_mosaic() plot built with x = product(var1, ..., varN),
# ggmosaic normally labels the x-axis with a composite of every
# variable involved (including any fill variable), which becomes
# unreadable -- or exposes text you didn't want shown -- once 3+
# variables are combined. mosaic_axis_breaks() instead computes the
# midpoint x-position of each level of a single named variable, by
# inspecting the rendered layer's block boundaries, so that variable
# alone can be labelled cleanly via
# scale_x_productlist(breaks = ..., labels = ...).
#
# Classic ggmosaic names each product() variable's computed column
# "x__<var>" (matched directly by name). ggmosaic2 instead names
# these columns positionally -- ".mosaic_x1", ".mosaic_x2", ... in
# the order the variables were passed to product() -- with no
# variable name embedded in the column itself, so the position of
# `var` within the plot's own x = product(...) mapping has to be
# recovered first via the layer's mapping expression. Both
# conventions are supported here (falling back to the classic
# "x__<var>" name if no ".mosaic_x<N>" columns are present), so this
# helper keeps working regardless of which of the two packages built
# the plot.
# ---------------------------------------------------------------

mosaic_axis_breaks <- function(plot, var) {
  layer_data <- ggplot2::layer_data(plot)

  if (any(grepl("^\\.mosaic_x[0-9]+$", names(layer_data)))) {
    # ggmosaic2: recover var's position among product()'s arguments
    # from the layer's own x mapping, e.g. product(age, tenure) ->
    # c("age", "tenure"), then match that position to ".mosaic_x<N>".
    x_expr <- rlang::quo_get_expr(plot$layers[[1]]$mapping$x)
    product_vars <- vapply(as.list(x_expr)[-1], rlang::as_name, character(1))
    idx <- match(var, product_vars)
    if (is.na(idx)) {
      stop(
        "'",
        var,
        "' is not one of this plot's product() variables: ",
        paste(product_vars, collapse = ", ")
      )
    }
    col <- paste0(".mosaic_x", idx)
  } else {
    # Classic ggmosaic: the column is named after the variable directly.
    col <- paste0("x__", var)
  }

  lv <- levels(plot$data[[var]])
  data.frame(
    label = lv,
    x = sapply(lv, function(l) {
      xmin <- min(layer_data$xmin[layer_data[[col]] == l])
      xmax <- max(layer_data$xmax[layer_data[[col]] == l])
      (xmin + xmax) / 2
    })
  )
}

# ---------------------------------------------------------------
# When a geom_mosaic() plot combines several x-product variables
# with a `fill` variable, ggmosaic labels each x tick with a
# composite of every variable involved -- fill included -- e.g.
# "Male:Rent:Bachelor:Metro". This makes it impossible to keep a
# fill variable purely as a colour (never appearing as text) using
# guide_axis_nested() directly, since it would render fill's own
# categories as one of the nested rows.
#
# strip_mosaic_fill_labels() removes the fill variable's segment
# from every composite x-axis label (it is always the first ":"
# -separated token, since ggmosaic lists fill before the x-product
# variables), returning the breaks/labels needed to rebuild
# scale_x_productlist() so that only the x-product variables are
# ever shown as text -- the fill variable remains visible solely
# through colour and the legend.
# ---------------------------------------------------------------

strip_mosaic_fill_labels <- function(plot) {
  built <- ggplot2::ggplot_build(plot)
  panel <- built$layout$panel_params[[1]]
  breaks <- panel$x$breaks
  labels <- panel$x$get_labels()
  list(breaks = breaks, labels = sub("^[^:]+:", "", labels))
}
