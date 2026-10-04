# ---------------------------------------------------------------
# Variant of scripts/panel_visualiser_prac_app.R: that app lets the
# user select any number of trait columns, shown as a double-decker
# mosaic plot with the last-selected variable used for fill. This
# version instead fixes the mosaic plot to exactly two variables --
# a single "axis variable" and a single "fill variable" picked via
# two separate dropdowns -- always showing the intersection between
# just those two traits, the same two-variable mosaic structure as
# the original scripts/panel_visualiser_app.R (there hardwired to Age
# x Tenure) but for any uploaded sample/lottery pair.
#
# Expected file structure (the same structure
# scripts/lottery_helpers.R and scripts/csv_helper.R already produce
# for this project's own simulated data):
#
#   Lottery file (a Panelot lottery.csv): one row per *possible*
#   panel the lottery could return, with
#     - a "return probability of panel" column, and
#     - one "panel member 1", "panel member 2", ... column per
#       selected member, holding that member's ID.
#
#   Sample file (the respondent pool/survey sample a lottery was
#   drawn from): one row per respondent, with
#     - optionally, an ID column -- recognised if named (case-
#       insensitively) one of "pool_member_identifier", "id",
#       "member_id", "respondent_id" or "identifier"; if none of
#       those is found, respondents are assumed to be identified by
#       row number (1, 2, 3, ...), matching lottery member IDs
#       against row position instead,
#     - any number (two or more) of other columns, each treated as
#       one categorical trait (e.g. age bracket, tenure, gender,
#       region, ...) -- values are coerced to character and sorted
#       alphabetically to form each trait's factor levels, since
#       there's no way to infer a meaningful domain-specific order
#       (e.g. "18-34" before "35-44") for an arbitrary uploaded
#       variable.
#
# Every trait column, not just the two selected for the mosaic plot,
# still appears in the marginal bar chart below.
#
# Run with shiny::runApp("scripts/panel_visualiser_2var_app.R") (or
# the editor's "Run App" button). Like panel_visualiser_prac_app.R,
# this app does not depend on any of this project's own simulated
# data or scripts -- it only needs the packages loaded below.
# ---------------------------------------------------------------

library(shiny)
library(bslib)
library(ggplot2)
library(ggmosaic)
library(patchwork)

# --- 1. Reading uploaded files into a common structure ---

id_column_candidates <- c(
  "pool_member_identifier",
  "id",
  "member_id",
  "respondent_id",
  "identifier"
)

# Case-insensitive match against id_column_candidates; NA if none of
# the uploaded sample's columns look like an ID column.
detect_id_column <- function(col_names) {
  hit <- which(tolower(col_names) %in% id_column_candidates)
  if (length(hit) > 0) col_names[hit[1]] else NA_character_
}

# Returns a data.frame with a character ID column plus one factor
# column per detected trait, a "trait_cols" attribute listing those
# (syntactically-safe) trait column names in their original order,
# and a "trait_labels" attribute mapping each safe name back to its
# original, human-readable column header.
#
# Trait columns are renamed to syntactically valid R names
# (make.names(), e.g. "age bucket" -> "age.bucket") before being
# used internally -- ggmosaic's geom_mosaic()/stat_mosaic() builds an
# internal model formula by pasting variable names together as plain
# text rather than deparsing them safely, so an uploaded column name
# containing a space (or other non-syntactic character, e.g.
# "age bucket") made stat_mosaic() fail outright with an unparseable
# formula, silently producing a blank plot for that variable. The
# original header is kept in "trait_labels" purely for axis/legend/
# facet display text, so the user never sees the sanitised name.
read_sample_upload <- function(path) {
  raw <- read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  if (nrow(raw) == 0 || ncol(raw) == 0) {
    stop("Sample file is empty.")
  }

  id_col <- detect_id_column(names(raw))
  if (is.na(id_col)) {
    id <- as.character(seq_len(nrow(raw)))
    trait_display <- names(raw)
  } else {
    id <- as.character(raw[[id_col]])
    trait_display <- setdiff(names(raw), id_col)
  }

  if (length(trait_display) < 2) {
    stop(
      "Sample file needs at least two trait columns (besides any ID ",
      "column) to draw a mosaic plot -- found only: ",
      paste(trait_display, collapse = ", ")
    )
  }

  trait_cols <- make.names(trait_display, unique = TRUE)

  traits <- lapply(trait_display, function(col) {
    v <- as.character(raw[[col]])
    factor(v, levels = sort(unique(v)))
  })
  names(traits) <- trait_cols

  out <- data.frame(
    ID = id,
    traits,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  attr(out, "trait_cols") <- trait_cols
  attr(out, "trait_labels") <- setNames(trait_display, trait_cols)
  attr(out, "id_col") <- id_col
  out
}

# Returns a data.frame with one row per possible panel: panel_id
# (the row number), return_probability, and member_ids (a
# list-column of that panel's member ID strings, matched against the
# sample's ID column as built by read_sample_upload() above).
#
# A "return probability of panel" column is expected for a genuine
# Panelot lottery.csv, but isn't strictly required here -- unlike the
# member columns below, its absence doesn't make the file unusable
# for the mosaic plot or the homogeneity test, only for the weighted-
# random button (see observeEvent(input$weighted_random, ...) in the
# server below), so it's left as NA rather than erroring out. The
# resulting data.frame carries a "has_probabilities" attribute so the
# server can tell the two cases apart without re-checking NA-ness
# itself every time.
read_lottery_upload <- function(path) {
  raw <- read.csv(path, check.names = FALSE)
  member_cols <- grep("^panel member", names(raw), ignore.case = TRUE)
  prob_col <- grep("return probability", names(raw), ignore.case = TRUE)

  if (length(member_cols) == 0) {
    stop(
      "No 'panel member <n>' columns found in the lottery file -- is ",
      "this actually a Panelot lottery.csv?"
    )
  }

  has_probabilities <- length(prob_col) > 0
  return_probability <- if (has_probabilities) {
    raw[[prob_col[1]]]
  } else {
    rep(NA_real_, nrow(raw))
  }

  out <- data.frame(
    panel_id = seq_len(nrow(raw)),
    return_probability = return_probability,
    member_ids = I(lapply(seq_len(nrow(raw)), function(i) {
      as.character(raw[i, member_cols])
    }))
  )
  attr(out, "has_probabilities") <- has_probabilities
  out
}

get_panel_members_generic <- function(sample_df, member_ids) {
  sample_df[sample_df$ID %in% member_ids, ]
}

# --- 2. Mosaic plot of exactly two trait columns: a single axis
# variable and a single fill variable ---
# x_vars is always length 2 here: c(axis_var, fill_var), both
# (sanitised, safe) column names selected via the two dropdowns
# below. Passed through to ggmosaic's product() exactly as
# panel_visualiser_prac_app.R's more general version does, with the
# *last* entry of x_vars (fill_var) used as the fill (colour)
# variable -- with only two variables this is just a standard
# two-way mosaic plot (divider = ddecker() still applies, but
# reduces to one spine split by axis_var, then split again and
# coloured by fill_var). var_labels maps every (safe) trait column
# name to its original, human-readable header (see
# read_sample_upload()).
#
# ggmosaic labels multi-variable axes by concatenating every
# variable's level with ":" (e.g. "1st:Male"), which gets unreadable
# fast with 2+ layers. Rather than a per-axis variable name (which
# can't be predicted in advance -- seee the note below), every ":" is
# replaced with a line break, so each layer's level prints on its own
# line underneath the axis (e.g. "1st" above "Male") -- this needs a
# throwaway ggplot_build() of the plot first, because
# scale_x_productlist()/scale_y_productlist() silently ignore a
# user-supplied *function* for `labels` (ggmosaic's internal training
# step overwrites any labels *function* with its own computed one),
# so the labels have to be supplied as an already-computed, modified
# character vector instead.
#
# Both axis titles are set to the same combined list of every
# selected variable's display label, since ggmosaic's own spine-
# building algorithm (not anything controlled here) decides which
# variables actually end up on the x axis versus the y axis once
# there are more than 2.
clean_productlist_axis_labels <- function(p, combined_label) {
  built <- ggplot2::ggplot_build(p)
  x_scale <- built$layout$panel_scales_x[[1]]
  y_scale <- built$layout$panel_scales_y[[1]]

  p <- p +
    scale_x_productlist(
      name = combined_label,
      breaks = x_scale$get_breaks(),
      labels = gsub(":", "\n", x_scale$get_labels(), fixed = TRUE)
    )
  y_breaks <- y_scale$get_breaks()
  if (!is.null(y_breaks)) {
    p <- p +
      scale_y_productlist(
        name = combined_label,
        breaks = y_breaks,
        labels = gsub(":", "\n", y_scale$get_labels(), fixed = TRUE)
      )
  }
  p
}

plot_mosaic_generic <- function(
  df,
  x_vars,
  title,
  subtitle = NULL,
  var_labels = setNames(x_vars, x_vars)
) {
  fill_var <- x_vars[length(x_vars)]
  combined_label <- paste(var_labels[x_vars], collapse = " \u00d7 ")

  p <- ggplot(df) +
    geom_mosaic(
      aes(x = product(!!!rlang::syms(x_vars)), fill = !!rlang::sym(fill_var)),
      divider = ddecker()
    ) +
    labs(fill = var_labels[[fill_var]], title = title, subtitle = subtitle) +
    theme_minimal()

  clean_productlist_axis_labels(p, combined_label)
}

# Survey-sample mosaic plot alongside the selected panel's, sample on
# the left -- same layout as panel_visualiser_app.R -- with the
# panel's own lottery return probability (the same figure
# panel_visualiser_app.R's plot_panel_mosaic() always showed, but
# dropped when this app was generalised) and its association-vs-
# sample p-value (see test_group_vs_sample_generic() below) appended
# to the panel pane's subtitle. The panel rank slider orders panels
# by *association similarity*, not by return probability -- the two
# need not agree, e.g. "most likely panel" need not sit at rank 1 --
# so showing the actual return probability here is what lets a user
# confirm "most likely panel" really did jump to the highest-
# probability panel, rather than just trusting the button name.
# return_probability is NA when the uploaded lottery file has no
# "return probability of panel" column (see read_lottery_upload()'s
# "has_probabilities" attribute) and is simply omitted from the
# subtitle in that case.
plot_sample_vs_panel_generic <- function(
  sample_df,
  panel_df,
  x_vars,
  panel_label,
  vs_sample_p,
  return_probability = NA_real_,
  var_labels = setNames(x_vars, x_vars)
) {
  subtitle_lines <- c(
    paste0("n = ", nrow(panel_df)),
    if (!is.na(return_probability)) {
      paste0("return probability = ", signif(return_probability, 3))
    },
    paste0("association vs. sample: p = ", signif(vs_sample_p, 3))
  )
  panel_pane <- plot_mosaic_generic(
    panel_df,
    x_vars,
    title = panel_label,
    subtitle = paste(subtitle_lines, collapse = "\n"),
    var_labels = var_labels
  )
  sample_pane <- plot_mosaic_generic(
    sample_df,
    x_vars,
    title = "Survey sample",
    subtitle = paste0("n = ", nrow(sample_df)),
    var_labels = var_labels
  )

  sample_pane + panel_pane + plot_layout(guides = "collect")
}

# --- 3. Marginal trait bar charts, for every detected trait column
# (not just the two used in the mosaic plot above) ---
trait_proportions_generic <- function(values_by_group, levels) {
  rows <- lapply(names(values_by_group), function(group) {
    data.frame(
      group = group,
      level = factor(as.character(values_by_group[[group]]), levels = levels)
    )
  })
  combined <- do.call(rbind, rows)

  tab <- as.data.frame(table(group = combined$group, level = combined$level))
  tab$total <- ave(tab$Freq, tab$group, FUN = sum)
  tab$prop <- tab$Freq / tab$total
  tab
}

# trait_labels maps each (safe) entry of trait_cols to the original,
# human-readable column header to show as that facet's strip label
# (see read_sample_upload() above) -- defaults to trait_cols
# themselves if not supplied.
plot_trait_bars_generic <- function(
  panel_df,
  sample_df,
  trait_cols,
  trait_labels = setNames(trait_cols, trait_cols)
) {
  group_levels <- c("Survey sample", "Panel")

  all_levels <- character(0)
  rows <- lapply(trait_cols, function(col) {
    levels_here <- levels(sample_df[[col]])
    all_levels <<- c(all_levels, levels_here)
    props <- trait_proportions_generic(
      list(Panel = panel_df[[col]], `Survey sample` = sample_df[[col]]),
      levels_here
    )
    props$trait <- trait_labels[[col]]
    props
  })
  bar_df <- do.call(rbind, rows)
  bar_df$group <- factor(bar_df$group, levels = group_levels)
  bar_df$level <- factor(as.character(bar_df$level), levels = all_levels)

  ggplot(bar_df, aes(x = level, y = prop, fill = group)) +
    geom_col(position = "dodge") +
    geom_text(
      aes(label = Freq),
      position = position_dodge(width = 0.9),
      vjust = -0.3,
      size = 3
    ) +
    facet_wrap(~trait, scales = "free_x") +
    labs(
      x = NULL,
      y = "Proportion of pane",
      fill = NULL,
      title = "Marginal trait distributions"
    ) +
    theme_minimal()
}

# --- 4. Homogeneity test (panel vs. sample association), generalised
# to however many trait columns are selected for the mosaic plot ---
# Generalises panel_visualiser_app.R's test_group_vs_sample() (a
# fixed Age:Tenure:Group three-way interaction) to an arbitrary
# number of trait variables k: build the fully saturated design over
# all k trait variables plus Group (every interaction up to the
# (k+1)-way term), then drop *only* the single highest-order term
# that spans all k trait variables and Group -- the remaining model
# allows every trait's own association with every other trait, and
# with Group, to differ freely, but assumes the full k-way
# intersectional pattern itself is the same for the panel as for the
# rest of the sample. A significant goodness-of-fit failure of that
# reduced model means the panel's specific intersectional composition
# (across all of the selected trait variables, taken together)
# differs from the sample's. With k = 2 this reduces to exactly the
# fixed Age x Tenure version above; with k = 1 it reduces to a plain
# trait-vs-Group independence test.
#
# Every additional trait variable multiplies the contingency table's
# cell count (by that variable's number of levels), so this becomes
# increasingly sparse -- and the resulting p-values increasingly
# unstable -- the more variables are selected relative to panel size.
build_homogeneity_design <- function(sample_df, trait_vars) {
  template <- do.call(
    expand.grid,
    c(
      setNames(
        lapply(trait_vars, function(v) levels(sample_df[[v]])),
        trait_vars
      ),
      list(Group = c("Rest", "Group"))
    )
  )

  full_vars <- c(trait_vars, "Group")
  full_formula <- as.formula(paste0(
    "~ (",
    paste(full_vars, collapse = " + "),
    ")^",
    length(full_vars)
  ))
  term_labels <- attr(terms(full_formula, data = template), "term.labels")
  # The one term spanning every trait variable *and* Group -- the
  # only difference between the saturated model and this reduced one.
  top_term <- which(lengths(strsplit(term_labels, ":")) == length(full_vars))
  reduced_formula <- as.formula(paste(
    "~",
    paste(term_labels[-top_term], collapse = " + ")
  ))

  list(
    trait_vars = trait_vars,
    X_reduced = model.matrix(reduced_formula, data = template)
  )
}

test_group_vs_sample_generic <- function(group_ids, sample_df, design) {
  Group <- factor(
    ifelse(sample_df$ID %in% group_ids, "Group", "Rest"),
    levels = c("Rest", "Group")
  )
  tab_args <- setNames(
    lapply(design$trait_vars, function(v) sample_df[[v]]),
    design$trait_vars
  )
  tab_args$Group <- Group
  y <- as.vector(do.call(table, tab_args))
  fit <- glm.fit(design$X_reduced, y, family = poisson())
  pchisq(fit$deviance, df = fit$df.residual, lower.tail = FALSE)
}

# --- 5. Order every unique panel by how much its association (over
# the selected trait columns) differs from the survey sample's, from
# most similar (rank 1) to most different (the last rank) -- same
# logic as panel_visualiser_app.R's compute_panel_order(). ---
compute_panel_order_generic <- function(sample_df, lottery_df, trait_vars) {
  signature <- vapply(
    lottery_df$member_ids,
    function(ids) paste(sort(ids), collapse = ","),
    character(1)
  )
  lottery_df <- lottery_df[!duplicated(signature), ]

  design <- build_homogeneity_design(sample_df, trait_vars)
  vs_sample_p <- vapply(
    seq_len(nrow(lottery_df)),
    function(i) {
      test_group_vs_sample_generic(
        lottery_df$member_ids[[i]],
        sample_df,
        design
      )
    },
    numeric(1)
  )

  ord <- order(-vs_sample_p)
  data.frame(
    panel_id = lottery_df$panel_id[ord],
    vs_sample_p = vs_sample_p[ord]
  )
}

# Small JS helper backing the "Copy to clipboard" button in the
# member-ID modal below (see observeEvent(input$copy_member_ids, ...)
# in the server) -- registered once, here, at the top level of the
# UI rather than inside the modal itself, since the modal's own
# contents are inserted/removed from the DOM each time it opens and
# closes and so aren't a reliable place to attach a *handler*
# (as opposed to the button it's attached to, which is fine to
# recreate each time). Shiny's custom-message channel is used rather
# than, say, an onclick attribute directly on the button, because the
# text to copy lives in the server's reactive state (the currently
# displayed panel's member IDs), not in anything static the button
# markup itself could embed.
copy_to_clipboard_js <- "
Shiny.addCustomMessageHandler('copy-to-clipboard', function(text) {
  navigator.clipboard.writeText(text);
});
"

ui <- page_sidebar(
  title = "Panel vs. sample explorer -- axis x fill (upload your own files)",
  theme = bs_theme(version = 5),
  # A <script> tag runs wherever it's placed in the page, regardless
  # of visual layout, so this just rides along as an (invisible) part
  # of page_sidebar()'s main content rather than needing a dedicated
  # "header"/"head" argument (page_sidebar() doesn't expose one).
  tags$script(HTML(copy_to_clipboard_js)),
  sidebar = sidebar(
    width = 320,
    fileInput(
      "lottery_upload",
      "Lottery file (Panelot lottery.csv)",
      accept = ".csv"
    ),
    fileInput(
      "sample_upload",
      "Sample / respondent-pool file",
      accept = ".csv"
    ),
    uiOutput("var_selectors"),
    sliderInput(
      "panel_rank",
      "Panel rank (1 = most similar to sample, highest = most different)",
      min = 1,
      max = 1,
      value = 1,
      step = 1
    ),
    numericInput(
      "panel_id_choice",
      "Or jump directly to a panel number",
      min = 1,
      max = 1,
      value = 1,
      step = 1
    ),
    actionButton("most_likely", "Jump to most likely panel"),
    actionButton("random_panel", "Jump to random panel"),
    # Rendered server-side (see output$weighted_random_ui below) so
    # it only appears when the uploaded lottery file actually carries
    # return probabilities -- weighting a random draw by a column
    # that doesn't exist isn't meaningful, so the button itself isn't
    # either.
    uiOutput("weighted_random_ui"),
    actionButton("show_ids", "Show panel member IDs")
  ),
  card(
    full_screen = TRUE,
    height = "650px",
    card_header("Panel vs. survey sample"),
    plotOutput("panel_plot", height = "550px")
  ),
  card(
    full_screen = TRUE,
    height = "450px",
    card_header("Marginal trait distributions"),
    plotOutput("trait_bars_plot", height = "350px")
  )
)

server <- function(input, output, session) {
  # --- Uploaded files, parsed once per upload ---
  sample_df <- reactive({
    req(input$sample_upload)
    read_sample_upload(input$sample_upload$datapath)
  })
  safe_sample_df <- function() tryCatch(sample_df(), error = function(e) NULL)

  lottery_df <- reactive({
    req(input$lottery_upload)
    read_lottery_upload(input$lottery_upload$datapath)
  })
  safe_lottery_df <- function() tryCatch(lottery_df(), error = function(e) NULL)

  # --- Variable pickers for the mosaic plot, rebuilt whenever the
  # sample file changes (every uploaded sample file may have
  # different trait columns) ---
  output$var_selectors <- renderUI({
    df <- safe_sample_df()
    if (is.null(df)) {
      return(helpText("Upload a sample file to choose which traits to plot."))
    }
    trait_cols <- attr(df, "trait_cols")
    trait_labels <- attr(df, "trait_labels")
    # Named vector: displayed label (original column header) -> value
    # actually passed to the server as input$axis_var/input$fill_var
    # (the sanitised column name) -- so the dropdowns show the user's
    # own header text even though the plotting code operates on the
    # safe name underneath.
    choices <- setNames(trait_cols, trait_labels[trait_cols])
    default_axis <- trait_cols[[1]]
    default_fill <- trait_cols[[min(2, length(trait_cols))]]
    tagList(
      selectInput(
        "axis_var",
        "Axis variable",
        choices = choices,
        selected = default_axis
      ),
      selectInput(
        "fill_var",
        "Fill variable",
        choices = choices,
        selected = default_fill
      )
    )
  })

  # --- Every unique panel, ordered from most similar to the sample's
  # association (across every selected mosaic variable) to most
  # different -- recomputed only when the files or the chosen
  # variables actually change, not on every slider move ---
  panel_order <- reactive({
    validate(need(
      length(input$axis_var) == 1 && length(input$fill_var) == 1,
      "Select an axis variable and a fill variable for the mosaic plot."
    ))
    validate(need(
      input$axis_var != input$fill_var,
      "Axis variable and fill variable must be different."
    ))
    compute_panel_order_generic(
      sample_df(),
      lottery_df(),
      c(input$axis_var, input$fill_var)
    )
  })
  safe_panel_order <- function() {
    tryCatch(panel_order(), error = function(e) NULL)
  }

  # Keep the slider's, and the numeric panel-number input's, ranges
  # in sync with whichever lottery/sample files (and variables) are
  # currently selected.
  observeEvent(
    list(
      input$lottery_upload,
      input$sample_upload,
      input$axis_var,
      input$fill_var
    ),
    {
      ps <- safe_lottery_df()
      po <- safe_panel_order()
      if (is.null(ps) || is.null(po)) {
        return()
      }
      n_panels <- nrow(po)
      updateSliderInput(
        session,
        "panel_rank",
        max = n_panels,
        value = min(input$panel_rank, n_panels)
      )
      updateNumericInput(
        session,
        "panel_id_choice",
        max = nrow(ps)
      )
    }
  )

  observeEvent(input$most_likely, {
    ps <- safe_lottery_df()
    po <- safe_panel_order()
    if (is.null(ps) || is.null(po)) {
      return()
    }
    if (!isTRUE(attr(ps, "has_probabilities"))) {
      showNotification(
        "This lottery file has no return probability column -- can't identify the most likely panel.",
        type = "warning",
        duration = 4
      )
      return()
    }
    most_likely_panel_id <- ps$panel_id[which.max(ps$return_probability)]
    rank <- match(most_likely_panel_id, po$panel_id)
    if (!is.na(rank)) {
      updateSliderInput(session, "panel_rank", value = rank)
      # Panel rank orders by association similarity, not by return
      # probability, so the two can land far apart -- spelling out
      # the actual probability and rank here (on top of the panel
      # plot's own subtitle) confirms this jump really did land on
      # the highest-probability panel rather than just trusting the
      # button's name.
      showNotification(
        paste0(
          "Panel ",
          most_likely_panel_id,
          " has the highest return probability (",
          signif(max(ps$return_probability, na.rm = TRUE), 3),
          "), currently at association rank ",
          rank,
          " of ",
          nrow(po),
          "."
        ),
        type = "message",
        duration = 6
      )
    }
  })

  observeEvent(input$random_panel, {
    po <- safe_panel_order()
    if (is.null(po)) {
      return()
    }
    updateSliderInput(session, "panel_rank", value = sample(nrow(po), 1))
  })

  # Only offer the weighted-random button once a lottery file with an
  # actual "return probability of panel" column is loaded -- see
  # read_lottery_upload()'s "has_probabilities" attribute above.
  output$weighted_random_ui <- renderUI({
    ps <- safe_lottery_df()
    if (is.null(ps) || !isTRUE(attr(ps, "has_probabilities"))) {
      return(NULL)
    }
    actionButton(
      "weighted_random",
      "Jump to weighted-random panel (by return probability)"
    )
  })

  # Draws a random row from the *full*, non-deduplicated lottery
  # (ps), weighted by its own return_probability -- the same table
  # input$most_likely draws its arg-max from above -- then maps that
  # row's panel_id down to its rank in the deduplicated po via
  # match(), exactly as input$most_likely does. Any rows sharing a
  # member-set signature with the drawn row (see
  # compute_panel_order_generic()'s dedup step) land on the same
  # rank, so duplicate enumerations of one underlying panel
  # contribute their probabilities additively to that panel's actual
  # chance of being selected here, rather than being favoured once
  # over any other equally-likely panel.
  observeEvent(input$weighted_random, {
    ps <- safe_lottery_df()
    po <- safe_panel_order()
    if (is.null(ps) || is.null(po) || !isTRUE(attr(ps, "has_probabilities"))) {
      return()
    }
    probs <- ps$return_probability
    probs[is.na(probs)] <- 0
    if (sum(probs) <= 0) {
      showNotification(
        "Return probabilities are all zero or missing -- can't draw a weighted random panel.",
        type = "warning",
        duration = 4
      )
      return()
    }
    chosen_panel_id <- sample(ps$panel_id, 1, prob = probs)
    rank <- match(chosen_panel_id, po$panel_id)
    if (!is.na(rank)) {
      updateSliderInput(session, "panel_rank", value = rank)
      showNotification(
        paste0(
          "Drew panel ",
          chosen_panel_id,
          " (return probability ",
          signif(ps$return_probability[ps$panel_id == chosen_panel_id][1], 3),
          "), currently at association rank ",
          rank,
          " of ",
          nrow(po),
          "."
        ),
        type = "message",
        duration = 6
      )
    }
  })

  # Typing a specific panel number moves the rank slider to that
  # panel's rank -- the numeric input is just an alternate way of
  # setting the same underlying rank, not a second, independent
  # source of truth, so every downstream plot updates to match.
  observeEvent(input$panel_id_choice, {
    po <- safe_panel_order()
    if (is.null(po)) {
      return()
    }
    rank <- match(input$panel_id_choice, po$panel_id)
    if (is.na(rank)) {
      showNotification(
        paste0(
          "Panel ",
          input$panel_id_choice,
          " isn't one of this lottery file's discovered panels."
        ),
        type = "warning",
        duration = 4
      )
      return()
    }
    if (!identical(rank, input$panel_rank)) {
      updateSliderInput(session, "panel_rank", value = rank)
    }
  })

  # Keep the numeric input showing whichever panel_id is actually
  # selected, including when the rank changes via the slider itself
  # or the "most likely"/"random" buttons.
  observeEvent(input$panel_rank, {
    po <- safe_panel_order()
    rank <- input$panel_rank
    if (is.null(po) || is.na(rank) || rank < 1 || rank > nrow(po)) {
      return()
    }
    actual_panel_id <- po$panel_id[rank]
    if (!identical(actual_panel_id, input$panel_id_choice)) {
      updateNumericInput(session, "panel_id_choice", value = actual_panel_id)
    }
  })

  # Bundles everything both renderPlot() blocks below need for the
  # currently selected rank.
  current_panel <- reactive({
    po <- panel_order()
    rank <- input$panel_rank
    validate(need(
      rank >= 1 && rank <= nrow(po),
      "Select a panel rank within range."
    ))
    list(
      panel_id = po$panel_id[rank],
      vs_sample_p = po$vs_sample_p[rank]
    )
  })

  # Holds whichever member-ID string the modal below is currently
  # showing, purely so observeEvent(input$copy_member_ids, ...) knows
  # what to copy without re-deriving it (and without risking a
  # mismatch if the underlying panel selection changes while the
  # modal happens to still be open).
  modal_member_ids_text <- reactiveVal(NULL)

  # Pops up the displayed panel's member IDs, comma-separated, in a
  # read-only text box -- selecting all the text (e.g. click, then
  # Ctrl/Cmd+A) and copying it, or clicking "Copy to clipboard" below
  # (backed by copy_to_clipboard_js above), are both ways to get them
  # out of the app, hence a <textarea> (which supports click-to-
  # select-all) rather than just printing them as plain text in the
  # sidebar.
  observeEvent(input$show_ids, {
    cp <- tryCatch(current_panel(), error = function(e) NULL)
    if (is.null(cp)) {
      showNotification(
        "No panel is currently displayed.",
        type = "warning",
        duration = 4
      )
      return()
    }
    ldf <- lottery_df()
    member_ids <- ldf$member_ids[[cp$panel_id]]
    ids_text <- paste(member_ids, collapse = ", ")
    modal_member_ids_text(ids_text)
    showModal(modalDialog(
      title = paste0("Panel ", cp$panel_id, " -- member IDs"),
      tags$textarea(
        ids_text,
        rows = min(10, max(3, length(member_ids) %/% 5 + 1)),
        readonly = "readonly",
        style = "width: 100%; font-family: monospace;"
      ),
      easyClose = TRUE,
      footer = tagList(
        actionButton(
          "copy_member_ids",
          "Copy to clipboard",
          icon = icon("copy")
        ),
        modalButton("Close")
      )
    ))
  })

  # Sends the modal's current member-ID text to the browser-side
  # handler registered by copy_to_clipboard_js above, via
  # navigator.clipboard.writeText() -- there's no reliable, synchronous
  # way back from that JS promise into Shiny, so the "Copied" message
  # here is optimistic rather than a confirmed result.
  observeEvent(input$copy_member_ids, {
    ids_text <- modal_member_ids_text()
    if (is.null(ids_text)) {
      return()
    }
    session$sendCustomMessage("copy-to-clipboard", ids_text)
    showNotification(
      "Copied member IDs to clipboard.",
      type = "message",
      duration = 3
    )
  })

  output$panel_plot <- renderPlot({
    result <- tryCatch(
      {
        sdf <- sample_df()
        ldf <- lottery_df()
        cp <- current_panel()
        member_ids <- ldf$member_ids[[cp$panel_id]]
        panel_members <- get_panel_members_generic(sdf, member_ids)
        trait_labels <- attr(sdf, "trait_labels")
        plot_sample_vs_panel_generic(
          sdf,
          panel_members,
          c(input$axis_var, input$fill_var),
          paste0("Panel ", cp$panel_id),
          cp$vs_sample_p,
          return_probability = ldf$return_probability[[cp$panel_id]],
          var_labels = trait_labels
        )
      },
      error = function(e) e
    )
    if (inherits(result, "error")) {
      validate(conditionMessage(result))
    }
    result
  })

  output$trait_bars_plot <- renderPlot({
    result <- tryCatch(
      {
        sdf <- sample_df()
        ldf <- lottery_df()
        cp <- current_panel()
        member_ids <- ldf$member_ids[[cp$panel_id]]
        panel_members <- get_panel_members_generic(sdf, member_ids)
        plot_trait_bars_generic(
          panel_members,
          sdf,
          attr(sdf, "trait_cols"),
          attr(sdf, "trait_labels")
        )
      },
      error = function(e) e
    )
    if (inherits(result, "error")) {
      validate(conditionMessage(result))
    }
    result
  })
}

shinyApp(ui, server)
