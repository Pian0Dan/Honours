# ---------------------------------------------------------------
# Small Shiny app wrapping plot_panel_vs_sample() (see
# scripts/panel_visualiser.R): lets a user interactively pick any
# panel_id from a Panelot lottery.csv (panelot_output/2var) and see
# that panel's age x tenure mosaic plot next to the survey sample it
# was drawn from, side by side.
#
# Run with shiny::runApp("scripts/panel_visualiser_app.R") (or the
# editor's "Run App" button) from an R session started in the
# project root -- the same working directory
# scripts/panel_visualiser.R itself assumes.
# ---------------------------------------------------------------

library(shiny)
library(bslib)

# shiny::runApp() sets the working directory to this file's own
# directory (scripts/) while the app runs, rather than the project
# root every other script here assumes -- step back up to the
# project root first so the sourced scripts below resolve their own
# "scripts/..." and "panelot_output/..." paths correctly regardless
# of how this file was launched (runApp() vs source()'d from root).
if (!dir.exists("scripts") && dir.exists("../scripts")) {
  setwd("..")
}

source("scripts/panel_visualiser.R") # lottery_files, load_panel_source, get_panel_members, plot_panel_mosaic, plot_sample_mosaic

# --- Survey-sample mosaic plot with the selected panel's mosaic
# plot placed alongside it, for direct visual comparison ---
# Local to this app -- sample on the left, panel on the right --
# rather than reusing plot_panel_vs_sample() from
# scripts/panel_visualiser.R (panel on the left, sample on the
# right), which other scripts (e.g. 05-panel-association-skeleton.qmd)
# still rely on in its original order. Also appends the panel's
# association-vs-sample p-value (see test_group_vs_sample() below)
# onto the panel pane's existing subtitle, so it's reported directly
# above the panel's mosaic plot.
plot_sample_vs_panel <- function(panel_source, panel_id, vs_sample_p) {
  panel_pane <- plot_panel_mosaic(panel_source, panel_id)
  panel_pane$labels$subtitle <- paste0(
    panel_pane$labels$subtitle,
    "\nassociation vs. sample: p = ",
    signif(vs_sample_p, 3),
    " (higher is more similar to the sample)"
  )

  plot_sample_mosaic(panel_source) +
    panel_pane +
    plot_layout(guides = "collect") +
    plot_annotation(title = basename(panel_source$file))
}

# --- Homogeneity test of one group's (a panel's) age x tenure
# association against the survey sample's, via the age:tenure:group
# three-way interaction in a Poisson log-linear model -- a
# significant interaction means the group's association differs
# from the sample's. This is the same test as
# test_panel_vs_sample()/test_group_vs_sample() in
# scripts/performance_evaluator.R / scripts/performance_evaluator2.R,
# reimplemented locally (rather than sourcing either of those
# scripts, which would also re-run their own full, unrelated
# evaluation of every lottery file at startup) using the same
# glm.fit()-based speedup as performance_evaluator2.R, since this
# app calls it for every panel in a lottery file every time the
# lottery file selection changes.
homogeneity_template <- expand.grid(
  Age = age_levels,
  Tenure = tenure_levels,
  Group = c("Rest", "Group")
)
X_reduced <- model.matrix(
  ~ Age * Tenure + Age * Group + Tenure * Group,
  data = homogeneity_template
)
poisson_family <- poisson()

test_group_vs_sample <- function(group_ids, survey_sample) {
  Group <- factor(
    ifelse(survey_sample$ID %in% group_ids, "Group", "Rest"),
    levels = c("Rest", "Group")
  )
  y <- as.vector(table(
    Age = survey_sample$Age,
    Tenure = survey_sample$Tenure,
    Group = Group
  ))
  fit <- glm.fit(X_reduced, y, family = poisson_family)
  pchisq(fit$deviance, df = fit$df.residual, lower.tail = FALSE)
}

# --- Order every unique panel in a panel source's lottery by how
# much its age x tenure association differs from the survey
# sample's, from most similar (lowest significance, highest
# p-value) to most different (lowest p-value) ---
# "Unique" panels: lottery.csv enumerates every *possible* panel the
# lottery could return, and in principle more than one row could
# enumerate the exact same set of members -- deduplicated by member
# set (rather than row order) before ranking, so no panel is counted,
# ranked or shown twice.
compute_panel_order <- function(panel_source) {
  lottery <- panel_source$lottery
  signature <- vapply(
    lottery$member_ids,
    function(ids) paste(sort(ids), collapse = ","),
    character(1)
  )
  lottery <- lottery[!duplicated(signature), ]

  vs_sample_p <- vapply(
    seq_len(nrow(lottery)),
    function(i) {
      test_group_vs_sample(lottery$member_ids[[i]], panel_source$survey_sample)
    },
    numeric(1)
  )

  # Descending p-value: most similar to the sample (highest p, i.e.
  # least significant difference) first, most different (lowest p)
  # last.
  ord <- order(-vs_sample_p)
  data.frame(
    panel_id = lottery$panel_id[ord],
    vs_sample_p = vs_sample_p[ord]
  )
}

# Computed once at startup, purely to size and position the slider
# below before any reactive context exists -- recomputed per lottery
# file inside the server itself. Sized against whichever file the
# "Lottery file" selectInput below will actually default to (its
# first entry in lottery_files) -- NOT necessarily cov_eq_source
# (scripts/panel_visualiser.R's own example file), since lottery_files
# is sorted alphabetically and "lottery_cov_ass.csv" sorts before
# "lottery_cov_eq.csv". Sizing the slider against the wrong file left
# its initial value pointing at an arbitrary rank in the file that's
# actually selected on load, rather than at that file's own most
# likely panel.
default_lottery_file <- lottery_files[[1]]
default_panel_source <- if (
  identical(default_lottery_file, cov_eq_source$file)
) {
  cov_eq_source
} else {
  load_panel_source(default_lottery_file)
}
initial_panel_order <- compute_panel_order(default_panel_source)
initial_most_likely_rank <- match(
  which.max(default_panel_source$lottery$return_probability),
  initial_panel_order$panel_id
)

# --- Proportions of one trait's levels, across the panel and survey
# sample panes ---
# One row per (group, level) combination -- proportions (each pane's
# counts divided by that pane's own total) rather than raw counts, so
# the panel and the (typically much larger) survey sample are
# comparable on the same y-axis. Independent of the other trait
# (unlike the mosaic plot above, which shows their joint Age x
# Tenure composition).
trait_proportions <- function(values_by_group, levels) {
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

# --- Grouped bar charts of each trait's (Age, Tenure) proportions,
# independently, across the panel and survey sample panes ---
# One facet per trait, with bars grouped (dodged) by pane within each
# trait's levels.
plot_trait_bars <- function(panel_members, sample_members) {
  group_levels <- c("Survey sample", "Panel")

  age_props <- trait_proportions(
    list(Panel = panel_members$Age, `Survey sample` = sample_members$Age),
    age_levels
  )
  age_props$trait <- "Age"

  tenure_props <- trait_proportions(
    list(Panel = panel_members$Tenure, `Survey sample` = sample_members$Tenure),
    tenure_levels
  )
  tenure_props$trait <- "Tenure"

  bar_df <- rbind(age_props, tenure_props)
  bar_df$group <- factor(bar_df$group, levels = group_levels)
  bar_df$level <- factor(
    as.character(bar_df$level),
    levels = c(age_levels, tenure_levels)
  )

  # Age/tenure quotas constrain every feasible panel's margins so
  # tightly that the panel's own bars barely move from one panel to
  # another (often by a single member) -- a count label on each bar
  # makes that genuine, if small, per-panel difference legible even
  # when the bar heights themselves look the same at this scale.
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

ui <- page_sidebar(
  title = "Panel composition explorer",
  theme = bs_theme(version = 5),
  sidebar = sidebar(
    selectInput(
      "lottery_file",
      "Lottery file",
      choices = setNames(lottery_files, basename(lottery_files))
    ),
    sliderInput(
      "panel_rank",
      "Panel rank (1 = most similar to sample, highest = most different)",
      min = 1,
      max = nrow(initial_panel_order),
      value = initial_most_likely_rank,
      step = 1
    ),
    numericInput(
      "panel_id_choice",
      "Or jump directly to a panel number",
      min = 1,
      max = nrow(default_panel_source$lottery),
      value = initial_panel_order$panel_id[initial_most_likely_rank],
      step = 1
    ),
    actionButton("most_likely", "Jump to most likely panel"),
    actionButton("random_panel", "Jump to random panel")
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
    card_header("Marginal trait distributions (Age, Tenure)"),
    plotOutput("trait_bars_plot", height = "350px")
  )
)

server <- function(input, output, session) {
  # Reactive so switching lottery.csv files re-reads the file and
  # its survey sample only when the selection actually changes, not
  # on every panel_id slider move.
  panel_source <- reactive({
    load_panel_source(input$lottery_file)
  })

  # An error thrown by panel_source() (e.g. a lottery file that's
  # been deleted/renamed/mid-rewrite since the app started, or one
  # whose name doesn't match the expected population/case pattern)
  # would otherwise propagate uncaught out of an observer, aborting
  # the *entire* reactive flush -- which is what left the plot box
  # blank while dumping a long stack trace to the console. Route
  # every access through this instead, so a bad selection degrades
  # to "do nothing here" in the observers below, while renderPlot()
  # (which uses panel_source() directly, wrapped in its own
  # tryCatch/validate()) still shows the real error message to the
  # user.
  safe_panel_source <- function() {
    tryCatch(panel_source(), error = function(e) NULL)
  }

  # Every unique panel in the selected lottery file, ordered from
  # most similar to the survey sample's age x tenure association
  # (rank 1) to most different (the last rank) -- see
  # compute_panel_order() above. Reactive on panel_source() alone, so
  # this (relatively expensive -- one homogeneity test per panel) is
  # only recomputed when the lottery file selection actually changes,
  # not on every slider move.
  panel_order <- reactive({
    compute_panel_order(panel_source())
  })

  safe_panel_order <- function() {
    tryCatch(panel_order(), error = function(e) NULL)
  }

  # Keep the slider's range, and the numeric panel-number input's
  # range, in sync with whichever lottery file is selected --
  # different lottery.csv files may contain different numbers of
  # (unique) panels, and panel_id runs over the *full*, non-deduped
  # lottery (ps$lottery), not just the unique panels in po.
  observeEvent(input$lottery_file, {
    ps <- safe_panel_source()
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
      max = nrow(ps$lottery)
    )
  })

  # Typing (or stepping) a specific panel number moves the rank
  # slider to that panel's rank -- the numeric input is just an
  # alternate way of setting the same underlying rank, not a second,
  # independent source of truth, so every downstream plot (which
  # reads off the rank slider via current_panel()) updates to match.
  # An out-of-range or never-discovered panel number (e.g. one
  # Panelot's lottery never actually returned) is reported instead of
  # silently ignored.
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
  # or the "most likely"/"random" buttons -- so the two controls
  # never visibly disagree. Triggered directly off input$panel_rank
  # (rather than current_panel(), which calls validate() -- meant to
  # be caught by renderPlot(), not observeEvent()) so an out-of-range
  # rank is simply skipped here rather than raised as an error.
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

  observeEvent(input$most_likely, {
    ps <- safe_panel_source()
    po <- safe_panel_order()
    if (is.null(ps) || is.null(po)) {
      return()
    }
    most_likely_panel_id <- which.max(ps$lottery$return_probability)
    updateSliderInput(
      session,
      "panel_rank",
      value = match(most_likely_panel_id, po$panel_id)
    )
  })

  observeEvent(input$random_panel, {
    po <- safe_panel_order()
    if (is.null(po)) {
      return()
    }
    updateSliderInput(
      session,
      "panel_rank",
      value = sample(nrow(po), 1)
    )
  })

  # Bundles everything both renderPlot() blocks below need for the
  # currently selected rank -- the actual underlying panel_id (as
  # expected by plot_panel_mosaic()/get_panel_members(), which index
  # directly into panel_source()$lottery by panel_id) and its
  # association-vs-sample p-value -- so the rank-to-panel_id lookup
  # happens in exactly one place.
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

  output$panel_plot <- renderPlot({
    # Surface any error visibly in the plot box (e.g. an
    # unrecognised/missing lottery file, or an out-of-range
    # panel_id) instead of leaving it silently blank.
    result <- tryCatch(
      {
        cp <- current_panel()
        plot_sample_vs_panel(panel_source(), cp$panel_id, cp$vs_sample_p)
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
        ps <- panel_source()
        cp <- current_panel()
        plot_trait_bars(
          get_panel_members(ps, cp$panel_id),
          ps$survey_sample
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
