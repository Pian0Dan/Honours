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
# still rely on in its original order.
plot_sample_vs_panel <- function(panel_source, panel_id) {
  plot_sample_mosaic(panel_source) +
    plot_panel_mosaic(panel_source, panel_id) +
    plot_layout(guides = "collect") +
    plot_annotation(title = basename(panel_source$file))
}

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

  ggplot(bar_df, aes(x = level, y = prop, fill = group)) +
    geom_col(position = "dodge") +
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
      "panel_id",
      "Panel ID",
      min = 1,
      max = nrow(cov_eq_source$lottery),
      value = most_likely_panel_id,
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

  # Keep the slider's range in sync with whichever lottery file is
  # selected -- different lottery.csv files may contain different
  # numbers of possible panels.
  observeEvent(input$lottery_file, {
    ps <- safe_panel_source()
    if (is.null(ps)) {
      return()
    }
    n_panels <- nrow(ps$lottery)
    updateSliderInput(
      session,
      "panel_id",
      max = n_panels,
      value = min(input$panel_id, n_panels)
    )
  })

  observeEvent(input$most_likely, {
    ps <- safe_panel_source()
    if (is.null(ps)) {
      return()
    }
    updateSliderInput(
      session,
      "panel_id",
      value = which.max(ps$lottery$return_probability)
    )
  })

  observeEvent(input$random_panel, {
    ps <- safe_panel_source()
    if (is.null(ps)) {
      return()
    }
    updateSliderInput(
      session,
      "panel_id",
      value = sample(nrow(ps$lottery), 1)
    )
  })

  output$panel_plot <- renderPlot({
    # Surface any error visibly in the plot box (e.g. an
    # unrecognised/missing lottery file, or an out-of-range
    # panel_id) instead of leaving it silently blank.
    result <- tryCatch(
      plot_sample_vs_panel(panel_source(), input$panel_id),
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
        plot_trait_bars(
          get_panel_members(ps, input$panel_id),
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
