# ---------------------------------------------------------------
# Copy of scripts/panel_vs_srs_app.R that adds, below the existing
# three-pane mosaic plot (Leximin panel vs. survey sample vs. random
# panel), a second section showing each trait's (Age, Tenure)
# marginal distribution independently, as grouped bar charts. The
# mosaic plot shows the *joint* Age x Tenure composition of each
# pane; these bar charts make it easier to see, for each trait on
# its own, how proportions differ across the panel, the survey
# sample it was drawn from, and its paired random panel.
#
# Run with shiny::runApp("scripts/panel_vs_srs_trait_bars_app.R") (or
# the editor's "Run App" button) from an R session started in the
# project root -- the same working directory
# scripts/panel_visualiser.R and scripts/srs_visualiser.R assume.
# ---------------------------------------------------------------

library(shiny)
library(bslib)
library(ggplot2)
library(patchwork)

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
source("scripts/srs_visualiser.R") # draw_random_sample_for_panel, plot_srs_mosaic

# --- Combined three-pane mosaic plot: Leximin panel, survey sample,
# and its uniquely paired random panel, all in a single field ---
plot_panel_vs_sample_vs_srs <- function(panel_source, panel_id, srs_members) {
  leximin_pane <- plot_panel_mosaic(panel_source, panel_id) +
    labs(title = paste0("Leximin panel ", panel_id))

  sample_pane <- plot_sample_mosaic(panel_source)

  random_pane <- plot_srs_mosaic(srs_members) +
    labs(
      title = "Random panel",
      subtitle = paste0(
        "n = ",
        nrow(srs_members),
        " (paired with Leximin panel ",
        panel_id,
        ")"
      )
    )

  leximin_pane +
    sample_pane +
    random_pane +
    plot_layout(guides = "collect", nrow = 1) +
    plot_annotation(title = basename(panel_source$file))
}

# --- Per-trait marginal distribution, as proportions, for one pane
# ---
# Returns one row per (pane, level) combination, with the share of
# that pane's members falling into each level of a single trait
# (e.g. Age or Tenure) -- proportions rather than raw counts, since
# the survey sample is typically far larger than a panel or random
# sample of the same size, so counts alone wouldn't be comparable
# across panes.
trait_proportions <- function(values_by_pane, levels) {
  rows <- lapply(names(values_by_pane), function(pane) {
    data.frame(
      pane = pane,
      level = factor(as.character(values_by_pane[[pane]]), levels = levels)
    )
  })
  combined <- do.call(rbind, rows)

  tab <- as.data.frame(table(pane = combined$pane, level = combined$level))
  tab$total <- ave(tab$Freq, tab$pane, FUN = sum)
  tab$prop <- tab$Freq / tab$total
  tab
}

# --- Grouped bar charts of each trait's marginal distribution,
# independently, across the Leximin panel, survey sample, and random
# panel panes ---
# One facet per trait (Age, Tenure), with bars grouped (dodged) by
# pane within each trait's levels -- complements the mosaic plot
# above, which shows the joint Age x Tenure composition of each pane
# but makes within-trait comparisons harder to read at a glance.
plot_trait_bars <- function(panel_members, sample_members, srs_members) {
  pane_levels <- c("Leximin panel", "Survey sample", "Random panel")

  age_props <- trait_proportions(
    list(
      "Leximin panel" = panel_members$Age,
      "Survey sample" = sample_members$Age,
      "Random panel" = srs_members$Age
    ),
    age_levels
  )
  age_props$trait <- "Age"

  tenure_props <- trait_proportions(
    list(
      "Leximin panel" = panel_members$Tenure,
      "Survey sample" = sample_members$Tenure,
      "Random panel" = srs_members$Tenure
    ),
    tenure_levels
  )
  tenure_props$trait <- "Tenure"

  bar_df <- rbind(age_props, tenure_props)
  bar_df$pane <- factor(bar_df$pane, levels = pane_levels)
  bar_df$level <- factor(as.character(bar_df$level), levels = c(age_levels, tenure_levels))

  ggplot(bar_df, aes(x = level, y = prop, fill = pane)) +
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
  title = "Panel vs. random sample explorer",
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
    actionButton("random_panel", "Jump to random Leximin panel")
  ),
  card(
    full_screen = TRUE,
    height = "650px",
    card_header("Leximin panel vs. survey sample vs. random panel"),
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
  # on every panel_id slider move. The same survey sample also
  # backs the random-panel pane, so all three panes are always
  # compared against an identical reference sample.
  panel_source <- reactive({
    load_panel_source(input$lottery_file)
  })

  # An error thrown by panel_source() (e.g. a lottery file that's
  # been deleted/renamed/mid-rewrite since the app started, or one
  # whose name doesn't match the expected population/case pattern)
  # would otherwise propagate uncaught out of an observer, aborting
  # the *entire* reactive flush -- route every access through this
  # instead, so a bad selection degrades to "do nothing here" in the
  # observers below, while renderPlot() (wrapped in its own
  # tryCatch/validate()) still shows the real error message.
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

  # Shared, reactive bundle of the three panes' member data -- both
  # renderPlot()s below need the exact same panel/sample/random-panel
  # members, so this is computed once per input change rather than
  # duplicated (and potentially drawing a different random panel)
  # across two separate renderPlot() blocks.
  pane_members <- reactive({
    ps <- panel_source()
    panel_members <- get_panel_members(ps, input$panel_id)
    srs_members <- draw_random_sample_for_panel(
      ps$survey_sample,
      input$lottery_file,
      input$panel_id,
      n = 50
    )
    list(
      panel_source = ps,
      panel_members = panel_members,
      sample_members = ps$survey_sample,
      srs_members = srs_members
    )
  })

  output$panel_plot <- renderPlot({
    result <- tryCatch(
      {
        pm <- pane_members()
        plot_panel_vs_sample_vs_srs(
          pm$panel_source,
          input$panel_id,
          pm$srs_members
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
        pm <- pane_members()
        plot_trait_bars(pm$panel_members, pm$sample_members, pm$srs_members)
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
