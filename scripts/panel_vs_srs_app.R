# ---------------------------------------------------------------
# Small Shiny app combining scripts/panel_visualiser.R's panel/
# survey-sample mosaic plots and scripts/srs_visualiser.R's random-
# sample mosaic plot into a single three-pane field: Leximin panel
# vs. survey sample vs. random panel. Lets a user interactively pick
# any panel_id from a Panelot lottery.csv; the "random panel" pane is
# a simple random sample of the same size (no quota constraints),
# deterministically paired to that specific panel_id via
# draw_random_sample_for_panel() -- so every unique Leximin panel
# has exactly one paired random panel, reproducible every time that
# panel_id is revisited, rather than being reshuffled independently.
#
# Run with shiny::runApp("scripts/panel_vs_srs_app.R") (or the
# editor's "Run App" button) from an R session started in the
# project root -- the same working directory
# scripts/panel_visualiser.R and scripts/srs_visualiser.R assume.
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

source("scripts/panel_visualiser.R") # lottery_files, load_panel_source, plot_panel_mosaic, plot_sample_mosaic
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

  output$panel_plot <- renderPlot({
    result <- tryCatch(
      {
        ps <- panel_source()
        srs_members <- draw_random_sample_for_panel(
          ps$survey_sample,
          input$lottery_file,
          input$panel_id,
          n = 50
        )
        plot_panel_vs_sample_vs_srs(ps, input$panel_id, srs_members)
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
