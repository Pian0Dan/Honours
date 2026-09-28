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

source("scripts/panel_visualiser.R") # lottery_files, load_panel_source, plot_panel_vs_sample

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
      plot_panel_vs_sample(panel_source(), input$panel_id),
      error = function(e) e
    )
    if (inherits(result, "error")) {
      validate(conditionMessage(result))
    }
    result
  })
}

shinyApp(ui, server)
