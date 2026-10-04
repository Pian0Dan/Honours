# ---------------------------------------------------------------
# Generalised version of scripts/panel_visualiser_app.R: rather than
# being wired to this project's own two-variable (age x tenure)
# simulated data, this app lets the user *upload* their own lottery
# file and respondent-pool ("sample") file, with any number of
# categorical variables, and reproduces the same mosaic-plot and
# marginal-trait-bar-chart structure for whatever is uploaded.
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
# Unlike panel_visualiser_app.R (exactly two traits, Age and Tenure,
# known in advance), this app lets the user pick which two of the
# uploaded sample's trait columns to use as the mosaic plot's x-axis
# and fill -- every trait column, not just those two, still appears
# in the marginal bar chart below.
#
# Run with shiny::runApp("scripts/panel_visualiser_prac_app.R") (or
# the editor's "Run App" button). Unlike panel_visualiser_app.R, this
# app does not depend on any of this project's own simulated data or
# scripts -- it only needs the packages loaded below.
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
  if (length(prob_col) == 0) {
    stop(
      "No 'return probability of panel' column found in the lottery ",
      "file -- is this actually a Panelot lottery.csv?"
    )
  }

  data.frame(
    panel_id = seq_len(nrow(raw)),
    return_probability = raw[[prob_col[1]]],
    member_ids = I(lapply(seq_len(nrow(raw)), function(i) {
      as.character(raw[i, member_cols])
    }))
  )
}

get_panel_members_generic <- function(sample_df, member_ids) {
  sample_df[sample_df$ID %in% member_ids, ]
}

# --- 2. Mosaic plots, for whichever two trait columns are selected
# ---
# var1/var2 are the sanitised (safe) column names actually present in
# df; var1_label/var2_label are the original, human-readable headers
# shown on the axes/legend (see read_sample_upload() above for why
# these can differ).
plot_mosaic_generic <- function(
  df,
  var1,
  var2,
  title,
  subtitle = NULL,
  var1_label = var1,
  var2_label = var2
) {
  ggplot(df) +
    geom_mosaic(aes(
      x = product(!!rlang::sym(var1)),
      fill = !!rlang::sym(var2)
    )) +
    scale_x_productlist(name = var1_label) +
    scale_y_productlist(name = var2_label) +
    labs(fill = var2_label, title = title, subtitle = subtitle) +
    theme_minimal()
}

# Survey-sample mosaic plot alongside the selected panel's, sample on
# the left -- same layout as panel_visualiser_app.R -- with the
# panel's association-vs-sample p-value (see test_group_vs_sample_generic()
# below) appended to the panel pane's subtitle.
plot_sample_vs_panel_generic <- function(
  sample_df,
  panel_df,
  var1,
  var2,
  panel_label,
  vs_sample_p,
  var1_label = var1,
  var2_label = var2
) {
  panel_pane <- plot_mosaic_generic(
    panel_df,
    var1,
    var2,
    title = panel_label,
    subtitle = paste0(
      "n = ",
      nrow(panel_df),
      "\nassociation vs. sample: p = ",
      signif(vs_sample_p, 3)
    ),
    var1_label = var1_label,
    var2_label = var2_label
  )
  sample_pane <- plot_mosaic_generic(
    sample_df,
    var1,
    var2,
    title = "Survey sample",
    subtitle = paste0("n = ", nrow(sample_df)),
    var1_label = var1_label,
    var2_label = var2_label
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
# to whichever two trait columns are selected for the mosaic plot ---
# Same age:tenure:group three-way-interaction logic (and the same
# glm.fit()-based speedup) as panel_visualiser_app.R's
# test_group_vs_sample(), but with the contingency table's dimensions
# -- and therefore the design matrix -- rebuilt from the selected
# variables' own observed levels instead of a fixed 5 x 3 Age x
# Tenure table.
build_homogeneity_design <- function(sample_df, var1, var2) {
  template <- expand.grid(
    V1 = levels(sample_df[[var1]]),
    V2 = levels(sample_df[[var2]]),
    Group = c("Rest", "Group")
  )
  list(
    X_reduced = model.matrix(
      ~ V1 * V2 + V1 * Group + V2 * Group,
      data = template
    )
  )
}

test_group_vs_sample_generic <- function(
  group_ids,
  sample_df,
  var1,
  var2,
  design
) {
  Group <- factor(
    ifelse(sample_df$ID %in% group_ids, "Group", "Rest"),
    levels = c("Rest", "Group")
  )
  y <- as.vector(table(
    V1 = sample_df[[var1]],
    V2 = sample_df[[var2]],
    Group = Group
  ))
  fit <- glm.fit(design$X_reduced, y, family = poisson())
  pchisq(fit$deviance, df = fit$df.residual, lower.tail = FALSE)
}

# --- 5. Order every unique panel by how much its association (over
# the two selected trait columns) differs from the survey sample's,
# from most similar (rank 1) to most different (the last rank) --
# same logic as panel_visualiser_app.R's compute_panel_order(). ---
compute_panel_order_generic <- function(sample_df, lottery_df, var1, var2) {
  signature <- vapply(
    lottery_df$member_ids,
    function(ids) paste(sort(ids), collapse = ","),
    character(1)
  )
  lottery_df <- lottery_df[!duplicated(signature), ]

  design <- build_homogeneity_design(sample_df, var1, var2)
  vs_sample_p <- vapply(
    seq_len(nrow(lottery_df)),
    function(i) {
      test_group_vs_sample_generic(
        lottery_df$member_ids[[i]],
        sample_df,
        var1,
        var2,
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

ui <- page_sidebar(
  title = "Generalised panel vs. sample explorer (upload your own files)",
  theme = bs_theme(version = 5),
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
    # actually passed to the server as input$var1/input$var2 (the
    # sanitised column name) -- so the dropdown shows the user's own
    # header text even though the plotting code operates on the safe
    # name underneath.
    choices <- setNames(trait_cols, trait_labels[trait_cols])
    tagList(
      selectInput(
        "var1",
        "Mosaic plot: x-axis variable",
        choices = choices,
        selected = trait_cols[1]
      ),
      selectInput(
        "var2",
        "Mosaic plot: fill variable",
        choices = choices,
        selected = trait_cols[min(2, length(trait_cols))]
      )
    )
  })

  # --- Every unique panel, ordered from most similar to the sample's
  # association (var1 x var2) to most different -- recomputed only
  # when the files or the chosen variables actually change, not on
  # every slider move ---
  panel_order <- reactive({
    req(input$var1, input$var2)
    validate(need(
      input$var1 != input$var2,
      "Choose two different variables for the mosaic plot's x-axis and fill."
    ))
    compute_panel_order_generic(
      sample_df(),
      lottery_df(),
      input$var1,
      input$var2
    )
  })
  safe_panel_order <- function() {
    tryCatch(panel_order(), error = function(e) NULL)
  }

  # Keep the slider's, and the numeric panel-number input's, ranges
  # in sync with whichever lottery/sample files (and variables) are
  # currently selected.
  observeEvent(
    list(input$lottery_upload, input$sample_upload, input$var1, input$var2),
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
    most_likely_panel_id <- ps$panel_id[which.max(ps$return_probability)]
    rank <- match(most_likely_panel_id, po$panel_id)
    if (!is.na(rank)) {
      updateSliderInput(session, "panel_rank", value = rank)
    }
  })

  observeEvent(input$random_panel, {
    po <- safe_panel_order()
    if (is.null(po)) {
      return()
    }
    updateSliderInput(session, "panel_rank", value = sample(nrow(po), 1))
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
          input$var1,
          input$var2,
          paste0("Panel ", cp$panel_id),
          cp$vs_sample_p,
          var1_label = trait_labels[[input$var1]],
          var2_label = trait_labels[[input$var2]]
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
