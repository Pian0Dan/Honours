# ---------------------------------------------------------------
# Flowchart summarising the end-to-end simulation and panel
# selection process used throughout this thesis, from simulating a
# population through to selecting a final panel:
#
#   1. Select a population       (simdata.R / simdata2.R /
#                                  simdata3.R / simdata5.R)
#   2. Create a sample slice     (simsamp.R / simsamp2.R /
#                                  simsamp3.R / simsamp5.R)
#   3. Collect responses         (simsamp.R / simsamp2.R /
#                                  simsamp3.R / simsamp5.R)
#   4. Run the LexiMin algorithm (leximin-implementation/R/leximin.R)
#   5. Select a panel            (leximin-implementation/R/leximin.R)
#
# Built directly in ggplot2 (geom_rect/geom_segment/geom_text) to
# stay consistent with the rest of the project's plotting approach,
# rather than introducing a dedicated flowchart/graph package.
# ---------------------------------------------------------------

library(ggplot2)

# --- 1. Define the flowchart's stages ---
# Each stage has a short title and a one-line description of what
# actually happens at that stage, plus the script(s) responsible.
stages <- data.frame(
  step = 1:5,
  title = c(
    "1. Select a population",
    "2. Create a sample slice",
    "3. Collect responses",
    "4. Run the LexiMin algorithm",
    "5. Select a panel"
  ),
  description = c(
    "Simulate a population's joint trait distribution\n(independent or associated) and expand it into\nindividual-level records",
    "Draw a random slice (e.g. 10%) of the population\nas the invited survey sample",
    "Simulate which invited individuals respond, under\nan equal-chance or associated response-rate case",
    "Use multiplicative weights + LP relaxation to find a\nset of feasible committees meeting category quotas,\nwith a probability attached to each",
    "Sample one committee from that probability\ndistribution using random number generation"
  ),
  script = c(
    "simdata2.R / simdata3.R / simdata5.R",
    "simsamp2.R / simsamp3.R / simsamp5.R",
    "simsamp2.R / simsamp3.R / simsamp5.R",
    "leximin-implementation/R/leximin.R",
    "leximin-implementation/R/leximin.R"
  )
)

# --- 2. Lay out one box per stage, stacked top to bottom ---
# Each box needs room for a title line, a multi-line description,
# and a script-name line without overlapping, so the box is tall
# and each line is placed at a fixed offset measured down from the
# top of the box (rather than centred), which scales cleanly
# regardless of how many description lines a stage has.
box_width <- 9
box_height <- 6.4
gap <- 1.6
n_stages <- nrow(stages)

stages$y_top <- seq(
  (n_stages - 1) * (box_height + gap),
  0,
  by = -(box_height + gap)
)
stages$y_bottom <- stages$y_top - box_height
stages$x_left <- -box_width / 2
stages$x_right <- box_width / 2

# Fixed offsets down from the top of each box for the title,
# description, and script lines.
stages$y_title <- stages$y_top - 0.7
stages$y_description <- stages$y_top - 2.6
stages$y_script <- stages$y_bottom + 0.5

# Arrows connect the bottom of each box to the top of the next.
arrows <- data.frame(
  x = 0,
  xend = 0,
  y = stages$y_bottom[-n_stages],
  yend = stages$y_top[-1]
)

# --- 3. Build the flowchart ---
flowchart_plot <- ggplot() +
  geom_rect(
    data = stages,
    aes(xmin = x_left, xmax = x_right, ymin = y_bottom, ymax = y_top),
    fill = "grey95",
    colour = "black",
    linewidth = 0.6
  ) +
  geom_text(
    data = stages,
    aes(x = 0, y = y_title, label = title),
    fontface = "bold",
    size = 4.5
  ) +
  geom_text(
    data = stages,
    aes(x = 0, y = y_description, label = description),
    size = 3.2,
    lineheight = 1.1,
    vjust = 1
  ) +
  geom_text(
    data = stages,
    aes(x = 0, y = y_script, label = script),
    size = 2.6,
    fontface = "italic",
    colour = "grey40"
  ) +
  geom_segment(
    data = arrows,
    aes(x = x, xend = xend, y = y, yend = yend),
    arrow = arrow(length = unit(0.25, "cm"), type = "closed"),
    linewidth = 0.6
  ) +
  coord_cartesian(
    xlim = c(-box_width / 2 - 0.5, box_width / 2 + 0.5),
    ylim = c(min(stages$y_bottom) - 0.5, max(stages$y_top) + 0.5)
  ) +
  labs(title = "Simulation and panel selection process") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))

flowchart_plot

# The flowchart is intentionally tall and narrow (5 stacked boxes),
# so it is saved at an explicit tall aspect ratio -- an interactive
# plot pane's default (roughly landscape) proportions would squash
# the vertical spacing and make the fixed-size text overflow its
# allocated space within each box.
ggsave(
  "Flowchart.png",
  plot = flowchart_plot,
  width = 8,
  height = 12,
  dpi = 300
)
