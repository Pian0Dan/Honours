# ---------------------------------------------------------------
# Double-decker plots of response rate by category (age x tenure x
# responded), for each population and each response case -- split out
# of scripts/simsamp2.R, where this code originally lived as that
# script's section 8.
#
# These plots are not referred to anywhere in the thesis: the actual
# respondent-stage figures used in 03-chap3.qmd/04-chap4.qmd are the
# plain (non-double-decker) respondents-only mosaic plots built in
# scripts/simsamp2yes.R instead. This file is therefore never sourced
# by anything else in the project (not simsamp2.R, not simsamp2yes.R,
# not any .qmd chapter) -- it exists purely so this now-unused code is
# still available to run directly, without being evaluated every time
# simsamp2.R itself is sourced.
#
# Isolating it here also keeps legendry::guide_axis_nested() (needed
# only for these plots' nested "Age:Tenure" x-axis labels) out of the
# active pipeline's dependency chain entirely. As of writing,
# guide_axis_nested() throws "object 'decor' not found" when the
# scale it's labelling was built with ggmosaic2 rather than classic
# ggmosaic (ggmosaic2 overwrites the to_basic.GeomMosaic S3 method
# for the whole R session once loaded, which breaks legendry's own
# key-building for any geom_mosaic() scale, including ones built
# before ggmosaic2 was ever loaded) -- running this script requires
# classic ggmosaic still being the one in effect, and will need
# revisiting (either an ggmosaic2/legendry fix, or a different way of
# labelling the nested axis) if it's ever revived.
# ---------------------------------------------------------------

source("scripts/simsamp2.R") # sim_data2_response_table, sim_data2_cov_response_table
library(legendry) # guide_axis_nested(), key_range_auto()

# Age and tenure are both nested on the x-axis (via divider =
# ddecker(), the double decker template: a sequence of spines in the
# same direction -- here age nested within tenure -- with the final
# spine, response status, laid out in the opposite direction),
# giving the y-axis its usual double decker role as the conditional
# proportion of Yes/No responses within every age x tenure
# combination. Response status is shown both as that y-axis split
# and as fill colour, matching the classic double decker
# presentation. guide_axis_nested() splits the combined "Age:Tenure"
# x-axis label into two clean, bracketed rows -- one per variable.
pop2_ind_eq_plot <- ggplot(sim_data2_response_table) +
  geom_mosaic(
    aes(x = product(Age, Tenure), fill = Responded1),
    divider = ddecker()
  ) +
  labs(fill = "Responded", title = "Response rate: ind_eq") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 7)) +
  guides(x = guide_axis_nested(key = key_range_auto(sep = ":")))

pop2_ind_ass_plot <- ggplot(sim_data2_response_table) +
  geom_mosaic(
    aes(x = product(Age, Tenure), fill = Responded2),
    divider = ddecker()
  ) +
  labs(fill = "Responded", title = "Response rate: ind_ass") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 7)) +
  guides(x = guide_axis_nested(key = key_range_auto(sep = ":")))

pop2_cov_eq_plot <- ggplot(sim_data2_cov_response_table) +
  geom_mosaic(
    aes(x = product(Age, Tenure), fill = Responded1),
    divider = ddecker()
  ) +
  labs(
    fill = "Responded",
    title = "Response rate: cov_eq"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 7)) +
  guides(x = guide_axis_nested(key = key_range_auto(sep = ":")))

pop2_cov_ass_plot <- ggplot(sim_data2_cov_response_table) +
  geom_mosaic(
    aes(x = product(Age, Tenure), fill = Responded2),
    divider = ddecker()
  ) +
  labs(
    fill = "Responded",
    title = "Response rate: cov_ass"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 7)) +
  guides(x = guide_axis_nested(key = key_range_auto(sep = ":")))

pop2_ind_eq_plot
pop2_ind_ass_plot
pop2_cov_eq_plot
pop2_cov_ass_plot
