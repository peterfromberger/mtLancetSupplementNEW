library(ggeffects)
library(ggplot2)
library(ggsci)
library(ggtext)
library(glue)
#library(svglite)

# Set global options
options(
  ggplot2.discrete.colour = ggsci::scale_colour_lancet,
  ggplot2.discrete.fill = ggsci::scale_fill_lancet
)

lanonc_colors <- pal_lancet(palette = "lanonc", alpha = 1)(9)

# CARES total score

# Estimated marginal means of MMRM
plot <- ggemmeans(
  fit2,
  terms = c("timepoint", "treatment")
) |>
  plot(
    colors = lanonc_colors,
    show_data = FALSE,
    show_ci = TRUE,
    show_residuals = FALSE,
    show_title = FALSE
  )

# save png
ggsave(
  here::here("_figures/fig-cares-mmrm-predicted-values-mbsb.png"),
  plot = plot,
  dpi = 300
)

# save svg
# ggsave(
#   here::here("_figures/fig-cares-mmrm-predicted-values-mbsb.svg"),
#   plot = plot,
#   dpi = 300,
#   device = svglite::svglite
# )

# Estimated marginal means with ggplot2
library(emmeans)
emm <- emmeans(fit2, ~ treatment | timepoint)
emm_df <- as.data.frame(emm)

# Get sample sizes by treatment and timepoint, excluding missing values in IoD_reduced
filtered_data <- data[!is.na(data$IoD_reduced), ]
sample_sizes_filtered <- table(filtered_data$treatment, filtered_data$timepoint)
sample_sizes_df_filtered <- as.data.frame(sample_sizes_filtered)
colnames(sample_sizes_df_filtered) <- c("treatment", "timepoint", "n")

# Create summary text for x-axis labels for all timepoints
timepoint_labels <- c("Module 1 (post)", "Module 2 (post)", "Module 3 (post)", "Module 4 (post)", "Module 5 (post)", "Module 6 (post)")
x_axis_labels <- character(length(timepoint_labels))

for(i in seq_along(timepoint_labels)) {
  tp <- timepoint_labels[i]
  
  # Get sample sizes for this timepoint
  intervention_n <- sample_sizes_df_filtered$n[sample_sizes_df_filtered$treatment == "Intervention" & sample_sizes_df_filtered$timepoint == tp]
  placebo_n <- sample_sizes_df_filtered$n[sample_sizes_df_filtered$treatment == "Placebo" & sample_sizes_df_filtered$timepoint == tp]
  
  # Set actual values
  x_axis_labels[i] <- paste0(tp, "\n(Intervention: n = ", 
                           ifelse(length(intervention_n) > 0, intervention_n, 0), 
                           ")\n(Placebo: n = ", 
                           ifelse(length(placebo_n) > 0, placebo_n, 0), ")")
}

# Merge sample sizes with emm data
plot_data <- merge(emm_df, sample_sizes_df_filtered, by = c("treatment", "timepoint"))

plot2 <- ggplot(
    plot_data, 
    aes(
      x = timepoint,
      y = emmean, 
      color = treatment, 
      group = treatment
    )
  ) +
  geom_line() +
  geom_point(size = 3) +
  geom_ribbon(
    aes(ymin = lower.CL, ymax = upper.CL),
    alpha = 0.2
  ) +
  scale_x_discrete(labels = x_axis_labels) +
  labs(
    y = "Estimated marginal mean",
    x = "",
    color = "Treatment"
  ) +
  theme_minimal()