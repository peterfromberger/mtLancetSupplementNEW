library(ggeffects)
library(ggplot2)
library(ggsci)
library(ggtext)
library(glue)
library(ggrain)
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

datt <- dat_clean %>% filter(!client_id %in% c(253, 396) & !is.na(client_group))

# Get sample sizes by treatment and timepoint, excluding missing values in IoD_reduced
filtered_data <- datt %>% filter(!is.na(IoD_reduced))
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



cares_total_score_descriptive <- function(data = data_long, dv_name) {


    plot <- ggplot(data, aes(fill = client_group, x = timepoint, y = IoD_reduced)) +
        geom_point(size = 2, alpha = 0.5, na.rm = TRUE, position = position_dodge(width=0.8), aes(colour = client_group)) +
        geom_violin(width = 0.8, alpha = 0.1, position="dodge") +
        geom_boxplot(width = 0.2, alpha = 0.2, outlier.shape = NA, coef = 0, position=position_dodge(width=0.8), fatten = 3) +
        labs(
            x = "Timepoint",
            y = "CARES (total score)"
        ) +
        scale_fill_manual(values=c("#004b8c", "#fcb900"), guide = "none") +
        scale_colour_manual(values=c("#004b8c", "#fcb900"), name = "") +
        ggplot2::theme_bw() + theme(
            legend.text = element_text(size=14),
            plot.subtitle = element_markdown(size = 15),
            plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
            axis.title = element_text(size = 14, face = "bold"),
            axis.text = element_text(size = 12),
            panel.border = element_blank(),
            legend.justification = "top",
            legend.direction = "horizontal",
            legend.position = c(0.15,1)
            #legend.position="top"
        ) +
        guides(colour = guide_legend(override.aes = list(size = 5, alpha = 1)))

    # int_centrality_description <- statsExpressions::centrality_description(data_long %>% group_by(client_group), timepoint, value, type = "nonparametric")

    # p + 
    #   geom_point(size = 5, alpha = 1.0, na.rm = TRUE, position = position_nudge(x = -0.2), data = int_centrality_description, inherit.aes = FALSE, aes(x = timepoint, y = value), colour = "#004b8c") +
    #   geom_path(data = int_centrality_description, inherit.aes = FALSE, aes(x = timepoint, y = value, group = 1L), position = position_nudge(x = -0.2), color = "#004b8c", alpha = 1.0)

    # p + stat_summary(
    #     fun = median,
    #     geom = 'line',
    #     aes(group = client_group, color = client_group),
    #     position = position_dodge(width = 0.85) 
    #   )

    return(plot)

}

plt <- cares_total_score_descriptive(
    data = dat_clean %>% filter(!client_id %in% c(253, 396) & !is.na(client_group)),
    dv_name = "CARES_total_score"
)

rain <- ggplot(
    data = dat_clean %>% filter(!client_id %in% c(253, 396) & !is.na(client_group)),
    aes(fill = client_group, x = timepoint, y = IoD_reduced)
  ) +
  geom_rain(
    boxplot.args = list(
      outlier.shape = NA,
      alpha = .8
    ),
    violin.args = list(
      alpha = .5,
      color = NA
    ),
    boxplot.args.pos = list(
      width = .1,
      position = ggpp::position_dodgenudge(
          x = c(
            -.1, -.1,
            -.1, .1,
            -.1, .1, 
            -.1, .1, 
            -.1, .1, 
            -.1, .1, 
            .1, .1
          )
        )
    ),
    violin.args.pos = list(
      width = .5,
      side = c(
        "l", "l", # baseline
        "l", "r", # t2
        "l", "r", # t3
        "l", "r", # t4
        "l", "r", # Module 5 (post)
        "l", "r", # Module 5 (post)
        "l", "r",
        "r", "r"  # Module 6 (post)
      ),
      position = ggpp::position_dodgenudge(
        width = 0.5,
        x = c(
          rep(-.2, 512), rep(-.2, 512), # baseline
          rep(-.2, 512), rep(.2, 512), # t2
          rep(-.2, 512), rep(.2, 512), # t3
          rep(-.2, 512), rep(.2, 512), # t4
          rep(-.2, 512), rep(.2, 512), # Module 5 (post)
          rep(-.2, 512), rep(.2, 512), # Module 5 (post)
          rep(.2, 512), rep(.2, 512) # Module 6 (post)
        )
      )
    )
  )

ggsave(
  here::here("_figures/fig-cares-rainplot.png"),
  plot = rain,
  dpi = 300,
  width = 16,
  height = 8,
)

sample_sizes_both <- table(dat_complete$treatment, dat_complete$timepoint)
sample_sizes_df_both <- as.data.frame(sample_sizes_both)
colnames(sample_sizes_df_both) <- c("treatment", "timepoint", "n")

print("Sample sizes by treatment and timepoint:")
print(sample_sizes_df_both)