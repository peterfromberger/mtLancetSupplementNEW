library(ggeffects)
library(ggplot2)
library(ggsci)

# Set global options
options(
  ggplot2.discrete.colour = ggsci::scale_colour_bmj,
  ggplot2.discrete.fill = ggsci::scale_fill_bmj
)

lanonc_colors <- pal_lancet(palette = "lanonc", alpha = 1)(9)

p <- ggplot(mpg, aes(displ, hwy, colour = factor(cyl), fill = factor(cyl))) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw()

p

# CARES total score
# Predicted values of MMRM
plot <- ggemmeans(
  fit2,
  terms = c("timepoint", "treatment", "static99_modified_calc")
) |>
  plot(
    colors = lanonc_colors,
    show_data = FALSE,
    show_ci = TRUE,
    show_residuals = FALSE,
    show_title = FALSE,
    ci_style = "ribbon"
  )



ggsave(
  here::here("_figures/fig-cares-mmrm-predicted-values.png"),
  plot = plot,
  dpi = 300
)


library(emmeans)
emm <- emmeans(fit2, ~ treatment | timepoint)
emm_df <- as.data.frame(emm)

plot2 <- ggplot(emm_df, aes(x = timepoint, y = emmean, color = treatment, group = treatment)) +
  geom_line() +
  geom_point(size = 2) +
  geom_errorbar(
    aes(ymin = lower.CL, ymax = upper.CL),
    width = 0.1
  ) +
  labs(
    y = "Estimated marginal mean",
    x = "Timepoint",
    color = "Treatment"
  ) +
  theme_minimal()
