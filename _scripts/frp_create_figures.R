library(ggeffects)
library(ggplot2)
library(ggsci)
library(ggtext)
library(glue)
library(ggrain)
library(emmeans)
library(stringr)
#library(svglite)

# Set global options
options(
  ggplot2.discrete.colour = ggsci::scale_colour_lancet,
  ggplot2.discrete.fill = ggsci::scale_fill_lancet
)

add_gg_theme <- function(plt) {
  plt <- plt +
    ggplot2::theme_minimal() + 
    ggplot2::theme(
          axis.title = element_text(size = 16, face = "bold"),
          axis.text = element_text(size = 14),
          legend.title = element_text(size = 16, face = "bold"),
          legend.text = element_text(size = 14),
      )

  return(plt)
}

lanonc_colors <- pal_lancet(palette = "lanonc", alpha = 1)(9)

# ################
# PRIMARY OUTCOMES
# ################

# ------------------
# CARES total score
# ------------------

# Estimated marginal means
emm <- emmeans(fit2, ~ treatment | timepoint)
emm_df <- as.data.frame(emm)

plt <- ggplot(
    emm_df, 
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
    alpha = 0.2,
    color = treatment,
    group = treatment
  ) +
  labs(
    y = "Estimated marginal mean +- 95%-CI (CARES total score)",
    x = "Timepoint",
    color = "Treatment"
  )
  
plt <- add_gg_theme(plt)

ggsave(
  here::here("_figures/fig-cares-estimated-marginal-means.png"),
  plot = plt,
  dpi = 300,
  width = 16,
  height = 8
)

ggsave(
  here::here("_figures/fig-cares-estimated-marginal-means.pdf"),
  plot = plt,
  dpi = 300,
  width = 16,
  height = 8
)

# Estimated marginal means with ggplot2
emm <- emmeans(fit2_c, ~ treatment | timepoint)
emm_df <- as.data.frame(emm)

plt <- ggplot(
    emm_df, 
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
  labs(
    y = "Estimated marginal mean +- 95%-CI (CARES total score)",
    x = "Timepoint",
    color = "Treatment"
  )

plt <- add_gg_theme(plt)

ggsave(
  here::here("_figures/fig-cares-estimated-marginal-means-time-since-baseline.png"),
  plot = plt,
  dpi = 300,
  width = 16,
  height = 8
)

ggsave(
  here::here("_figures/fig-cares-estimated-marginal-means-time-since-baseline.pdf"),
  plot = plt,
  dpi = 300,
  width = 16,
  height = 8
)


# RAIN plot (descriptive)
plt <- ggplot(
    data = dat_clean %>% filter(!client_id %in% c(253, 396) & !is.na(client_group)),
    aes(fill = client_group, x = timepoint, y = IoD_reduced, color = client_group)
  ) +
  geom_rain(
    seed = 42,
    #id.long.var = "IoD_reduced",
    point.args = list(
      alpha = 0.3,
      shape = 21
    ),
    point.args.pos = rlang::list2(
      position = position_jitter(
        width = 0.1,
        height = 0,
      )
    ),
    boxplot.args = list(
      outlier.shape = NA,
      color = "black",
      alpha = .7
    ),
    violin.args = list(
      alpha = .2
    ),
    boxplot.args.pos = list(
      width = .1,
      position = ggpp::position_dodgenudge(
          x = c(
            -.01, -.01,
            -.01, .01,
            -.01, .01, 
            -.01, .01, 
            -.01, .01, 
            -.01, .01, 
            .01, .01
          ),
          width = 0.7
        )
    ),
    violin.args.pos = list(
      width = 1,
      side = c(
        "l", "r", # baseline
        "l", "r", # t1
        "l", "r", # t2
        "l", "r", # t3
        "l", "r", # Module 4 (post)
        "l", "r", # Module 5 (post)
        "l", "r"  # Module 6 (post)
      ),
      position = ggpp::position_dodgenudge(
        width = 0.95
      )
    )
  )

plt <- add_gg_theme(plt)

ggsave(
  here::here("_figures/fig-cares-rainplot.png"),
  plot = plt,
  dpi = 300,
  width = 16,
  height = 8,
)

ggsave(
  here::here("_figures/fig-cares-rainplot.pdf"),
  plot = plt,
  dpi = 300,
  width = 16,
  height = 8
)

# RAIN plot (difference from baseline)
plt <- ggplot(
    data = df_diff_iod_reduced %>% 
      filter(!client_id %in% c(253, 396) & !is.na(treatment)) %>%
      pivot_longer(
        cols = starts_with("diff_IoD_reduced Module"),
        names_to = "timepoint",
        values_to = "value"
      ) %>%
      mutate(
        timepoint = case_when(
          str_detect(timepoint, "Module 1") ~ "Module 1 (post)",
          str_detect(timepoint, "Module 2") ~ "Module 2 (post)",
          str_detect(timepoint, "Module 3") ~ "Module 3 (post)",
          str_detect(timepoint, "Module 4") ~ "Module 4 (post)",
          str_detect(timepoint, "Module 5") ~ "Module 5 (post)",
          str_detect(timepoint, "Module 6") ~ "Module 6 (post)"
        )
      ),
    aes(fill = treatment, x = timepoint, y = value, color = treatment)
  ) +
  geom_rain(
    seed = 42,
    #id.long.var = "IoD_reduced",
    point.args = list(
      alpha = 0.3,
      shape = 21
    ),
    boxplot.args = list(
      outlier.shape = NA,
      color = "black",
      alpha = .7
    ),
    violin.args = list(
      alpha = .2
    ),
  )

plt <- add_gg_theme(plt)

ggsave(
  here::here("_figures/fig-cares-rainplot-difference-from-baseline.png"),
  plot = plt,
  dpi = 300,
  width = 16,
  height = 8,
)

ggsave(
  here::here("_figures/fig-cares-rainplot-difference-from-baseline.pdf"),
  plot = plt,
  dpi = 300,
  width = 16,
  height = 8
)

# ------------------
# IOD total score
# ------------------

# Estimated marginal means
emm <- emmeans(fit, ~ treatment | timepoint)
emm_df <- as.data.frame(emm)

plt <- ggplot(
    emm_df, 
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
  labs(
    y = "Estimated marginal mean +- 95%-CI (IoD totalscore)",
    x = "Timepoint",
    color = "Treatment"
  )
  
plt <- add_gg_theme(plt)

ggsave(
  here::here("_figures/fig-iod-estimated-marginal-means.png"),
  plot = plt,
  dpi = 300,
  width = 16,
  height = 8
)

ggsave(
  here::here("_figures/fig-iod-estimated-marginal-means.pdf"),
  plot = plt,
  dpi = 300,
  width = 16,
  height = 8
)

# Estimated marginal means with ggplot2
emm <- emmeans(fit1_c, ~ treatment | timepoint)
emm_df <- as.data.frame(emm)

plt <- ggplot(
    emm_df, 
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
  labs(
    y = "Estimated marginal mean +- 95%-CI (IoD totalscore)",
    x = "Timepoint",
    color = "Treatment"
  )

plt <- add_gg_theme(plt)

ggsave(
  here::here("_figures/fig-iod-estimated-marginal-means-time-since-baseline.png"),
  plot = plt,
  dpi = 300,
  width = 16,
  height = 8
)

ggsave(
  here::here("_figures/fig-iod-estimated-marginal-means-time-since-baseline.pdf"),
  plot = plt,
  dpi = 300,
  width = 16,
  height = 8
)


# RAIN plot (descriptive)
plt <- ggplot(
    data = dat_clean %>% filter(!client_id %in% c(253, 396) & !is.na(client_group)),
    aes(fill = client_group, x = timepoint, y = IoD, color = client_group)
  ) +
  geom_rain(
    seed = 42,
    point.args = list(
      alpha = 0.3,
      shape = 21
    ),
    point.args.pos = rlang::list2(
      position = position_jitter(
        width = 0.1,
        height = 0,
      )
    ),
    boxplot.args = list(
      outlier.shape = NA,
      color = "black",
      alpha = .7
    ),
    violin.args = list(
      alpha = .2
    ),
    boxplot.args.pos = list(
      width = .1,
      position = ggpp::position_dodgenudge(
          x = c(
            -.01, -.01,
            -.01, .01,
            -.01, .01, 
            -.01, .01, 
            -.01, .01, 
            -.01, .01, 
            .01, .01
          ),
          width = 0.7
        )
    ),
    violin.args.pos = list(
      width = 1,
      side = c(
        "l", "r", # baseline
        "l", "r", # t1
        "l", "r", # t2
        "l", "r", # t3
        "l", "r", # Module 4 (post)
        "l", "r", # Module 5 (post)
        "l", "r"  # Module 6 (post)
      ),
      position = ggpp::position_dodgenudge(
        width = 0.95
      )
    )
  )

plt <- add_gg_theme(plt)

ggsave(
  here::here("_figures/fig-iod-rainplot.png"),
  plot = plt,
  dpi = 300,
  width = 16,
  height = 8,
)

ggsave(
  here::here("_figures/fig-iod-rainplot.pdf"),
  plot = plt,
  dpi = 300,
  width = 16,
  height = 8
)

# RAIN plot (difference from baseline)
plt <- ggplot(
    data = df_diff_iod %>% 
      filter(!client_id %in% c(253, 396) & !is.na(treatment)) %>%
      pivot_longer(
        cols = starts_with("diff_IoD Module"),
        names_to = "timepoint",
        values_to = "value"
      ) %>%
      mutate(
        timepoint = case_when(
          str_detect(timepoint, "Module 1") ~ "Module 1 (post)",
          str_detect(timepoint, "Module 2") ~ "Module 2 (post)",
          str_detect(timepoint, "Module 3") ~ "Module 3 (post)",
          str_detect(timepoint, "Module 4") ~ "Module 4 (post)",
          str_detect(timepoint, "Module 5") ~ "Module 5 (post)",
          str_detect(timepoint, "Module 6") ~ "Module 6 (post)"
        )
      ),
    aes(fill = treatment, x = timepoint, y = value, color = treatment)
  ) +
  geom_rain(
    seed = 42,
    #id.long.var = "IoD_reduced",
    point.args = list(
      alpha = 0.3,
      shape = 21
    ),
    boxplot.args = list(
      outlier.shape = NA,
      color = "black",
      alpha = .7
    ),
    violin.args = list(
      alpha = .2
    ),
  )

plt <- add_gg_theme(plt)

ggsave(
  here::here("_figures/fig-iod-rainplot-difference-from-baseline.png"),
  plot = plt,
  dpi = 300,
  width = 16,
  height = 8,
)

ggsave(
  here::here("_figures/fig-iod-rainplot-difference-from-baseline.pdf"),
  plot = plt,
  dpi = 300,
  width = 16,
  height = 8
)


# ############
# SECONDARY OUTCOMES
# ############
