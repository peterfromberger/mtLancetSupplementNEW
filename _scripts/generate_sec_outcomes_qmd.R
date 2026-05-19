# generate_qmd.R
# Dieses Script generiert automatisch das Quarto-Dokument für die sekundären Endpunkte.
library(glue)

# =============================================================================
# Caption-Templates pro model_type (nur 1x definiert)
# =============================================================================

# tbl_cap Templates
tbl_cap_templates <- list(
  single_fit    = 'paste0("{model_label} for the {full_name} (", q_name, ") with n = ", as.character(nobs(fit)), " observations.")',
  two_fit_outlier = 'paste0("{model_label} for the {full_name} (", q_name, ") with n = ", as.character(nobs(fit1)), " observations.")',
  two_fit_compare = 'paste0("{model_label} for the {full_name} (", q_name, " with n = ", as.character(nobs(fit1)), " observations.")',
  ordinal       = 'paste0("{model_label} for the {full_name} (", q_name, ") with n = ", as.character(nobs(fit1)), " observations.{note_suffix}")',
  logistic      = 'paste0("{model_label} for the {full_name} (", q_name, ") with n = ", as.character(nobs(fit1)), " observations.{note_suffix}")',
  no_table      = 'paste0("{model_label} for the {full_name} (", q_name, ") with n = ", as.character(nobs(fit)), " observations.")'
)

# fig_cap Templates
fig_cap_templates <- list(
  single_fit      = 'paste("Diagnostic plots for linear regression models with (", q_name, ").")',
  two_fit_outlier = 'paste("Diagnostic plots for linear regression models with (", q_name, "). The plots on the left correspond to the model fitted with all data points, the plots on the right correspond to the model after the removal of identified outliers. Both models showed similar results.")',
  two_fit_compare = 'paste("Diagnostic plots for linear regression models with (", q_name, ").{fig_note}")',
  ordinal         = 'paste("Diagnostic plot for a linear regression models with (", q_name, "). Diagnostic plots indicate violations of the normality assumption; therefore, the model was replaced with an ordinal regression model.")',
  logistic        = 'paste("Diagnostic plot for a linear regression models with (", q_name, "). Diagnostic plots indicate violations of the normality assumption; therefore, the model was replaced with a logistic regression model.")',
  no_table        = 'paste("Diagnostic plots for linear regression models with (", q_name, ".)")'
)

# model_label pro model_type
model_labels <- list(
  single_fit      = "Linear regression model",
  two_fit_outlier = "Linear regression model",
  two_fit_compare = "Linear regression model",
  ordinal         = "Ordinal regression model",
  logistic        = "Logistic regression model",
  no_table        = "Linear regression model"
)

# =============================================================================
# Funktion: Ergänzt automatisch alle abgeleiteten Felder
# =============================================================================

complete_section <- function(s) {
  # var_nice aus var ableiten
  if (is.null(s$var_nice))      s$var_nice <- gsub("_", "-", s$var)
  # comment = heading falls nicht gesetzt
  if (is.null(s$comment))       s$comment <- s$heading
  # rainplot_ref
  if (is.null(s$rainplot_ref))  s$rainplot_ref <- paste0("fig-sec-rainplot-", s$var_nice)
  # diag_ref
  if (is.null(s$diag_ref))      s$diag_ref <- paste0("fig-sec-diag-", s$var_nice)
  # tbl_save_prefix
  if (is.null(s$tbl_save_prefix) && s$model_type != "no_table") {
    s$tbl_save_prefix <- paste0("tbl-sec-lm-", s$var_nice)
  }
  
  mt <- s$model_type
  fn <- s$full_name
  qn <- s$q_name
  module <- s$module
  note <- if (!is.null(s$note)) s$note else ""
  note_suffix <- if (nchar(note) > 0) paste0(" ", note) else ""
  fig_note <- if (!is.null(s$fig_note)) paste0(" ", s$fig_note) else ""
  model_label <- model_labels[[mt]]
  
  # Lokales Environment für glue erstellen
  env <- list2env(list(
    model_label = model_label,
    full_name = fn,
    note_suffix = note_suffix,
    fig_note = fig_note,
    module = module
  ), parent = emptyenv())

  # rainplot_cap
  if (is.null(s$rainplot_cap)) {
    s$rainplot_cap <- paste0("Distribution of the ", fn, " (", qn, ") scores by trial arms. Shown are scores before (pre) and after (post) ", module ,". Red denotes to the intervention arm and blue denotes the placebo arm. For each timepoint, violin plots illustrate the score distributions, overlaid with boxplots indicating the median and interquartile range, and jittered points representing individual participants scores. Solid lines connect the group means across timepoints, indicating mean trajectories over time.")
  }
  
  # diag_cap
if (is.null(s$diag_cap)) {
  s$diag_cap <- paste0("Diagnostic plots for the ", fn, " (", qn, ").")
  if (model_label == "Logistic regression model") {
    s$diag_cap <- paste0(s$diag_cap, " The plots indicate violations of the normality assumption; therefore, the model was replaced with a logistic regression model.")
  }
  if (model_label == "Ordinal regression model") {
    s$diag_cap <- paste0(s$diag_cap, " The plots indicate violations of the normality assumption; therefore, the model was replaced with an ordinal regression model.")
  }
  if (mt == "two_fit_compare") {
    s$diag_cap <- paste0(s$diag_cap, " The plots on the left correspond to an ordinary linear model, the plots on the right correspond to the model where the score was log-transformed. Both models showed similar results.")
  }
  if (mt == "two_fit_outlier") {
    s$diag_cap <- paste0(s$diag_cap, " The plots on the left correspond to the model fitted with all data points, the plots on the right correspond to the model after the removal of identified outliers. Both models showed similar results.")
  }
  if (mt == "single_fit") {
    s$diag_cap <- paste0(s$diag_cap, " The plots provide no indication of violations of the normality assumption.")
  }
}
  
  # tbl_cap_template
  if (is.null(s$tbl_cap_template)) {
    s$tbl_cap_template <- glue(tbl_cap_templates[[mt]], .envir = env)
  }
  
  # fig_cap_template
  if (is.null(s$fig_cap_template)) {
    s$fig_cap_template <- glue(fig_cap_templates[[mt]], .envir = env)
  }
  
  s
  }

# =============================================================================
# Minimale Sektionsdefinitionen – jeder Text nur 1x
# =============================================================================

sections_raw <- list(
  list(
    var = "cvtrq_calc_total",
    q_name = "CVTRQ, total score",
    heading = "CVTRQ",
    full_name = "Corrections Victoria Treatment Readiness Questionnaire",
    module = "module one (motivation)",
    model_type = "single_fit",
    fit_var = "fit_cvtrq_calc_total",
    lm_table_var = "lm_Table_cvtrq_calc_total"
  ),
  list(
    var = "rcq_calc_total",
    q_name = "RCQ, total score",
    heading = "RCQ",
    full_name = "Readiness to Change Questionnaire",
    module = "module one (motivation)",
    model_type = "two_fit_outlier",
    fit1_var = "fit_rcq_calc_total",
    fit2_var = "fit_rcq_calc_total3",
    lm_table_var = "lm_Table_rcq_calc_total"
  ),
  list(
    var = "fsozu_calc_total",
    q_name = "F-Soz-U, total score",
    heading = "F-Soz-U",
    full_name = "Social Support Questionnaire",
    module = "module two (supervision and social relationship)",
    model_type = "single_fit",
    fit_var = "fit_fsozu_calc_total",
    lm_table_var = "lm_Table_fsozu_calc_total"
  ),
  list(
    var = "ors_calc_total",
    q_name = "OQMPR, total score",
    heading = "OQMPR",
    full_name = "Questionnaire for the Measurement of Psychological Reactance",
    module = "module two (supervision and social relationship)",
    model_type = "single_fit",
    fit_var = "fit_ors_calc_total",
    lm_table_var = "lm_Table_ors_calc_total"
  ),
  list(
    var = "soi_total_score",
    q_name = "SOI-R, subscale desire for sexual activity with children",
    heading = "SOI-R",
    full_name = "Sexual Outlet Inventory Revised",
    module = "module six (sexuality)",
    model_type = "ordinal",
    lm_table_var = "glm_Table_soi_calc_total_ordinal_diff",
    fit1_var = "fit_soi_calc_total_ordinal_diff",
    fit2_var = "fit_soi_calc_total",
    note = "Note, that the dependent variable has 3 ordered categories: worsening (increase), no change, improvement (decrease)."
  ),
  list(
    var = "ucla_calc_total",
    q_name = "UCLA, total score",
    heading = "UCLA",
    full_name = "UCLA Loneliness Scale",
    module = "module two (supervision and social relationship)",
    model_type = "two_fit_outlier",
    fit1_var = "fit_ucla_calc_total",
    fit2_var = "fit_ucla_calc_total3",
    lm_table_var = "lm_Table_ucla_calc_total"
  ),
  list(
    var = "bis_calc_total",
    q_name = "BIS-15, total score",
    heading = "BIS-15",
    full_name = "Barratt Impulsiveness Scale-15",
    module = "module three (emotion management)",
    model_type = "two_fit_compare",
    fit1_var = "fit_bis_calc_total",
    fit2_var = "fit_bis_calc_total2",
    lm_table_var = "lm_Table_bis_calc_total",
    fig_note = "The plots on the left correspond to an ordinary linear model, the plots on the right correspond to the model where the score was log-transformed. Both models showed similar results."
  ),
  list(
    var = "cusi_calc_total",
    q_name = "CUSI, total score",
    heading = "CUSI",
    full_name = "Coping Using Sex Inventory",
    module = "module two (supervision and social relationship)",
    model_type = "two_fit_outlier",
    fit1_var = "fit_cusi_calc_total",
    fit2_var = "fit_cusi_calc_total3",
    lm_table_var = "lm_Table_cusi_calc_total"
  ),
  list(
    var = "spsi_calc_total",
    q_name = "SPSI-R, total score",
    heading = "SPSI-R",
    full_name = "Social Problem-Solving Inventory Revised",
    module = "module four (problem solving)",
    model_type = "single_fit",
    fit_var = "fit_spsi_calc_total",
    lm_table_var = "lm_Table_spsi_calc_total"
  ),
  list(
    var = "ekk_calc_total",
    q_name = "EKK-R, total score",
    heading = "EKK-R",
    full_name = "Emotional Congruence with Children-Revised",
    module = "module six (sexuality)",
    model_type = "single_fit",
    fit_var = "fit_ekk_calc_total",
    lm_table_var = "lm_Table_ekk_calc_total"
  ),
  list(
    var = "ders_calc_imp",
    q_name = "DERS, subscore impulsivity",
    heading = "DERS",
    full_name = "Difficulties in Emotion Regulation Scale",
    module = "module three (emotion management)",
    model_type = "ordinal",
    fit1_var = "fit_ders_calc_imp_ordinal",
    fit2_var = "fit_ders_calc_imp",
    lm_table_var = "glm_Table_ders_calc_imp",
    note = "Note, that for the dependent variable, values greater than 9 were merged into a single category.",
    tbl_save_prefix = "tbl-sec-lm-ders-calc-total"
  ),
  list(
    var = "narq_calc_ris",
    q_name = "NARQ, subscale externalizing strategies",
    heading = "NARQ",
    full_name = "Negative Affect Repair Questionnaire",
    module = "module three (emotion management)",
    model_type = "ordinal",
    fit1_var = "fit_narq_calc_ris_ordinal",
    fit2_var = "fit_narq_calc_ris",
    lm_table_var = "glm_Table_narq_calc_ris",
    note = "Note, that for the dependent variable, values greater than 4 were merged into a single category.",
    tbl_save_prefix = "tbl-sec-lm-narq-calc-total"
  ),
  list(
    var = "esiq_calc_total_child",
    q_name = "ESIQ, subsale child",
    heading = "ESIQ",
    full_name = "Explicit Sexual Interest Questionnaire",
    module = "module six (sexuality)",
    model_type = "ordinal",
    fit1_var = "fit_esiq_calc_total_child_ordinal_diff",
    fit2_var = "fit_esiq_calc_total_child",
    lm_table_var = "glm_Table_esiq_calc_total_child_ordinal_diff",
    note = "Note, that the dependent variable has 3 ordered categories: worsening (increase), no change, improvement (decrease)."
  ),
  list(
    var = "kvm_score",
    q_name = "BMS, total score",
    heading = "BMS",
    full_name = "Bumby Molest Scale",
    module = "module five (offense-supportive attitudes)",
    model_type = "logistic",
    fit1_var = "fit_kvm_score_logistic",
    fit2_var = "fit_kvm_score",
    lm_table_var = "glm_Table_kvm_score",
    note = "Note, that for the dependent variable, values were dichotomized based on the cut-off greater or equal to 43.",
    tbl_save_prefix = "tbl-sec-lm-bms-calc-total"
  ),
  list(
    var = "hbi_calc_total",
    q_name = "HBI-19, total score",
    heading = "HBI-19",
    full_name = "Hypersexual Behavior Inventory-19",
    module = "module six (sexuality)",
    model_type = "logistic",
    fit1_var = "fit_hbi_calc_total_logistic",
    fit2_var = "fit_hbi_calc_total",
    lm_table_var = "glm_Table_hbi_calc_total",
    note = "Note, that for the dependent variable, values were dichotomized based on the cut-off greater or equal to 24."
  ),
  list(
    var = "ssik_calc_total",
    q_name = "SSIC, total score",
    heading = "SSIC",
    full_name = "Specific self-efficacy for modifying Sexual Interest in Children",
    module = "module six (sexuality)",
    model_type = "logistic",
    fit1_var = "fit_ssik_calc_total_logistic",
    fit2_var = "fit_ssik_calc_total",
    lm_table_var = "glm_Table_ssik_calc_total",
    note = "Note, that for the dependent variable, values were dichotomized based on the cut-off greater or equal to 30.",
    tbl_save_prefix = "tbl-sec-lm-ssic-calc-total"
  )
)

# =============================================================================
# Alle Sektionen vervollständigen
# =============================================================================

sections <- lapply(sections_raw, complete_section)

# =============================================================================
# Header-Block generieren (einmalig am Anfang)
# =============================================================================

generate_header <- function() {
  '### Differences from baseline

```{r}
#| echo: false
#| message: false
#| warning: false
#| label: init-tbl-sec-diffscore

library(here)
library(dplyr)
library(purrr)
library(rlang)
library(gtsummary)
library(gt)
library(glue)

source(here::here("_scripts/sec_diffscore_functions.R"))
source(here::here("_scripts/sec_pairwise_functions.R"))

# global theme for gtsummary
gtsummary::theme_gtsummary_journal(journal = "lancet")


dvs_mbsb <- c(
  "cvtrq_calc_total",
  "rcq_calc_total",
  "fsozu_calc_total",
  "ors_calc_total",
  "ucla_calc_total",
  "bis_calc_total",
  "cusi_calc_total",
  "ders_calc_imp",
  "narq_calc_ris",
  "spsi_calc_total",
  "kvm_score",
  "esiq_calc_total_child",
  "soi_total_score",
  "ssik_calc_total",
  "ekk_calc_total",
  "hbi_calc_total"
)

dvs <- c(
  "cvtrq_calc_total",
  "rcq_calc_total",
  "fsozu_calc_total",
  "ors_calc_total",
  "ucla_calc_total",
  "bis_calc_total",
  "cusi_calc_total",
  "ders_calc_imp",
  "narq_calc_ris",
  "spsi_calc_total",
  "kvm_calc_total",
  "esiq_calc_total_child",
  "soi_total_score",
  "ssik_calc_total",
  "ekk_calc_total",
  "hbi_calc_total"
  
)

labels <- c(
  "CVTRQ (total score)",
  "RCQ (total score)",
  "F-Soz-U (total score)",
  "OQMPR (total score)",
  "UCLA (total score)",
  "BIS-15 (total score)",
  "CUSI (total score)",
  "DERS (subscale impulsivity)",
  "NARQ (subscale externalizing strategies)",
  "SPSI-R (total score)",
  "BMS (total score)",
  "ESIQ (subscale child)",
  "SOI (Item 2a)",
  "SSIC (total score)",
  "EKK-R (total score)",
  "HBI-19 (total score)"
)

types <- c(
  "continuous2",
  "continuous2",
  "continuous2",
  "continuous2",
  "continuous2",
  "continuous2",
  "continuous2",
  "continuous2",
  "continuous2",
  "continuous2",
  "continuous2",
  "continuous2",
  "continuous2",
  "continuous2",
  "continuous2",
  "continuous2"
)

abbreviations <- c(
  "*CVTRQ* = Corrections Victoria Treatment Readiness Questionnaire [@caseyAssessingSuitabilityOffender2007]",
  "*RCQ* = Readiness to Change Questionnaire [@rollnickDevelopmentShortReadiness1992] - German version [@hannoverReadinessChangeQuestionnaire2002] adapted for individuals who committed sexual offenses against children by project team",
  "*F-Soz-U* = Seven-item short version of the Social Support Questionnaire [@dunkelEvaluationShortformSocial2005]",
  "*OQMPR* = Questionnaire for the Measurement of Psychological Reactance [@merzQuestionnaireMeasurementPsychological1983]",
  "*UCLA* = UCLA Loneliness Scale [@russell_ucla_1996] - German short version [@bilskySocialSupportLoneliness1998]",
  "*BIS-15* = Barratt Impulsiveness Scale-15 [@pattonFactorStructureBarratt1995]",
  "*CUSI* = Coping Using Sex Inventory [@cortoniSexCopingStrategy2001]",
  "*DERS* = Difficulties in Emotion Regulation Scale [@gratzMultidimensionalAssessmentEmotion2003]",
  "*NARQ* = Negative Affect Repair Questionnaire [@schererNegativeAffectRepair2013]",
  "*SPSI-R* = Social Problem-Solving Inventory Revised [@dzurillaManualSocialProblemSolving2002]",
  "*BMS* = Bumby Molest Scale [@bumbyAssessingCognitiveDistortions]",
  "*EKK-R* = Questionnaire on Emotional Congruence with Children-Revised [@mackEmotionaleKongruenzMit2012]",
  "*ESIQ* = Explicit Sexual Interest Questionnaire [@banseIndirectMeasuresSexual2010]",
  "*SOI-R* = Sexual Outlet Inventory revised, subscale desire for sexual activity with children [@bbrikenSexualOutletInventory2010]",
  "*HBI-19* = Hypersexual Behavior Inventory-19 [@kleinValidierungsstudieDeutschenVersion2013]",
  "*SSIC* = Specific self-efficacy for modifying Sexual Interest in Children [@tozdanSpezifischeSelbstwirksamkeitZur2015]"
)
```


```{r}
#| echo: false
#| message: false
#| warning: false
#| label: tbl-sec-diffscore
#| tbl-cap: "Difference between before (pre) and after (post) for each secondary endpoint. Differences (from pre to post) of secondary endpoints have been compared between the treatment groups using the Brunner-Munzel test for unpaired data."

mbsb <- TRUE
sample_str <- "itt"

tbl <- create_tbl_diffscore_bm_unpaired(mbsb, sample_str, dvs, dvs_mbsb, labels, types, abbreviations)

# save tbl as image
gt::gtsave(tbl, file = file.path(here::here("_tables/tbl-sec-diffscore.pdf")))
gt::gtsave(tbl, file = file.path(here::here("_tables/tbl-sec-diffscore.docx")))
gt::gtsave(tbl, file = file.path(here::here("_tables/tbl-sec-diffscore.png")))

tbl
```
  
'
}

# =============================================================================
# Init-Block generieren
# =============================================================================

generate_init_block <- function(s) {
  label_suffix <- gsub("_", "-", s$var)
  
  if (s$model_type == "single_fit" || s$model_type == "no_table") {
    fit_assignment <- glue('fit <- {s$fit_var}')
  } else {
    fit_assignment <- glue('fit1 <- {s$fit1_var}\nfit2 <- {s$fit2_var}')
  }
  
  if (!is.null(s$lm_table_var)) {
    lm_table_line <- glue('lm_table <- {s$lm_table_var}')
  } else {
    lm_table_line <- '#lm_table <- lm_Table_soi_calc_total'
  }
  
  glue('
<!-- {s$comment} -->

```{{r}}
#| echo: false
#| message: false
#| warning: false
#| label: sec-init-{label_suffix}
var <- "{s$var}"
var_nice <- "{s$var_nice}"
q_name <- "{s$q_name}"
{fit_assignment}
{lm_table_line}
module <- "{s$module}"
tbl_cap <- {s$tbl_cap_template}
fig_cap <- {s$fig_cap_template}
```
  
')
}

# =============================================================================
# Diagnostic-Plot-Block generieren
# =============================================================================

generate_diag_block <- function(s) {
  use_add_gg_theme <- is.null(s$no_add_gg_theme) || !s$no_add_gg_theme
  add_theme_line <- if (use_add_gg_theme) "plt <- add_gg_theme(plt)" else ""
  
  if (s$model_type == "single_fit" || s$model_type == "no_table") {
    plot_code <- glue('
library(ggResidpanel)
library(knitr)

plt <- ggResidpanel::resid_panel(fit, plots=c("qq", "resid"))
{add_theme_line}')
    plot_width <- 16
    plot_height <- 8
  } else if (s$model_type %in% c("ordinal", "logistic")) {
    plot_code <- glue('
library(ggResidpanel)
plt <- ggResidpanel::resid_panel(fit2, plots=c("qq", "resid"))
{add_theme_line}')
    plot_width <- 16
    plot_height <- 8
  } else {
    plot_code <- glue('
library(ggResidpanel)
plt <- ggResidpanel::resid_compare(list(fit1, fit2), plots = c("qq", "resid"))
{add_theme_line}')
    plot_width <- 16
    plot_height <- 16
  }
  
  plot_code <- gsub("\n\n\n", "\n\n", plot_code)
  
  glue('

```{{r}}
#| label: fig-sec-lm-{s$var_nice}
#| echo: false
#| message: false
#| warning: false
{plot_code}

name_png <- glue("_figures/fig-sec-diagnostic-plots-{{var_nice}}.png")
name_pdf <- glue("_figures/fig-sec-diagnostic-plots-{{var_nice}}.pdf")

ggsave(
    here::here(name_png),
    plot = plt,
    dpi = 300,
    width = {plot_width},
    height = {plot_height},
)

ggsave(
    here::here(name_pdf),
    plot = plt,
    dpi = 300,
    width = {plot_width},
    height = {plot_height}
)
```
  
')
}

# =============================================================================
# Table-Block generieren
# =============================================================================

generate_table_block <- function(s) {
  if (s$model_type == "no_table") {
    return(glue('

```{{r}}
#| label: tbl-sec-lm-{s$var_nice}
#| echo: false
#| message: false
#| warning: false
#| tbl-cap: !expr \'tbl_cap\'

#lm_table
```
    
'))
  }
  
  glue('
```{{r}}
#| label: tbl-sec-lm-{s$var_nice}
#| echo: false
#| message: false
#| warning: false
#| tbl-cap: !expr \'tbl_cap\'

tbl <- lm_table %>%
    as_gt() %>%
    gt::tab_options(
      table.font.names = "Times New Roman",
          table.font.size = 10,
          quarto.use_bootstrap = FALSE,
          quarto.disable_processing = TRUE,
          data_row.padding = px(1),
          summary_row.padding = gt::px(1),
          grand_summary_row.padding = gt::px(1),
          row_group.padding = gt::px(1)
    ) %>%
  tab_style(style = cell_text(align = "left"), locations = cells_source_notes()) %>%
  tab_style(style = cell_text(align = "left"), locations = cells_footnotes())

# save tbl as image
gt::gtsave(tbl, file = file.path(here::here("_tables/{s$tbl_save_prefix}.pdf")))
gt::gtsave(tbl, file = file.path(here::here("_tables/{s$tbl_save_prefix}.docx")))
gt::gtsave(tbl, file = file.path(here::here("_tables/{s$tbl_save_prefix}.png")))

tbl
```
  
')
}

# =============================================================================
# Komplette Sektion generieren
# =============================================================================

generate_section <- function(s) {
  init <- generate_init_block(s)
  diag <- generate_diag_block(s)
  tbl <- generate_table_block(s)
  
  glue('
{init}
  
{{{{< pagebreak >}}}}

### {s$heading}

<!-- RAIN PLOT -->

![{s$rainplot_cap}](_figures/fig-sec-rainplot-{s$var_nice}.png){{#{s$rainplot_ref}}}

{diag}

![{s$diag_cap}](_figures/fig-sec-diagnostic-plots-{s$var_nice}.png){{#{s$diag_ref}}}

{tbl}
  
')
}

# =============================================================================
# Alles zusammenbauen und schreiben
# =============================================================================
library(here)
output_file <- here::here("_includes/apx_SECONDARY_OUTCOMES.qmd")

# Header
header <- generate_header()

# Sektionen
all_sections <- paste(sapply(sections, generate_section), collapse = "\n")

# Zusammensetzen
full_content <- paste0(header, "\n", all_sections)

# Schreiben
writeLines(full_content, output_file)

cat("QMD file generated:", output_file, "\n")