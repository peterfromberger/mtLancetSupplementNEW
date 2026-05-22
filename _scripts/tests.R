create_prim_pairwise_tbl <- function(data, dvs, label_list, type_list, abbreviations) {

  tbl <- data %>%
    dplyr::select(all_of(dvs), timepoint) %>%
    tbl_summary(
      by = timepoint,
      type = type_list,
      statistic = all_continuous() ~ c(
            "{mean} ({sd})",
            "{median} ({p25}, {p75})",
            "{min}, {max}"
          ),
      missing_text = "Missing",
      label = label_list
    ) %>%
    # brunner-munzel test
    add_stat(fns = everything() ~ bm_paired_test) %>%
    add_q(method = "holm") %>%
    modify_header(
      label = "**Module**",
      p.value = "**p**",
      q.value = "**q**"
    ) %>%
    bold_labels() %>%
    # damit quarto citations im footer erkeent, dürfen keine footnotes vorhanden sein!
    remove_footnote_header(columns = "q.value") %>%
    # abbreviations
    modify_abbreviation("Q1 = 25th percentile, Q3 = 75th percentile.")


  tbl <- tbl %>%
    as_gt() %>%
    gt::tab_options(
      table.font.names = "Times New Roman",
          table.font.size = 10,
          quarto.use_bootstrap = FALSE,
          quarto.disable_processing = TRUE,
          data_row.padding = px(1),
          summary_row.padding = gt::px(1),
          grand_summary_row.padding = gt::px(1),
          #footnotes.padding = gt::px(2),
          #source_notes.padding = gt::px(2),
          row_group.padding = gt::px(1)
    ) %>%
  tab_style(style = cell_text(align = "left"), locations = cells_source_notes()) %>%
  tab_style(style = cell_text(align = "left"), locations = cells_footnotes())

  return(tbl)

}

subgroup_str = "Intervention"

dvs_mbsb <- c(
  "module_1",
  "module_2",
  "module_3",
  "module_4",
  "module_5",
  "module_6"
)

labels <- c(
  "Module 1",
  "Module 2",
  "Module 3",
  "Module 4",
  "Module 5",
  "Module 6"
)

types <- rep("continuous2", 6)

abbreviations <- c("")
variable <- "who_calc_total"
grouping_var <- "treatment"
mbsb <- TRUE

if (!mbsb) {
  stop("Only mbsb = TRUE is implemented")
}

# Daten erstellen
if (grepl("reduced", dvs_mbsb[1])) {
  dat <- create_who_pairwise_dataframe(paste0(variable, "_reduced"))
} else {
  dat <- create_who_pairwise_dataframe(variable)
}

# nach Gruppen splitten (treatment bleibt erhalten!)
dat_i <- dat %>% 
  dplyr::filter(treatment == "Intervention")

dat_p <- dat %>% 
  dplyr::filter(treatment == "Placebo")

# Spaltennamen anpassen
data_intervention <- rename_colnames_iod(
  data = dat_i,
  dv = dvs_mbsb[1],
  grouping_var
)

data_placebo <- rename_colnames_iod(
  data = dat_p,
  dv = dvs_mbsb[1],
  grouping_var
)

# label und type listen
label_list <- purrr::map2(dvs_mbsb, labels, ~rlang::new_formula(rlang::sym(.x), .y))
type_list <- purrr::map2(dvs_mbsb, types, ~rlang::new_formula(rlang::sym(.x), .y))

# richtige Gruppe auswählen
if (subgroup_str == "Intervention") {
  tbl <- create_prim_pairwise_tbl(
    data_intervention,
    dvs_mbsb,
    label_list,
    type_list,
    abbreviations
  )
} else if (subgroup_str == "Placebo") {
  tbl <- create_prim_pairwise_tbl(
    data_placebo,
    dvs_mbsb,
    label_list,
    type_list,
    abbreviations
  )
} else {
  stop("subgroup_str must be either 'Intervention' or 'Placebo'")
}

tbl_placebo <- data_placebo %>%
    dplyr::select(all_of(dvs_mbsb), timepoint) %>%
    tbl_summary(
      by = timepoint,
      type = type_list,
      statistic = all_continuous() ~ c(
            "{mean} ({sd})",
            "{median} ({p25}, {p75})",
            "{min}, {max}"
          ),
      missing_text = "Missing",
      label = label_list
    ) %>%
    # brunner-munzel test
    add_stat(fns = everything() ~ bm_paired_test) %>%
    add_q(method = "holm") %>%
    modify_header(
      label = "**Module**",
      p.value = "**p**",
      q.value = "**q**"
    ) %>%
    bold_labels() %>%
    # damit quarto citations im footer erkeent, dürfen keine footnotes vorhanden sein!
    remove_footnote_header(columns = "q.value") %>%
    # abbreviations
    modify_abbreviation("Q1 = 25th percentile, Q3 = 75th percentile.")

tbl_intervention <- data_intervention %>%
    dplyr::select(all_of(dvs_mbsb), timepoint) %>%
    tbl_summary(
      by = timepoint,
      type = type_list,
      statistic = all_continuous() ~ c(
            "{mean} ({sd})",
            "{median} ({p25}, {p75})",
            "{min}, {max}"
          ),
      missing_text = "Missing",
      label = label_list
    ) %>%
    # brunner-munzel test
    add_stat(fns = everything() ~ bm_paired_test) %>%
    add_q(method = "holm") %>%
    modify_header(
      label = "**Module**",
      p.value = "**p**",
      q.value = "**q**"
    ) %>%
    bold_labels() %>%
    # damit quarto citations im footer erkeent, dürfen keine footnotes vorhanden sein!
    remove_footnote_header(columns = "q.value") %>%
    # abbreviations
    modify_abbreviation("Q1 = 25th percentile, Q3 = 75th percentile.")
