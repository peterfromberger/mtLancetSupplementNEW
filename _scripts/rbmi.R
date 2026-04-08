library(rbmi)
library(tibble)

### IoD
fit <- mmrm(
  formula = IoD ~ `IoD_baseline` + treatment*timepoint + `Indexdelikt` + `Aktuelle_Betreuung` + `Aktuelle_zusatzliche_Behandlung` +
    ad(timepoint | client_id),
  data = dat_clean
)

dat_clean2 <- dat_clean %>% tidyr::drop_na(`IoD_baseline`, treatment, `Indexdelikt`, `Aktuelle_Betreuung`, `Aktuelle_zusatzliche_Behandlung`, `static99_modified_calc`, timepoint, client_id)
# remove clients with missing baseline values or treatment:
dat_clean2 <- dat_clean2 %>% filter(client_id %in% (dat_clean2 %>% filter(timepoint=="Baseline") %>% .$client_id) & !client_id %in% c(533, 253, 396))  %>%
  filter(!timepoint=="Baseline") %>%
  mutate(client_id = droplevels(client_id),
         timepoint = droplevels(timepoint))

# Use expand_locf to add rows corresponding to visits with missing outcomes to the dataset
dat_clean2 <- expand_locf(
  dat_clean2,
  client_id = levels(dat_clean2$client_id), # expand by PATIENT and VISIT
  timepoint = levels(dat_clean2$timepoint),
  vars = c("IoD_baseline", "treatment", "Indexdelikt", "Aktuelle_Betreuung", "Aktuelle_zusatzliche_Behandlung", "static99_modified_calc"), # fill with LOCF BASVAL and THERAPY
  group = c("client_id"),
  order = c("client_id", "timepoint")
)

dat_ice <- dat_clean2 %>%
  arrange(client_id, timepoint) %>%
  filter(is.na(IoD)) %>%
  group_by(client_id) %>%
  dplyr::slice(1) %>%
  ungroup() %>%
  select(client_id, timepoint) %>%
  mutate(strategy = "JR")

vars <- set_vars(
  outcome = "IoD",
  visit = "timepoint",
  subjid = "client_id",
  group = "treatment",
  covariates = c("IoD_baseline", "treatment*timepoint", "Indexdelikt", "Aktuelle_Betreuung", "Aktuelle_zusatzliche_Behandlung", "static99_modified_calc")
)
# Define which imputation method to use (here: Bayesian multiple imputation with 150 imputed datsets)
method <- method_condmean(n_samples = 20, covariance = "us")

set.seed(987)
drawObj <- draws(
  data = dat_clean2,
  data_ice = dat_ice,
  vars = vars,
  method = method,
  quiet = TRUE
)

drawObj

imputeObj <- impute(
  drawObj,
  references = c("Intervention" = "Intervention", "Placebo" = "Placebo")
)
imputeObj

ancova_modified <- function(data, ...) {
  data2 <- data %>% mutate(IoD = IoD - IoD_baseline)
  rbmi::ancova(data2, ...)
}

anaObj <- analyse(
  imputeObj,
  ancova_modified,
  vars = set_vars(
    subjid = "client_id",
    outcome = "IoD",
    visit = "timepoint",
    group = "treatment",
    covariates = c("IoD_baseline", "Indexdelikt", "Aktuelle_Betreuung", "Aktuelle_zusatzliche_Behandlung", "static99_modified_calc")
  )
)
anaObj

poolObj <- pool(
  anaObj,
  conf.level = 0.95,
  alternative = "two.sided"
)
poolObj

rbmi_tab <- poolObj %>% tibble::as_tibble() %>%
  mutate(
    # Erst Gruppe extrahieren, bevor wir die Labels bereinigen
    group = stringr::str_extract(parameter, "Module [0-9]+"),
    parameter = parameter %>%
      gsub("trt_", "Group differences ", .) %>% 
      gsub("lsm_ref_", "Least square mean [intervention] ", .) %>% 
      gsub("lsm_alt_", "Least square mean [placebo] ", .) %>%
      gsub(" Module [0-9]+ [(]post[)]", "", .),
    # Lancet p-Wert Formatierung
    pval = case_when(
      pval < 0.001 ~ "<0\u00b7001",
      pval < 0.01  ~ as.character(round(pval, 3)) %>% gsub("\\.", "\u00b7", .),
      TRUE         ~ as.character(round(pval, 2))  %>% gsub("\\.", "\u00b7", .)
    ),
    ci = paste0("(", 
                gsub("\\.", "\u00b7", sprintf("%.2f", lci)), 
                ", ", 
                gsub("\\.", "\u00b7", sprintf("%.2f", uci)), 
                ")"),
    est = gsub("\\.", "\u00b7", sprintf("%.2f", est))
  ) %>%
  dplyr::select(parameter, group, est, ci, pval)


### CARES
fit_cares <- mmrm(
  formula = IoD_reduced ~ `IoD_reduced_baseline` + treatment*timepoint + `Indexdelikt` + `Aktuelle_Betreuung` + `Aktuelle_zusatzliche_Behandlung` +
    ad(timepoint | client_id),
  data = dat_clean
)

dat_clean2 <- dat_clean %>% tidyr::drop_na(`IoD_reduced_baseline`, treatment, `Indexdelikt`, `Aktuelle_Betreuung`, `Aktuelle_zusatzliche_Behandlung`, `static99_modified_calc`, timepoint, client_id)
# remove clients with missing baseline values or treatment:
dat_clean2 <- dat_clean2 %>% filter(client_id %in% (dat_clean2 %>% filter(timepoint=="Baseline") %>% .$client_id) & !client_id %in% c(533, 253, 396))  %>%
  filter(!timepoint=="Baseline") %>%
  mutate(client_id = droplevels(client_id),
         timepoint = droplevels(timepoint))

# Use expand_locf to add rows corresponding to visits with missing outcomes to the dataset
dat_clean2 <- expand_locf(
  dat_clean2,
  client_id = levels(dat_clean2$client_id), # expand by PATIENT and VISIT
  timepoint = levels(dat_clean2$timepoint),
  vars = c("IoD_reduced_baseline", "treatment", "Indexdelikt", "Aktuelle_Betreuung", "Aktuelle_zusatzliche_Behandlung", "static99_modified_calc"), # fill with LOCF BASVAL and THERAPY
  group = c("client_id"),
  order = c("client_id", "timepoint")
)

dat_ice <- dat_clean2 %>%
  arrange(client_id, timepoint) %>%
  filter(is.na(IoD_reduced)) %>%
  group_by(client_id) %>%
  dplyr::slice(1) %>%
  ungroup() %>%
  select(client_id, timepoint) %>%
  mutate(strategy = "JR")

vars <- set_vars(
  outcome = "IoD_reduced",
  visit = "timepoint",
  subjid = "client_id",
  group = "treatment",
  covariates = c("IoD_reduced_baseline", "treatment*timepoint", "Indexdelikt", "Aktuelle_Betreuung", "Aktuelle_zusatzliche_Behandlung", "static99_modified_calc")
)
# Define which imputation method to use (here: Bayesian multiple imputation with 150 imputed datsets)
method <- method_condmean(n_samples = 20, covariance = "us")

set.seed(987)
drawObj <- draws(
  data = dat_clean2,
  data_ice = dat_ice,
  vars = vars,
  method = method,
  quiet = TRUE
)

drawObj

imputeObj <- impute(
  drawObj,
  references = c("Intervention" = "Intervention", "Placebo" = "Placebo")
)
imputeObj

ancova_modified <- function(data, ...) {
  data2 <- data %>% mutate(IoD_reduced = IoD_reduced - IoD_reduced_baseline)
  rbmi::ancova(data2, ...)
}

anaObj <- analyse(
  imputeObj,
  ancova_modified,
  vars = set_vars(
    subjid = "client_id",
    outcome = "IoD",
    visit = "timepoint",
    group = "treatment",
    covariates = c("IoD_reduced_baseline", "Indexdelikt", "Aktuelle_Betreuung", "Aktuelle_zusatzliche_Behandlung", "static99_modified_calc")
  )
)
anaObj

poolObj <- pool(
  anaObj,
  conf.level = 0.95,
  alternative = "two.sided"
)
poolObj

rbmi_cares_tab <- poolObj %>% tibble::as_tibble() %>%
  mutate(
    # Erst Gruppe extrahieren, bevor wir die Labels bereinigen
    group = stringr::str_extract(parameter, "Module [0-9]+"),
    parameter = parameter %>%
      gsub("trt_", "Group differences ", .) %>% 
      gsub("lsm_ref_", "Least square mean [intervention] ", .) %>% 
      gsub("lsm_alt_", "Least square mean [placebo] ", .) %>%
      gsub(" Module [0-9]+ [(]post[)]", "", .),
    # Lancet p-Wert Formatierung
    pval = case_when(
      pval < 0.001 ~ "<0\u00b7001",
      pval < 0.01  ~ as.character(round(pval, 3)) %>% gsub("\\.", "\u00b7", .),
      TRUE         ~ as.character(round(pval, 2))  %>% gsub("\\.", "\u00b7", .)
    ),
    ci = paste0("(", 
                gsub("\\.", "\u00b7", sprintf("%.2f", lci)), 
                ", ", 
                gsub("\\.", "\u00b7", sprintf("%.2f", uci)), 
                ")"),
    est = gsub("\\.", "\u00b7", sprintf("%.2f", est))
  ) %>%
  dplyr::select(parameter, group, est, ci, pval)
