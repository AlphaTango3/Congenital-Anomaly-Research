# ============================================================
# Congenital Anomaly Research
# 02 - Descriptive Analysis
# ============================================================

# Load packages
library(tidyverse)
library(janitor)
library(here)

# Read raw dataset
dataset <- read_csv(
  here("data_raw", "Dataset.csv"),
  show_col_types = FALSE
)

# Clean column names
dataset <- dataset %>%
  clean_names()

# ============================================================

# Clean labels for presentation
variable_labels <- c(
  maternal_age = "Maternal age (years)",
  consanguinity = "Consanguinity",
  registered_antenatally = "Antenatal registration",
  anc_immunisation = "Antenatal immunisation",
  anc_anomaly = "Antenatal anomaly detected",
  maternal_medical_history = "Maternal medical history",
  iufd_history = "Previous IUFD",
  para = "Parity",
  living_children = "Living children",
  child_deaths = "Child deaths",
  total_abortions = "Total abortions",
  spontaneous_abortions = "Spontaneous abortions",
  mtp_abortions = "MTP abortions",
  family_history_congenital_anomaly = "Family history of congenital anomaly",
  delivery_mode = "Mode of delivery",
  sex = "Sex",
  gestation = "Gestational age",
  growth_restriction = "Growth restriction",
  birth_weight = "Birth weight (kg)",
  nicu_required = "NICU admission",
  heart_rate_status = "Heart rate status",
  cyanosis = "Cyanosis",
  pallor = "Pallor",
  growth_status = "Growth status",
  spine_abnormality = "Spine abnormality",
  limb_musculoskeletal_abnormality = "Limb/musculoskeletal abnormality",
  craniofacial_abnormality = "Craniofacial abnormality",
  urinary_abnormality = "Urinary abnormality",
  rs_abnormality = "Respiratory system abnormality",
  cvs_abnormality = "Cardiovascular abnormality",
  cns_abnormality = "Central nervous system abnormality",
  pa_abnormality = "Per-abdomen abnormality",
  genital_abnormality = "Genital abnormality",
  congenital_anomaly = "Congenital anomaly"
)

# ============================================================
# Overall study population
# ============================================================

# Sample size
nrow(dataset)

# Sex
table(dataset$sex)

# Gestational age
table(dataset$gestation)

# Mode of delivery
table(dataset$delivery_mode)

# Congenital anomaly
table(dataset$congenital_anomaly)

# Maternal age
summary(dataset$maternal_age)

# Birth weight
summary(dataset$birth_weight)

# ============================================================
# Table 1: Basic characteristics of the study population
# ============================================================

table_1 <- tibble(
  Variable = c(
    "Study population",
    "Maternal age (years)",
    "Birth weight (kg)",
    "Sex",
    "  Male",
    "  Female",
    "Gestational age",
    "  Term",
    "  Preterm",
    "  Post-term",
    "Mode of delivery",
    "  NVD",
    "  LSCS",
    "  Vacuum",
    "  Forceps",
    "Congenital anomaly",
    "  No",
    "  Yes"
  ),
  
  Value = c(
    nrow(dataset),
    
    paste0(
      round(mean(dataset$maternal_age), 2),
      " ± ",
      round(sd(dataset$maternal_age), 2)
    ),
    
    paste0(
      round(mean(dataset$birth_weight), 2),
      " ± ",
      round(sd(dataset$birth_weight), 2)
    ),
    
    "",
    
    paste0(
      sum(dataset$sex == "Male"),
      " (",
      round(mean(dataset$sex == "Male") * 100, 1),
      "%)"
    ),
    
    paste0(
      sum(dataset$sex == "Female"),
      " (",
      round(mean(dataset$sex == "Female") * 100, 1),
      "%)"
    ),
    
    "",
    
    paste0(
      sum(dataset$gestation == "Term"),
      " (",
      round(mean(dataset$gestation == "Term") * 100, 1),
      "%)"
    ),
    
    paste0(
      sum(dataset$gestation == "Preterm"),
      " (",
      round(mean(dataset$gestation == "Preterm") * 100, 1),
      "%)"
    ),
    
    paste0(
      sum(dataset$gestation == "Post-term"),
      " (",
      round(mean(dataset$gestation == "Post-term") * 100, 1),
      "%)"
    ),
    
    "",
    
    paste0(
      sum(dataset$delivery_mode == "NVD"),
      " (",
      round(mean(dataset$delivery_mode == "NVD") * 100, 1),
      "%)"
    ),
    
    paste0(
      sum(dataset$delivery_mode == "LSCS"),
      " (",
      round(mean(dataset$delivery_mode == "LSCS") * 100, 1),
      "%)"
    ),
    
    paste0(
      sum(dataset$delivery_mode == "Vacuum"),
      " (",
      round(mean(dataset$delivery_mode == "Vacuum") * 100, 1),
      "%)"
    ),
    
    paste0(
      sum(dataset$delivery_mode == "Forceps"),
      " (",
      round(mean(dataset$delivery_mode == "Forceps") * 100, 1),
      "%)"
    ),
    
    "",
    
    paste0(
      sum(dataset$congenital_anomaly == "No"),
      " (",
      round(mean(dataset$congenital_anomaly == "No") * 100, 1),
      "%)"
    ),
    
    paste0(
      sum(dataset$congenital_anomaly == "Yes"),
      " (",
      round(mean(dataset$congenital_anomaly == "Yes") * 100, 1),
      "%)"
    )
  )
)

table_1

# ============================================================
# Save Table 1: Overall study population
# ============================================================

saveRDS(
  table_1,
  here(
    "tables",
    "table_1_overall_study_population.rds"
  )
)

# ============================================================
# Maternal and obstetric characteristics
# ============================================================

maternal_obstetric <- dataset %>%
  select(
    maternal_age,
    consanguinity,
    registered_antenatally,
    anc_immunisation,
    anc_anomaly,
    maternal_medical_history,
    iufd_history,
    para,
    living_children,
    child_deaths,
    total_abortions,
    spontaneous_abortions,
    mtp_abortions,
    family_history_congenital_anomaly
  )

# Continuous variables

summary(maternal_obstetric$maternal_age)
summary(maternal_obstetric$para)
summary(maternal_obstetric$living_children)
summary(maternal_obstetric$child_deaths)
summary(maternal_obstetric$total_abortions)
summary(maternal_obstetric$spontaneous_abortions)
summary(maternal_obstetric$mtp_abortions)

# Categorical variables

table(maternal_obstetric$consanguinity)

table(maternal_obstetric$registered_antenatally)

table(maternal_obstetric$anc_immunisation)

table(maternal_obstetric$anc_anomaly)

table(maternal_obstetric$maternal_medical_history)

table(maternal_obstetric$iufd_history)

table(maternal_obstetric$family_history_congenital_anomaly)

# ============================================================
# Maternal and obstetric characteristics
# ============================================================

maternal_obstetric <- dataset %>%
  select(
    maternal_age,
    consanguinity,
    registered_antenatally,
    anc_immunisation,
    anc_anomaly,
    maternal_medical_history,
    iufd_history,
    para,
    living_children,
    child_deaths,
    total_abortions,
    spontaneous_abortions,
    mtp_abortions,
    family_history_congenital_anomaly
  )

# Continuous variables

summary(maternal_obstetric$maternal_age)
summary(maternal_obstetric$para)
summary(maternal_obstetric$living_children)
summary(maternal_obstetric$child_deaths)
summary(maternal_obstetric$total_abortions)
summary(maternal_obstetric$spontaneous_abortions)
summary(maternal_obstetric$mtp_abortions)

# Categorical variables

table(maternal_obstetric$consanguinity)

table(maternal_obstetric$registered_antenatally)

table(maternal_obstetric$anc_immunisation)

table(maternal_obstetric$anc_anomaly)

table(maternal_obstetric$maternal_medical_history)

table(maternal_obstetric$iufd_history)

table(maternal_obstetric$family_history_congenital_anomaly)

# ============================================================
# Neonatal characteristics
# ============================================================

neonatal <- dataset %>%
  select(
    sex,
    gestation,
    growth_restriction,
    birth_weight,
    apgar,
    nicu_required,
    heart_rate_status,
    cyanosis,
    pallor,
    growth_status
  )

# Continuous variable

summary(neonatal$birth_weight)

# Categorical variables

table(neonatal$sex)

table(neonatal$gestation)

table(neonatal$growth_restriction)

table(neonatal$apgar)

table(neonatal$nicu_required)

table(neonatal$heart_rate_status)

table(neonatal$cyanosis)

table(neonatal$pallor)

table(neonatal$growth_status)

# ============================================================
# Congenital anomaly distribution
# ============================================================

anomaly_variables <- dataset %>%
  select(
    spine_abnormality,
    limb_musculoskeletal_abnormality,
    craniofacial_abnormality,
    urinary_abnormality,
    rs_abnormality,
    cvs_abnormality,
    cns_abnormality,
    pa_abnormality,
    genital_abnormality
  )

# Frequency of each congenital anomaly system

table(anomaly_variables$spine_abnormality)

table(anomaly_variables$limb_musculoskeletal_abnormality)

table(anomaly_variables$craniofacial_abnormality)

table(anomaly_variables$urinary_abnormality)

table(anomaly_variables$rs_abnormality)

table(anomaly_variables$cvs_abnormality)

table(anomaly_variables$cns_abnormality)

table(anomaly_variables$pa_abnormality)

table(anomaly_variables$genital_abnormality)

# Overall congenital anomaly status

table(dataset$congenital_anomaly)

# ============================================================
# Figure 1: Congenital anomalies by organ system
# ============================================================

anomaly_plot_data <- tibble(
  system = c(
    "Spine",
    "Limb/musculoskeletal",
    "Craniofacial",
    "Urinary",
    "Respiratory",
    "Cardiovascular",
    "Central nervous system",
    "Per-abdomen",
    "Genital"
  ),
  
  n = c(
    sum(dataset$spine_abnormality == "Yes"),
    sum(dataset$limb_musculoskeletal_abnormality == "Yes"),
    sum(dataset$craniofacial_abnormality == "Yes"),
    sum(dataset$urinary_abnormality == "Yes"),
    sum(dataset$rs_abnormality == "Yes"),
    sum(dataset$cvs_abnormality == "Yes"),
    sum(dataset$cns_abnormality == "Yes"),
    sum(dataset$pa_abnormality == "Yes"),
    sum(dataset$genital_abnormality == "Yes")
  )
)

anomaly_plot_data
# ============================================================
# Figure 1: Congenital anomalies by organ system
# ============================================================

figure_1 <- ggplot(
  anomaly_plot_data,
  aes(
    x = reorder(system, n),
    y = n
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Congenital anomalies by organ system",
    x = "Organ system",
    y = "Number of infants"
  ) +
  theme_minimal()

figure_1

# There are 17 infants with congenital anomalies, but 20 positive organ-system flags.

#saving figure 1
ggsave(
  filename = here(
    "figures",
    "figure_1_congenital_anomalies_by_system.png"
  ),
  plot = figure_1,
  width = 8,
  height = 5,
  dpi = 300
)

# ============================================================
# Figure 2: Overall congenital anomaly status
# ============================================================

congenital_plot_data <- dataset %>%
  count(congenital_anomaly) %>%
  mutate(
    percentage = n / sum(n) * 100
  )

congenital_plot_data

#creating and saving figure 2
figure_2 <- ggplot(
  congenital_plot_data,
  aes(
    x = congenital_anomaly,
    y = percentage
  )
) +
  geom_col() +
  labs(
    title = "Congenital anomaly status",
    x = "Congenital anomaly",
    y = "Infants (%)"
  ) +
  theme_minimal()

figure_2

ggsave(
  filename = here(
    "figures",
    "figure_2_congenital_anomaly_status.png"
  ),
  plot = figure_2,
  width = 6,
  height = 5,
  dpi = 300
)

# ============================================================

# Figure 3: Birth-weight distribution continous

# ============================================================
figure_3 <- ggplot(
  dataset,
  aes(x = birth_weight)
) +
  geom_histogram(
    bins = 15
  ) +
  labs(
    title = "Distribution of birth weight",
    x = "Birth weight (kg)",
    y = "Number of infants"
  ) +
  theme_minimal()

figure_3

ggsave(
  filename = here(
    "figures",
    "figure_3_birth_weight_distribution.png"
  ),
  plot = figure_3,
  width = 8,
  height = 5,
  dpi = 300
)

# ============================================================

# Figure 4: Birth-weight distribution according to Who classification

# ============================================================
figure_4_data <- dataset %>%
  mutate(
    birth_weight_category = case_when(
      birth_weight < 1.0 ~ "Extremely low birth weight",
      birth_weight < 1.5 ~ "Very low birth weight",
      birth_weight < 2.5 ~ "Low birth weight",
      birth_weight >= 2.5 ~ "Normal birth weight"
    )
  ) %>%
  count(birth_weight_category) %>%
  mutate(
    percentage = n / sum(n) * 100
  )

figure_4_data

figure_4 <- ggplot(
  figure_4_data,
  aes(
    x = birth_weight_category,
    y = percentage
  )
) +
  geom_col() +
  labs(
    title = "Distribution of birth weight categories",
    x = "Birth weight category",
    y = "Infants (%)"
  ) +
  theme_minimal()

figure_4

figure_4 <- ggplot(
  figure_4_data,
  aes(
    x = birth_weight_category,
    y = percentage
  )
) +
  geom_col() +
  labs(
    title = "Distribution of birth weight categories",
    x = "Birth weight category",
    y = "Infants (%)"
  ) +
  theme_minimal()

figure_4

# ============================================================
# Figure 5: Gestational-age distribution
# ============================================================

figure_5_data <- dataset %>%
  count(gestation) %>%
  mutate(
    percentage = n / sum(n) * 100
  )

figure_5_data


figure_5 <- ggplot(
  figure_5_data,
  aes(
    x = gestation,
    y = percentage
  )
) +
  geom_col() +
  labs(
    title = "Gestational age distribution",
    x = "Gestational age",
    y = "Infants (%)"
  ) +
  theme_minimal()

figure_5


ggsave(
  filename = here(
    "figures",
    "figure_5_gestational_age_distribution.png"
  ),
  plot = figure_5,
  width = 7,
  height = 5,
  dpi = 300
)

# ============================================================
# Figure 6: Mode of delivery distribution
# ============================================================

figure_6_data <- dataset %>%
  count(delivery_mode) %>%
  mutate(
    percentage = n / sum(n) * 100
  )

figure_6_data


figure_6 <- ggplot(
  figure_6_data,
  aes(
    x = delivery_mode,
    y = percentage
  )
) +
  geom_col() +
  labs(
    title = "Mode of delivery distribution",
    x = "Mode of delivery",
    y = "Infants (%)"
  ) +
  theme_minimal()

figure_6


ggsave(
  filename = here(
    "figures",
    "figure_6_mode_of_delivery_distribution.png"
  ),
  plot = figure_6,
  width = 7,
  height = 5,
  dpi = 300
)
# ============================================================
# Figure 7: Sex distribution
# ============================================================

figure_7_data <- dataset %>%
  count(sex) %>%
  mutate(
    percentage = n / sum(n) * 100
  )

figure_7_data


figure_7 <- ggplot(
  figure_7_data,
  aes(
    x = sex,
    y = percentage
  )
) +
  geom_col() +
  labs(
    title = "Sex distribution",
    x = "Sex",
    y = "Infants (%)"
  ) +
  theme_minimal()

figure_7


ggsave(
  filename = here(
    "figures",
    "figure_7_sex_distribution.png"
  ),
  plot = figure_7,
  width = 6,
  height = 5,
  dpi = 300
)

# ============================================================
# Figure 8: Antenatal registration status
# ============================================================

figure_8_data <- dataset %>%
  count(registered_antenatally) %>%
  mutate(
    percentage = n / sum(n) * 100
  )

figure_8_data


figure_8 <- ggplot(
  figure_8_data,
  aes(
    x = registered_antenatally,
    y = percentage
  )
) +
  geom_col() +
  labs(
    title = "Antenatal registration status",
    x = "Antenatal registration",
    y = "Infants (%)"
  ) +
  theme_minimal()

figure_8


ggsave(
  filename = here(
    "figures",
    "figure_8_antenatal_registration.png"
  ),
  plot = figure_8,
  width = 6,
  height = 5,
  dpi = 300
)

# ============================================================
# Figure 9: Growth restriction distribution
# ============================================================

figure_9_data <- dataset %>%
  count(growth_restriction) %>%
  mutate(
    percentage = n / sum(n) * 100
  )

figure_9_data


figure_9 <- ggplot(
  figure_9_data,
  aes(
    x = growth_restriction,
    y = percentage
  )
) +
  geom_col() +
  labs(
    title = "Growth restriction distribution",
    x = "Growth restriction",
    y = "Infants (%)"
  ) +
  theme_minimal()

figure_9


ggsave(
  filename = here(
    "figures",
    "figure_9_growth_restriction.png"
  ),
  plot = figure_9,
  width = 6,
  height = 5,
  dpi = 300
)

# ============================================================
# Figure 10: NICU admission
# ============================================================

figure_10_data <- dataset %>%
  count(nicu_required) %>%
  mutate(
    percentage = n / sum(n) * 100
  )

figure_10_data


figure_10 <- ggplot(
  figure_10_data,
  aes(
    x = nicu_required,
    y = percentage
  )
) +
  geom_col() +
  labs(
    title = "NICU admission",
    x = "NICU admission",
    y = "Infants (%)"
  ) +
  theme_minimal()

figure_10


ggsave(
  filename = here(
    "figures",
    "figure_10_nicu_admission.png"
  ),
  plot = figure_10,
  width = 6,
  height = 5,
  dpi = 300
)
# ============================================================
# Figure 11: Growth status
# ============================================================

figure_11_data <- dataset %>%
  count(growth_status) %>%
  mutate(
    percentage = n / sum(n) * 100
  )

figure_11_data


figure_11 <- ggplot(
  figure_11_data,
  aes(
    x = growth_status,
    y = percentage
  )
) +
  geom_col() +
  labs(
    title = "Growth status distribution",
    x = "Growth status",
    y = "Infants (%)"
  ) +
  theme_minimal()

figure_11


ggsave(
  filename = here(
    "figures",
    "figure_11_growth_status.png"
  ),
  plot = figure_11,
  width = 6,
  height = 5,
  dpi = 300
)
# ============================================================
# Figure 12: Congenital anomaly status by sex
# ============================================================

figure_12_data <- dataset %>%
  count(sex, congenital_anomaly) %>%
  group_by(sex) %>%
  mutate(
    percentage = n / sum(n) * 100
  ) %>%
  ungroup()

figure_12_data

figure_12 <- ggplot(
  figure_12_data,
  aes(
    x = sex,
    y = percentage,
    fill = congenital_anomaly
  )
) +
  geom_col(
    position = "dodge"
  ) +
  labs(
    title = "Congenital anomaly status by sex",
    x = "Sex",
    y = "Infants (%)",
    fill = "Congenital anomaly"
  ) +
  theme_minimal()

figure_12

ggsave(
  filename = here(
    "figures",
    "figure_12_congenital_anomaly_by_sex.png"
  ),
  plot = figure_12,
  width = 7,
  height = 5,
  dpi = 300
)

# ============================================================
# Figure 13: Congenital anomaly status by gestational age
# ============================================================

figure_13_data <- dataset %>%
  count(gestation, congenital_anomaly) %>%
  group_by(gestation) %>%
  mutate(
    percentage = n / sum(n) * 100
  ) %>%
  ungroup()

figure_13_data

figure_13 <- ggplot(
  figure_13_data,
  aes(
    x = gestation,
    y = percentage,
    fill = congenital_anomaly
  )
) +
  geom_col(
    position = "dodge"
  ) +
  labs(
    title = "Congenital anomaly status by gestational age",
    x = "Gestational age",
    y = "Infants (%)",
    fill = "Congenital anomaly"
  ) +
  theme_minimal()

figure_13

ggsave(
  filename = here(
    "figures",
    "figure_13_congenital_anomaly_by_gestational_age.png"
  ),
  plot = figure_13,
  width = 7,
  height = 5,
  dpi = 300
)

# ============================================================
# Figure 14: Congenital anomaly status by birth-weight category
# ============================================================

figure_14_data <- dataset %>%
  mutate(
    birth_weight_category = case_when(
      birth_weight < 1.0 ~ "Extremely low birth weight",
      birth_weight < 1.5 ~ "Very low birth weight",
      birth_weight < 2.5 ~ "Low birth weight",
      birth_weight >= 2.5 ~ "Normal birth weight"
    )
  ) %>%
  count(birth_weight_category, congenital_anomaly) %>%
  group_by(birth_weight_category) %>%
  mutate(
    percentage = n / sum(n) * 100
  ) %>%
  ungroup()

figure_14_data

figure_14 <- ggplot(
  figure_14_data,
  aes(
    x = birth_weight_category,
    y = percentage,
    fill = congenital_anomaly
  )
) +
  geom_col(
    position = "dodge"
  ) +
  labs(
    title = "Congenital anomaly status by birth-weight category",
    x = "Birth weight category",
    y = "Infants (%)",
    fill = "Congenital anomaly"
  ) +
  theme_minimal()

figure_14

ggsave(
  filename = here(
    "figures",
    "figure_14_congenital_anomaly_by_birth_weight_category.png"
  ),
  plot = figure_14,
  width = 9,
  height = 5,
  dpi = 300
)

# ============================================================
# Figure 15: Congenital anomaly status by NICU admission
# ============================================================

figure_15_data <- dataset %>%
  count(nicu_required, congenital_anomaly) %>%
  group_by(nicu_required) %>%
  mutate(
    percentage = n / sum(n) * 100
  ) %>%
  ungroup()

figure_15_data

figure_15 <- ggplot(
  figure_15_data,
  aes(
    x = nicu_required,
    y = percentage,
    fill = congenital_anomaly
  )
) +
  geom_col(
    position = "dodge"
  ) +
  labs(
    title = "Congenital anomaly status by NICU admission",
    x = "NICU admission",
    y = "Infants (%)",
    fill = "Congenital anomaly"
  ) +
  theme_minimal()

figure_15

ggsave(
  filename = here(
    "figures",
    "figure_15_congenital_anomaly_by_nicu.png"
  ),
  plot = figure_15,
  width = 7,
  height = 5,
  dpi = 300
)
# ============================================================
# Figure 16: Congenital anomaly status by growth restriction
# ============================================================

figure_16_data <- dataset %>%
  count(growth_restriction, congenital_anomaly) %>%
  group_by(growth_restriction) %>%
  mutate(
    percentage = n / sum(n) * 100
  ) %>%
  ungroup()

figure_16_data

figure_16 <- ggplot(
  figure_16_data,
  aes(
    x = growth_restriction,
    y = percentage,
    fill = congenital_anomaly
  )
) +
  geom_col(
    position = "dodge"
  ) +
  labs(
    title = "Congenital anomaly status by growth restriction",
    x = "Growth restriction",
    y = "Infants (%)",
    fill = "Congenital anomaly"
  ) +
  theme_minimal()

figure_16

ggsave(
  filename = here(
    "figures",
    "figure_16_congenital_anomaly_by_growth_restriction.png"
  ),
  plot = figure_16,
  width = 7,
  height = 5,
  dpi = 300
)

# ============================================================
# Figure 17: Maternal age by congenital anomaly status
# ============================================================

figure_17 <- ggplot(
  dataset,
  aes(
    x = congenital_anomaly,
    y = maternal_age
  )
) +
  geom_boxplot() +
  labs(
    title = "Maternal age by congenital anomaly status",
    x = "Congenital anomaly",
    y = "Maternal age (years)"
  ) +
  theme_minimal()

figure_17

ggsave(
  filename = here(
    "figures",
    "figure_17_maternal_age_by_congenital_anomaly.png"
  ),
  plot = figure_17,
  width = 7,
  height = 5,
  dpi = 300
)
# ============================================================
# Figure 18: Birth weight by congenital anomaly status
# ============================================================

figure_18 <- ggplot(
  dataset,
  aes(
    x = congenital_anomaly,
    y = birth_weight
  )
) +
  geom_boxplot() +
  labs(
    title = "Birth weight by congenital anomaly status",
    x = "Congenital anomaly",
    y = "Birth weight (kg)"
  ) +
  theme_minimal()

figure_18

ggsave(
  filename = here(
    "figures",
    "figure_18_birth_weight_by_congenital_anomaly.png"
  ),
  plot = figure_18,
  width = 7,
  height = 5,
  dpi = 300
)

# ============================================================
# Figure 19: Congenital anomaly status by growth status
# ============================================================

figure_19_data <- dataset %>%
  count(growth_status, congenital_anomaly) %>%
  group_by(growth_status) %>%
  mutate(
    percentage = n / sum(n) * 100
  ) %>%
  ungroup()

figure_19_data

figure_19 <- ggplot(
  figure_19_data,
  aes(
    x = growth_status,
    y = percentage,
    fill = congenital_anomaly
  )
) +
  geom_col(
    position = "dodge"
  ) +
  labs(
    title = "Congenital anomaly status by growth status",
    x = "Growth status",
    y = "Infants (%)",
    fill = "Congenital anomaly"
  ) +
  theme_minimal()

figure_19

ggsave(
  filename = here(
    "figures",
    "figure_19_congenital_anomaly_by_growth_status.png"
  ),
  plot = figure_19,
  width = 7,
  height = 5,
  dpi = 300
)

# ============================================================
# Table 2: Maternal and obstetric characteristics
#        by congenital anomaly status
# ============================================================

table_2 <- dataset %>%
  group_by(congenital_anomaly) %>%
  summarise(
    n = n(),
    
    maternal_age_mean = mean(maternal_age),
    maternal_age_sd = sd(maternal_age),
    
    para_mean = mean(para),
    para_sd = sd(para),
    
    living_children_mean = mean(living_children),
    living_children_sd = sd(living_children),
    
    child_deaths_mean = mean(child_deaths),
    child_deaths_sd = sd(child_deaths),
    
    total_abortions_mean = mean(total_abortions),
    total_abortions_sd = sd(total_abortions),
    
    spontaneous_abortions_mean =
      mean(spontaneous_abortions),
    spontaneous_abortions_sd =
      sd(spontaneous_abortions),
    
    mtp_abortions_mean = mean(mtp_abortions),
    mtp_abortions_sd = sd(mtp_abortions),
    
    consanguinity_yes = sum(consanguinity == "Yes"),
    
    registered_antenatally_yes =
      sum(registered_antenatally == "Yes"),
    
    anc_anomaly_yes =
      sum(anc_anomaly == "Yes"),
    
    maternal_medical_history_yes =
      sum(maternal_medical_history == "Yes"),
    
    iufd_history_yes =
      sum(iufd_history == "Yes"),
    
    family_history_yes =
      sum(family_history_congenital_anomaly == "Yes"),
    
    .groups = "drop"
  )

table_2

# ============================================================
# Table 2: Inspect complete output
# ============================================================

print(table_2, width = Inf)

# ============================================================
# Table 2: Maternal and obstetric characteristics
#        by congenital anomaly status
# ============================================================

table_2_clean <- tibble(
  
  Characteristic = c(
    "Maternal age (years)",
    "Parity",
    "Living children",
    "Child deaths",
    "Total abortions",
    "Spontaneous abortions",
    "MTP abortions",
    "Consanguinity",
    "  Yes",
    "Antenatal registration",
    "  Yes",
    "Antenatal anomaly detected",
    "  Yes",
    "Maternal medical history",
    "  Yes",
    "Previous IUFD",
    "  Yes",
    "Family history of congenital anomaly",
    "  Yes"
  ),
  
  `No congenital anomaly (n=483)` = c(
    
    paste0(
      round(table_2$maternal_age_mean[1], 2),
      " ± ",
      round(table_2$maternal_age_sd[1], 2)
    ),
    
    paste0(
      round(table_2$para_mean[1], 2),
      " ± ",
      round(table_2$para_sd[1], 2)
    ),
    
    paste0(
      round(table_2$living_children_mean[1], 2),
      " ± ",
      round(table_2$living_children_sd[1], 2)
    ),
    
    paste0(
      round(table_2$child_deaths_mean[1], 2),
      " ± ",
      round(table_2$child_deaths_sd[1], 2)
    ),
    
    paste0(
      round(table_2$total_abortions_mean[1], 2),
      " ± ",
      round(table_2$total_abortions_sd[1], 2)
    ),
    
    paste0(
      round(table_2$spontaneous_abortions_mean[1], 2),
      " ± ",
      round(table_2$spontaneous_abortions_sd[1], 2)
    ),
    
    paste0(
      round(table_2$mtp_abortions_mean[1], 2),
      " ± ",
      round(table_2$mtp_abortions_sd[1], 2)
    ),
    
    "",
    paste0(
      table_2$consanguinity_yes[1],
      " (",
      round(table_2$consanguinity_yes[1] /
              table_2$n[1] * 100, 1),
      "%)"
    ),
    
    "",
    paste0(
      table_2$registered_antenatally_yes[1],
      " (",
      round(table_2$registered_antenatally_yes[1] /
              table_2$n[1] * 100, 1),
      "%)"
    ),
    
    "",
    paste0(
      table_2$anc_anomaly_yes[1],
      " (",
      round(table_2$anc_anomaly_yes[1] /
              table_2$n[1] * 100, 1),
      "%)"
    ),
    
    "",
    paste0(
      table_2$maternal_medical_history_yes[1],
      " (",
      round(table_2$maternal_medical_history_yes[1] /
              table_2$n[1] * 100, 1),
      "%)"
    ),
    
    "",
    paste0(
      table_2$iufd_history_yes[1],
      " (",
      round(table_2$iufd_history_yes[1] /
              table_2$n[1] * 100, 1),
      "%)"
    ),
    
    "",
    paste0(
      table_2$family_history_yes[1],
      " (",
      round(table_2$family_history_yes[1] /
              table_2$n[1] * 100, 1),
      "%)"
    )
  ),
  
  `Congenital anomaly (n=17)` = c(
    
    paste0(
      round(table_2$maternal_age_mean[2], 2),
      " ± ",
      round(table_2$maternal_age_sd[2], 2)
    ),
    
    paste0(
      round(table_2$para_mean[2], 2),
      " ± ",
      round(table_2$para_sd[2], 2)
    ),
    
    paste0(
      round(table_2$living_children_mean[2], 2),
      " ± ",
      round(table_2$living_children_sd[2], 2)
    ),
    
    paste0(
      round(table_2$child_deaths_mean[2], 2),
      " ± ",
      round(table_2$child_deaths_sd[2], 2)
    ),
    
    paste0(
      round(table_2$total_abortions_mean[2], 2),
      " ± ",
      round(table_2$total_abortions_sd[2], 2)
    ),
    
    paste0(
      round(table_2$spontaneous_abortions_mean[2], 2),
      " ± ",
      round(table_2$spontaneous_abortions_sd[2], 2)
    ),
    
    paste0(
      round(table_2$mtp_abortions_mean[2], 2),
      " ± ",
      round(table_2$mtp_abortions_sd[2], 2)
    ),
    
    "",
    paste0(
      table_2$consanguinity_yes[2],
      " (",
      round(table_2$consanguinity_yes[2] /
              table_2$n[2] * 100, 1),
      "%)"
    ),
    
    "",
    paste0(
      table_2$registered_antenatally_yes[2],
      " (",
      round(table_2$registered_antenatally_yes[2] /
              table_2$n[2] * 100, 1),
      "%)"
    ),
    
    "",
    paste0(
      table_2$anc_anomaly_yes[2],
      " (",
      round(table_2$anc_anomaly_yes[2] /
              table_2$n[2] * 100, 1),
      "%)"
    ),
    
    "",
    paste0(
      table_2$maternal_medical_history_yes[2],
      " (",
      round(table_2$maternal_medical_history_yes[2] /
              table_2$n[2] * 100, 1),
      "%)"
    ),
    
    "",
    paste0(
      table_2$iufd_history_yes[2],
      " (",
      round(table_2$iufd_history_yes[2] /
              table_2$n[2] * 100, 1),
      "%)"
    ),
    
    "",
    paste0(
      table_2$family_history_yes[2],
      " (",
      round(table_2$family_history_yes[2] /
              table_2$n[2] * 100, 1),
      "%)"
    )
  )
)

table_2_clean

# ============================================================
# Save Table 2: Maternal and obstetric characteristics
# ============================================================

saveRDS(
  table_2_clean,
  here(
    "tables",
    "table_2_maternal_obstetric_by_congenital_anomaly.rds"
  )
)
# ============================================================
# Table 3: Neonatal characteristics
#        by congenital anomaly status
# ============================================================

table_3 <- dataset %>%
  
  group_by(congenital_anomaly) %>%
  
  summarise(
    
    n = n(),
    
    birth_weight_mean = mean(birth_weight),
    
    birth_weight_sd = sd(birth_weight),
    
    male = sum(sex == "Male"),
    
    female = sum(sex == "Female"),
    
    term = sum(gestation == "Term"),
    
    preterm = sum(gestation == "Preterm"),
    
    post_term = sum(gestation == "Post-term"),
    
    growth_restriction_yes =
      
      sum(growth_restriction == "Yes"),
    
    nicu_yes =
      
      sum(nicu_required == "Yes"),
    
    bradycardia =
      
      sum(heart_rate_status == "Bradycardia"),
    
    tachycardia =
      
      sum(heart_rate_status == "Tachycardia"),
    
    cyanosis_yes =
      
      sum(cyanosis == "Yes"),
    
    pallor_yes =
      
      sum(pallor == "Yes"),
    
    aga =
      
      sum(growth_status == "AGA"),
    
    sga =
      
      sum(growth_status == "SGA"),
    
    .groups = "drop"
    
  )

table_3

# ============================================================
# Table 3: Neonatal characteristics by congenital anomaly status
# ============================================================

table_3_clean <- tibble(
  
  Characteristic = c(
    "Birth weight (kg)",
    "Sex",
    "  Male",
    "  Female",
    "Gestational age",
    "  Term",
    "  Preterm",
    "  Post-term",
    "Growth restriction",
    "  Yes",
    "NICU admission",
    "  Yes",
    "Heart rate status",
    "  Bradycardia",
    "  Tachycardia",
    "Cyanosis",
    "  Yes",
    "Pallor",
    "  Yes",
    "Growth status",
    "  AGA",
    "  SGA"
  ),
  
  `No congenital anomaly (n=483)` = c(
    
    paste0(
      round(table_3$birth_weight_mean[1], 2),
      " ± ",
      round(table_3$birth_weight_sd[1], 2)
    ),
    
    "",
    
    paste0(
      table_3$male[1],
      " (",
      round(table_3$male[1] / table_3$n[1] * 100, 1),
      "%)"
    ),
    
    paste0(
      table_3$female[1],
      " (",
      round(table_3$female[1] / table_3$n[1] * 100, 1),
      "%)"
    ),
    
    "",
    
    paste0(
      table_3$term[1],
      " (",
      round(table_3$term[1] / table_3$n[1] * 100, 1),
      "%)"
    ),
    
    paste0(
      table_3$preterm[1],
      " (",
      round(table_3$preterm[1] / table_3$n[1] * 100, 1),
      "%)"
    ),
    
    paste0(
      table_3$post_term[1],
      " (",
      round(table_3$post_term[1] / table_3$n[1] * 100, 1),
      "%)"
    ),
    
    "",
    
    paste0(
      table_3$growth_restriction_yes[1],
      " (",
      round(table_3$growth_restriction_yes[1] / table_3$n[1] * 100, 1),
      "%)"
    ),
    
    "",
    
    paste0(
      table_3$nicu_yes[1],
      " (",
      round(table_3$nicu_yes[1] / table_3$n[1] * 100, 1),
      "%)"
    ),
    
    "",
    
    paste0(
      table_3$bradycardia[1],
      " (",
      round(table_3$bradycardia[1] / table_3$n[1] * 100, 1),
      "%)"
    ),
    
    paste0(
      table_3$tachycardia[1],
      " (",
      round(table_3$tachycardia[1] / table_3$n[1] * 100, 1),
      "%)"
    ),
    
    "",
    
    paste0(
      table_3$cyanosis_yes[1],
      " (",
      round(table_3$cyanosis_yes[1] / table_3$n[1] * 100, 1),
      "%)"
    ),
    
    "",
    
    paste0(
      table_3$pallor_yes[1],
      " (",
      round(table_3$pallor_yes[1] / table_3$n[1] * 100, 1),
      "%)"
    ),
    
    "",
    
    paste0(
      table_3$aga[1],
      " (",
      round(table_3$aga[1] / table_3$n[1] * 100, 1),
      "%)"
    ),
    
    paste0(
      table_3$sga[1],
      " (",
      round(table_3$sga[1] / table_3$n[1] * 100, 1),
      "%)"
    )
  ),
  
  `Congenital anomaly (n=17)` = c(
    
    paste0(
      round(table_3$birth_weight_mean[2], 2),
      " ± ",
      round(table_3$birth_weight_sd[2], 2)
    ),
    
    "",
    
    paste0(
      table_3$male[2],
      " (",
      round(table_3$male[2] / table_3$n[2] * 100, 1),
      "%)"
    ),
    
    paste0(
      table_3$female[2],
      " (",
      round(table_3$female[2] / table_3$n[2] * 100, 1),
      "%)"
    ),
    
    "",
    
    paste0(
      table_3$term[2],
      " (",
      round(table_3$term[2] / table_3$n[2] * 100, 1),
      "%)"
    ),
    
    paste0(
      table_3$preterm[2],
      " (",
      round(table_3$preterm[2] / table_3$n[2] * 100, 1),
      "%)"
    ),
    
    paste0(
      table_3$post_term[2],
      " (",
      round(table_3$post_term[2] / table_3$n[2] * 100, 1),
      "%)"
    ),
    
    "",
    
    paste0(
      table_3$growth_restriction_yes[2],
      " (",
      round(table_3$growth_restriction_yes[2] / table_3$n[2] * 100, 1),
      "%)"
    ),
    
    "",
    
    paste0(
      table_3$nicu_yes[2],
      " (",
      round(table_3$nicu_yes[2] / table_3$n[2] * 100, 1),
      "%)"
    ),
    
    "",
    
    paste0(
      table_3$bradycardia[2],
      " (",
      round(table_3$bradycardia[2] / table_3$n[2] * 100, 1),
      "%)"
    ),
    
    paste0(
      table_3$tachycardia[2],
      " (",
      round(table_3$tachycardia[2] / table_3$n[2] * 100, 1),
      "%)"
    ),
    
    "",
    
    paste0(
      table_3$cyanosis_yes[2],
      " (",
      round(table_3$cyanosis_yes[2] / table_3$n[2] * 100, 1),
      "%)"
    ),
    
    "",
    
    paste0(
      table_3$pallor_yes[2],
      " (",
      round(table_3$pallor_yes[2] / table_3$n[2] * 100, 1),
      "%)"
    ),
    
    "",
    
    paste0(
      table_3$aga[2],
      " (",
      round(table_3$aga[2] / table_3$n[2] * 100, 1),
      "%)"
    ),
    
    paste0(
      table_3$sga[2],
      " (",
      round(table_3$sga[2] / table_3$n[2] * 100, 1),
      "%)"
    )
  )
)

table_3_clean

# ============================================================
# Table 3: Inspect complete output
# ============================================================

print(
  table_3_clean,
  n = Inf,
  width = Inf
)

# ============================================================
# Save Table 3: Neonatal characteristics
# ============================================================

saveRDS(
  table_3_clean,
  here(
    "tables",
    "table_3_neonatal_by_congenital_anomaly.rds"
  )
)

file.exists(
  here(
    "tables",
    "table_3_neonatal_by_congenital_anomaly.rds"
  )
)

# ============================================================
# Table 4: Congenital anomalies by organ system
# ============================================================

table_4_data <- dataset %>%
  filter(congenital_anomaly == "Yes") %>%
  summarise(
    Spine = sum(spine_abnormality == "Yes"),
    `Limb/musculoskeletal` =
      sum(limb_musculoskeletal_abnormality == "Yes"),
    Craniofacial =
      sum(craniofacial_abnormality == "Yes"),
    Urinary =
      sum(urinary_abnormality == "Yes"),
    Respiratory =
      sum(rs_abnormality == "Yes"),
    Cardiovascular =
      sum(cvs_abnormality == "Yes"),
    `Central nervous system` =
      sum(cns_abnormality == "Yes"),
    `Per-abdomen` =
      sum(pa_abnormality == "Yes"),
    Genital =
      sum(genital_abnormality == "Yes")
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Organ system",
    values_to = "n"
  ) %>%
  mutate(
    percentage = n / 17 * 100
  )

table_4_data

# ============================================================
# Table 4: Congenital anomalies by organ system
# ============================================================

table_4_clean <- table_4_data %>%
  mutate(
    `n (%)` = paste0(
      n,
      " (",
      round(percentage, 1),
      "%)"
    )
  ) %>%
  select(
    `Organ system`,
    `n (%)`
  )

table_4_clean

# this gives us organ system/n in percentage
# here Percentages are calculated using 17 congenital-anomaly infants as the denominator

# ============================================================
# Save Table 4: Congenital anomalies by organ system
# ============================================================

saveRDS(
  table_4_clean,
  here(
    "tables",
    "table_4_congenital_anomalies_by_organ_system.rds"
  )
)

file.exists(
  here(
    "tables",
    "table_4_congenital_anomalies_by_organ_system.rds"
  )
)

# ============================================================
# End of descriptive analysis
# ============================================================

# Tables created:
# Table 1: Overall study population
# Table 2: Maternal and obstetric characteristics
#         by congenital anomaly status
# Table 3: Neonatal characteristics
#         by congenital anomaly status
# Table 4: Congenital anomalies by organ system

# Figures created:
# Figure 1: Congenital anomalies by organ system
# Figure 2: Congenital anomaly status
# Figure 3: Birth-weight distribution
# Figure 4: Birth-weight distribution according to WHO classification
# Figure 5: Gestational-age distribution
# Figure 6: Mode of delivery distribution
# Figure 7: Sex distribution
# Figure 8: Antenatal registration status
# Figure 9: Growth restriction distribution
# Figure 10: NICU admission
# Figure 11: Growth status distribution
# Figure 12: Congenital anomaly status by sex
# Figure 13: Congenital anomaly status by gestational age
# Figure 14: Congenital anomaly status by birth-weight category
# Figure 15: Congenital anomaly status by NICU admission
# Figure 16: Congenital anomaly status by growth restriction
# Figure 17: Maternal age by congenital anomaly status
# Figure 18: Birth weight by congenital anomaly status
# Figure 19: Congenital anomaly status by growth status