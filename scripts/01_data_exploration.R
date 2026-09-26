library(tidyverse)
library(janitor)
library(here)

dataset <- read_csv(
  here("data_raw", "Dataset.csv"),
  show_col_types = FALSE
)

dataset <- dataset %>%
  clean_names()

glimpse(dataset)

#Basic checks one at a time

dim(dataset)
names(dataset)
colSums(is.na(dataset))
sum(duplicated(dataset))
sum(duplicated(dataset$id))

# Basic clinical-variable checks
table(dataset$sex)
table(dataset$gestation)
table(dataset$congenital_anomaly)
summary(dataset$maternal_age)
summary(dataset$birth_weight)

# all system variables
table(dataset$spine_abnormality)
table(dataset$limb_musculoskeletal_abnormality)
table(dataset$craniofacial_abnormality)
table(dataset$urinary_abnormality)
table(dataset$rs_abnormality)
table(dataset$cvs_abnormality)
table(dataset$cns_abnormality)
table(dataset$pa_abnormality)
table(dataset$genital_abnormality)

# Abnormality diagnosis variables at glance: not all congenital
table(dataset$diagnosis)

#check for  categorical variables contain unexpected values

table(dataset$consanguinity)
table(dataset$registered_antenatally)
table(dataset$anc_immunisation)
table(dataset$anc_anomaly)
table(dataset$iufd_history)
table(dataset$family_history_congenital_anomaly)
table(dataset$delivery_mode)
table(dataset$growth_restriction)
table(dataset$nicu_required)
table(dataset$heart_rate_status)
table(dataset$cyanosis)
table(dataset$pallor)
table(dataset$growth_status)