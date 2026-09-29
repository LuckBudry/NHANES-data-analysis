### Loading packages
library(pacman)
pacman::p_load(rio, here, janitor, dplyr, readr, tidyverse, skimr)

### Importing the datasets
demographics_data_2013_2014 <- import(here("data", "DEMO_H.xpt"))
blood_pressure_data_2013_2014 <- import(here("data", "BPX_H.xpt"))

demographics_data_2015_2016 <- import(here("data", "DEMO_I.xpt"))
blood_pressure_data_2015_2016 <- import(here("data", "BPX_I.xpt"))

demographics_data_2017_2018 <- import(here("data", "DEMO_J.xpt"))
blood_pressure_data_2017_2018 <- import(here("data", "BPX_J.xpt"))

### Initial review of the datasets
dim(demographics_data_2013_2014)
dim(blood_pressure_data_2013_2014)

dim(demographics_data_2015_2016)
dim(blood_pressure_data_2015_2016)

dim(demographics_data_2017_2018)
dim(blood_pressure_data_2017_2018)

colnames(blood_pressure_data_2013_2014)

# Select the columns related to systolic and diastolic blood pressure
blood_pressure_sys_di_2013_2014 <- blood_pressure_data_2013_2014 %>%
  select(SEQN, PEASCCT1, BPXSY1, BPXDI1, BPXSY2,
         BPXDI2, BPXSY3, BPXDI3, BPXSY4, BPXDI4)

blood_pressure_sys_di_2015_2016 <- blood_pressure_data_2015_2016 %>%
  select(SEQN, PEASCCT1, BPXSY1, BPXDI1, BPXSY2,
         BPXDI2, BPXSY3, BPXDI3, BPXSY4, BPXDI4)

blood_pressure_sys_di_2017_2018 <- blood_pressure_data_2017_2018 %>%
  select(SEQN, PEASCCT1, BPXSY1, BPXDI1, BPXSY2,
         BPXDI2, BPXSY3, BPXDI3, BPXSY4, BPXDI4)

# Combine the 3 waves of blood_pressure_data into one dataset
blood_pressure_sys_di_all_waves <- bind_rows(
  blood_pressure_sys_di_2013_2014 %>% mutate(wave_id = "2013-2014"),
  blood_pressure_sys_di_2015_2016 %>% mutate(wave_id = "2015-2016"),
  blood_pressure_sys_di_2017_2018 %>% mutate(wave_id = "2017-2018")
)


# select the columns needed in the demographics datasets
demo_data_2013_2014 <- demographics_data_2013_2014 %>%
  select( SEQN, RIAGENDR, RIDAGEYR, RIDRETH1, RIDRETH3,
          DMDEDUC2, DMDMARTL, DMDFMSIZ, INDFMIN2)

demo_data_2015_2016 <- demographics_data_2015_2016 %>%
  select( SEQN, RIAGENDR, RIDAGEYR, RIDRETH1, RIDRETH3,
          DMDEDUC2, DMDMARTL, DMDFMSIZ, INDFMIN2)

demo_data_2017_2018 <- demographics_data_2017_2018 %>%
  select( SEQN, RIAGENDR, RIDAGEYR, RIDRETH1, RIDRETH3,
          DMDEDUC2, DMDMARTL, DMDFMSIZ, INDFMIN2)

# Combine the 3 waves of demoraphics data into one dataset
demo_data_all_waves <- bind_rows(
  demo_data_2013_2014 %>% mutate(wave_id = "2013-2014"),
  demo_data_2015_2016 %>% mutate(wave_id = "2015-2016"),
  demo_data_2017_2018 %>% mutate(wave_id = "2017-2018")
)

# Merge the 2 combined datasets (demo and blood pressure ) into one full dataset
demo_bp_all <- full_join(
  demo_data_all_waves, blood_pressure_sys_di_all_waves,
  by = "SEQN"
)
# Dimension of the final dataset
skim(demo_bp_all)
colnames(demo_bp_all)
# automatic column names cleaning
demo_bp_all_cleaned <- demo_bp_all %>%
  clean_names()
# keeping one column for wave id and renaming columns
demo_bp_all_cleaned <- demo_bp_all_cleaned %>%
  select(-wave_id_y) %>%
  rename(wave_id = wave_id_x,
         sex = riagendr,
         age = ridageyr,
         ethnicity = ridreth1,
         race = ridreth3,
         educ_level = dmdeduc2,
         marital_status = dmdmartl,
         family_size = dmdfmsiz,
         family_income = indfmin2,
         bpx_comment = peascct1)
# Verify that all the columns are renamed properly
colnames(demo_bp_all_cleaned)
skim(demo_bp_all_cleaned)
head(demo_bp_all_cleaned)

#recoding 
demo_bp_all_recoded <- demo_bp_all_cleaned %>%
  mutate(
    sex = recode(
      sex,
      "1" = "Male",
      "2" = "Female"
    ),
    ethnicity = recode(
      ethnicity,
      "1" = "Mexican american",
      "2" = "Other hispanic",
      "3" = "Non-Hispanic White",
      "4" = "Non-Hispanic Black",
      "5" = "Other race - Including Multi-Racial"
    ),
    race = recode(
      race,
      "1" = "Mexican american",
      "2" = "Other hispanic",
      "3" = "Non-Hispanic White",
      "4" = "Non-Hispanic Black",
      "6" = "Non-Hispanic Asian",
      "7" = "Other race - Including Multi-Racial"
    ),
    educ_level = recode(
      educ_level,
      "1" = "Less than 9th grade",
      "2" = "9-11th grade (Includes 12th grade with no diploma)",
      "3" = "High school graduate/GED or equivalent",
      "4" = "Some college or AA degree",
      "5" = "College graduate or above",
      "7" = "Refused",
      "9" = "Don't Know"
    ),
    marital_status = recode(
      marital_status,
      "1" = "Married",
      "2" = "Widowed",
      "3" = "Divorced",
      "4" = "Separated",
      "5" = "Never married",
      "6" = "Living with partner",
      "77" = "Refused",
      "99" = "Don't Know"
    ),
    family_income = recode(
      family_income,
      "1" = "$ 0 to $ 4,999",
      "2" = "$ 5,000 to $ 9,999",
      "3" = "$10,000 to $14,999",
      "4" = "$15,000 to $19,999",
      "5" = "$20,000 to $24,999",
      "6" = "$25,000 to $34,999",
      "7" = "$35,000 to $44,999",
      "8" = "$45,000 to $54,999",
      "9" = "$55,000 to $64,999",
      "10" = "$65,000 to $74,999",
      "12" = "$20,000 and Over",
      "13" = "Under $20,000",
      "14" = "$75,000 to $99,999",
      "15" = "$100,000 and Over",
      "77" = "Refused",
      "99" = "Don't Know"
    ),
    family_size = recode(
      family_size,
      "1" = "1",
      "2" = "2",
      "3" = "3",
      "4" = "4",
      "5" = "5",
      "6" = "6",
      "7" = "7 or more people in the Family"
    ),
    age_cat = cut(
      age,
      breaks = c(-1, 19, 39, 59, 79, 100),
      labels = c(
        "Less than 20 years of age",
        "20 to 39 years of age",
        "40 to 59 years of age",
        "60 to 79 years of age",
        "80 years of age and over")
    )
  )
#Verify that all the recoded column show the expected categories
head(demo_bp_all_recoded)

# check if there are duplicates
nrow(unique(demo_bp_all_recoded)) == nrow(demo_bp_all_recoded)

# Invalid as missing
# identify the codes that indicate invalid blood pressure in the bpx_comment variable
invalid_bpx <- c(1, 2, 3, 4, 5, 6, 7, 56, 72, 84, 99, 122)
sum(demo_bp_all_recoded$bpx_comment %in% invalid_bpx)
# setting bpx measurement to NA
demo_bp_all_recoded <- demo_bp_all_recoded %>%
  mutate(
    across(
      c(bpxsy1, bpxdi1, bpxsy2, bpxdi2, bpxsy3, bpxdi3, bpxsy4, bpxdi4),
      ~ if_else(bpx_comment %in% invalid_bpx, NA, .)
    )
  )
#check
demo_bp_all_recoded %>%
  filter(bpx_comment %in% invalid_bpx) %>%
  select(
    seqn, bpx_comment,
    bpxsy1, bpxsy2, bpxsy3, bpxsy4,
    bpxdi1, bpxdi2, bpxdi3, bpxdi4
  )
dim(demo_bp_all_recoded)

#exporting
export(demo_bp_all_recoded, here("data", "demo_bp_data.rds"))

# Filtering and reshaping a dataset
demo_bp_all_adult <- demo_bp_all_recoded %>%
  filter(age >= 21) %>%
  select(age, seqn, sex, bpxsy1, bpxsy2, bpxsy3,
         bpxsy4, bpxdi1, bpxdi2, bpxdi3, bpxdi4)
head(demo_bp_all_adult)
# pivot long
demo_bp_all_adult_long <- demo_bp_all_adult %>%
  pivot_longer(
    cols = c(bpxsy1:bpxsy4, bpxdi1:bpxdi4),
    names_to = "bp_measurement",
    values_to = "bp_reading"
  )
# lets look at the new format
head(demo_bp_all_adult_long)