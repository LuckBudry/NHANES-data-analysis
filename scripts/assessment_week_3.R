# Load the required packages
library(pacman)
pacman::p_load(
  rio, here, tidyverse, knitr, kableExtra, skimr, RColorBrewer, viridis,
  formatR, gridExtra, janitor, ggplot2, ggpubr, cowplot, gghighlight, ggokabeito
)

# Import the clean NHANES dataset
nhanes_data <- import(here("data", "cleaned_NHANES.csv"))

## Reproducung and arranging ggplot2 figures
figure_1 <- nhanes_data %>% 
  filter(age >=18) %>%
ggplot(aes(x = age, fill = gender)) +
  geom_histogram(binwidth = 5, position = "dodge")

figure_2 <- nhanes_data %>% 
  ggplot(aes(x = ethnicity_2, fill = gender)) +
  geom_bar(position = "fill")

figure_2
# combine the plots
plot_grid(figure_1, figure_2)

# combine the plots with ggarrange
ggarrange(figure_1, figure_2,
  common.legend = TRUE, legend = "top"
)
# The layout produced with ggarrange() looks better for these two plots because it allows us to use a single legend for both figures.
# the cowplot package layout produced two separate legends, which took up unnecessary space and made the combined figure look less clean and almost unreadable.

# Exercise 2 : Visualizing key characteristics
# Age distribution
p_age <- ggplot(nhanes_data, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "steelblue") +
  labs(
    title = "Age distribrution",
    x = "Age (years)",
    y = "Count"
  )
p_age
ggsave(
  "age_distribrution.png",
  plot = p_age,
  path = here("figures"),
  width = 8, height = 6
)
# For the age distribution in our sample, there appears to be a relatively high number of participants under 18 years old, with up to 3,000 participants in the 5–10 and 10–15 age groups.
# From ages 20 to 80, there are approximately 1,000 to 1,500 participants in each 5-year age group. The only exception is the 70–75 age group, which has fewer than 1,000 participants.
# Gender distribution
p_gender <- ggplot(nhanes_data, aes(x = gender)) +
  geom_bar(aes(fill = gender)) +
  labs(
    title = "Gender distribrution",
    x = "Gender",
    y = "Count",
    fill = "Gender"
  )
p_gender
ggsave(
  "gender_distribrution.png",
  plot = p_gender,
  path = here("figures"),
  width = 8, height = 6
)
# The sample appears to have a relatively even 50–50 split between males and females, with slightly more female participants than male participants.
# ethnicity_1 and ethnicity_2
p_eth1 <- ggplot(nhanes_data, aes(y = ethnicity_1)) +
  geom_bar(aes(fill = ethnicity_1)) +
  labs(
    title = "Ethnicity 1 distribrution",
    x = "Count",
    y = "Ethnicity 1",
    fill = "Ethnicity 1"
  )
p_eth1
ggsave(
  "ethnicity_1_distribrution.png",
  plot = p_eth1,
  path = here("figures"),
  width = 8, height = 6
)

p_eth2 <- ggplot(nhanes_data, aes(y = ethnicity_2)) +
  geom_bar(aes(fill = ethnicity_2)) +
  labs(
    title = "Ethnicity 2 distribrution",
    x = "Count",
    y = "Ethnicity 2",
    fill = "Ethnicity 2"
  )
ggsave(
  "ethnicity_2_distribrution.png",
  plot = p_eth2,
  path = here("figures"),
  width = 8, height = 6
)
p_eth2
# combine the plots with ggarrange
p_eth_1_2 <- ggarrange(
  p_eth1, p_eth2,
  common.legend = FALSE, legend = "top"
)

p_eth_1_2
ggsave(
  "combined_ethnicity_distribrution.png",
  plot = p_eth2,
  path = here("figures"),
  width = 8, height = 6
)

# For both ethnicity variables, there are more White participants in the sample (approximately 10,000), followed by Black participants (about 6,500) and Mexican participants (about 5,000). 
# There are also approximately 3,000 Asian participants, 3,000 participants in the Other Hispanic category, and 1,500 in the Other category.

# When comparing the two ethnicity variables, ethnicity_1 does not have a separate category for Asian participants and instead includes them in the Other category. 
# Therefore, I would choose to keep ethnicity_2 because it provides more detailed information about the participants’ ethnicity and allows us to distinguish the Asian group from the Other category

# remove ethnicity_1
nhanes_data <- nhanes_data %>%
  select(-ethnicity_1)

diet_data <- import(here("data", "diet.csv"))
ggplot(diet_data, aes(x = Week, y = Weight, color = factor(Participant))) +
  geom_line()

ggplot(diet_data, aes(x = Week, y = Weight, group = Participant)) +
  geom_line() +
  labs(
    x = "Week",
    y = "Weight"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line()
  )
# Exercise 4 : Exploring relationships through visualizations
# Create average Systolic blood pressure
nhanes_data <- nhanes_data %>%
  mutate(
    avg_sbp = rowMeans(
      select(., systolic_bp_1, systolic_bp_2, systolic_bp_3, systolic_bp_4),
      na.rm = TRUE
    ),
    avg_sbp = ifelse(is.nan(avg_sbp), NA, avg_sbp)
  )
# remove individuals who are missing age, gender, or average SBP
nhanes_final <- nhanes_data %>%
  filter(
    !is.na(age),
    !is.na(gender),
    !is.na(avg_sbp)
  )
# Create the hypertension category
nhanes_final <- nhanes_final %>%
  mutate(
    sbp_category = case_when(
      avg_sbp < 120 ~ "Normal",
      avg_sbp >= 120 & avg_sbp < 130 ~ "Elevated",
      avg_sbp >= 130 & avg_sbp < 140 ~ "Stage 1 hypertension",
      avg_sbp >= 140 ~ "Stage 2 hypertension"
    ),
    sbp_category = factor(
      sbp_category,
      levels = c(
        "Normal",
        "Elevated",
        "Stage 1 hypertension",
        "Stage 2 hypertension"
      )
    )
  )

# Final sample description
# sample size
n_data <- nrow(nhanes_data)
n_final <- nrow(nhanes_final)
n_diff <- n_original - n_final
# The original NHANES dataset included X participants. 
# After excluding participants with missing age, gender, or average SBP, Y participants remained in the analytical sample, corresponding to round(n_final / n_original * 100, 1)% of the original sample.

# Hypertension category distribution
ggplot(nhanes_final, aes(x = sbp_category)) +
  geom_bar(fill = "steelblue") +
  labs(
    x = "Average systolic blood pressure category",
    y = "Number of participants"
  ) +
  theme_minimal()
# Gender distribution in final sample
ggplot(nhanes_final, aes(x = gender)) +
  geom_bar(aes(fill = gender)) +
  labs(
    title = "Gender distribrution",
    x = "Gender",
    y = "Number of participants",
    fill = "Gender"
  )

# hypertension across age and gender
ggplot(
  nhanes_final,
  aes(x = age_cat, fill = sbp_category)
) +
  geom_bar(position = "dodge") +
  facet_wrap(~ gender) +
  scale_fill_manual(
        values = c(
          "Normal" = "#D9F0D3",
          "Elevated" = "#A6DBA0",
          "Stage 1 hypertension" = "#5AAE61",
          "Stage 2 hypertension" = "#1B7837"
          )
      ) +
  labs(
    x = "Age category",
    y = "Number of participants",
    fill = "SBP category"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )
