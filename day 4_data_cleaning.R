# ==============================================
# DAY 4: DATA CLEANING & FEATURE ENGINEERING
# ==============================================
# Author: Moussa Rashaideh
# Project: Global Health Disparities Analysis
# Date: 2026-01-19
# ==============================================

# Load libraries
library(tidyverse)
library(visdat)
library(naniar)
library(corrplot)

# Clear workspace
rm(list = ls())

# 1. LOAD DATA FROM DAY 3
cat("Loading data from Day 3...\n")
wide_data <- read_csv("data/worldbank_wide_data.csv")

# Check initial data
cat("Initial data dimensions:", dim(wide_data), "\n")
cat("Countries:", length(unique(wide_data$country)), "\n")
cat("Years:", min(wide_data$year), "-", max(wide_data$year), "\n")

# 2. DATA QUALITY VISUALIZATION
dir.create("plots", showWarnings = FALSE)

# Missing data visualization
missing_plot <- vis_miss(wide_data, cluster = TRUE) +
  labs(title = "Missing Data Pattern") +
  theme_minimal()

ggsave("plots/day4_missing_data.png", missing_plot, width = 12, height = 8)

# 3. HANDLE MISSING VALUES
cat("\n=== HANDLING MISSING VALUES ===\n")

# Calculate missing percentages
missing_stats <- wide_data %>%
  summarise(across(everything(), ~round(sum(is.na(.))/n()*100, 2))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "missing_pct") %>%
  arrange(desc(missing_pct))

print(missing_stats)

# Strategy: Different imputation for different variables
cleaned_data <- wide_data

# For life expectancy: linear interpolation for time series
cleaned_data <- cleaned_data %>%
  group_by(country) %>%
  arrange(year) %>%
  mutate(
    life_exp = ifelse(is.na(life_exp), 
                     approx(year, life_exp, year, method = "linear")$y,
                     life_exp)
  ) %>%
  ungroup()

# For economic indicators: forward/backward fill within countries
economic_vars <- c("gdp_pc", "health_exp", "edu_exp", "poverty")
cleaned_data <- cleaned_data %>%
  group_by(country) %>%
  arrange(year) %>%
  mutate(across(all_of(economic_vars), 
                ~zoo::na.locf(.x, na.rm = FALSE, fromLast = FALSE))) %>%
  mutate(across(all_of(economic_vars), 
                ~zoo::na.locf(.x, na.rm = FALSE, fromLast = TRUE))) %>%
  ungroup()

# For healthcare indicators: impute with regional median
health_vars <- c("sanitation", "infant_mort", "hospital_beds", "literacy")
cleaned_data <- cleaned_data %>%
  group_by(continent, year) %>%
  mutate(across(all_of(health_vars),
                ~ifelse(is.na(.x), median(.x, na.rm = TRUE), .x))) %>%
  ungroup()

# Remove countries with still too many missing values
country_missing <- cleaned_data %>%
  group_by(country) %>%
  summarise(missing_life = sum(is.na(life_exp))/n()) %>%
  filter(missing_life < 0.3)  # Keep countries with <30% missing life data

cleaned_data <- cleaned_data %>%
  filter(country %in% country_missing$country)

# 4. CREATE NEW FEATURES
cat("\n=== CREATING NEW FEATURES ===\n")

cleaned_data <- cleaned_data %>%
  mutate(
    # Income categories (World Bank classification)
    income_group = case_when(
      gdp_pc < 1045 ~ "Low income",
      gdp_pc >= 1045 & gdp_pc < 4095 ~ "Lower-middle income",
      gdp_pc >= 4095 & gdp_pc < 12695 ~ "Upper-middle income",
      gdp_pc >= 12695 ~ "High income",
      TRUE ~ "Unknown"
    ),
    
    # Health investment ratio
    health_investment_ratio = health_exp / gdp_pc * 100,
    
    # Education investment ratio
    edu_investment_ratio = edu_exp / gdp_pc * 100,
    
    # Development index (simplified)
    dev_index = scale(life_exp) + scale(log(gdp_pc)) + scale(literacy/100),
    
    # Healthcare capacity score
    healthcare_capacity = scale(hospital_beds) + scale(health_exp/gdp_pc),
    
    # Region-income interaction
    region_income = paste(continent, income_group, sep = "-"),
    
    # Time period
    period = case_when(
      year <= 2015 ~ "2010-2015",
      year > 2015 ~ "2016-2022"
    ),
    
    # Population categories
    pop_category = case_when(
      population < 1e6 ~ "Small (<1M)",
      population >= 1e6 & population < 10e6 ~ "Medium (1-10M)",
      population >= 10e6 & population < 100e6 ~ "Large (10-100M)",
      population >= 100e6 ~ "Very Large (>100M)"
    )
  )

# 5. OUTLIER DETECTION AND HANDLING
cat("\n=== OUTLIER DETECTION ===\n")

# Identify outliers using IQR method
identify_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower <- Q1 - 1.5 * IQR
  upper <- Q3 + 1.5 * IQR
  return(x < lower | x > upper)
}

# Check key variables for outliers
outlier_summary <- cleaned_data %>%
  summarise(
    life_exp_outliers = sum(identify_outliers(life_exp), na.rm = TRUE),
    gdp_outliers = sum(identify_outliers(log(gdp_pc)), na.rm = TRUE),
    infant_mort_outliers = sum(identify_outliers(infant_mort), na.rm = TRUE)
  )

print(outlier_summary)

# Winsorize extreme outliers (cap at 1st and 99th percentiles)
winsorize <- function(x, lower_quant = 0.01, upper_quant = 0.99) {
  lower_bound <- quantile(x, lower_quant, na.rm = TRUE)
  upper_bound <- quantile(x, upper_quant, na.rm = TRUE)
  x[x < lower_bound] <- lower_bound
  x[x > upper_bound] <- upper_bound
  return(x)
}

cleaned_data <- cleaned_data %>%
  mutate(
    gdp_pc = winsorize(gdp_pc),
    health_exp = winsorize(health_exp),
    infant_mort = winsorize(infant_mort)
  )

# 6. DATA NORMALIZATION
cat("\n=== DATA NORMALIZATION ===\n")

# Create normalized versions for comparison
cleaned_data <- cleaned_data %>%
  mutate(
    # Z-score normalization
    life_exp_z = scale(life_exp),
    gdp_pc_z = scale(log(gdp_pc)),
    
    # Min-max normalization (0-1)
    life_exp_norm = (life_exp - min(life_exp, na.rm = TRUE)) / 
                   (max(life_exp, na.rm = TRUE) - min(life_exp, na.rm = TRUE)),
    gdp_pc_norm = (gdp_pc - min(gdp_pc, na.rm = TRUE)) / 
                 (max(gdp_pc, na.rm = TRUE) - min(gdp_pc, na.rm = TRUE))
  )

# 7. DATA CONSISTENCY CHECKS
cat("\n=== DATA CONSISTENCY CHECKS ===\n")

# Check for logical inconsistencies
consistency_issues <- cleaned_data %>%
  filter(
    life_exp < 20 | life_exp > 90 |           # Unrealistic life expectancy
    infant_mort > 200 |                       # Unrealistic infant mortality
    sanitation > 100 | literacy > 100         # Percentages over 100%
  )

if (nrow(consistency_issues) > 0) {
  cat("Found", nrow(consistency_issues), "potential data issues\n")
  print(head(consistency_issues))
} else {
  cat("No major consistency issues found\n")
}

# 8. SAVE CLEANED DATA
cat("\n=== SAVING CLEANED DATA ===\n")

# Save full cleaned dataset
write_csv(cleaned_data, "data/cleaned_data_full.csv")

# Create and save 2022 snapshot for analysis
data_2022 <- cleaned_data %>%
  filter(year == 2022) %>%
  drop_na(life_exp, gdp_pc)

write_csv(data_2022, "data/cleaned_data_2022.csv")

# Create panel data structure
panel_data <- cleaned_data %>%
  select(country, iso3c, continent, region, year, 
         life_exp, gdp_pc, health_exp, edu_exp, 
         sanitation, infant_mort, hospital_beds, 
         literacy, poverty, population, income_group)

write_csv(panel_data, "data/panel_data.csv")

# 9. VISUALIZE CLEANED DATA
# Distribution after cleaning
p_clean_life <- cleaned_data %>%
  filter(year == 2022) %>%
  ggplot(aes(x = life_exp)) +
  geom_histogram(bins = 30, fill = "#2ecc71", alpha = 0.8) +
  labs(title = "Life Expectancy Distribution After Cleaning (2022)",
       x = "Life Expectancy (years)", y = "Count") +
  theme_minimal()

# Income group distribution
p_income_groups <- cleaned_data %>%
  filter(year == 2022) %>%
  count(income_group) %>%
  ggplot(aes(x = reorder(income_group, n), y = n, fill = income_group)) +
  geom_col() +
  geom_text(aes(label = n), hjust = -0.2) +
  labs(title = "Countries by Income Group (2022)",
       x = "Income Group", y = "Number of Countries") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("plots/day4_life_after_cleaning.png", p_clean_life, width = 10, height = 6)
ggsave("plots/day4_income_groups.png", p_income_groups, width = 10, height = 6)

# 10. GENERATE DATA QUALITY REPORT
cat("\n=== DATA QUALITY REPORT ===\n")
cat("Final dataset dimensions:", dim(cleaned_data), "\n")
cat("Countries remaining:", length(unique(cleaned_data$country)), "\n")
cat("Years:", min(cleaned_data$year), "-", max(cleaned_data$year), "\n")

# Missing data after cleaning
missing_final <- cleaned_data %>%
  summarise(across(c(life_exp, gdp_pc, health_exp, sanitation), 
                   ~round(sum(is.na(.))/n()*100, 2)))

cat("\nMissing values after cleaning (%):\n")
print(missing_final)

# Summary statistics
cat("\n=== SUMMARY STATISTICS (2022) ===\n")
data_2022 %>%
  summarise(
    mean_life = mean(life_exp, na.rm = TRUE),
    sd_life = sd(life_exp, na.rm = TRUE),
    mean_gdp = mean(gdp_pc, na.rm = TRUE),
    median_gdp = median(gdp_pc, na.rm = TRUE),
    n_countries = n()
  ) %>%
  print()

cat("\n=== DAY 4 COMPLETED ===\n")
cat("✓ Data cleaning completed\n")
cat("✓ Created", ncol(cleaned_data) - ncol(wide_data), "new features\n")
cat("✓ Removed outliers and handled missing values\n")
cat("✓ Saved cleaned data to data/ folder\n")
cat("✓ Generated data quality report\n")
cat("✓ Next step: Run day5_eda.R for exploratory analysis\n")
