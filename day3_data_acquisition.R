# ==============================================
# DAY 3: WORLD BANK DATA ACQUISITION & EXPLORATION
# ==============================================
# Author: Mousssa rashaideh
# Project: Global Health Disparities Analysis
# Date: 2026-01-19
# ==============================================

# Install required packages (run once)
# install.packages(c("wbstats", "tidyverse", "ggplot2", "countrycode", "DT"))

# Load libraries
library(wbstats)
library(tidyverse)
library(ggplot2)
library(countrycode)
library(DT)

# Clear workspace
rm(list = ls())

# Set working directory (adjust to your project folder)
# setwd("~/global-health-disparities-analysis")

# 1. DEFINE INDICATORS OF INTEREST
indicators <- c(
  "SP.DYN.LE00.IN",        # Life expectancy at birth
  "NY.GDP.PCAP.PP.KD",     # GDP per capita (PPP)
  "SH.XPD.CHEX.PP.CD",     # Health expenditure per capita
  "SE.XPD.TOTL.GD.ZS",     # Education expenditure (% of GDP)
  "SH.STA.ACSN",           # Improved sanitation access (% population)
  "SP.DYN.IMRT.IN",        # Infant mortality rate
  "SH.MED.BEDS.ZS",        # Hospital beds per 1,000 people
  "SP.POP.TOTL",           # Population total
  "SI.POV.NAHC",           # Poverty headcount ratio
  "SE.ADT.LITR.ZS"         # Literacy rate
)

# Indicator names for better readability
indicator_names <- c(
  "Life Expectancy",
  "GDP per capita (PPP)",
  "Health expenditure per capita",
  "Education expenditure",
  "Sanitation access",
  "Infant mortality",
  "Hospital beds",
  "Population",
  "Poverty rate",
  "Literacy rate"
)

names(indicators) <- indicator_names

# 2. FETCH DATA FROM WORLD BANK API (2010-2022)
cat("Downloading World Bank data for 2010-2022...\n")

# Download data for all indicators
wb_data <- wb_data(
  country = "countries_only",  # Only countries, no aggregates
  indicator = indicators,
  start_date = 2010,
  end_date = 2022,
  return_wide = FALSE
)

# Check data structure
cat("Data download complete!\n")
cat("Total observations:", nrow(wb_data), "\n")
cat("Number of countries:", length(unique(wb_data$country)), "\n")
cat("Years covered:", min(wb_data$date), "to", max(wb_data$date), "\n")

# 3. EXPLORE DATA STRUCTURE
cat("\n=== DATA STRUCTURE ===\n")
str(wb_data)

cat("\n=== SUMMARY STATISTICS ===\n")
summary(wb_data$value)

# 4. RESHAPE DATA TO WIDE FORMAT
wide_data <- wb_data %>%
  pivot_wider(
    id_cols = c(country, iso3c, date),
    names_from = indicator,
    values_from = value
  )

# Rename columns for easier reference
colnames(wide_data) <- c("country", "iso3c", "year", 
                         "life_exp", "gdp_pc", "health_exp", 
                         "edu_exp", "sanitation", "infant_mort",
                         "hospital_beds", "population", "poverty", "literacy")

# 5. ADD REGIONAL CLASSIFICATION
wide_data <- wide_data %>%
  mutate(
    region = countrycode(iso3c, "iso3c", "region"),
    continent = countrycode(iso3c, "iso3c", "continent")
  )

# 6. INITIAL EXPLORATION VISUALIZATIONS
# Create output directory
dir.create("plots", showWarnings = FALSE)
dir.create("data", showWarnings = FALSE)

# Plot 1: Life Expectancy Distribution (2022)
latest_data <- wide_data %>%
  filter(year == 2022) %>%
  drop_na(life_exp)

p1 <- ggplot(latest_data, aes(x = life_exp)) +
  geom_histogram(bins = 30, fill = "#3498db", alpha = 0.8) +
  geom_vline(aes(xintercept = median(life_exp, na.rm = TRUE)), 
             color = "red", linetype = "dashed", size = 1) +
  labs(title = "Distribution of Life Expectancy (2022)",
       subtitle = paste("Median:", round(median(latest_data$life_exp, na.rm = TRUE), 1), "years"),
       x = "Life Expectancy (years)", y = "Number of Countries") +
  theme_minimal(base_size = 12)

# Plot 2: Life Expectancy by Continent
p2 <- latest_data %>%
  drop_na(life_exp, continent) %>%
  ggplot(aes(x = reorder(continent, life_exp, median), y = life_exp, fill = continent)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "Life Expectancy by Continent (2022)",
       x = "Continent", y = "Life Expectancy (years)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none") +
  coord_flip()

# Plot 3: GDP vs Life Expectancy Scatter
p3 <- latest_data %>%
  drop_na(life_exp, gdp_pc) %>%
  ggplot(aes(x = log(gdp_pc), y = life_exp, size = population, color = continent)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "GDP per capita vs Life Expectancy (2022)",
       x = "Log(GDP per capita) - PPP",
       y = "Life Expectancy (years)",
       size = "Population",
       color = "Continent") +
  theme_minimal(base_size = 12)

# Save plots
ggsave("plots/day3_life_exp_distribution.png", p1, width = 10, height = 6)
ggsave("plots/day3_life_by_continent.png", p2, width = 10, height = 6)
ggsave("plots/day3_gdp_vs_life.png", p3, width = 10, height = 6)

# 7. SAVE RAW DATA
write_csv(wb_data, "data/worldbank_raw_data.csv")
write_csv(wide_data, "data/worldbank_wide_data.csv")

# 8. DATA QUALITY CHECK
cat("\n=== DATA QUALITY REPORT ===\n")

missing_summary <- wide_data %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "missing_count") %>%
  mutate(missing_pct = round(missing_count / nrow(wide_data) * 100, 1))

print(missing_summary)

# 9. CREATE INTERACTIVE TABLE (for HTML output)
if (interactive()) {
  DT::datatable(
    latest_data %>% 
      select(country, continent, life_exp, gdp_pc, health_exp) %>%
      arrange(desc(life_exp)) %>%
      head(20),
    options = list(pageLength = 10),
    caption = "Top 20 Countries by Life Expectancy (2022)"
  )
}

# 10. GENERATE REPORT
cat("\n=== DAY 3 COMPLETED ===\n")
cat("✓ Data downloaded for", length(unique(wide_data$country)), "countries\n")
cat("✓ Time period:", min(wide_data$year), "-", max(wide_data$year), "\n")
cat("✓ Variables:", ncol(wide_data) - 3, "indicators\n")
cat("✓ Saved raw data to: data/worldbank_raw_data.csv\n")
cat("✓ Saved wide data to: data/worldbank_wide_data.csv\n")
cat("✓ Generated 3 exploratory plots in plots/ folder\n")
cat("✓ Next step: Run day4_data_cleaning.R\n")

# Print summary statistics
cat("\n=== KEY STATISTICS (2022) ===\n")
cat("Average Life Expectancy:", round(mean(latest_data$life_exp, na.rm = TRUE), 1), "years\n")
cat("Highest Life Expectancy:", max(latest_data$life_exp, na.rm = TRUE), "years\n")
cat("Lowest Life Expectancy:", min(latest_data$life_exp, na.rm = TRUE), "years\n")
cat("Range:", round(max(latest_data$life_exp, na.rm = TRUE) - min(latest_data$life_exp, na.rm = TRUE), 1), "years\n")

# Print top and bottom 5 countries
cat("\nTop 5 Countries by Life Expectancy:\n")
latest_data %>%
  arrange(desc(life_exp)) %>%
  select(country, life_exp) %>%
  head(5) %>%
  print()

cat("\nBottom 5 Countries by Life Expectancy:\n")
latest_data %>%
  arrange(life_exp) %>%
  select(country, life_exp) %>%
  head(5) %>%
  print()
