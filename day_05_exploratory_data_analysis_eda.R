# ==============================================
# DAY 5: EXPLORATORY DATA ANALYSIS (EDA)
# ==============================================
# Author: Moussa Rashaideh
# Project: Global Health Disparities Analysis
# Date: 2026-01-19
# ==============================================

# Load libraries
library(tidyverse)
library(ggplot2)
library(GGally)
library(patchwork)
library(ggcorrplot)
library(viridis)
library(plotly)

# Clear workspace
rm(list = ls())

# 1. LOAD CLEANED DATA
cat("Loading cleaned data...\n")
data_2022 <- read_csv("data/cleaned_data_2022.csv")
panel_data <- read_csv("data/panel_data.csv")

cat("2022 data:", nrow(data_2022), "countries\n")
cat("Panel data:", nrow(panel_data), "observations\n")

# 2. BASIC DESCRIPTIVE STATISTICS
cat("\n=== DESCRIPTIVE STATISTICS ===\n")

# Summary table
summary_stats <- data_2022 %>%
  select(life_exp, gdp_pc, health_exp, infant_mort, sanitation, literacy) %>%
  psych::describe() %>%
  select(mean, sd, min, max, skew, kurtosis) %>%
  round(2)

print(summary_stats)

# 3. CORRELATION ANALYSIS
cat("\n=== CORRELATION MATRIX ===\n")

# Select numeric variables for correlation
cor_vars <- data_2022 %>%
  select(life_exp, gdp_pc, health_exp, edu_exp, 
         sanitation, infant_mort, hospital_beds, literacy)

cor_matrix <- cor(cor_vars, use = "complete.obs")
print(round(cor_matrix, 3))

# Visualize correlation matrix
p_corr <- ggcorrplot(cor_matrix, 
                    method = "circle",
                    type = "lower",
                    lab = TRUE,
                    colors = c("#6D9EC1", "white", "#E46726"),
                    title = "Correlation Matrix of Health Indicators")

# 4. UNIVARIATE ANALYSIS
cat("\n=== UNIVARIATE DISTRIBUTIONS ===\n")

# Create distribution plots for key variables
variables <- c("life_exp", "gdp_pc", "health_exp", "infant_mort", "literacy")
var_names <- c("Life Expectancy", "GDP per capita", "Health Expenditure", 
               "Infant Mortality", "Literacy Rate")

# Create multi-panel distribution plot
dist_plots <- list()
for (i in 1:length(variables)) {
  dist_plots[[i]] <- ggplot(data_2022, aes(x = .data[[variables[i]]])) +
    geom_histogram(bins = 30, fill = viridis(5)[i], alpha = 0.8) +
    geom_density(aes(y = after_stat(count) * 2), color = "darkred", size = 1) +
    labs(title = var_names[i],
         x = var_names[i],
         y = "Count") +
    theme_minimal()
}

# Combine distribution plots
p_distributions <- wrap_plots(dist_plots, ncol = 2) +
  plot_annotation(title = "Distribution of Key Variables")

# 5. BIVARIATE ANALYSIS
cat("\n=== BIVARIATE RELATIONSHIPS ===\n")

# Scatter plot matrix
p_scatter_matrix <- ggpairs(data_2022 %>%
                             select(life_exp, gdp_pc, health_exp, infant_mort, literacy),
                           upper = list(continuous = wrap("cor", size = 3)),
                           lower = list(continuous = "smooth"),
                           progress = FALSE) +
  theme_minimal()

# 6. INCOME GROUP ANALYSIS
cat("\n=== ANALYSIS BY INCOME GROUP ===\n")

# Life expectancy by income group
p_income_life <- data_2022 %>%
  ggplot(aes(x = income_group, y = life_exp, fill = income_group)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.5, size = 1) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "red") +
  labs(title = "Life Expectancy by Income Group",
       x = "Income Group", y = "Life Expectancy (years)") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

# Calculate group statistics
income_stats <- data_2022 %>%
  group_by(income_group) %>%
  summarise(
    mean_life = mean(life_exp, na.rm = TRUE),
    mean_gdp = mean(gdp_pc, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

print(income_stats)

# 7. REGIONAL ANALYSIS
cat("\n=== REGIONAL ANALYSIS ===\n")

# Life expectancy by continent
p_continent_life <- data_2022 %>%
  ggplot(aes(x = reorder(continent, life_exp, median), 
             y = life_exp, 
             fill = continent)) +
  geom_violin(alpha = 0.7) +
  geom_boxplot(width = 0.2, fill = "white") +
  labs(title = "Life Expectancy Distribution by Continent",
       x = "Continent", y = "Life Expectancy (years)") +
  theme_minimal() +
  theme(legend.position = "none")

# Regional statistics
regional_stats <- data_2022 %>%
  group_by(continent) %>%
  summarise(
    median_life = median(life_exp, na.rm = TRUE),
    median_gdp = median(gdp_pc, na.rm = TRUE),
    countries = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(median_life))

print(regional_stats)

# 8. TIME SERIES ANALYSIS
cat("\n=== TIME TRENDS (2010-2022) ===\n")

# Global average trends
global_trends <- panel_data %>%
  group_by(year) %>%
  summarise(
    avg_life = mean(life_exp, na.rm = TRUE),
    avg_gdp = mean(gdp_pc, na.rm = TRUE),
    n_countries = n(),
    .groups = "drop"
  )

p_global_trend <- ggplot(global_trends, aes(x = year)) +
  geom_line(aes(y = avg_life), color = "#3498db", size = 1.5) +
  geom_point(aes(y = avg_life), color = "#3498db", size = 2) +
  geom_line(aes(y = avg_gdp/5000), color = "#e74c3c", size = 1.5) +
  geom_point(aes(y = avg_gdp/5000), color = "#e74c3c", size = 2) +
  scale_y_continuous(
    name = "Life Expectancy (years)",
    sec.axis = sec_axis(~.*5000, name = "GDP per capita (PPP $)")
  ) +
  labs(title = "Global Trends: Life Expectancy vs GDP (2010-2022)",
       subtitle = "Blue: Life Expectancy, Red: GDP per capita") +
  theme_minimal() +
  theme(axis.title.y.right = element_text(color = "#e74c3c"),
        axis.text.y.right = element_text(color = "#e74c3c"))

# Trends by income group
income_trends <- panel_data %>%
  group_by(year, income_group) %>%
  summarise(
    avg_life = mean(life_exp, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

p_income_trend <- ggplot(income_trends, aes(x = year, y = avg_life, color = income_group)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(title = "Life Expectancy Trends by Income Group",
       x = "Year", y = "Life Expectancy (years)",
       color = "Income Group") +
  theme_minimal()

# 9. OUTLIER DETECTION VISUALIZATION
cat("\n=== OUTLIER IDENTIFICATION ===\n")

# Cook's distance for regression
model <- lm(life_exp ~ log(gdp_pc) + health_exp + literacy, data = data_2022)
cooksd <- cooks.distance(model)

# Identify influential points
influential <- which(cooksd > 4/length(cooksd))
if (length(influential) > 0) {
  cat("Found", length(influential), "influential observations:\n")
  print(data_2022[influential, c("country", "life_exp", "gdp_pc")])
}

# Visualize influential points
p_influential <- ggplot(data_2022, aes(x = log(gdp_pc), y = life_exp)) +
  geom_point(aes(color = ifelse(row_number() %in% influential, "Influential", "Normal"),
                 size = ifelse(row_number() %in% influential, 3, 1)),
             alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "darkred") +
  labs(title = "Influential Points Detection",
       subtitle = "Red points are influential observations",
       x = "Log(GDP per capita)", y = "Life Expectancy") +
  scale_color_manual(values = c("Influential" = "red", "Normal" = "blue")) +
  theme_minimal() +
  theme(legend.position = "none")

# 10. INTERACTIVE VISUALIZATION (optional)
if (interactive()) {
  # Create interactive scatter plot
  p_interactive <- plot_ly(data = data_2022,
                          x = ~log(gdp_pc),
                          y = ~life_exp,
                          text = ~paste("Country:", country, 
                                       "<br>Life Exp:", round(life_exp, 1),
                                       "<br>GDP:", round(gdp_pc)),
                          color = ~continent,
                          size = ~population,
                          hoverinfo = "text",
                          type = "scatter",
                          mode = "markers") %>%
    layout(title = "Interactive: GDP vs Life Expectancy by Continent",
           xaxis = list(title = "Log(GDP per capita)"),
           yaxis = list(title = "Life Expectancy (years)"))
  
  print(p_interactive)
}

# 11. SAVE ALL PLOTS
cat("\n=== SAVING VISUALIZATIONS ===\n")

ggsave("plots/day5_correlation_matrix.png", p_corr, width = 10, height = 8)
ggsave("plots/day5_distributions.png", p_distributions, width = 12, height = 10)
ggsave("plots/day5_scatter_matrix.png", p_scatter_matrix, width = 12, height = 10)
ggsave("plots/day5_income_life.png", p_income_life, width = 10, height = 6)
ggsave("plots/day5_continent_life.png", p_continent_life, width = 10, height = 6)
ggsave("plots/day5_global_trend.png", p_global_trend, width = 10, height = 6)
ggsave("plots/day5_income_trend.png", p_income_trend, width = 10, height = 6)
ggsave("plots/day5_influential_points.png", p_influential, width = 10, height = 6)

# 12. GENERATE EDA REPORT
cat("\n=== EDA FINDINGS ===\n")

# Key findings
findings <- list(
  strongest_correlation = which.max(abs(cor_matrix[lower.tri(cor_matrix)])),
  life_gdp_cor = cor_matrix["life_exp", "gdp_pc"],
  life_infant_cor = cor_matrix["life_exp", "infant_mort"],
  highest_life_continent = regional_stats$continent[1],
  lowest_life_continent = regional_stats$continent[nrow(regional_stats)],
  global_life_increase = round(max(global_trends$avg_life) - min(global_trends$avg_life), 1)
)

cat("1. Strongest correlation:", round(max(abs(cor_matrix[lower.tri(cor_matrix)])), 3), "\n")
cat("2. Life expectancy vs GDP correlation:", round(findings$life_gdp_cor, 3), "\n")
cat("3. Life expectancy vs Infant mortality correlation:", round(findings$life_infant_cor, 3), "\n")
cat("4. Highest life expectancy continent:", findings$highest_life_continent, "\n")
cat("5. Lowest life expectancy continent:", findings$lowest_life_continent, "\n")
cat("6. Global life expectancy increase (2010-2022):", findings$global_life_increase, "years\n")
cat("7. Countries analyzed:", nrow(data_2022), "\n")
cat("8. Income groups represented:", length(unique(data_2022$income_group)), "\n")

# 13. SAVE EDA RESULTS
eda_results <- list(
  summary_stats = summary_stats,
  cor_matrix = cor_matrix,
  income_stats = income_stats,
  regional_stats = regional_stats,
  global_trends = global_trends,
  findings = findings
)

saveRDS(eda_results, "results/eda_results.rds")

cat("\n=== DAY 5 COMPLETED ===\n")
cat("✓ Comprehensive EDA performed\n")
cat("✓ Generated 8 visualization plots\n")
cat("✓ Analyzed correlations and trends\n")
cat("✓ Identified key patterns and outliers\n")
cat("✓ Saved results to results/ folder\n")
cat("✓ Next step: Run day6_modeling.R for statistical modeling\n")
