
# 01_data_collection.R
library(WDI)
library(tidyverse)

cat("Collecting World Bank data...\n")
data <- WDI(
  indicator = c(
    life_expectancy = "SP.DYN.LE00.IN",
    gdp_per_capita = "NY.GDP.PCAP.PP.KD"
  ),
  start = 2015,
  end = 2022,
  extra = TRUE
)

cat("Data collected:", nrow(data), "rows\n")
saveRDS(data, "data/wdi_data.rds")
cat("Data saved to data/wdi_data.rds\n")

