# ====================================================
# DAY 8: INTERACTIVE DASHBOARD
# ====================================================
# Simple Shiny Dashboard for Health Disparities Analysis
# ====================================================
# Load required packages
if (!require("shiny")) install.packages("shiny")
if (!require("shinydashboard")) install.packages("shinydashboard")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("plotly")) install.packages("plotly")
library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
# Load data
data <- read.csv("data/cleaned_data_2022.csv")
# Load model
if (file.exists("models/random_forest.rds")) {
rf_model <- readRDS("models/random_forest.rds")
} else {
# Create simple model if not exists
rf_model <- lm(life_exp ~ gdp_pc + health_exp + infant_mort, data = data)
}
# UI Definition
ui <- dashboardPage(
skin = "blue",
dashboardHeader(
title = "Global Health Disparities Dashboard",
titleWidth = 300
),
dashboardSidebar(
width = 300,
sidebarMenu(
menuItem("Dashboard Overview", tabName = "overview", icon =
icon("dashboard")),
menuItem("Country Explorer", tabName = "explorer", icon =
icon("globe")),
menuItem("Predictive Analytics", tabName = "predict", icon =
icon("chart-line")),
menuItem("Regional Analysis", tabName = "regional", icon = icon("map")),
menuItem("Data Download", tabName = "download", icon = icon("download"))
),
br(),
h4("Filters", style = "padding-left: 20px;"),
selectInput("continent", "Select Continent:",
choices = c("All", sort(unique(data$continent))),
selected = "All"),
selectInput("income", "Select Income Group:",
choices = c("All", sort(unique(data$income_group))),
selected = "All"),
sliderInput("gdp_range", "GDP Range ($):",
min = min(data$gdp_pc),
max = max(data$gdp_pc),
value = c(min(data$gdp_pc), max(data$gdp_pc))),
actionButton("reset", "Reset Filters", icon = icon("refresh"))
),
dashboardBody(
tabItems(
# Tab 1: Overview
tabItem(tabName = "overview",
fluidRow(
valueBoxOutput("total_countries"),
valueBoxOutput("avg_life"),
valueBoxOutput("avg_gdp")
),
fluidRow(
box(
title = "Global Life Expectancy Distribution",
status = "primary",
solidHeader = TRUE,
width = 6,
plotlyOutput("life_dist")
),
box(
title = "GDP vs Life Expectancy",
status = "success",
solidHeader = TRUE,
width = 6,
plotlyOutput("gdp_life_plot")
)
),
fluidRow(
box(
title = "Top 10 Countries",
status = "info",
solidHeader = TRUE,
width = 6,
tableOutput("top_countries")
),
box(
title = "Bottom 10 Countries",
status = "warning",
solidHeader = TRUE,
width = 6,
tableOutput("bottom_countries")
)
)
),
# Tab 2: Country Explorer
tabItem(tabName = "explorer",
fluidRow(
box(
title = "Select Countries to Compare",
status = "primary",
solidHeader = TRUE,
width = 4,
selectInput("country1", "Country 1:",
choices = sort(unique(data$country)),
selected = data$country[1]),
selectInput("country2", "Country 2:",
choices = sort(unique(data$country)),
selected = data$country[2]),
selectInput("country3", "Country 3:",
choices = sort(unique(data$country)),
selected = data$country[3])
),
box(
title = "Country Comparison",
status = "success",
solidHeader = TRUE,
width = 8,
plotlyOutput("country_comparison")
)
),
fluidRow(
box(
title = "Country Details",
status = "info",
solidHeader = TRUE,
width = 12,
uiOutput("country_details")
)
)
),
# Tab 3: Predictive Analytics
tabItem(tabName = "predict",
fluidRow(
box(
title = "Predict Life Expectancy",
status = "primary",
solidHeader = TRUE,
width = 6,
numericInput("pred_gdp", "GDP per capita ($):",
value = 10000, min = 500, max = 100000),
numericInput("pred_health", "Health expenditure ($):",
value = 500, min = 10, max = 10000),
numericInput("pred_infant", "Infant mortality (per 1000):",
value = 20, min = 1, max = 100),
numericInput("pred_sanitation", "Sanitation access (%):",
value = 70, min = 0, max = 100),
actionButton("predict_btn", "Predict",
class = "btn-success btn-lg")
),
box(
title = "Prediction Results",
status = "success",
solidHeader = TRUE,
width = 6,
h3("Predicted Life Expectancy:"),
verbatimTextOutput("prediction_output"),
hr(),
h4("Interpretation:"),
textOutput("prediction_interpretation"),
hr(),
h4("Similar Countries:"),
textOutput("similar_countries")
)
),
fluidRow(
box(
title = "What-If Analysis",
status = "info",
solidHeader = TRUE,
width = 12,
sliderInput("whatif_gdp", "What if GDP changes to:",
min = 1000, max = 50000, value = 10000),
plotlyOutput("whatif_plot")
)
)
),
# Tab 4: Regional Analysis
tabItem(tabName = "regional",
fluidRow(
box(
title = "Life Expectancy by Region",
status = "primary",
solidHeader = TRUE,
width = 8,
plotlyOutput("regional_plot")
),
box(
title = "Region Statistics",
status = "success",
solidHeader = TRUE,
width = 4,
tableOutput("region_stats")
)
),
fluidRow(
box(
title = "Income Group Analysis",
status = "info",
solidHeader = TRUE,
width = 12,
plotlyOutput("income_plot")
)
)
),
# Tab 5: Data Download
tabItem(tabName = "download",
box(
title = "Download Data",
status = "primary",
solidHeader = TRUE,
width = 12,
h4("Select data format:"),
radioButtons("format", "Format:",
choices = c("CSV", "Excel", "JSON"),
inline = TRUE),
br(),
downloadButton("download_data", "Download Data"),
hr(),
h4("Data Preview:"),
dataTableOutput("data_preview")
)
)
)
)
)
# Server Logic
server <- function(input, output, session) {
# Reactive filtered data
filtered_data <- reactive({
df <- data
if (input$continent != "All") {
df <- df[df$continent == input$continent, ]
}
if (input$income != "All") {
df <- df[df$income_group == input$income, ]
}
df <- df[df$gdp_pc >= input$gdp_range[1] & df$gdp_pc <=
input$gdp_range[2], ]
return(df)
})
# Reset filters
observeEvent(input$reset, {
updateSelectInput(session, "continent", selected = "All")
updateSelectInput(session, "income", selected = "All")
updateSliderInput(session, "gdp_range",
value = c(min(data$gdp_pc), max(data$gdp_pc)))
})
# Value boxes
output$total_countries <- renderValueBox({
valueBox(
nrow(filtered_data()),
"Countries",
icon = icon("flag"),
color = "blue"
)
})
output$avg_life <- renderValueBox({
valueBox(
round(mean(filtered_data()$life_exp), 1),
"Avg Life Expectancy (years)",
icon = icon("heart"),
color = "green"
)
})
output$avg_gdp <- renderValueBox({
valueBox(
paste0("$", round(mean(filtered_data()$gdp_pc), 0)),
"Avg GDP per capita",
icon = icon("dollar-sign"),
color = "yellow"
)
})
# Life expectancy distribution
output$life_dist <- renderPlotly({
p <- ggplot(filtered_data(), aes(x = life_exp)) +
geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
labs(title = "Life Expectancy Distribution",
x = "Life Expectancy (years)", y = "Count") +
theme_minimal()
ggplotly(p)
})
# GDP vs Life expectancy
output$gdp_life_plot <- renderPlotly({
p <- ggplot(filtered_data(), aes(x = gdp_pc, y = life_exp,
color = continent,
text = paste("Country:", country,
"<br>Life:", round(life_exp,
1),
"<br>GDP: $", round(gdp_pc,
0)))) +
geom_point(alpha = 0.7) +
geom_smooth(method = "lm", se = FALSE, color = "black") +
labs(title = "GDP vs Life Expectancy",
x = "GDP per capita ($)", y = "Life Expectancy (years)") +
theme_minimal()
ggplotly(p, tooltip = "text")
})
# Top countries table
output$top_countries <- renderTable({
filtered_data() %>%
arrange(desc(life_exp)) %>%
head(10) %>%
select(Country = country, `Life Exp` = life_exp, GDP = gdp_pc)
})
# Bottom countries table
output$bottom_countries <- renderTable({
filtered_data() %>%
arrange(life_exp) %>%
head(10) %>%
select(Country = country, `Life Exp` = life_exp, GDP = gdp_pc)
})
# Country comparison
output$country_comparison <- renderPlotly({
selected_countries <- c(input$country1, input$country2, input$country3)
comp_data <- data[data$country %in% selected_countries, ]
# Reshape for plotting
plot_data <- data.frame(
Country = rep(comp_data$country, 5),
Indicator = rep(c("Life Expectancy", "GDP", "Health Exp",
"Infant Mortality", "Sanitation"), each =
nrow(comp_data)),
Value = c(comp_data$life_exp, comp_data$gdp_pc/1000,
comp_data$health_exp, comp_data$infant_mort,
comp_data$sanitation)
)
p <- ggplot(plot_data, aes(x = Indicator, y = Value, fill = Country)) +
geom_bar(stat = "identity", position = "dodge") +
labs(title = "Country Comparison", x = "", y = "Value") +
theme_minimal() +
theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplotly(p)
})
# Country details
output$country_details <- renderUI({
countries <- c(input$country1, input$country2, input$country3)
details <- lapply(countries, function(cntry) {
country_data <- data[data$country == cntry, ]
if (nrow(country_data) > 0) {
tags$div(
h4(cntry),
p(strong("Continent:"), country_data$continent),
p(strong("Income Group:"), country_data$income_group),
p(strong("Life Expectancy:"), round(country_data$life_exp, 1),
"years"),
p(strong("GDP:"), paste0("$", round(country_data$gdp_pc, 0))),
hr()
)
}
})
tagList(details)
})
# Prediction
observeEvent(input$predict_btn, {
# Prepare input data
new_data <- data.frame(
log_gdp = log(input$pred_gdp),
health_ratio = input$pred_health / input$pred_gdp,
edu_ratio = 0.05, # Default
sanitation = input$pred_sanitation,
infant_mort = input$pred_infant,
literacy = 80, # Default
hospital_beds = 2.5 # Default
)
# Make prediction
if (exists("rf_model")) {
prediction <- predict(rf_model, newdata = new_data)
} else {
# Fallback formula
prediction <- 65 + (log(input$pred_gdp) * 3) - (input$pred_infant * 0.5)
+ (input$pred_sanitation * 0.2)
}
output$prediction_output <- renderText({
paste(round(prediction, 1), "years")
})
output$prediction_interpretation <- renderText({
if (prediction < 60) return("Below global average - focus on healthcare
and economic development")
if (prediction < 75) return("Around global average - continue with
targeted improvements")
return("Above global average - maintain current policies")
})
# Find similar countries
similar <- data %>%
mutate(diff = abs(life_exp - prediction)) %>%
arrange(diff) %>%
head(3) %>%
pull(country)
output$similar_countries <- renderText({
paste(similar, collapse = ", ")
})
})
# What-if plot
output$whatif_plot <- renderPlotly({
gdp_range <- seq(1000, 50000, by = 1000)
predictions <- sapply(gdp_range, function(gdp) {
65 + (log(gdp) * 3) - (20 * 0.5) + (70 * 0.2)
})
plot_data <- data.frame(GDP = gdp_range, Life_Expectancy = predictions)
p <- ggplot(plot_data, aes(x = GDP, y = Life_Expectancy)) +
geom_line(color = "steelblue", size = 1.5) +
geom_vline(xintercept = input$whatif_gdp, color = "red", linetype =
"dashed") +
labs(title = "Impact of GDP on Life Expectancy",
x = "GDP per capita ($)", y = "Predicted Life Expectancy (years)")
+
theme_minimal()
ggplotly(p)
})
# Regional plot
output$regional_plot <- renderPlotly({
p <- ggplot(filtered_data(), aes(x = continent, y = life_exp, fill =
continent)) +
geom_boxplot() +
labs(title = "Life Expectancy by Continent",
x = "Continent", y = "Life Expectancy (years)") +
theme_minimal() +
theme(legend.position = "none")
ggplotly(p)
})
# Region statistics
output$region_stats <- renderTable({
filtered_data() %>%
group_by(continent) %>%
summarise(
Countries = n(),
`Avg Life` = round(mean(life_exp), 1),
`Avg GDP` = round(mean(gdp_pc), 0)
)
})
# Income group plot
output$income_plot <- renderPlotly({
p <- ggplot(filtered_data(), aes(x = income_group, y = life_exp, fill =
income_group)) +
geom_boxplot() +
labs(title = "Life Expectancy by Income Group",
x = "Income Group", y = "Life Expectancy (years)") +
theme_minimal() +
theme(axis.text.x = element_text(angle = 45, hjust = 1),
legend.position = "none")
ggplotly(p)
})
# Data preview
output$data_preview <- renderDataTable({
filtered_data()[, 1:8] # First 8 columns
}, options = list(pageLength = 10))
# Download handler
output$download_data <- downloadHandler(
filename = function() {
paste("global_health_data_", Sys.Date(), ".",
tolower(input$format), sep = "")
},
content = function(file) {
if (input$format == "CSV") {
write.csv(filtered_data(), file, row.names = FALSE)
} else if (input$format == "Excel") {
if (require("writexl")) {
writexl::write_xlsx(filtered_data(), file)
} else {
write.csv(filtered_data(), file, row.names = FALSE)
}
} else {
if (require("jsonlite")) {
write_json(filtered_data(), file)
} else {
write.csv(filtered_data(), file, row.names = FALSE)
}
}
}
)
}
# Run the app
cat("\n" , paste(rep("=", 60), collapse = "") , "\n")
cat("DAY 8: INTERACTIVE DASHBOARD\n")
cat(paste(rep("=", 60), collapse = "") , "\n\n")
cat("To run the dashboard:\n")
cat("1. Make sure you have shiny package installed\n")
cat("2. Run: shiny::runApp()\n")
cat("3. Or in RStudio: Click 'Run App' button\n")
cat("\nThe dashboard includes:\n")
cat("✓ Overview with key metrics\n")
cat("✓ Country comparison tool\n")
cat("✓ Life expectancy predictor\n")
cat("✓ Regional analysis\n")
cat("✓ Data download options\n")
# Uncomment to run automatically
# shinyApp(ui = ui, server = server)
