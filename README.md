🌍 Global Health Disparities Analysis Dashboard
Project Overview

The Global Health Disparities Analysis Dashboard is an interactive R Shiny application designed to explore and visualize global health inequalities across countries.
The dashboard focuses on relationships between economic indicators (GDP per capita) and health outcomes (life expectancy and related metrics) using simulated data for demonstration purposes.

This project was developed as part of a data analytics and global health coursework project, with an emphasis on:

Data visualization

Interactive dashboards

Health disparity analysis

Reproducible research practices

🔗 Live Application

The application is publicly deployed on shinyapps.io and can be accessed here:

👉 Live Dashboard:
https://moussarashaideh.shinyapps.io/global_health_disparities_analysis_using_r/

📊 Key Features
Global Overview

Summary metrics (number of countries, average life expectancy, GDP, health spending)

Interactive GDP vs. Life Expectancy scatter plot

Tables showing Top 10 and Bottom 10 countries by life expectancy

Country Comparison

Compare up to three countries simultaneously

Visual comparison cards

Interactive bar charts

Comparison tables for key indicators

Country Details

In-depth view for a single country

Life expectancy, GDP, health spending, and infant mortality indicators

Interactive bar chart of health indicators

Summary table of country-specific metrics

About Page

Project description

Author information

Technical details

Usage instructions

🧪 Data Description

Data Source: Simulated (generated within the application)

Countries: 50 countries across multiple regions

Variables include:

Life expectancy

GDP per capita

Continent / region

Derived indicators (health spending, infant mortality, literacy, sanitation)

⚠️ Note:
This dashboard uses synthetic data for demonstration purposes only.
In a real-world implementation, data could be sourced from:

World Health Organization (WHO)

World Bank

United Nations (UN)

🛠️ Technologies Used

R

Shiny

shinydashboard

plotly

ggplot2

DT

dplyr

▶️ How to Run the App Locally
1. Clone the Repository
git clone https://github.com/Mosesrash/global-health-disparities-analysis.git

2. Open RStudio

Open the project folder or app.R file in RStudio.

3. Install Required Packages

Run the following in R:

packages <- c(
  "shiny",
  "shinydashboard",
  "plotly",
  "ggplot2",
  "DT",
  "dplyr"
)

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
  }
}

4. Run the Application
shiny::runApp("app.R")

📁 Repository Structure
global-health-disparities-analysis/
│
├── app.R                       # Main Shiny application
├── README.md                   # Project documentation
├── data/                       # (Optional) Data files
├── scripts/                    # Data generation or analysis scripts
├── plots/                      # Exported plots (optional)
├── results/                    # Results or summaries (optional)
├── .gitignore
└── LICENSE

🚀 Deployment

The application is deployed using the rsconnect package to shinyapps.io.

Key deployment considerations:

No absolute file paths are used

All required packages are declared

Data is generated internally to ensure portability

👤 Author

Moussa Rashaideh
📧 Email: Rashaidehmoussa@icloud.com

📞 Phone: 206-566-1154
🐙 GitHub: https://github.com/mosesrash

📌 Academic Disclaimer

This project was created for educational purposes.
All data shown is simulated and should not be interpreted as real health statistics.

📄 License

This project is licensed under the MIT License.
You are free to use, modify, and distribute this project with attribution.
