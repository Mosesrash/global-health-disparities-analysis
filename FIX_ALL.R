# FIX_ALL.R - Project setup fix script
# Run this if project structure needs repair

cat("🔧 Fixing project setup...\n")
cat("==================================================\n")

# Create folders if missing
folders <- c("outputs", "reports", "viz", "data/raw", "data/processed")
for (folder in folders) {
  if (!dir.exists(folder)) {
    dir.create(folder, recursive = TRUE)
  }
}

# Create .gitignore if missing
if (!file.exists(".gitignore")) {
  writeLines(c(
    "# R specific", ".Rproj.user", ".Rhistory", ".RData", ".Ruserdata",
    "", "# Output files", "*.html", "*.pdf", "*.png", "*.jpg",
    "", "# Data files", "data/raw/", "*.rds",
    "", "# OS files", ".DS_Store", "Thumbs.db"
  ), ".gitignore")
}

cat("✅ Setup fix complete!\n")
