# ============================================================
# Shinylive Deployment Script
# ============================================================
# This script exports your Shiny app to run in the browser
# using shinylive (WebAssembly-powered R)
# ============================================================

cat("🚀 MRM-LD Shinylive Deployment Script\n")
cat("====================================\n\n")

# Check if shinylive is installed
if (!require("shinylive", quietly = TRUE)) {
  cat("📦 Installing shinylive package...\n")
  install.packages("shinylive")
}

library(shinylive)

# Export the app
cat("🔄 Exporting app to shinylive format...\n")
cat("   This may take a few minutes...\n\n")

shinylive::export(
  appdir = ".",           # Current directory (contains app.R)
  destdir = "docs"        # Output to docs/ for GitHub Pages
)

cat("\n✅ Export complete!\n\n")
cat("📋 Next steps:\n")
cat("   1. Add and commit the changes:\n")
cat("      git add .\n")
cat("      git commit -m 'Deploy shinylive version'\n")
cat("      git push origin main\n\n")
cat("   2. Enable GitHub Pages:\n")
cat("      - Go to your repo Settings > Pages\n")
cat("      - Source: main branch, /docs folder\n")
cat("      - Click Save\n\n")
cat("   3. Your app will be live at:\n")
cat("      https://zhouwenjie26.github.io/Multiple_Response_Model_Interactive_Visualization_System/\n\n")
cat("⏱️  Note: GitHub Pages may take 2-5 minutes to build.\n")
cat("    First load will take ~30-60 seconds for R/WebAssembly initialization.\n")
