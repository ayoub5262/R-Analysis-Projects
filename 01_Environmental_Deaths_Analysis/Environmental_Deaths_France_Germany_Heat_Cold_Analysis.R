# =============================================================================
# ENVIRONMENTAL DEATHS ANALYSIS: FRANCE vs GERMANY (HEAT/COLD)
# Analysis of environmental deaths due to heat and cold in France and Germany
# =============================================================================

library(mosaic)  
library(readr)
library(dplyr)
library(ggplot2)

# Read and inspect data
dataset <- read_delim("Environmental_Deaths_Heat_Cold.csv", 
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Initial data exploration
cat("=== INITIAL DATA OVERVIEW ===\n")
cat("Dataset dimensions:", dim(dataset), "\n")
cat("Column names:", names(dataset), "\n")
cat("Year range:", min(dataset$Year, na.rm=T), "to", max(dataset$Year, na.rm=T), "\n")
inspect(dataset)

# Check data quality
cat("\n=== DATA QUALITY ASSESSMENT ===\n")
missing_values <- sum(is.na(dataset))
cat("Total missing values:", missing_values, "\n")
cat("Missing values by column:\n")
sapply(dataset, function(x) sum(is.na(x)))
# =============================================================================
# OUTLIER ANALYSIS
# =============================================================================

cat("\n=== OUTLIER DETECTION ===\n")

# France outliers
extreme_values_france <- boxplot.stats(dataset$France_EnvironmentalHeatCold)$out
cat("France - Number of outliers:", length(extreme_values_france), "\n")
if(length(extreme_values_france) > 0) {
  cat("France - Outlier values:", extreme_values_france, "\n")
  outlier_years_france <- dataset$Year[dataset$France_EnvironmentalHeatCold %in% extreme_values_france]
  cat("France - Outlier years:", outlier_years_france, "\n")
}

# Germany outliers
extreme_values_germany <- boxplot.stats(dataset$Germany_EnvironmentalHeatCold)$out
cat("Germany - Number of outliers:", length(extreme_values_germany), "\n")
if(length(extreme_values_germany) > 0) {
  cat("Germany - Outlier values:", extreme_values_germany, "\n")
  outlier_years_germany <- dataset$Year[dataset$Germany_EnvironmentalHeatCold %in% extreme_values_germany]
  cat("Germany - Outlier years:", outlier_years_germany, "\n")
}
# =============================================================================
# DESCRIPTIVE STATISTICS
# =============================================================================

cat("\n=== DESCRIPTIVE STATISTICS ===\n")

# Overall statistics for France
cat("FRANCE - Environmental Deaths (Heat/Cold):\n")
france_stats <- favstats(~France_EnvironmentalHeatCold, data=dataset)
print(france_stats)

# Overall statistics for Germany
cat("\nGERMANY - Environmental Deaths (Heat/Cold):\n")
germany_stats <- favstats(~Germany_EnvironmentalHeatCold, data=dataset)
print(germany_stats)

# Calculate the sum for each year
dataset <- dataset %>%
  mutate(Total_Deaths = France_EnvironmentalHeatCold + Germany_EnvironmentalHeatCold,
         Difference = France_EnvironmentalHeatCold - Germany_EnvironmentalHeatCold)

# Statistics for combined data
cat("\nCOMBINED (France + Germany):\n")
total_stats <- favstats(~Total_Deaths, data=dataset)
print(total_stats)

# Statistics for difference
cat("\nDIFFERENCE (France - Germany):\n")
diff_stats <- favstats(~Difference, data=dataset)
print(diff_stats)
# =============================================================================
# DATA VISUALIZATION
# =============================================================================

cat("\n=== CREATING VISUALIZATIONS ===\n")

# Set up plotting layout
par(mfrow=c(2,3))

# 1. Time series plots for individual countries
plot(dataset$Year, dataset$France_EnvironmentalHeatCold, 
     main = "France: Envi-Deaths Over Time",
     xlab = "Year", ylab = "Deaths", pch = 19, col = "blue", cex = 1.2)
lines(dataset$Year, dataset$France_EnvironmentalHeatCold, col = "blue", lwd = 2)

plot(dataset$Year, dataset$Germany_EnvironmentalHeatCold, 
     main = "Germany: Envi-Deaths Over Time",
     xlab = "Year", ylab = "Deaths", pch = 19, col = "red", cex = 1.2)
lines(dataset$Year, dataset$Germany_EnvironmentalHeatCold, col = "red", lwd = 2)

# 2. Combined comparison plot
plot(dataset$Year, dataset$France_EnvironmentalHeatCold, 
     main = "Fr vs Ger: Envi-Deaths",
     xlab = "Year", ylab = "Deaths", pch = 19, col = "blue", cex = 1.2,
     ylim = range(c(dataset$France_EnvironmentalHeatCold, dataset$Germany_EnvironmentalHeatCold), na.rm = TRUE))
points(dataset$Year, dataset$Germany_EnvironmentalHeatCold, pch = 17, col = "red", cex = 1.2)
lines(dataset$Year, dataset$France_EnvironmentalHeatCold, col = "blue", lwd = 2)
lines(dataset$Year, dataset$Germany_EnvironmentalHeatCold, col = "red", lwd = 2)
legend("topright", legend = c("Fr", "Ger"), 
       col = c("blue", "red"), pch = c(19, 17), lty = 1, lwd = 2)

# 3. Boxplots for comparison
boxplot(dataset$France_EnvironmentalHeatCold, dataset$Germany_EnvironmentalHeatCold,
        names = c("France", "Germany"),
        main = "Distribution Comparison",
        ylab = "Deaths",
        col = c("lightblue", "lightcoral"))

# 4. Total deaths over time
plot(dataset$Year, dataset$Total_Deaths, 
     main = "Combined Deaths: Fr + Ger",
     xlab = "Year", ylab = "Total Deaths", pch = 19, col = "darkgreen", cex = 1.2)
lines(dataset$Year, dataset$Total_Deaths, col = "darkgreen", lwd = 2)

# 5. Difference plot
plot(dataset$Year, dataset$Difference, 
     main = "Difference: Fr - Ger",
     xlab = "Year", ylab = "Death Difference", pch = 19, col = "purple", cex = 1.2)
lines(dataset$Year, dataset$Difference, col = "purple", lwd = 2)
abline(h = 0, col = "gray", lty = 2)

par(mfrow=c(1,1))  # Reset layout
# =============================================================================
# CORRELATION ANALYSIS
# =============================================================================

cat("\n=== CORRELATION ANALYSIS ===\n")

# Year vs France deaths
cor_year_france <- cor(dataset$Year, dataset$France_EnvironmentalHeatCold, use = "complete.obs")
cat("Year vs France deaths:", round(cor_year_france, 4), "\n")

# Year vs Germany deaths
cor_year_germany <- cor(dataset$Year, dataset$Germany_EnvironmentalHeatCold, use = "complete.obs")
cat("Year vs Germany deaths:", round(cor_year_germany, 4), "\n")

# Year vs Total deaths
cor_year_total <- cor(dataset$Year, dataset$Total_Deaths, use = "complete.obs")
cat("Year vs Total deaths:", round(cor_year_total, 4), "\n")

# France vs Germany deaths
cor_france_germany <- cor(dataset$France_EnvironmentalHeatCold, dataset$Germany_EnvironmentalHeatCold, use = "complete.obs")
cat("France vs Germany deaths:", round(cor_france_germany, 4), "\n")

# Correlation interpretation function
interpret_correlation <- function(r) {
  if(abs(r) >= 0.7) return("Strong")
  if(abs(r) >= 0.3) return("Moderate") 
  return("Weak")
}

cat("\nCorrelation Interpretations:\n")
cat("- Year vs France:", interpret_correlation(cor_year_france), 
    ifelse(cor_year_france > 0, "positive", "negative"), "relationship\n")
cat("- Year vs Germany:", interpret_correlation(cor_year_germany), 
    ifelse(cor_year_germany > 0, "positive", "negative"), "relationship\n")
cat("- France vs Germany:", interpret_correlation(cor_france_germany), 
    ifelse(cor_france_germany > 0, "positive", "negative"), "relationship\n")
# =============================================================================
# REGRESSION ANALYSIS
# =============================================================================

cat("\n=== REGRESSION ANALYSIS ===\n")

# Multiple regression models
model_total <- lm(Total_Deaths ~ Year, data = dataset)
model_france <- lm(France_EnvironmentalHeatCold ~ Year, data = dataset)
model_germany <- lm(Germany_EnvironmentalHeatCold ~ Year, data = dataset)

cat("TOTAL DEATHS vs YEAR:\n")
summary_total <- summary(model_total)
print(summary_total)

cat("\nFRANCE DEATHS vs YEAR:\n")
summary_france <- summary(model_france)
print(summary_france)

cat("\nGERMANY DEATHS vs YEAR:\n")
summary_germany <- summary(model_germany)
print(summary_germany)

# =============================================================================
# REGRESSION DIAGNOSTICS
# =============================================================================

cat("\n=== REGRESSION DIAGNOSTICS ===\n")
par(mfrow=c(2,2))
plot(model_total, main = "Total Deaths Model Diagnostics")
par(mfrow=c(1,1))

# =============================================================================
# FINAL VISUALIZATION WITH TREND LINES
# =============================================================================

cat("\n=== FINAL TREND VISUALIZATION ===\n")

# Enhanced scatter plot with regression line
plot(dataset$Year, dataset$Total_Deaths, 
     main = "Environmental Deaths Trend: France + Germany",
     xlab = "Year", ylab = "Total Deaths", 
     pch = 19, col = "darkgreen", cex = 1.5)

# Add trend line
abline(model_total, col = "red", lwd = 3)

# Add equation and R-squared to plot
r_squared <- round(summary_total$r.squared, 3)
slope <- round(coef(model_total)[2], 2)
intercept <- round(coef(model_total)[1], 2)

legend("topright", 
       legend = c(paste("y =", slope, "x +", intercept),
                  paste("R² =", r_squared)),
       bty = "n", cex = 1.2)

# =============================================================================
# SUMMARY REPORT
# =============================================================================

cat("\n======================================================================\n")
cat("COMPREHENSIVE ANALYSIS SUMMARY\n")
cat("======================================================================\n")

cat("Dataset Information:\n")
cat("- Time period:", min(dataset$Year, na.rm=T), "to", max(dataset$Year, na.rm=T), "\n")
cat("- Total observations:", nrow(dataset), "\n")

cat("\nDescriptive Statistics Summary:\n")
cat("- France average deaths/year:", round(mean(dataset$France_EnvironmentalHeatCold, na.rm=T), 1), "\n")
cat("- Germany average deaths/year:", round(mean(dataset$Germany_EnvironmentalHeatCold, na.rm=T), 1), "\n")
cat("- Combined average deaths/year:", round(mean(dataset$Total_Deaths, na.rm=T), 1), "\n")

cat("\nTrend Analysis:\n")
cat("- France trend: R² =", round(summary_france$r.squared, 3), 
    ifelse(coef(model_france)[2] > 0, "(increasing)", "(decreasing)"), "\n")
cat("- Germany trend: R² =", round(summary_germany$r.squared, 3), 
    ifelse(coef(model_germany)[2] > 0, "(increasing)", "(decreasing)"), "\n")
cat("- Combined trend: R² =", round(summary_total$r.squared, 3), 
    ifelse(coef(model_total)[2] > 0, "(increasing)", "(decreasing)"), "\n")

cat("\nKey Correlations:\n")
cat("- France-Germany correlation:", round(cor_france_germany, 3), 
    "(", interpret_correlation(cor_france_germany), "relationship)\n")

cat("\nOutlier Summary:\n")
cat("- France outliers:", length(extreme_values_france), "\n")
cat("- Germany outliers:", length(extreme_values_germany), "\n")

cat("\n======================================================================\n")
