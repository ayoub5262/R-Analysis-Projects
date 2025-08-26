# =============================================================================
# PHONE OWNERSHIP ANALYSIS (2020-2022)
# Analysis of telephone ownership per 100 people across countries
# =============================================================================

library(mosaic)  
library(readr)
library(dplyr)
library(ggplot2)
library(knitr)

# Read and inspect data
dataset <- read.delim("Telephones_per_100_people.csv",
                      header = TRUE, sep = ",")

# Fix column names for easier reference
names(dataset)[names(dataset) == "Land"] <- "Entity"

# Initial data exploration
cat("=== INITIAL DATA OVERVIEW ===\n")
cat("Dataset dimensions:", dim(dataset), "\n")
cat("Column names:", names(dataset), "\n")
inspect(dataset)

# =============================================================================
# DATA CLEANING AND PREPARATION
# =============================================================================

# Check data before filtering
cat("\n=== BEFORE FILTERING ===\n")
cat("Year range:", min(dataset$Year, na.rm=T), "to", max(dataset$Year, na.rm=T), "\n")
cat("Number of observations:", nrow(dataset), "\n")

# Reduce dataset to recent years (2020-2022)
dataset <- dataset %>% filter(Year >= 2020) 

cat("\n=== AFTER FILTERING TO 2020+ ===\n")
cat("Year range:", min(dataset$Year, na.rm=T), "to", max(dataset$Year, na.rm=T), "\n")
cat("Number of observations:", nrow(dataset), "\n")

# Check data types and non-numeric values
cat("\n=== DATA TYPE ANALYSIS ===\n")
cat("Data type of Telephones.per.100.people:", class(dataset$Telephones.per.100.people), "\n")

non_numeric_values <- dataset$Telephones.per.100.people[
  !sapply(dataset$Telephones.per.100.people, is.numeric)]
cat("Non-numeric values found:", length(non_numeric_values), "\n")
if(length(non_numeric_values) > 0) {
  cat("Examples of non-numeric values:", head(non_numeric_values), "\n")
}

# Convert to numeric and check for introduced NAs
original_nas <- sum(is.na(dataset$Telephones.per.100.people))
dataset$Telephones.per.100.people <- as.numeric(
  as.character(dataset$Telephones.per.100.people))
new_nas <- sum(is.na(dataset$Telephones.per.100.people))

cat("Original missing values:", original_nas, "\n")
cat("Missing values after conversion:", new_nas, "\n")
cat("NAs introduced by conversion:", new_nas - original_nas, "\n")

# =============================================================================
# STATISTICAL ANALYSIS
# =============================================================================

# Extreme values analysis
cat("\n=== OUTLIER ANALYSIS ===\n")
extreme_values <- boxplot.stats(dataset$Telephones.per.100.people)$out
cat("Number of extreme values:", length(extreme_values), "\n")
if(length(extreme_values) > 0) {
  cat("Extreme values:", extreme_values, "\n")
  
  # Identify countries with extreme values
  extreme_countries <- dataset$Entity[dataset$Telephones.per.100.people %in% extreme_values]
  cat("Countries with extreme values:", paste(extreme_countries, collapse=", "), "\n")
}

# Overall descriptive statistics
cat("\n=== DESCRIPTIVE STATISTICS ===\n")
overall_stats <- favstats(~Telephones.per.100.people, data=dataset)
print(overall_stats)

# Statistics by year
cat("\n=== STATISTICS BY YEAR ===\n")
year_stats <- favstats(Telephones.per.100.people ~ Year, data=dataset)
print(year_stats)

# Count observations by year
cat("\n=== OBSERVATIONS BY YEAR ===\n")
year_counts <- tally(~Year, data=dataset)
print(year_counts)

# Top and bottom countries (most recent year available)
latest_year <- max(dataset$Year, na.rm=T)
latest_data <- dataset %>% 
  filter(Year == latest_year) %>% 
  filter(!is.na(Telephones.per.100.people)) %>%  # Remove NA values
  arrange(desc(Telephones.per.100.people))

cat(paste("\n=== TOP/BOTTOM COUNTRIES (", latest_year, ") ===\n"))
cat("Top 5 countries:\n")

# Ensure we have data and columns exist
if(nrow(latest_data) > 0 && "Entity" %in% names(latest_data) && "Telephones.per.100.people" %in% names(latest_data)) {
  top_5 <- head(latest_data[c("Entity", "Telephones.per.100.people")], 5)
  print(top_5)
  
  cat("\nBottom 5 countries:\n")
  bottom_5 <- tail(latest_data[c("Entity", "Telephones.per.100.people")], 5)
  print(bottom_5)
} else {
  cat("No data available for", latest_year, "\n")
}

# =============================================================================
# DATA VISUALIZATION
# =============================================================================

cat("\n=== CREATING VISUALIZATIONS ===\n")

# 1. Enhanced boxplots with better formatting
par(mfrow=c(2,2))

# Treat year as categorical for boxplot
dataset$Year_factor <- as.factor(dataset$Year)

boxplot(Telephones.per.100.people ~ Year_factor, data = dataset,
        main = "Phone Ownership Distribution by Year",
        xlab = "Year", ylab = "Phones per 100 Inhabitants", 
        col = c("lightblue", "lightgreen", "lightcoral"),
        notch = TRUE)  # Add notches for median comparison

# 2. Histogram of phone ownership
hist(dataset$Telephones.per.100.people, 
     main = "Distribution of Phone Ownership",
     xlab = "Phones per 100 Inhabitants",
     ylab = "Frequency",
     col = "skyblue", 
     breaks = 20)

# 3. Enhanced scatter plot with trend line
plot(dataset$Year, dataset$Telephones.per.100.people, 
     main = "Phone Ownership Trend Over Time",
     xlab = "Year", ylab = "Phones per 100 Inhabitants", 
     pch = 19, col = alpha("blue", 0.6),
     cex = 0.8)

# Add trend line
if(sum(!is.na(dataset$Telephones.per.100.people)) > 1) {
  abline(lm(Telephones.per.100.people ~ Year, data = dataset), 
         col = "red", lwd = 2)
}

# 4. Year-over-year comparison (if multiple years available)
if(length(unique(dataset$Year)) > 1) {
  year_means <- dataset %>%
    group_by(Year) %>%
    summarise(Mean_Phones = mean(Telephones.per.100.people, na.rm = TRUE),
              .groups = 'drop')
  
  plot(year_means$Year, year_means$Mean_Phones,
       type = "b", pch = 19, col = "darkgreen", lwd = 2,
       main = "Average Phone Ownership by Year",
       xlab = "Year", ylab = "Average Phones per 100 Inhabitants")
}

par(mfrow=c(1,1))  # Reset layout


# =============================================================================
# CORRELATION AND TREND ANALYSIS
# =============================================================================

cat("\n=== CORRELATION ANALYSIS ===\n")

# Calculate correlation coefficient
correlation_coefficient <- cor(dataset$Year, dataset$Telephones.per.100.people,
                               use = "complete.obs")
cat("Correlation coefficient (Year vs Phone Ownership):", round(correlation_coefficient, 4), "\n")

# Interpret correlation strength
if(abs(correlation_coefficient) > 0.7) {
  strength <- "strong"
} else if(abs(correlation_coefficient) > 0.3) {
  strength <- "moderate"
} else {
  strength <- "weak"
}

direction <- ifelse(correlation_coefficient > 0, "positive", "negative")
cat("Interpretation:", strength, direction, "correlation\n")

# Linear regression analysis
if(sum(!is.na(dataset$Telephones.per.100.people)) > 1) {
  lm_model <- lm(Telephones.per.100.people ~ Year, data = dataset)
  cat("\n=== LINEAR REGRESSION ANALYSIS ===\n")
  print(summary(lm_model))
  
  # Extract key statistics
  r_squared <- summary(lm_model)$r.squared
  cat("R-squared:", round(r_squared, 4), "\n")
  cat("Interpretation: Year explains", round(r_squared * 100, 2), "% of variance in phone ownership\n")
}

# =============================================================================
# SUMMARY REPORT
# =============================================================================

cat("\n", paste(rep("=", 60), collapse=""), "\n")
cat("FINAL SUMMARY REPORT\n")
cat(paste(rep("=", 60), collapse=""), "\n")

cat("Dataset Information:\n")
cat("- Total observations:", nrow(dataset), "\n")
cat("- Years covered:", min(dataset$Year, na.rm=T), "to", max(dataset$Year, na.rm=T), "\n")
cat("- Countries/entities:", length(unique(dataset$Entity)), "\n")

cat("\nPhone Ownership Statistics:\n")
cat("- Mean:", round(mean(dataset$Telephones.per.100.people, na.rm=T), 2), "phones per 100 people\n")
cat("- Median:", round(median(dataset$Telephones.per.100.people, na.rm=T), 2), "phones per 100 people\n")
cat("- Range:", round(min(dataset$Telephones.per.100.people, na.rm=T), 2), "to", 
    round(max(dataset$Telephones.per.100.people, na.rm=T), 2), "\n")

cat("\nData Quality:\n")
cat("- Missing values:", sum(is.na(dataset$Telephones.per.100.people)), "\n")
cat("- Outliers detected:", length(extreme_values), "\n")

cat("\nTrend Analysis:\n")
cat("- Correlation with year:", round(correlation_coefficient, 4), "\n")
cat("- Trend direction:", ifelse(correlation_coefficient > 0, "Increasing", "Decreasing"), "\n")

cat("\n", paste(rep("=", 60), collapse=""), "\n")
