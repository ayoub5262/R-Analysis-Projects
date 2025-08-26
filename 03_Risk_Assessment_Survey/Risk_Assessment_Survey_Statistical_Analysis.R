library(mosaic)  
library(readr)
library(dplyr)
library(ggplot2)  # For better visualizations

# Read and inspect data
dataset <- read_delim("Risk_Assessment_Survey_Dataset.csv", 
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)
inspect(dataset)

# Basic data exploration
cat("Dataset dimensions:", dim(dataset), "\n")
cat("Column names:", names(dataset), "\n")
cat("Summary of missing values:\n")
sapply(dataset, function(x) sum(is.na(x)))
# =============================================================================
# DATA CLEANING
# =============================================================================

# Check initial distributions
cat("\n=== BEFORE CLEANING ===\n")
tally(~Age, data=dataset)
tally(~Gender, data=dataset)
tally(~Risk_Assessment, data=dataset)

# Standardize Gender values
dataset$Gender[dataset$Gender=="man"] <- "male"
dataset$Gender[dataset$Gender=="woman"] <- "female"

# Remove any empty strings or whitespace-only entries
dataset$Gender[dataset$Gender == "" | is.na(dataset$Gender)] <- NA

# Clean Risk_Assessment column (remove invalid entries)
dataset$Risk_Assessment[dataset$Risk_Assessment=="Risk_Assessment"] <- NA

# Remove any rows where essential variables are missing
cat("Rows before removing missing data:", nrow(dataset), "\n")
dataset <- dataset[!is.na(dataset$Gender) & !is.na(dataset$Age), ]
cat("Rows after removing missing Gender/Age:", nrow(dataset), "\n")

# Check distributions after cleaning
cat("\n=== AFTER CLEANING ===\n")
tally(~Gender, data=dataset)
tally(~Risk_Assessment, data=dataset)

# Check for any remaining data quality issues
cat("\nFinal missing values check:\n")
sapply(dataset, function(x) sum(is.na(x)))
# =============================================================================
# STATISTICAL ANALYSIS
# =============================================================================

# Age statistics
cat("\n=== AGE ANALYSIS ===\n")
favstats(~Age, data=dataset)

# Gender distribution
cat("\n=== GENDER ANALYSIS ===\n")
gender_table <- tally(~Gender, data=dataset)
prop.table(gender_table) * 100  # Percentages

# Risk Assessment analysis
cat("\n=== RISK ASSESSMENT ANALYSIS ===\n")
risk_table <- tally(~Risk_Assessment, data=dataset)
prop.table(risk_table) * 100  # Percentages

# Cross-tabulation: Gender vs Risk Assessment
cat("\n=== GENDER vs RISK ASSESSMENT ===\n")
cross_tab <- tally(Gender~Risk_Assessment, data=dataset)
print(cross_tab)
print("Percentages by Gender:")
prop.table(cross_tab, margin=1) * 100  # Row percentages

# Age by Gender and Risk Assessment
cat("\n=== AGE BY GENDER ===\n")
favstats(Age~Gender, data=dataset)

cat("\n=== AGE BY RISK ASSESSMENT ===\n")
# =============================================================================
# VISUALIZATIONS
# =============================================================================

# 1. Age Distribution
cat("\n=== CREATING VISUALIZATIONS ===\n")

# Histogram and boxplot of Age
par(mfrow=c(1,2))
hist(dataset$Age, main="Distribution of Age", xlab="Age", col="lightblue", breaks=10)
boxplot(dataset$Age, main="Boxplot of Age", ylab="Age", col="lightgreen")

# 2. Gender Distribution
par(mfrow=c(1,1))
barplot(table(dataset$Gender), main="Gender Distribution", 
        col=c("pink", "lightblue"), ylab="Count")

# 3. Risk Assessment Distribution  
barplot(table(dataset$Risk_Assessment), main="Risk Assessment Types",
        col=rainbow(length(table(dataset$Risk_Assessment))), 
        ylab="Count", las=2)  # las=2 rotates labels

# 4. Age by Gender
gf_boxplot(Age ~ Gender, data=dataset, 
           title="Age Distribution by Gender",
           xlab="Gender", ylab="Age")

# 5. Age by Risk Assessment
gf_boxplot(Age ~ Risk_Assessment, data=dataset,
           title="Age Distribution by Risk Assessment Type",
           xlab="Risk Assessment", ylab="Age") %>%
  gf_theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 6. Stacked bar chart: Gender vs Risk Assessment
ggplot(dataset, aes(x=Risk_Assessment, fill=Gender)) +
  geom_bar(position="dodge") +
  theme_minimal() +
  labs(title="Risk Assessment Types by Gender",
       x="Risk Assessment Type",
       y="Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Reset plotting parameters
par(mfrow=c(1,1))

# =============================================================================
# STATISTICAL TESTS
# =============================================================================

cat("\n=== STATISTICAL TESTS ===\n")

# 1. Test if age differs by gender (t-test)
cat("\n1. Age difference by Gender (t-test):\n")

# Check Gender levels before t-test
cat("Gender distribution before t-test:\n")
print(table(dataset$Gender, useNA = "always"))

# For t-test, we need exactly 2 groups. Let's filter to just male/female for comparison
binary_gender_data <- dataset[dataset$Gender %in% c("male", "female"), ]

cat("Sample sizes for t-test: male =", sum(binary_gender_data$Gender == "male"), 
    ", female =", sum(binary_gender_data$Gender == "female"), "\n")

if(nrow(binary_gender_data) > 0 && length(unique(binary_gender_data$Gender)) == 2) {
  t_test_result <- t.test(Age ~ Gender, data=binary_gender_data)
  print(t_test_result)
  
  # Also test all three groups with ANOVA
  cat("\n1b. Age difference by Gender - All groups (ANOVA):\n")
  if(length(unique(dataset$Gender[!is.na(dataset$Gender)])) > 2) {
    anova_gender <- aov(Age ~ Gender, data=dataset)
    print(summary(anova_gender))
  }
} else {
  cat("Error: Insufficient data for t-test analysis.\n")
}

# 2. Test if there's association between Gender and Risk Assessment
cat("\n2. Association between Gender and Risk Assessment (Chi-square test):\n")
chi_test <- chisq.test(table(dataset$Gender, dataset$Risk_Assessment))
print(chi_test)

# 3. ANOVA: Age differences across Risk Assessment types
cat("\n3. Age differences across Risk Assessment types (ANOVA):\n")
anova_result <- aov(Age ~ Risk_Assessment, data=dataset)
summary(anova_result)

# Post-hoc test if ANOVA is significant
if(summary(anova_result)[[1]]$`Pr(>F)`[1] < 0.05) {
  cat("\nPost-hoc test (Tukey HSD):\n")
  print(TukeyHSD(anova_result))
}

# =============================================================================
# SUMMARY REPORT
# =============================================================================

cat("\n=== SUMMARY REPORT ===\n")
cat("Dataset contains", nrow(dataset), "observations\n")
cat("Age range:", min(dataset$Age, na.rm=T), "to", max(dataset$Age, na.rm=T), "years\n")
cat("Gender distribution:", paste(names(table(dataset$Gender)), ":", table(dataset$Gender), collapse=", "), "\n")
cat("Risk Assessment types:", length(unique(dataset$Risk_Assessment[!is.na(dataset$Risk_Assessment)])), "\n")
cat("Missing values: Age =", sum(is.na(dataset$Age)), 
    ", Gender =", sum(is.na(dataset$Gender)),
    ", Risk_Assessment =", sum(is.na(dataset$Risk_Assessment)), "\n")
