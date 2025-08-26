library(mosaic)
library(readr)
library(dplyr)
entry_survey <- read_delim("Entry_Survey.csv",
                                ";", escape_double = FALSE,
                                locale = locale(decimal_mark = ","),
                                trim_ws = TRUE, 
                                na = c("", "NA", "2much", "many", "2w"))
inspect(entry_survey)
tally(~Age, data = entry_survey)
tally(~WhatsApp, data = entry_survey)
tally(~Birth_Month, data = entry_survey)
select(entry_survey, 3)
filter(entry_survey, Birth_Month >= 10)
which(entry_survey$Birth_Month > 12)
entry_survey$Birth_Month[21] <- NA
tally(~Posts, data = entry_survey)
# Use gf_bar for categorical data or clean the data first
# gf_histogram(~Posts, data = entry_survey)  # This causes error due to non-numeric values
gf_bar(~Posts, data = entry_survey)  # Use bar chart for categorical data
# favstats(~Posts, data = entry_survey)  # This will also cause issues with non-numeric data
# Clean the Posts data by converting non-numeric values to NA
entry_survey$Posts_numeric <- as.numeric(entry_survey$Posts)
# Check the numeric version
favstats(~Posts_numeric, data = entry_survey)
gf_histogram(~Posts_numeric, data = entry_survey)

# New variable with values of 0 and >20 as NA
entry_survey$Posts2 <- ifelse(entry_survey$Posts_numeric == 0 | entry_survey$Posts_numeric > 20,
                                   NA, entry_survey$Posts_numeric)
# Statistics and histogram for "Posts2"
favstats(~Posts2, data = entry_survey)
gf_histogram(~Posts2, data = entry_survey, binwidth = 5)
# Calculate new variable "Birth Year"
entry_survey$Birth_Year <- 2025 - entry_survey$Age
# Statistics for "Birth Year"
favstats(~Birth_Year, data = entry_survey)
# Boxplot for "Birth Year" split by gender
gf_boxplot(Birth_Year ~ Gender, data = entry_survey)
favstats(Commute_Distance ~ Commute_Method, data = entry_survey)
data_bicycle <- entry_survey %>%  filter(Commute_Method %in% c("Bicycle", "Car")) 
favstats(Commute_Distance ~ Commute_Method, data = data_bicycle)
tally(~Posts2, data = entry_survey)
