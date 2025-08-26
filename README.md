# Data Analysis & Visualization Projects in R

This repository contains a collection of statistical analysis and data visualization projects implemented in R, focusing on environmental, demographic, and technological datasets.

## 📊 Projects Overview

### 1. Environmental Deaths Analysis (France vs Germany)
**File:** `Environmental_Deaths_France_Germany_Heat_Cold_Analysis.R`
- Analysis of environmental deaths due to heat and cold exposure
- Comparative study between France and Germany
- Dataset: `Environmental_Deaths_Heat_Cold.csv`

### 2. Phone Ownership Analysis (2020-2022)
**File:** `Phone_Ownership_Analysis_2021.R`
- Analysis of telephone ownership per 100 people across countries
- Time series analysis for the period 2020-2022
- Dataset: `Telephones_per_100_people.csv`

### 3. Risk Assessment Survey Analysis
**File:** `Risk_Assessment_Survey_Statistical_Analysis.R`
- Statistical analysis of risk assessment survey data
- Dataset: `Risk_Assessment_Survey_Dataset.csv`

### 4. Renewable Energy Analysis
**Files:** 
- `Renewable_Energy_Germany_vs_Global_Report.Rmd` (R Markdown report)
- `Renewable_Energy_Germany_vs_Global_Report.pdf` (Generated report)
- Analysis of renewable energy consumption patterns
- Dataset: `Renewable_energy_consumption_worldwide.csv`

### 5. Student Survey Data Tutorial
**File:** `Student_Survey_Data_Analysis_Tutorial.R`
- Tutorial-style analysis of student survey data
- Dataset: `Entry_Survey.csv`

## 🛠 Technologies Used

- **R**: Primary programming language for statistical analysis
- **Libraries**: 
  - `mosaic`: Statistical modeling and data analysis
  - `readr`: Data import/export
  - `dplyr`: Data manipulation
  - `ggplot2`: Data visualization
  - `knitr`: Dynamic report generation

## 📁 File Structure

```
├── Environmental_Deaths_France_Germany_Heat_Cold_Analysis.R
├── Environmental_Deaths_Heat_Cold.csv
├── Phone_Ownership_Analysis_2021.R
├── Telephones_per_100_people.csv
├── Risk_Assessment_Survey_Statistical_Analysis.R
├── Risk_Assessment_Survey_Dataset.csv
├── Renewable_Energy_Germany_vs_Global_Report.Rmd
├── Renewable_Energy_Germany_vs_Global_Report.pdf
├── Renewable_energy_consumption_worldwide.csv
├── Student_Survey_Data_Analysis_Tutorial.R
├── Entry_Survey.csv
├── Rplots.pdf
└── README.md
```

## 🚀 Getting Started

### Prerequisites
- R (version 4.0 or higher)
- RStudio (recommended)

### Required R Packages
```r
install.packages(c("mosaic", "readr", "dplyr", "ggplot2", "knitr"))
```

### Running the Analysis
1. Clone this repository
2. Open any `.R` file in RStudio
3. Ensure the corresponding CSV dataset is in the same directory
4. Run the script section by section or entirely

## 📈 Analysis Features

- **Data Quality Assessment**: Missing value analysis, outlier detection
- **Descriptive Statistics**: Summary statistics, distribution analysis
- **Data Visualization**: Comprehensive plots using ggplot2
- **Comparative Analysis**: Cross-country and temporal comparisons
- **Statistical Testing**: Hypothesis testing and confidence intervals

## 📊 Outputs

- Statistical summaries and insights
- Publication-ready visualizations
- PDF reports (for R Markdown files)
- Data quality assessments

## 🎓 Academic Context

This project is part of coursework for:
- **Course**: Data Literacy (DaLi)
- **Semester**: 3
- **Institution**: University Studies

## 📄 License

This project is for educational purposes as part of university coursework.

## 🤝 Contributing

This is an academic project. For suggestions or questions, please refer to the course materials or contact the instructor.

---

*Note: Ensure all CSV files are placed in the same directory as the R scripts before running the analyses.*
