# Data Analysis & Visualization Projects in R

This repository contains a collection of statistical analysis and data visualization projects implemented in R, focusing on environmental, demographic, and technological datasets.

## 📊 Projects Overview

### 1. Environmental Deaths Analysis (France vs Germany)
**Folder:** `01_Environmental_Deaths_Analysis/`
**Files:** 
- `Environmental_Deaths_France_Germany_Heat_Cold_Analysis.R`
- `Envi-Deaths_France_Germany_Heat_Cold_Plots.pdf` (Generated visualizations)
- Analysis of environmental deaths due to heat and cold exposure
- Comparative study between France and Germany
- Dataset: `Environmental_Deaths_Heat_Cold.csv`

### 2. Phone Ownership Analysis (2020-2022)
**Folder:** `02_Phone_Ownership_Analysis/`
**Files:**
- `Phone_Ownership_Analysis_2021.R`
- `Phone_Ownerships_Plots.pdf` (Generated visualizations)
- Analysis of telephone ownership per 100 people across countries
- Time series analysis for the period 2020-2022
- Dataset: `Telephones_per_100_people.csv`

### 3. Risk Assessment Survey Analysis
**Folder:** `03_Risk_Assessment_Survey/`
**Files:**
- `Risk_Assessment_Survey_Statistical_Analysis.R`
- `Risk_Assessment_Plots.pdf` (Generated visualizations)
- Statistical analysis of risk assessment survey data
- Dataset: `Risk_Assessment_Survey_Dataset.csv`

### 4. Renewable Energy Analysis
**Folder:** `04_Renewable_Energy_Analysis/`
**Files:** 
- `Renewable_Energy_Germany_vs_Global_Report.Rmd` (R Markdown report)
- `Renewable_Energy_Germany_vs_Global_Report.pdf` (Generated report)
- Analysis of renewable energy consumption patterns
- Dataset: `Renewable_energy_consumption_worldwide.csv`

### 5. Student Survey Data Tutorial
**Folder:** `05_Student_Survey_Tutorial/`
**Files:**
- `Student_Survey_Data_Analysis_Tutorial.R`
- `Student_Survey_Plots.pdf` (Generated visualizations)
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
├── 01_Environmental_Deaths_Analysis/
│   ├── Environmental_Deaths_France_Germany_Heat_Cold_Analysis.R
│   ├── Environmental_Deaths_Heat_Cold.csv
│   └── Envi-Deaths_France_Germany_Heat_Cold_Plots.pdf
├── 02_Phone_Ownership_Analysis/
│   ├── Phone_Ownership_Analysis_2021.R
│   ├── Telephones_per_100_people.csv
│   └── Phone_Ownerships_Plots.pdf
├── 03_Risk_Assessment_Survey/
│   ├── Risk_Assessment_Survey_Statistical_Analysis.R
│   ├── Risk_Assessment_Survey_Dataset.csv
│   └── Risk_Assessment_Plots.pdf
├── 04_Renewable_Energy_Analysis/
│   ├── Renewable_Energy_Germany_vs_Global_Report.Rmd
│   ├── Renewable_Energy_Germany_vs_Global_Report.pdf
│   └── Renewable_energy_consumption_worldwide.csv
├── 05_Student_Survey_Tutorial/
│   ├── Student_Survey_Data_Analysis_Tutorial.R
│   ├── Entry_Survey.csv
│   └── Student_Survey_Plots.pdf
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
2. Navigate to the specific project folder you want to work with
3. Open the `.R` or `.Rmd` file in RStudio
4. Ensure the corresponding CSV dataset is in the same project folder
5. Run the script section by section or entirely

## 📈 Analysis Features

- **Data Quality Assessment**: Missing value analysis, outlier detection
- **Descriptive Statistics**: Summary statistics, distribution analysis
- **Data Visualization**: Comprehensive plots using ggplot2
- **Comparative Analysis**: Cross-country and temporal comparisons
- **Statistical Testing**: Hypothesis testing and confidence intervals

## 📊 Outputs

- Statistical summaries and insights
- Publication-ready visualizations
- **PDF Plot Collections:**
  - `Envi-Deaths_France_Germany_Heat_Cold_Plots.pdf`
  - `Phone_Ownerships_Plots.pdf`
  - `Risk_Assessment_Plots.pdf`
  - `Student_Survey_Plots.pdf`
- PDF reports (for R Markdown files)
- Data quality assessments

---

*Note: Ensure all CSV files are placed in the same directory as the R scripts before running the analyses.*
