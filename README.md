# Meta-Analysis Shiny App

A comprehensive web application for conducting meta-analyses of binary and continuous outcome data using R Shiny.

## Features

- **Dual Outcome Support**: Analyze both binary outcomes (risk ratios, odds ratios) and continuous outcomes (standardized mean differences)
- **Multiple Effect Measures**: 
  - Binary: Risk Ratio, Odds Ratio
  - Continuous: Cohen's d, Hedges' g, Glass's Δ
- **Advanced Analyses**: 
  - Random and fixed-effects models
  - Three-level models for multi-arm studies
  - Publication bias assessment (funnel plots, Egger's test, trim-and-fill)
  - Subgroup analyses
- **User-Friendly Interface**: Excel file upload with automatic data validation and cleaning

## Installation

### Prerequisites
- R (≥ 4.0.0)
- RStudio (recommended)

### Required R Packages
```r
install.packages(c(
  "shiny",
  "readxl", 
  "dplyr",
  "meta",
  "stringr"
))
```

## Usage

### Running the App
1. Clone or download this repository
2. Open `app.R` in RStudio
3. Click "Run App" or use `shiny::runApp()`

### Data Format

#### Binary Outcomes
Your Excel file must contain these columns:
- `author`: Study identifier (e.g., 'Smith_2023_1')
- `t_incident`: Number of events in treatment group
- `t_noincident`: Number of non-events in treatment group  
- `c_incident`: Number of events in control group
- `c_noincident`: Number of non-events in control group

#### Continuous Outcomes
Your Excel file must contain these columns:
- `author`: Study identifier (e.g., 'Smith_2023_1')
- `t_mean`: Mean value in treatment group
- `t_sd`: Standard deviation in treatment group
- `t_n`: Sample size in treatment group
- `c_mean`: Mean value in control group
- `c_sd`: Standard deviation in control group
- `c_n`: Sample size in control group

#### Optional Columns
- `name`: Study title
- `cluster`: Cluster identifier for multi-arm studies
- `year`: Publication year for meta-regression
- Additional categorical/continuous variables for subgroup analyses

### Example Data Structure

**Binary Outcomes:**
| author | t_incident | t_noincident | c_incident | c_noincident |
|--------|------------|--------------|------------|--------------|
| Smith_2023_1 | 25 | 75 | 15 | 85 |
| Jones_2024_1 | 30 | 70 | 20 | 80 |

**Continuous Outcomes:**
| author | t_mean | t_sd | t_n | c_mean | c_sd | c_n |
|--------|---------|------|-----|---------|------|-----|
| Smith_2023_1 | 12.5 | 2.1 | 50 | 10.2 | 2.3 | 48 |
| Jones_2024_1 | 15.3 | 3.2 | 45 | 12.8 | 2.9 | 47 |

## App Features

### Main Analysis
- Random-effects meta-analysis using inverse variance method
- Forest plots with customizable effect measures
- Study selection and filtering

### Sensitivity Analysis  
- Fixed-effects models (Mantel-Haenszel for binary, inverse variance for continuous)
- Comparison with random-effects results

### Publication Bias Assessment
- Funnel plots
- Egger's test for funnel plot asymmetry
- Trim-and-fill analysis for missing studies

### Subgroup Analysis
- Manual assignment of studies to up to 4 subgroups
- Between-group heterogeneity testing
- Subgroup-specific forest plots

### Advanced Options
- Three-level models for multi-arm studies
- Automatic cluster detection
- Data validation and cleaning

## Statistical Methods

The app uses the `meta` package in R, implementing:
- **Binary outcomes**: `metabin()` function
- **Continuous outcomes**: `metacont()` function
- **Effect measures**: 
  - Risk Ratio (RR), Odds Ratio (OR)
  - Cohen's d, Hedges' g, Glass's Δ
- **Models**: Random-effects (REML), Fixed-effects (Mantel-Haenszel/Inverse Variance)
- **Multi-arm**: Three-level meta-analysis for clustered data

## Contributing

Contributions are welcome! Please feel free to:
- Report bugs
- Suggest new features
- Submit pull requests
- Improve documentation

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Citation

If you use this app in your research, please cite:

```
Meta-Analysis Shiny App [Computer software]. (2024). 
Available from: https://github.com/[your-username]/meta-analysis-shiny-app
```

## Support

For questions, issues, or feature requests, please open an issue on GitHub.

## Acknowledgments

- Built with [R Shiny](https://shiny.rstudio.com/)
- Meta-analysis functionality provided by the [`meta` package](https://cran.r-project.org/package=meta)
- Inspired by the need for accessible meta-analysis tools in research