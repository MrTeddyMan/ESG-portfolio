# ESG and Market Return Analysis in R

## Overview
This project analyzes the relationship between stock returns, market excess returns, and Environmental, Social, and Governance (ESG) ratings. The script processes market return data, risk-free rates, stock betas, and ESG scores from multiple rating agencies. It then constructs factor-based portfolios to evaluate the impact of ESG scores and uncertainty on financial performance.

## Features
- **Market Return Calculations**: Computes excess market returns.
- **ESG Rating Standardization**: Normalizes ESG ratings from different agencies.
- **Portfolio Construction**:
  - **Beta-sorted portfolios**: Groups stocks into 5 portfolios based on beta values.
- **ESG-sorted portfolios**: Groups stocks based on ESG ratings.
- **ESG Uncertainty (ESGU) portfolios**: Assesses variations in ESG scores across rating agencies.
- **Asset Pricing Models**:
  - Implements the **Capital Asset Pricing Model (CAPM)**.
- Constructs **three-factor models** incorporating ESG and ESG uncertainty.
- Evaluates factor risk premia and significance using regressions.
- **Impact of ESG News**:
  - Analyzes changes in ESG scores over time.
- Examines their influence on stock returns.
- **Double Sorts**:
  - Assesses stock returns based on beta and ESG scores.
- Computes long-short portfolio returns.

## Data Inputs
The script reads the following datasets from the `Data/` directory:
  - `Market return.csv`: Market returns per year.
- `Risk-free rate.csv`: Annual risk-free rate.
- `Stock return.csv`: Stock returns for different years.
- `Stock beta.csv`: Beta values for stocks.
- `ESG rating agency A-F.csv`: ESG ratings from six agencies.

## Setup & Execution
1. Clone or download the repository.
2. Ensure R is installed along with required libraries (`lm()` is used for regression).
3. Set the working directory to the project folder:
  ```r
setwd("/path/to/project")
```
4. Run the script in an R environment:
  ```r
source("analysis.R")
```

## Key Outputs
- **Summary statistics** for portfolio returns.
- **Regression results** for asset pricing models.
- **Graphical analysis** of factor relationships.
- **Correlation measures** between ESG uncertainty and stock returns.

## Project Structure
```
📂 ESG Market Analysis
├── 📄 analysis.R        # Main R script
├── 📂 Data              # Folder containing input datasets
│   ├── Market return.csv
│   ├── Risk-free rate.csv
│   ├── Stock return.csv
│   ├── Stock beta.csv
│   ├── ESG rating agency A-F.csv
└── 📄 README.md         # Project documentation
```

## License
This project is open-source and available for modification and distribution.

