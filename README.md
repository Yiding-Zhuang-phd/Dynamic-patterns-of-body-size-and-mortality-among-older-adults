# Dynamic-patterns-of-body-size-and-mortality-among-older-adults
Code for the articile "Dynamic patterns of body size and mortality among older adults:  Evidence from a cohort of 1.4 million individuals"

This project involves a two-stage workflow combining R and Python for variable selection and survival analysis.

1. Data Preparation and Cleaning (Data preparation for Stable Cox & full analysis.R)
This R script handles data import, cleaning, merging, and structuring for the StableCox pipeline.
It prepares the dataset into the format required for the StableCox.py script.
Note: Some variable names and comments in this R script are written in Chinese, due to the original data structure and variable definitions.

2. Stable Cox Variable Selection (Stable Cox.py)
This Python script performs StableCox feature selection, optimized for multi-CPU and multi-GPU computation.
It identifies stable and statistically significant covariates (based on Cox regression with reweighting methods such as DWR or SRDO).
The feature‐selection pipeline in the Python script (Stable Cox.py) is adapted from the open-source project StableCox (github.com/googlebaba/StableCox).
The output includes:
A text file listing significant features.
A CSV file containing detailed coefficient and p-value results.

3. Final Cox Regression and Visualization (Back to R)
After feature selection, the significant variables are imported back into R.
Final Cox proportional hazards models, sensitivity analyses, and visualizations/tables are performed in R for publication-quality results.

