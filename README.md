# Dynamic body-size patterns and mortality: public R analysis

This repository provides analysis code for an older-adult cohort study of body-size changes and mortality. It accepts **analysis-ready datasets**. Participant-level data, original identifiers, source-file layouts, record-linkage procedures, raw-data cleaning, and local data paths are not distributed.

The script covers adjusted Cox models, sex-by-age strata, cause-specific mortality, sensitivity analyses, multiple-imputation pooling, three-class BMI trajectories, and figures. The adjustment sets are fixed from the supplied study analysis; variable selection is not rerun.

## Files and installation

- `public_analysis.R`: analysis functions; sourcing it does not execute analyses.
- `variable_dictionary.csv`: expected analysis variables and category labels.
- `.gitignore`: excludes local data, model objects, and generated outputs from future untracked additions. It does not remove tracked files or Git history.

```r
install.packages(c("survival", "ggplot2", "openxlsx"))
# Only needed for optional analyses:
install.packages(c("mice", "broom", "lcmm"))
source("public_analysis.R", encoding = "UTF-8")
```

Dependencies must be installed locally; the script does not install packages automatically. Record `sessionInfo()` with each private analysis run. Historical package versions were not supplied and are not reconstructed here.

## Main analysis

Prepare `analysis_data` privately, following the study protocol and the variable dictionary. Use the published category definitions; this public script deliberately does not reconstruct exposure categories from raw measurements. No real or simulated participant records are included.

```r
# analysis_data is an existing analysis-ready data.frame in your local R session.
main <- run_main_analysis(analysis_data, output_dir = "outputs",
                          subgroups = TRUE, causes = TRUE, figures = TRUE)
```

The six exposures are absolute weight change, absolute waist change, relative BMI change, relative weight change, relative waist change, and BMI-category transition. Change-category reference: `Stable`. BMI-transition reference: `Normal weight → Normal weight`. Six age-sex strata use 65–<75, 75–<80 and ≥80 years in men and women.

Change and trajectory models adjust for SEX, age, EDU_new, SMOKE, TC, LDL, HDL, RSBP, PAF, MAR, HB, WCELL, AST, SCR, GPT, FBG and TBIL. BMI-transition models replace HDL with TG. Within sex-specific strata, constant SEX is omitted. SMOKE and PAF retain ordered polynomial contrasts from the study script; ALCOHOL also uses ordered contrasts when included. Other listed categorical covariates use treatment contrasts.

Models use `survival::coxph`, Efron ties and complete cases for the corresponding model. Descriptive deaths and person-years use records with observed exposure, time and event, as in the source analysis. Consequently their denominator may exceed the adjusted model sample. Output includes both denominators, model event count and exclusions due to missingness. Missing or non-estimable estimates remain missing; they are never changed to a reference HR.

Optional cause columns are described in the dictionary. Competing deaths are censored at their observed time. Deaths with unknown cause have missing cause-specific event status and are excluded, following the source implementation. These are cause-specific Cox models. A requested cause model with no events stops with an error and requires review.

Output workbooks contain numeric HR, LCI, UCI and raw p, plus a display string rounded to two decimal places. Graphics use the numeric estimates. The figures are reorganized forest plots using the source's exposure groupings and colors; they are not exact copies of the manuscript's edited bar/matrix layouts. PDF and 600-dpi TIFF files are generated. Invalid subgroup fits are recorded in the subgroup workbook with a warning and are omitted from figures.

## Sensitivity analyses

```r
# Each optional object below is prepared independently and privately.
prepared <- list(no_chronic = analysis_no_chronic,
                 no_extreme_levels = analysis_no_extreme_levels,
                 no_extreme_changes = analysis_no_extreme_changes,
                 older_bmi = analysis_older_bmi,
                 landmark = analysis_landmark)
sens <- run_sensitivities(analysis_data, prepared = prepared)
# Without the optional datasets:
# sens <- run_sensitivities(analysis_data)
```

The function reruns models after restricting main follow-up to **≥3 years** and with Benjamini–Hochberg p-value adjustment within each exposure model. The death-exclusion analyses keep the original survival time origin. If the interval is absent, both lag analyses are skipped with a warning. BH correction adds a separate adjusted p column and does not change HRs or ordinary 95% confidence intervals.

Optional prepared cohorts represent exclusion of chronic disease, exclusion of measurements outside ±4 SD, exclusion of changes outside ±4 SD, older-adult BMI definitions, and the 2017–2020 exposure-window landmark analysis. For `older_bmi`, provide the preclassified exposure in `BMI_change_pattern`, with reference `Appropriate → Appropriate`. For `landmark`, provide eligibility, exposures and covariates already defined at 2020-12-31, and `Time_years`/`Status` measured from that landmark. The script cannot infer or verify these cohort-construction rules from the final table. Raw recoding and cohort construction are intentionally absent.

## Multiple imputation

```r
# imp is the original privately generated mice::mids object, or use a list
# of at least two completed analysis datasets with consistent category labels.
mi <- run_imputed_models(imp, descriptive_data = analysis_data,
                         include_alcohol = TRUE)
```

The source used five imputations, ten iterations, seed 123, with EDU_new=`logreg`, SMOKE/MAR/PAF=`polyreg`, and RSBP/FBG=`pmm`. The original imputation data and predictor matrix are not reconstructed. This function fits each completed dataset and pools via `mice::pool`; descriptive rates use `descriptive_data`. The **last MI implementation supplied by the author additionally adjusts for ALCOHOL**, unlike the main complete-case models. That distinction is retained explicitly with `include_alcohol = TRUE`; verify it against the final manuscript specification before reporting results. Setting FALSE intentionally changes that MI adjustment set.

## BMI trajectory sensitivity analysis

```r
# bmi_long: only anonymous numeric id_num, time (years), BMI are required.
tr <- fit_bmi_trajectories(bmi_long, seed = 123)
export_bmi_trajectories(tr)

# Inspect the curves before assigning biological names. This mapping matches
# the original fit ONLY if the new fit has the same class ordering.
labels <- c("1" = "Stable BMI trajectory",
            "2" = "Low BMI increasing trajectory",
            "3" = "High BMI declining trajectory")
tc <- trajectory_cox(tr, trajectory_survival, class_labels = labels,
                     reference = "Stable BMI trajectory", causes = TRUE)
```

Prepare the longitudinal observation window ending 2020-12-31 and retain participants with at least three eligible measurements. Time is years since the first eligible measurement. The model uses `lcmm::hlme` with `fixed = BMI ~ time`, `mixture = ~time`, a random intercept and three classes; initialization uses a one-class model and `B = random(m1)`, matching the supplied analysis. See [lcmm documentation](https://cecileproust-lima.github.io/lcmm/articles/latent_class_model_with_hlme.html).

Unlike the original unseeded fit, this wrapper specifies a seed. Convergence is checked, but a single randomized start does not establish a global maximum. Estimates and class numbers can vary between runs, package versions and initial values. Inspect the fitted curves and classification quality before setting `class_labels`; the code does not infer clinical names from class numbers.

`trajectory_survival` must have one row per anonymous `id_num`, covariates at the last eligible visit, and Time_years/Status from that visit to death or follow-up end (2023-12-31), following the supplied analysis. This timing differs from the fixed landmark analysis. Optional cause columns allow cause-specific trajectory Cox models. Class assignments are joined only in memory. No original IDs, dates or mapping table are required by these functions.

Only predicted class curves, class-level summaries and aggregate Cox results are exported. **Do not publish `tr`, fitted model objects, completed datasets, or R session images:** they can contain individual records or assignments. Aggregate outputs also require the study's usual disclosure review, particularly small trajectory classes or rare causes.

## Scope and validation

This is an analysis-only reorganization of the supplied revised scripts. It is not an end-to-end raw-data pipeline and does not reproduce the separate Python Stable Cox covariate-selection procedure. A full reproduction requires authorized analysis-ready data, the correct prepared sensitivity cohorts, and the original MI specification. No claim of numerical agreement with the paper can be made without those inputs.

Core Cox, subgroup, cause-specific, follow-up/lag sensitivity and BH functions, workbook output and graphics were exercised using 9,000 simulated records. Main exposure coefficients were compared with direct `coxph` fits. The optional MI-pooling and LCMM fitting functions have not been executed in this validation environment. They require local verification before reporting results. Do not upload private data to make the examples executable.

## Update

Replaced the previous data-preparation R script with analysis-only code to reduce exposure of data-cleaning procedures and potentially sensitive data-processing details. Participant-level data are not included.

The existing `Stable Cox.py` is unchanged and retains its origin in [googlebaba/StableCox](https://github.com/googlebaba/StableCox).
