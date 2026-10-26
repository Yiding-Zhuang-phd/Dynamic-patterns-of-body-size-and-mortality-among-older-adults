# Public analysis code: dynamic body-size patterns and mortality
# Analysis-ready inputs only. No participant data are distributed.
# source("public_analysis.R") defines functions; it does not read or write data.
# See README.md and variable_dictionary.csv for input contracts and examples.
# Required: survival, ggplot2, openxlsx. Optional: mice, broom, lcmm.

EXPOSURES <- c("Weight Change" = "weight_change_cat",
               "Waist Change" = "waist_change_cat_abs",
               "BMI Change (perc)" = "BMI_change_perc_cat",
               "Weight Change (perc)" = "weight_change_perc_cat",
               "Waist Change (perc)" = "waist_change_cat_perc",
               "BMI Change Pattern" = "BMI_change_pattern")
CHANGE_LEVELS <- c("Stable", "Mild decrease", "Mild increase",
                   "Substantial decrease", "Substantial increase")
BMI_LEVELS <- c("Underweight", "Normal weight", "Overweight", "Obesity")
BMI_PATTERNS <- unlist(lapply(BMI_LEVELS, function(x) paste(x, BMI_LEVELS, sep = " → ")))
COMMON_COVARIATES <- c("SEX", "age", "EDU_new", "SMOKE", "TC", "LDL", "RSBP",
                      "PAF", "MAR", "HB", "WCELL", "AST", "SCR", "GPT", "FBG", "TBIL")
TOP_CAUSES <- c("Top_Communicable", "Top_NonCommunicable", "Top_Injuries")
MAJOR_CAUSES <- c("Second_Circulatory", "Second_Respiratory", "Second_Neoplasms",
                  "Second_Digestive", "Second_Metabolic", "Second_Falls")

need_packages <- function(packages) {
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) stop("Install required packages: ", paste(missing, collapse = ", "))
}
require_columns <- function(data, columns) {
  missing <- setdiff(columns, names(data))
  if (length(missing)) stop("Missing analysis columns: ", paste(missing, collapse = ", "))
}
analysis_factor <- function(x, levels, ordered = FALSE) {
  bad <- setdiff(unique(as.character(x[!is.na(x)])), levels)
  if (length(bad)) stop("Unexpected category label(s): ", paste(bad, collapse = ", "))
  z <- factor(as.character(x), levels = levels, ordered = ordered)
  # Explicit contrasts preserve the source's ordered-factor treatment,
  # independently of global options(contrasts = ...).
  contrasts(z) <- if (ordered) stats::contr.poly(length(levels)) else
    stats::contr.treatment(levels, base = 1)
  z
}
covariates_for <- function(exposure, include_alcohol = FALSE) {
  c(COMMON_COVARIATES, if (exposure == "BMI_change_pattern") "TG" else "HDL",
    if (include_alcohol) "ALCOHOL")
}
prepare_model_factors <- function(data, exposure, reference, include_alcohol = FALSE) {
  specs <- list(SEX = c("Female", "Male"), EDU_new = c("<9 years", "≥9 years"),
                SMOKE = c("Never smoker", "Former smoker", "Current smoker"),
                PAF = c("No exercise", "Occasionally", "Weekly+", "Daily"),
                MAR = c("Married", "Single", "Widowed", "Divorced"))
  if (include_alcohol) specs$ALCOHOL <- c("Non-drinker", "Casual drinker", "Frequent drinker", "Daily drinker")
  for (nm in names(specs)) data[[nm]] <- analysis_factor(data[[nm]], specs[[nm]],
                                                       nm %in% c("SMOKE", "PAF", "ALCOHOL"))
  lev <- if (exposure %in% unname(EXPOSURES[-6])) CHANGE_LEVELS else
    if (exposure == "BMI_change_pattern" && reference == "Normal weight → Normal weight") BMI_PATTERNS else
      if (is.factor(data[[exposure]])) levels(data[[exposure]]) else sort(unique(as.character(data[[exposure]])))
  if (!reference %in% lev) stop("Reference category is absent from exposure levels")
  data[[exposure]] <- analysis_factor(data[[exposure]], c(reference, setdiff(lev, reference)))
  data
}
cox_formula <- function(exposure, covariates, event = "Status") {
  stats::as.formula(paste0("survival::Surv(Time_years, ", event, ") ~ ",
                          paste(c(exposure, covariates), collapse = " + ")))
}
check_survival <- function(data, event) {
  require_columns(data, c("Time_years", event))
  if (!is.numeric(data$Time_years) || any(!is.na(data$Time_years) &
      (!is.finite(data$Time_years) | data$Time_years <= 0))) stop("Follow-up must be positive years")
  if (!(is.numeric(data[[event]]) || is.logical(data[[event]])) ||
      any(!is.na(data[[event]]) & !data[[event]] %in% c(0, 1))) stop("Event must be numeric 0/1")
}
describe_exposure <- function(data, exposure, event) {
  # Descriptive rates use available exposure, follow-up and event records,
  # as in the source. They can have a different denominator from adjusted Cox.
  ok <- stats::complete.cases(data[, c(exposure, "Time_years", event), drop = FALSE])
  rows <- lapply(levels(data[[exposure]]), function(lev) {
    ii <- which(ok & data[[exposure]] == lev)
    years <- sum(data$Time_years[ii]); deaths <- sum(data[[event]][ii])
    data.frame(type = lev, N_descriptive = length(ii), Death = deaths, YPOP = years,
               YPOP_1000 = if (years > 0) deaths / years * 1000 else NA_real_)
  })
  do.call(rbind, rows)
}
format_hr <- function(hr, lo, hi, p) {
  star <- ifelse(is.na(p), "", ifelse(p < .001, "***", ifelse(p < .01, "**", ifelse(p < .05, "*", ""))))
  ifelse(is.finite(hr) & is.finite(lo) & is.finite(hi),
         sprintf("%.2f (%.2f-%.2f)%s", hr, lo, hi, star), NA_character_)
}

fit_exposure <- function(data, exposure, reference = if (exposure == "BMI_change_pattern")
                         "Normal weight → Normal weight" else "Stable",
                         event = "Status", include_alcohol = FALSE, drop_sex = FALSE) {
  need_packages("survival")
  covs <- covariates_for(exposure, include_alcohol)
  require_columns(data, c(exposure, covs, "Time_years", event))
  check_survival(data, event)
  numeric_covs <- setdiff(covs, c("SEX", "EDU_new", "SMOKE", "PAF", "MAR", "ALCOHOL"))
  for (nm in numeric_covs) if (!is.numeric(data[[nm]]) ||
      any(!is.na(data[[nm]]) & !is.finite(data[[nm]])))
    stop("Covariate must be finite numeric or NA: ", nm)
  data <- prepare_model_factors(data, exposure, reference, include_alcohol)
  if (drop_sex) covs <- setdiff(covs, "SEX")
  ff <- cox_formula(exposure, covs, event)
  cc <- stats::complete.cases(data[, all.vars(ff), drop = FALSE])
  z <- data[cc, , drop = FALSE]
  if (!nrow(z) || !any(z[[event]] == 1)) stop("No complete records with events")
  if (!any(z[[exposure]] == reference)) stop("Reference category absent in model sample")
  fit <- survival::coxph(ff, data = z, ties = "efron", na.action = stats::na.fail,
                         model = FALSE, x = FALSE, y = FALSE)
  s <- summary(fit); rn <- rownames(s$coefficients)
  lev <- levels(data[[exposure]])
  j <- match(paste0(exposure, lev), rn)
  out <- describe_exposure(data, exposure, event)
  out$HR <- s$coefficients[j, "exp(coef)"]
  out$LCI <- s$conf.int[j, "lower .95"]
  out$UCI <- s$conf.int[j, "upper .95"]
  out$p <- s$coefficients[j, "Pr(>|z|)"]
  ref <- out$type == reference
  out$HR[ref] <- out$LCI[ref] <- out$UCI[ref] <- 1
  out$is_reference <- ref
  out$HR_model <- format_hr(out$HR, out$LCI, out$UCI, out$p)
  out$HR_model[ref] <- "REF"
  out$model_N <- fit$n; out$model_events <- fit$nevent
  out$model_excluded_missing <- nrow(data) - fit$n
  out$model_N_in_category <- as.integer(table(z[[exposure]])[out$type])
  out$model_status <- ifelse(ref, "reference", ifelse(is.finite(out$HR) &
    is.finite(out$LCI) & is.finite(out$UCI), "estimated", "not_estimable"))
  out$adjustment <- paste(covs, collapse = " + ")
  out
}
run_models <- function(data, bmi_reference = "Normal weight → Normal weight", ...) {
  lapply(EXPOSURES, function(v) fit_exposure(data, v,
    reference = if (v == "BMI_change_pattern") bmi_reference else "Stable", ...))
}
save_tables <- function(tables, path) {
  need_packages("openxlsx")
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  wb <- openxlsx::createWorkbook()
  for (nm in names(tables)) {
    openxlsx::addWorksheet(wb, nm)
    openxlsx::writeData(wb, nm, tables[[nm]])
    openxlsx::freezePane(wb, nm, firstRow = TRUE)
  }
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  invisible(path)
}
subgroup_models <- function(data) {
  require_columns(data, c("SEX", "age"))
  if (!is.numeric(data$age)) stop("age must be numeric years")
  ag <- cut(data$age, breaks = c(65, 75, 80, Inf), right = FALSE,
            labels = c("65–<75 years", "75–<80 years", "≥80 years"))
  lapply(EXPOSURES, function(v) {
    pieces <- list()
    for (sex in c("Male", "Female")) for (a in levels(ag)) {
      d <- data[which(data$SEX == sex & ag == a), , drop = FALSE]
      t <- tryCatch(fit_exposure(d, v, drop_sex = TRUE), error = function(e) {
        warning(paste(v, sex, a, conditionMessage(e)), call. = FALSE)
        data.frame(type = NA_character_, HR = NA_real_, LCI = NA_real_, UCI = NA_real_,
                   model_status = paste0("failed: ", conditionMessage(e)))
      })
      t$gender <- sex; t$age_group <- a
      pieces[[length(pieces) + 1L]] <- t
    }
    # Failures are retained in the workbook, not silently treated as references.
    all_names <- unique(unlist(lapply(pieces, names)))
    pieces <- lapply(pieces, function(x) { x[setdiff(all_names, names(x))] <- NA; x[all_names] })
    do.call(rbind, pieces)
  })
}
cause_specific_models <- function(data, cause_column, targets, exposures = EXPOSURES,
                                  reference = NULL) {
  require_columns(data, c(cause_column, "Status"))
  lapply(exposures, function(v) do.call(rbind, lapply(targets, function(target) {
    d <- data
    # Competing deaths are censored at their observed time. Deaths with unknown
    # cause have missing event indicators and are excluded, matching source code.
    d$event_cause <- ifelse(d[[cause_column]] == target & d$Status == 1, 1, 0)
    ref <- if (!is.null(reference)) reference else if (v == "BMI_change_pattern")
      "Normal weight → Normal weight" else "Stable"
    t <- fit_exposure(d, v, reference = ref, event = "event_cause")
    t$Cause <- target
    t
  })))
}

plot_forest <- function(table, title = NULL, facet = NULL) {
  need_packages("ggplot2")
  d <- table[which(is.finite(table$HR) & table$HR > 0 & is.finite(table$LCI) &
                     table$LCI > 0 & is.finite(table$UCI)), , drop = FALSE]
  if (!nrow(d)) stop("No finite positive hazard ratios to plot")
  d$type <- factor(d$type, levels = rev(unique(table$type[!is.na(table$type)])))
  p <- ggplot2::ggplot(d, ggplot2::aes(x = type, y = HR, ymin = LCI, ymax = UCI)) +
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed", colour = "#B30000") +
    ggplot2::geom_pointrange(linewidth = .35, colour = "#1F77B4") +
    ggplot2::coord_flip() + ggplot2::scale_y_log10() +
    ggplot2::labs(x = NULL, y = "Hazard ratio (95% CI)", title = title) +
    ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
  if (!is.null(facet)) p <- p + ggplot2::facet_wrap(stats::as.formula(paste("~", facet)))
  p
}
save_forest_set <- function(tables, directory, facet = NULL, dpi = 600) {
  need_packages("ggplot2")
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  for (nm in names(tables)) {
    p <- plot_forest(tables[[nm]], nm, facet)
    stem <- gsub("[^A-Za-z0-9]+", "_", nm)
    ggplot2::ggsave(file.path(directory, paste0(stem, ".pdf")), p,
                   width = if (is.null(facet)) 9 else 15, height = 8)
    ggplot2::ggsave(file.path(directory, paste0(stem, ".tiff")), p,
                   width = if (is.null(facet)) 9 else 15, height = 8, dpi = dpi,
                   compression = "lzw", bg = "white")
  }
  invisible(NULL)
}
run_main_analysis <- function(data, output_dir = "outputs", subgroups = TRUE,
                              causes = TRUE, figures = TRUE, dpi = 600) {
  main <- run_models(data)
  save_tables(main, file.path(output_dir, "Table_COX_Regression_Results.xlsx"))
  if (figures) save_forest_set(main, file.path(output_dir, "figures", "main"), dpi = dpi)
  if (subgroups) {
    sg <- subgroup_models(data)
    save_tables(sg, file.path(output_dir, "Subgroup_Cox_results.xlsx"))
    if (figures) save_forest_set(sg, file.path(output_dir, "figures", "subgroups"),
                                 facet = "gender + age_group", dpi = dpi)
  }
  if (causes) for (spec in list(list("Top_level_outcomes", TOP_CAUSES),
                                list("Major_outcomes", MAJOR_CAUSES))) {
    if (!spec[[1]] %in% names(data)) next
    cs <- cause_specific_models(data, spec[[1]], spec[[2]])
    save_tables(cs, file.path(output_dir, paste0(spec[[1]], "_CauseSpecificCox.xlsx")))
    if (figures) save_forest_set(cs, file.path(output_dir, "figures", spec[[1]]), "Cause", dpi)
  }
  invisible(main) # Aggregate results only; no fitted objects or individual records.
}

run_sensitivities <- function(data, prepared = list(), output_dir = "outputs/sensitivity") {
  # prepared may contain independently prepared, analysis-ready datasets named:
  # no_chronic, no_extreme_levels, no_extreme_changes, older_bmi, landmark.
  # Cohort construction, raw-source mappings and date linkage are intentionally omitted.
  check_survival(data, "Status")
  scenarios <- list(followup_ge3 = data[which(data$Time_years >= 3), , drop = FALSE])
  # Main Time_years begins at baseline in the supplied script. The lag exclusion
  # instead uses death minus LAST measurement: these clocks are not interchangeable.
  if ("years_after_last" %in% names(data)) {
    x <- data$years_after_last
    if (!is.numeric(x) || any(!is.na(x) & (!is.finite(x) | x < 0)))
      stop("years_after_last must be nonnegative numeric, or NA")
    scenarios$exclude_deaths_1y <- data[which(is.na(x) | x > 1), , drop = FALSE]
    scenarios$exclude_deaths_2y <- data[which(is.na(x) | x > 2), , drop = FALSE]
  } else warning("Lag analyses skipped: provide years_after_last (death minus last measurement in years)", call. = FALSE)
  scenarios <- c(scenarios, prepared)
  if (anyDuplicated(names(scenarios)) || any(!grepl("^[A-Za-z0-9_]+$", names(scenarios))))
    stop("Scenario names must be unique simple file names")
  ans <- list()
  for (nm in names(scenarios)) {
    ref <- if (nm == "older_bmi") "Appropriate → Appropriate" else "Normal weight → Normal weight"
    t <- run_models(scenarios[[nm]], bmi_reference = ref)
    save_tables(t, file.path(output_dir, paste0(nm, ".xlsx")))
    ans[[nm]] <- t
  }
  bh <- run_models(data)
  bh <- lapply(bh, function(t) {
    t$p_BH <- NA_real_
    j <- which(!t$is_reference)
    t$p_BH[j] <- stats::p.adjust(t$p[j], method = "BH")
    t # HR, ordinary 95% CI, and raw p are retained unchanged.
  })
  save_tables(bh, file.path(output_dir, "BH_adjustment.xlsx"))
  ans$BH <- bh
  invisible(ans)
}

run_imputed_models <- function(completed, descriptive_data, include_alcohol = TRUE,
                                path = "outputs/sensitivity/multiple_imputation.xlsx") {
  # Accept the original locally generated mids object or a list of completed data.
  # Source imputation: m=5, maxit=10, seed=123; EDU_new=logreg;
  # SMOKE/MAR/PAF=polyreg; RSBP/FBG=pmm. Predictor matrix is not reconstructed here.
  # The final source MI formula includes ALCOHOL; main models do not.
  need_packages(c("survival", "mice", "broom"))
  if (inherits(completed, "mids")) completed <- lapply(seq_len(completed$m), function(i) mice::complete(completed, i))
  if (!is.list(completed) || length(completed) < 2) stop("Provide at least two completed datasets")
  out <- lapply(EXPOSURES, function(v) {
    ref <- if (v == "BMI_change_pattern") "Normal weight → Normal weight" else "Stable"
    covs <- covariates_for(v, include_alcohol)
    ff <- cox_formula(v, covs)
    fits <- lapply(completed, function(d) {
      require_columns(d, c(v, covs, "Time_years", "Status")); check_survival(d, "Status")
      d <- prepare_model_factors(d, v, ref, include_alcohol)
      survival::coxph(ff, data = d, ties = "efron", na.action = stats::na.fail)
    })
    p <- summary(mice::pool(mice::as.mira(fits)), conf.int = TRUE, exponentiate = TRUE)
    # mice versions use either broom-style or percentage-style CI names.
    low_name <- intersect(c("conf.low", "2.5 %", "2.5%"), names(p))
    high_name <- intersect(c("conf.high", "97.5 %", "97.5%"), names(p))
    if (!length(low_name) || !length(high_name)) stop("Unrecognized pooled CI columns")
    d <- prepare_model_factors(descriptive_data, v, ref, include_alcohol)
    t <- describe_exposure(d, v, "Status")
    j <- match(paste0(v, t$type), p$term)
    t$HR <- p$estimate[j]; t$LCI <- p[[low_name[1]]][j]; t$UCI <- p[[high_name[1]]][j]; t$p <- p$p.value[j]
    t$is_reference <- t$type == ref
    t$HR[t$is_reference] <- t$LCI[t$is_reference] <- t$UCI[t$is_reference] <- 1
    t$HR_model <- format_hr(t$HR, t$LCI, t$UCI, t$p); t$HR_model[t$is_reference] <- "REF"
    t$imputations <- length(completed)
    t$model_N_min <- min(vapply(fits, function(x) x$n, numeric(1)))
    t$model_N_max <- max(vapply(fits, function(x) x$n, numeric(1)))
    t$adjustment <- paste(covs, collapse = " + ")
    t
  })
  save_tables(out, path)
  invisible(out)
}

fit_bmi_trajectories <- function(long_data, seed = 123) {
  # Required input: anonymous integer id_num, time in years, BMI.
  # Prepared observation window ends 2020-12-31; >=3 visits per participant.
  # No source IDs, exact visit dates or linkage tables are needed by this model.
  need_packages("lcmm")
  require_columns(long_data, c("id_num", "time", "BMI"))
  d <- long_data[, c("id_num", "time", "BMI")]
  if (!all(vapply(d, is.numeric, logical(1))) || any(!is.finite(as.matrix(d))))
    stop("Trajectory inputs must be finite numeric values")
  if (any(d$id_num != floor(d$id_num)) || any(d$time < 0) || any(d$BMI <= 0) ||
      any(table(d$id_num) < 3)) stop("Invalid IDs, time, BMI or visit count")
  set.seed(seed)
  m1 <- lcmm::hlme(fixed = BMI ~ time, random = ~1, subject = "id_num", ng = 1, data = d)
  if (m1$conv != 1) stop("One-class model did not converge")
  m3 <- lcmm::hlme(fixed = BMI ~ time, mixture = ~time, random = ~1,
                   subject = "id_num", ng = 3, data = d, B = random(m1), verbose = FALSE)
  if (m3$conv != 1) stop("Three-class model did not converge; review initial values locally")
  tt <- data.frame(time = seq(min(d$time), max(d$time), length.out = 100))
  pred <- lcmm::predictY(m3, newdata = tt, var.time = "time", draws = FALSE)
  curves <- do.call(rbind, lapply(1:3, function(k) data.frame(time = tt$time,
                    class = as.character(k), Predicted_BMI = pred$pred[, k])))
  pp <- m3$pprob
  summaries <- do.call(rbind, lapply(1:3, function(k) {
    j <- which(pp$class == k)
    data.frame(class = as.character(k), N = length(j), share = length(j) / nrow(pp),
               mean_assigned_probability = if (length(j)) mean(pp[[paste0("prob", k)]][j]) else NA_real_)
  }))
  # Returned assignments are for private in-memory linkage only. Do not publish
  # this object or saveRDS() it: it contains individual class assignments.
  list(curves = curves, summary = summaries,
       assignments = pp[, c("id_num", "class")], seed = seed)
}
export_bmi_trajectories <- function(trajectory, output_dir = "outputs/trajectory", dpi = 600) {
  need_packages(c("ggplot2", "openxlsx"))
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  save_tables(list(Predicted_curves = trajectory$curves, Class_summary = trajectory$summary),
               file.path(output_dir, "BMI_trajectory_summary.xlsx"))
  p <- ggplot2::ggplot(trajectory$curves, ggplot2::aes(time, Predicted_BMI, linetype = class)) +
    ggplot2::geom_line(linewidth = 1) + ggplot2::theme_classic(base_size = 12) +
    ggplot2::labs(x = "Years since first measurement", y = "Predicted BMI (kg/m²)", linetype = "Class")
  ggplot2::ggsave(file.path(output_dir, "BMI_trajectory.tiff"), p,
                 width = 10, height = 7, dpi = dpi, compression = "lzw", bg = "white")
  ggplot2::ggsave(file.path(output_dir, "BMI_trajectory.pdf"), p, width = 10, height = 7)
  invisible(NULL)
}
trajectory_cox <- function(trajectory, survival_data, class_labels, reference,
                           output_dir = "outputs/trajectory", causes = FALSE) {
  # Inspect the fitted curves before specifying class_labels and reference.
  # Numerical latent-class IDs can permute between fits. Source fit was:
  # 1 Stable BMI; 2 Low BMI increasing; 3 High BMI declining.
  if (!setequal(names(class_labels), c("1", "2", "3")) ||
      anyDuplicated(unname(class_labels)) || !reference %in% class_labels)
    stop("Provide three uniquely named class labels and their intended reference")
  require_columns(survival_data, "id_num")
  if (anyNA(survival_data$id_num) || anyDuplicated(survival_data$id_num))
    stop("Trajectory survival data must have one row per anonymous id_num")
  # Covariates and survival time are already prepared at the last eligible visit.
  # No exact dates, original identifiers or source-data linkage are used here.
  j <- match(trajectory$assignments$id_num, survival_data$id_num)
  if (anyNA(j)) stop("Missing survival records for trajectory participants")
  d <- survival_data[j, , drop = FALSE]
  d$trajectory <- analysis_factor(unname(class_labels[as.character(trajectory$assignments$class)]),
                                   c(reference, setdiff(unname(class_labels), reference)))
  t <- fit_exposure(d, "trajectory", reference = reference)
  save_tables(list(BMI_Trajectory = t), file.path(output_dir, "BMI_trajectory_Cox_results.xlsx"))
  if (causes) for (spec in list(list("Top_level_outcomes", TOP_CAUSES), list("Major_outcomes", MAJOR_CAUSES))) {
    if (!spec[[1]] %in% names(d)) next
    ct <- cause_specific_models(d, spec[[1]], spec[[2]], c(BMI_Trajectory = "trajectory"), reference)
    save_tables(ct, file.path(output_dir, paste0("Trajectory_", spec[[1]], ".xlsx")))
  }
  invisible(t)
}
