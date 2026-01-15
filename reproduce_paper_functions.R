# reproduce_paper_functions.R
# Reusable functions for paper analyses
# Usage: source this file and call run_paper_analyses() with your data and specifications

library(dplyr)
library(ggplot2)
library(tidyr)
library(stargazer)
library(officer)
library(flextable)
library(gtsummary)
library(lme4)


#' Group Comparison (Table 1)
#'
#' Compares two groups across multiple variables using t-tests
#'
#' @param df Data frame containing the variables
#' @param group_var Name of the grouping variable (e.g., "in_surf")
#' @param compare_vars Character vector of variables to compare across groups
#' @param compare_labels Optional named vector of labels for variables
#' @param output_dir Optional directory to save outputs (NULL = don't save)
#' @return List with tbl (gtsummary object) and summary_df (data frame version)
group_comparison <- function(df, group_var, compare_vars, compare_labels = NULL, output_dir = NULL) {

  # Build label list if provided
  if (!is.null(compare_labels)) {
    label_list <- as.list(compare_labels)
    names(label_list) <- compare_vars
  } else {
    label_list <- NULL
  }

  # Create the comparison table
  tbl <- df |>
    select(all_of(c(group_var, compare_vars))) |>
    tbl_summary(
      by = all_of(group_var),
      type = all_continuous() ~ "continuous2",
      statistic = all_continuous() ~ "{mean} ({sd})",
      label = label_list,
      missing = "no"
    ) |>
    add_p(test = all_continuous() ~ "t.test") |>
    add_overall()

  # Convert to data frame for saving
  summary_df <- as_tibble(tbl)

  # Save outputs if directory specified
  if (!is.null(output_dir)) {
    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

    # Save as HTML
    tbl |>
      as_gt() |>
      gt::gtsave(file.path(output_dir, "group_comparison.html"))

    # Save as CSV
    write.csv(summary_df, file.path(output_dir, "group_comparison.csv"), row.names = FALSE)
  }

  list(
    tbl = tbl,
    summary_df = summary_df
  )
}


#' Univariate Analysis
#'
#' Generates histograms and summary statistics for target variables
#'
#' @param df Data frame containing the variables (school-level)
#' @param vars Character vector of variable names to analyze
#' @param labels Character vector of labels (same length as vars)
#' @param student_df Optional student-level data frame for ICC calculation
#' @param school_id_var Name of the school ID variable in student_df (required if student_df provided)
#' @param output_dir Optional directory to save outputs (NULL = don't save)
#' @return List with histogram (ggplot) and summary_table (data.frame)
univariate_analysis <- function(df, vars, labels, student_df = NULL, school_id_var = NULL, output_dir = NULL) {

  # Validate inputs
  if (length(vars) != length(labels)) {
    stop("vars and labels must have the same length")
  }

  if (!is.null(student_df) && is.null(school_id_var)) {
    stop("school_id_var is required when student_df is provided")
  }

  # Create named vector for labeling
  names(labels) <- vars

  # Pivot data to long format for faceted plotting
  df_long <- df |>
    select(all_of(vars)) |>
    pivot_longer(cols = everything(), names_to = "variable", values_to = "value") |>
    mutate(variable_label = labels[variable])

  # Multi-panel histogram
  histogram <- ggplot(df_long, aes(x = value)) +
    geom_histogram(bins = 30, fill = "steelblue", color = "white", alpha = 0.7) +
    facet_wrap(~ variable_label, scales = "free") +
    labs(x = "Value", y = "Count", title = "Distribution of Target Variables") +
    theme_minimal()

  # Summary table with mean and SD
  summary_table <- df |>
    select(all_of(vars)) |>
    summarise(across(everything(), list(
      mean = ~mean(.x, na.rm = TRUE),
      sd = ~sd(.x, na.rm = TRUE),
      n = ~sum(!is.na(.x))
    ))) |>
    pivot_longer(cols = everything(),
                 names_to = c("variable", "statistic"),
                 names_sep = "_(?=[^_]+$)") |>
    pivot_wider(names_from = statistic, values_from = value) |>
    mutate(label = labels[variable]) |>
    select(variable, label, n, mean, sd)

  # Calculate ICCs if student-level data provided
  if (!is.null(student_df)) {
    icc_values <- sapply(vars, function(v) {
      if (v %in% names(student_df)) {
        # Build formula for random intercept model
        formula_str <- paste0(v, " ~ 1 + (1 | ", school_id_var, ")")
        tryCatch({
          model <- lmer(as.formula(formula_str), data = student_df, REML = TRUE)
          vc <- as.data.frame(VarCorr(model))
          var_between <- vc$vcov[vc$grp == school_id_var]
          var_within <- vc$vcov[vc$grp == "Residual"]
          icc <- var_between / (var_between + var_within)
          return(icc)
        }, error = function(e) {
          warning(paste("Could not calculate ICC for", v, ":", e$message))
          return(NA_real_)
        })
      } else {
        return(NA_real_)
      }
    })

    # Also calculate student-level SD
    student_sd_values <- sapply(vars, function(v) {
      if (v %in% names(student_df)) {
        sd(student_df[[v]], na.rm = TRUE)
      } else {
        NA_real_
      }
    })

    summary_table <- summary_table |>
      mutate(
        student_sd = student_sd_values[variable],
        icc = icc_values[variable]
      )
  }

  # Save outputs if directory specified
  if (!is.null(output_dir)) {
    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
    ggsave(file.path(output_dir, "univariate_histogram.pdf"), histogram, width = 10, height = 8)
    write.csv(summary_table, file.path(output_dir, "univariate_summary.csv"), row.names = FALSE)
  }

  list(
    histogram = histogram,
    summary_table = summary_table
  )
}


#' Bivariate Analysis
#'
#' Calculates correlations and fits regressions for variable pairs
#'
#' @param df Data frame containing the variables
#' @param var_pairs List of character vectors, each c(x_var, y_var)
#' @param background_vars Character vector of control variable names
#' @param output_dir Optional directory to save outputs (NULL = don't save)
#' @return List of results for each pair (correlation, models, stargazer output)
bivariate_analysis <- function(df, var_pairs, background_vars, output_dir = NULL) {

  results <- list()

  for (i in seq_along(var_pairs)) {
    pair <- var_pairs[[i]]
    x_var <- pair[1]
    y_var <- pair[2]

    # Calculate correlation
    cor_val <- cor(df[[x_var]], df[[y_var]], use = "complete.obs")

    # Unadjusted regression
    formula_unadj <- as.formula(paste(y_var, "~", x_var))
    model_unadj <- lm(formula_unadj, data = df)

    # Adjusted regression (with background variables)
    formula_adj <- as.formula(paste(y_var, "~", x_var, "+", paste(background_vars, collapse = " + ")))
    model_adj <- lm(formula_adj, data = df)

    # Capture stargazer output
    stargazer_text <- capture.output(
      stargazer(model_unadj, model_adj,
                type = "text",
                title = paste("Regression:", y_var, "on", x_var),
                column.labels = c("Unadjusted", "Adjusted"),
                dep.var.labels = y_var,
                omit.stat = c("f", "ser"),
                star.cutoffs = c(0.05, 0.01, 0.001))
    )

    # Create scatterplot with LOESS curve
    scatter_plot <- ggplot(df, aes(x = .data[[x_var]], y = .data[[y_var]])) +
      geom_point(alpha = 0.5, color = "steelblue") +
      geom_smooth(method = "loess", se = TRUE, color = "darkred", fill = "pink", alpha = 0.3) +
      labs(x = x_var, y = y_var,
           title = paste(y_var, "vs", x_var),
           subtitle = paste("r =", round(cor_val, 3))) +
      theme_minimal()

    # Store results
    results[[i]] <- list(
      pair = pair,
      x_var = x_var,
      y_var = y_var,
      correlation = cor_val,
      model_unadjusted = model_unadj,
      model_adjusted = model_adj,
      stargazer_text = stargazer_text,
      scatter_plot = scatter_plot
    )

    # Save outputs if directory specified
    if (!is.null(output_dir)) {
      if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
      # Save as text
      writeLines(stargazer_text,
                 file.path(output_dir, paste0("regression_", x_var, "_", y_var, ".txt")))
      # Save as HTML
      stargazer(model_unadj, model_adj,
                type = "html",
                title = paste("Regression:", y_var, "on", x_var),
                column.labels = c("Unadjusted", "Adjusted"),
                dep.var.labels = y_var,
                omit.stat = c("f", "ser"),
                star.cutoffs = c(0.05, 0.01, 0.001),
                out = file.path(output_dir, paste0("regression_", x_var, "_", y_var, ".html")))
      # Save scatterplot
      ggsave(file.path(output_dir, paste0("scatter_", x_var, "_", y_var, ".pdf")),
             scatter_plot, width = 8, height = 6)
    }
  }

  # Print summary to console
  cat("\n========================================\n")
  cat("Bivariate Analysis Summary\n")
  cat("========================================\n")
  for (i in seq_along(results)) {
    cat("\nPair", i, ":", results[[i]]$x_var, "->", results[[i]]$y_var, "\n")
    cat("Correlation:", round(results[[i]]$correlation, 3), "\n")
    cat(results[[i]]$stargazer_text, sep = "\n")
  }

  results
}


#' Multivariate PCA Analysis
#'
#' Performs PCA and generates scree plot, loadings table, and scatterplot
#'
#' @param df Data frame containing the variables
#' @param pca_vars Character vector of variable names to include in PCA
#' @param output_dir Optional directory to save outputs (NULL = don't save)
#' @return List with pca_object, scree_plot, loadings_table, scatter_plot
multivariate_pca <- function(df, pca_vars, output_dir = NULL) {

  # Subset to PCA variables and remove rows with missing values
  df_pca <- df |>
    select(all_of(pca_vars)) |>
    na.omit()

  cat("\nPCA using", nrow(df_pca), "complete observations out of", nrow(df), "total\n")

  # Run PCA with scaling
  pca_result <- prcomp(df_pca, scale. = TRUE, center = TRUE)

  # Variance explained

  var_explained <- pca_result$sdev^2 / sum(pca_result$sdev^2)
  cum_var_explained <- cumsum(var_explained)

  # Scree plot data
  scree_data <- data.frame(
    PC = factor(1:length(var_explained)),
    Variance = var_explained,
    Cumulative = cum_var_explained
  )

  # Scree plot
  scree_plot <- ggplot(scree_data, aes(x = PC, y = Variance)) +
    geom_col(fill = "steelblue", alpha = 0.7) +
    geom_line(aes(y = Cumulative, group = 1), color = "darkred", linewidth = 1) +
    geom_point(aes(y = Cumulative), color = "darkred", size = 3) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    labs(x = "Principal Component",
         y = "Proportion of Variance",
         title = "Scree Plot with Cumulative Variance") +
    theme_minimal()

  # Loadings table
  loadings_table <- as.data.frame(pca_result$rotation) |>
    tibble::rownames_to_column("Variable")

  # PC scores for scatterplot
  pc_scores <- as.data.frame(pca_result$x)

  # Scatterplot of PC1 vs PC2
  scatter_plot <- ggplot(pc_scores, aes(x = PC1, y = PC2)) +
    geom_point(alpha = 0.6, color = "steelblue") +
    labs(x = paste0("PC1 (", round(var_explained[1] * 100, 1), "%)"),
         y = paste0("PC2 (", round(var_explained[2] * 100, 1), "%)"),
         title = "Principal Components Scatterplot") +
    theme_minimal()

  # Save outputs if directory specified
  if (!is.null(output_dir)) {
    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
    ggsave(file.path(output_dir, "pca_scree_plot.pdf"), scree_plot, width = 8, height = 6)
    ggsave(file.path(output_dir, "pca_scatter_plot.pdf"), scatter_plot, width = 8, height = 6)
    write.csv(loadings_table, file.path(output_dir, "pca_loadings.csv"), row.names = FALSE)
    write.csv(scree_data, file.path(output_dir, "pca_variance_explained.csv"), row.names = FALSE)
  }

  list(
    pca_object = pca_result,
    variance_explained = var_explained,
    cumulative_variance = cum_var_explained,
    scree_plot = scree_plot,
    loadings_table = loadings_table,
    pc_scores = pc_scores,
    scatter_plot = scatter_plot
  )
}


#' Create Collated Word Document Report
#'
#' Generates a Word document containing all analysis outputs
#'
#' @param results List returned by run_paper_analyses()
#' @param output_path Path for the output .docx file
#' @param title Optional title for the report
#' @return The officer document object (invisibly)
create_report_docx <- function(results, output_path, title = "Paper Analyses Report") {

  # Initialize document
  doc <- read_docx()


  # Title

  doc <- doc |>
    body_add_par(title, style = "heading 1") |>
    body_add_par("", style = "Normal")

  # ========================================
  # Section 0: Group Comparison (Table 1)
  # ========================================
  if (!is.null(results$group_comparison)) {
    doc <- doc |>
      body_add_par("1. Group Comparison", style = "heading 2") |>
      body_add_par("", style = "Normal")

    # Convert gtsummary table to flextable
    grp_ft <- results$group_comparison$tbl |>
      as_flex_table() |>
      autofit()

    doc <- doc |>
      body_add_flextable(grp_ft) |>
      body_add_par("Note: Values are Mean (SD). P-values from t-tests.", style = "Normal") |>
      body_add_par("", style = "Normal")
  }

  # ========================================
  # Section 2: Univariate Analysis
  # ========================================
  doc <- doc |>
    body_add_par(ifelse(!is.null(results$group_comparison), "2. Univariate Analysis", "1. Univariate Analysis"), style = "heading 2") |>
    body_add_par("", style = "Normal")

  # Summary table
  doc <- doc |>
    body_add_par("Summary Statistics", style = "heading 3")

  # Format the summary table nicely
  uni_table <- results$univariate$summary_table |>
    mutate(
      mean = round(mean, 2),
      sd = round(sd, 2)
    ) |>
    select(Label = label, N = n, Mean = mean, SD = sd) |>
    flextable() |>
    autofit() |>
    theme_booktabs()

  doc <- doc |>
    body_add_flextable(uni_table) |>
    body_add_par("", style = "Normal")

  # Histogram
  doc <- doc |>
    body_add_par("Distribution of Variables", style = "heading 3")

  # Save plot to temp file and add to doc
  hist_temp <- tempfile(fileext = ".png")
  ggsave(hist_temp, results$univariate$histogram, width = 10, height = 8, dpi = 150)
  doc <- doc |>
    body_add_img(hist_temp, width = 6, height = 4.8) |>
    body_add_par("", style = "Normal")

  # ========================================
  # Section: Bivariate Analysis
  # ========================================
  bivariate_num <- ifelse(!is.null(results$group_comparison), 3, 2)
  doc <- doc |>
    body_add_par(paste0(bivariate_num, ". Bivariate Analysis"), style = "heading 2") |>
    body_add_par("", style = "Normal")

  for (i in seq_along(results$bivariate)) {
    res <- results$bivariate[[i]]

    doc <- doc |>
      body_add_par(paste0("Pair ", i, ": ", res$x_var, " -> ", res$y_var), style = "heading 3") |>
      body_add_par(paste0("Correlation: ", round(res$correlation, 3)), style = "Normal") |>
      body_add_par("", style = "Normal")

    # Create regression table using flextable
    # Extract coefficients from both models
    coef_unadj <- summary(res$model_unadjusted)$coefficients
    coef_adj <- summary(res$model_adjusted)$coefficients

    # Build a combined table
    all_vars <- unique(c(rownames(coef_unadj), rownames(coef_adj)))

    reg_table_data <- data.frame(
      Variable = all_vars,
      Unadj_Est = NA_real_,
      Unadj_SE = NA_real_,
      Adj_Est = NA_real_,
      Adj_SE = NA_real_
    )

    for (v in all_vars) {
      if (v %in% rownames(coef_unadj)) {
        reg_table_data$Unadj_Est[reg_table_data$Variable == v] <- coef_unadj[v, "Estimate"]
        reg_table_data$Unadj_SE[reg_table_data$Variable == v] <- coef_unadj[v, "Std. Error"]
      }
      if (v %in% rownames(coef_adj)) {
        reg_table_data$Adj_Est[reg_table_data$Variable == v] <- coef_adj[v, "Estimate"]
        reg_table_data$Adj_SE[reg_table_data$Variable == v] <- coef_adj[v, "Std. Error"]
      }
    }

    # Format for display
    reg_table_display <- reg_table_data |>
      mutate(
        Unadjusted = ifelse(is.na(Unadj_Est), "",
                           paste0(round(Unadj_Est, 3), " (", round(Unadj_SE, 3), ")")),
        Adjusted = ifelse(is.na(Adj_Est), "",
                         paste0(round(Adj_Est, 3), " (", round(Adj_SE, 3), ")"))
      ) |>
      select(Variable, Unadjusted, Adjusted)

    # Add model fit statistics
    reg_table_display <- rbind(
      reg_table_display,
      data.frame(Variable = "N",
                 Unadjusted = as.character(nobs(res$model_unadjusted)),
                 Adjusted = as.character(nobs(res$model_adjusted))),
      data.frame(Variable = "R-squared",
                 Unadjusted = round(summary(res$model_unadjusted)$r.squared, 3),
                 Adjusted = round(summary(res$model_adjusted)$r.squared, 3))
    )

    reg_ft <- reg_table_display |>
      flextable() |>
      autofit() |>
      theme_booktabs() |>
      add_header_row(values = c("", paste("DV:", res$y_var)), colwidths = c(1, 2))

    doc <- doc |>
      body_add_flextable(reg_ft) |>
      body_add_par("Note: Standard errors in parentheses", style = "Normal") |>
      body_add_par("", style = "Normal")

    # Add scatterplot
    scatter_temp <- tempfile(fileext = ".png")
    ggsave(scatter_temp, res$scatter_plot, width = 8, height = 6, dpi = 150)
    doc <- doc |>
      body_add_img(scatter_temp, width = 5, height = 3.75) |>
      body_add_par("", style = "Normal")
  }

  # ========================================
  # Section: Multivariate (PCA) Analysis
  # ========================================
  multivariate_num <- ifelse(!is.null(results$group_comparison), 4, 3)
  doc <- doc |>
    body_add_par(paste0(multivariate_num, ". Multivariate (PCA) Analysis"), style = "heading 2") |>
    body_add_par("", style = "Normal")

  # Variance explained table
  doc <- doc |>
    body_add_par("Variance Explained", style = "heading 3")

  var_table <- data.frame(
    PC = paste0("PC", 1:length(results$multivariate$variance_explained)),
    Variance = round(results$multivariate$variance_explained * 100, 1),
    Cumulative = round(results$multivariate$cumulative_variance * 100, 1)
  ) |>
    rename(`Variance (%)` = Variance, `Cumulative (%)` = Cumulative) |>
    flextable() |>
    autofit() |>
    theme_booktabs()

  doc <- doc |>
    body_add_flextable(var_table) |>
    body_add_par("", style = "Normal")

  # Scree plot
  doc <- doc |>
    body_add_par("Scree Plot", style = "heading 3")

  scree_temp <- tempfile(fileext = ".png")
  ggsave(scree_temp, results$multivariate$scree_plot, width = 8, height = 6, dpi = 150)
  doc <- doc |>
    body_add_img(scree_temp, width = 5, height = 3.75) |>
    body_add_par("", style = "Normal")

  # Loadings table
  doc <- doc |>
    body_add_par("PCA Loadings", style = "heading 3")

  loadings_ft <- results$multivariate$loadings_table |>
    mutate(across(where(is.numeric), ~round(.x, 3))) |>
    flextable() |>
    autofit() |>
    theme_booktabs()

  doc <- doc |>
    body_add_flextable(loadings_ft) |>
    body_add_par("", style = "Normal")

  # PC Scatterplot
  doc <- doc |>
    body_add_par("PC1 vs PC2 Scatterplot", style = "heading 3")

  scatter_temp <- tempfile(fileext = ".png")
  ggsave(scatter_temp, results$multivariate$scatter_plot, width = 8, height = 6, dpi = 150)
  doc <- doc |>
    body_add_img(scatter_temp, width = 5, height = 3.75) |>
    body_add_par("", style = "Normal")

  # Save document
  print(doc, target = output_path)
  cat("Word document saved to:", output_path, "\n")

  invisible(doc)
}


#' Create Summary Word Document Report
#'
#' Generates a condensed Word document with key results only
#'
#' @param results List returned by run_paper_analyses()
#' @param output_path Path for the output .docx file
#' @param title Optional title for the report
#' @return The officer document object (invisibly)
create_summary_docx <- function(results, output_path, title = "Paper Analyses Summary") {

 # Initialize document
  doc <- read_docx()

  # Title
  doc <- doc |>
    body_add_par(title, style = "heading 1") |>
    body_add_par("", style = "Normal")

  # ========================================
  # Section 1: Group Comparison (Table 1)
  # ========================================
  if (!is.null(results$group_comparison)) {
    doc <- doc |>
      body_add_par("1. Group Comparison", style = "heading 2") |>
      body_add_par("", style = "Normal")

    grp_ft <- results$group_comparison$tbl |>
      as_flex_table() |>
      autofit()

    doc <- doc |>
      body_add_flextable(grp_ft) |>
      body_add_par("Note: Values are Mean (SD). P-values from t-tests.", style = "Normal") |>
      body_add_par("", style = "Normal")
  }

  # ========================================
  # Section 2: Univariate Histograms (2 per row, larger)
  # ========================================
  doc <- doc |>
    body_add_par("2. Distribution of Variables", style = "heading 2") |>
    body_add_par("", style = "Normal")

  # Recreate histogram with 2 columns
  hist_data <- results$univariate$histogram$data
  hist_plot <- ggplot(hist_data, aes(x = value)) +
    geom_histogram(bins = 30, fill = "steelblue", color = "white", alpha = 0.7) +
    facet_wrap(~ variable_label, scales = "free", ncol = 2) +
    labs(x = "Value", y = "Count") +
    theme_minimal() +
    theme(strip.text = element_text(size = 11, face = "bold"))

  hist_temp <- tempfile(fileext = ".png")
  ggsave(hist_temp, hist_plot, width = 10, height = 12, dpi = 150)
  doc <- doc |>
    body_add_img(hist_temp, width = 6.5, height = 7.8) |>
    body_add_par("", style = "Normal")

  # Univariate summary table
  doc <- doc |>
    body_add_par("Summary Statistics", style = "heading 3")

  # Format table - check if ICC columns exist
  uni_summary <- results$univariate$summary_table
  has_icc <- "icc" %in% names(uni_summary)

  if (has_icc) {
    uni_table_display <- uni_summary |>
      mutate(
        mean = round(mean, 2),
        sd = round(sd, 2),
        student_sd = round(student_sd, 2),
        icc = round(icc, 3)
      ) |>
      select(Label = label, N = n, Mean = mean, `School SD` = sd,
             `Student SD` = student_sd, ICC = icc)
  } else {
    uni_table_display <- uni_summary |>
      mutate(
        mean = round(mean, 2),
        sd = round(sd, 2)
      ) |>
      select(Label = label, N = n, Mean = mean, SD = sd)
  }

  uni_ft <- uni_table_display |>
    flextable() |>
    autofit() |>
    theme_booktabs()

  doc <- doc |>
    body_add_flextable(uni_ft) |>
    body_add_par("", style = "Normal")

  # ========================================
  # Section 3: Bivariate Summary Table
  # ========================================
  doc <- doc |>
    body_add_par("3. Bivariate Associations", style = "heading 2") |>
    body_add_par("", style = "Normal")

  # Build consolidated table from bivariate results
  bivariate_summary <- do.call(rbind, lapply(results$bivariate, function(res) {
    # Get coefficients
    coef_unadj <- summary(res$model_unadjusted)$coefficients
    coef_adj <- summary(res$model_adjusted)$coefficients

    # Extract the x variable coefficient (not intercept)
    x_var <- res$x_var
    unadj_est <- coef_unadj[x_var, "Estimate"]
    unadj_se <- coef_unadj[x_var, "Std. Error"]
    adj_est <- coef_adj[x_var, "Estimate"]
    adj_se <- coef_adj[x_var, "Std. Error"]

    data.frame(
      X = res$x_var,
      Y = res$y_var,
      r = round(res$correlation, 3),
      Unadjusted = paste0(round(unadj_est, 3), " (", round(unadj_se, 3), ")"),
      Adjusted = paste0(round(adj_est, 3), " (", round(adj_se, 3), ")")
    )
  }))

  colnames(bivariate_summary) <- c("X Variable", "Y Variable", "r", "Unadjusted β (SE)", "Adjusted β (SE)")

  bivariate_ft <- bivariate_summary |>
    flextable() |>
    autofit() |>
    theme_booktabs() |>
    align(j = 3:5, align = "center", part = "all")

  doc <- doc |>
    body_add_flextable(bivariate_ft) |>
    body_add_par("Note: β = regression coefficient, SE = standard error. Adjusted models control for background variables.", style = "Normal") |>
    body_add_par("", style = "Normal")

  # Save document
  print(doc, target = output_path)
  cat("Summary document saved to:", output_path, "\n")

  invisible(doc)
}


#' Run All Paper Analyses
#'
#' Main wrapper function that runs univariate, bivariate, and multivariate analyses
#'
#' @param df Data frame containing all variables (school-level)
#' @param group_var Optional grouping variable for Table 1 comparison (NULL to skip)
#' @param compare_vars Optional variables to compare across groups
#' @param compare_labels Optional labels for comparison variables
#' @param target_vars Character vector of variable names for univariate analysis
#' @param target_labels Character vector of labels for target variables
#' @param student_df Optional student-level data for ICC calculation
#' @param school_id_var Name of school ID variable in student_df (required if student_df provided)
#' @param var_pairs List of c(x_var, y_var) pairs for bivariate analysis
#' @param background_vars Character vector of control variable names
#' @param pca_vars Character vector of variable names for PCA
#' @param output_dir Optional directory to save all outputs (NULL = don't save)
#' @return List containing results from all analyses
#'
#' @examples
#' \dontrun{
#' results <- run_paper_analyses(
#'   df = my_data,
#'   group_var = "in_program",
#'   compare_vars = c("math_score", "ela_score"),
#'   compare_labels = c("Math Score", "ELA Score"),
#'   target_vars = c("math_score", "ela_score", "attendance"),
#'   target_labels = c("Math Score", "ELA Score", "Attendance Rate"),
#'   student_df = student_data,
#'   school_id_var = "school_id",
#'   var_pairs = list(
#'     c("pct_lowinc", "math_score"),
#'     c("pct_ell", "ela_score")
#'   ),
#'   background_vars = c("enrollment", "pct_white"),
#'   pca_vars = c("math_score", "ela_score", "attendance", "pct_lowinc"),
#'   output_dir = "output/paper_results"
#' )
#' }
run_paper_analyses <- function(df,
                               group_var = NULL,
                               compare_vars = NULL,
                               compare_labels = NULL,
                               target_vars,
                               target_labels,
                               student_df = NULL,
                               school_id_var = NULL,
                               var_pairs,
                               background_vars,
                               pca_vars,
                               output_dir = NULL) {

  cat("========================================\n")
  cat("Running Paper Analyses\n")
  cat("========================================\n\n")

  # Track section number
 section_num <- 1

  # Group comparison (Table 1) - optional
  group_comparison_results <- NULL
  if (!is.null(group_var) && !is.null(compare_vars)) {
    cat(section_num, ". Group Comparison (Table 1)\n", sep = "")
    cat("   Grouping by:", group_var, "\n")
    cat("   Variables:", paste(compare_vars, collapse = ", "), "\n\n")
    comparison_dir <- if (!is.null(output_dir)) file.path(output_dir, "group_comparison") else NULL
    group_comparison_results <- group_comparison(df, group_var, compare_vars, compare_labels, comparison_dir)
    print(group_comparison_results$tbl)
    cat("\n")
    section_num <- section_num + 1
  }

  # Univariate analysis
  cat(section_num, ". Univariate Analysis\n", sep = "")
  cat("   Variables:", paste(target_vars, collapse = ", "), "\n")
  if (!is.null(student_df)) {
    cat("   ICCs: Calculating from student-level data\n")
  }
  cat("\n")
  univariate_dir <- if (!is.null(output_dir)) file.path(output_dir, "univariate") else NULL
  univariate_results <- univariate_analysis(df, target_vars, target_labels,
                                            student_df = student_df,
                                            school_id_var = school_id_var,
                                            output_dir = univariate_dir)
  print(univariate_results$summary_table)
  cat("\n")

  section_num <- section_num + 1

  # Bivariate analysis
  cat(section_num, ". Bivariate Analysis\n", sep = "")
  cat("   Pairs:", length(var_pairs), "\n")
  cat("   Background vars:", paste(background_vars, collapse = ", "), "\n\n")
  bivariate_dir <- if (!is.null(output_dir)) file.path(output_dir, "bivariate") else NULL
  bivariate_results <- bivariate_analysis(df, var_pairs, background_vars, bivariate_dir)
  cat("\n")
  section_num <- section_num + 1

  # Multivariate (PCA) analysis
  cat(section_num, ". Multivariate (PCA) Analysis\n", sep = "")
  cat("   Variables:", paste(pca_vars, collapse = ", "), "\n\n")
  multivariate_dir <- if (!is.null(output_dir)) file.path(output_dir, "multivariate") else NULL
  pca_results <- multivariate_pca(df, pca_vars, multivariate_dir)
  cat("\nPCA Loadings:\n")
  print(pca_results$loadings_table)

  # Compile results
  results <- list(
    group_comparison = group_comparison_results,
    univariate = univariate_results,
    bivariate = bivariate_results,
    multivariate = pca_results
  )

  # Generate Word documents if output_dir is specified
  if (!is.null(output_dir)) {
    # Full detailed report
    docx_path <- file.path(output_dir, "paper_analyses_report.docx")
    create_report_docx(results, docx_path, title = "Paper Analyses Report")

    # Summary report
    summary_path <- file.path(output_dir, "paper_analyses_summary.docx")
    create_summary_docx(results, summary_path, title = "Paper Analyses Summary")
  }

  cat("\n========================================\n")
  cat("Analysis Complete\n")
  if (!is.null(output_dir)) {
    cat("Outputs saved to:", output_dir, "\n")
  }
  cat("========================================\n")

  results
}
