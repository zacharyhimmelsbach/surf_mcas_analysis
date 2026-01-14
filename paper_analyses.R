library(dplyr)
library(gtsummary)
library(ggplot2)
library(tidyr)
library(stargazer)

# Load analytic data for focal year
df <- targets::tar_read(analysis_df) |> dplyr::filter(year_spring == 2024)


# ==== USER CONFIGURATION ====

# Group comparison (Table 1): compare two groups across variables
group_var <- "GROUP_VAR"
# PLACEHOLDER: enter grouping variable, e.g., "in_surf"

compare_vars <- c("COMPARE_VAR1", "COMPARE_VAR2", "COMPARE_VAR3")
# PLACEHOLDER: enter variables to compare, e.g., c("avg_scaled_score_MATH", "avg_scaled_score_ELA", "sch_attendance_rate")

compare_labels <- c("Compare Label 1", "Compare Label 2", "Compare Label 3")
# PLACEHOLDER: enter labels for comparison variables, e.g., c("Math Score", "ELA Score", "Attendance Rate")

# Target variables for univariate analysis
target_vars <- c("VAR1", "VAR2", "VAR3")
# PLACEHOLDER: enter variable names, e.g., c("avg_scaled_score_MATH", "avg_scaled_score_ELA", "sch_attendance_rate")

# Labels for target variables (must match order of target_vars)
target_labels <- c("Label 1", "Label 2", "Label 3")
# PLACEHOLDER: enter labels, e.g., c("Math Score", "ELA Score", "Attendance Rate")

# Variable pairs for bivariate analysis: list of c(x_var, y_var)
var_pairs <- list(
  c("X_VAR1", "Y_VAR1"),
  c("X_VAR2", "Y_VAR2")
)
# PLACEHOLDER: enter variable pairs, e.g., list(c("pct_lowinc", "avg_scaled_score_MATH"), c("pct_ell", "avg_scaled_score_ELA"))

# Background/control variables for adjusted regressions
background_vars <- c("BG_VAR1", "BG_VAR2")
# PLACEHOLDER: enter control variables, e.g., c("hs_total_enrl", "pct_white", "pct_sped")

# Variables for PCA
pca_vars <- c("PCA_VAR1", "PCA_VAR2", "PCA_VAR3")
# PLACEHOLDER: enter PCA variables, e.g., c("avg_scaled_score_MATH", "avg_scaled_score_ELA", "sch_attendance_rate", "pct_lowinc")

# ==== END USER CONFIGURATION ====


# Comparing SURF and non-SURF schools ----
table1 <- df |>
  select(in_surf,
         avg_scaled_score_MATH,
         avg_scaled_score_ELA,
         sch_attendance_rate,
         hs_total_enrl,
         pct_white,
         pct_ell,
         pct_lowinc,
         pct_sped) |>
  tbl_summary(
    by = in_surf,
    type = all_continuous() ~ "continuous2",   # shows mean (SD)
    statistic = all_continuous() ~ "{mean} ({sd})"
  ) |> add_p(list(all_continuous() ~ "t.test"))

table1

######################

# Univariate description of SURF measures ----

# Create named vector for labeling
names(target_labels) <- target_vars

# Pivot data to long format for faceted plotting
df_long <- df |>
  select(all_of(target_vars)) |>
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") |>
  mutate(variable_label = target_labels[variable])

# Multi-panel histogram
univariate_histogram <- ggplot(df_long, aes(x = value)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white", alpha = 0.7) +
  facet_wrap(~ variable_label, scales = "free") +
  labs(x = "Value", y = "Count", title = "Distribution of Target Variables") +
  theme_minimal()

print(univariate_histogram)

# Summary table with mean and SD
univariate_summary <- df |>
  select(all_of(target_vars)) |>
  summarise(across(everything(), list(
    mean = ~mean(.x, na.rm = TRUE),
    sd = ~sd(.x, na.rm = TRUE),
    n = ~sum(!is.na(.x))
  ))) |>
  pivot_longer(cols = everything(),
               names_to = c("variable", "statistic"),
               names_sep = "_(?=[^_]+$)") |>
  pivot_wider(names_from = statistic, values_from = value) |>
  mutate(label = target_labels[variable]) |>
  select(variable, label, n, mean, sd)

print(univariate_summary)


######################

# Bivariate Analyses ----

# Store results for each pair
bivariate_results <- list()

for (i in seq_along(var_pairs)) {
  pair <- var_pairs[[i]]
  x_var <- pair[1]
  y_var <- pair[2]

  cat("\n========================================\n")
  cat("Pair", i, ":", x_var, "->", y_var, "\n")
  cat("========================================\n")

  # Calculate correlation
  cor_val <- cor(df[[x_var]], df[[y_var]], use = "complete.obs")
  cat("Correlation:", round(cor_val, 3), "\n\n")

  # Unadjusted regression
  formula_unadj <- as.formula(paste(y_var, "~", x_var))
  model_unadj <- lm(formula_unadj, data = df)

  # Adjusted regression (with background variables)
  formula_adj <- as.formula(paste(y_var, "~", x_var, "+", paste(background_vars, collapse = " + ")))
  model_adj <- lm(formula_adj, data = df)

  # Generate stargazer table
  cat("Regression Table:\n")
  stargazer(model_unadj, model_adj,
            type = "text",
            title = paste("Regression:", y_var, "on", x_var),
            column.labels = c("Unadjusted", "Adjusted"),
            dep.var.labels = y_var,
            omit.stat = c("f", "ser"),
            star.cutoffs = c(0.05, 0.01, 0.001))

  # Store results
  bivariate_results[[i]] <- list(
    pair = pair,
    correlation = cor_val,
    model_unadjusted = model_unadj,
    model_adjusted = model_adj
  )
}


######################

# Multivariate Analyses ----

# Subset to PCA variables and remove rows with missing values
df_pca <- df |>
  select(all_of(pca_vars)) |>
  na.omit()

cat("\nPCA using", nrow(df_pca), "complete observations\n")

# Run PCA with scaling
pca_result <- prcomp(df_pca, scale. = TRUE, center = TRUE)

# Scree plot: variance explained
var_explained <- pca_result$sdev^2 / sum(pca_result$sdev^2)
cum_var_explained <- cumsum(var_explained)

scree_data <- data.frame(
  PC = factor(1:length(var_explained)),
  Variance = var_explained,
  Cumulative = cum_var_explained
)

scree_plot <- ggplot(scree_data, aes(x = PC, y = Variance)) +
  geom_col(fill = "steelblue", alpha = 0.7) +
  geom_line(aes(y = Cumulative, group = 1), color = "darkred", linewidth = 1) +
  geom_point(aes(y = Cumulative), color = "darkred", size = 3) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(x = "Principal Component",
       y = "Proportion of Variance",
       title = "Scree Plot with Cumulative Variance") +
  theme_minimal()

print(scree_plot)

# Loadings/weights table
loadings_table <- as.data.frame(pca_result$rotation) |>
  tibble::rownames_to_column("Variable")

cat("\nPCA Loadings:\n")
print(loadings_table)

# Scatterplot of PC1 vs PC2
pc_scores <- as.data.frame(pca_result$x)

pca_scatter <- ggplot(pc_scores, aes(x = PC1, y = PC2)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  labs(x = paste0("PC1 (", round(var_explained[1] * 100, 1), "%)"),
       y = paste0("PC2 (", round(var_explained[2] * 100, 1), "%)"),
       title = "Principal Components Scatterplot") +
  theme_minimal()

print(pca_scatter)


######################
