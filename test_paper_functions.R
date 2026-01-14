# test_paper_functions.R
# Generate fake data and test all paper analysis functions

# Source the functions
source("reproduce_paper_functions.R")

# ==== GENERATE FAKE DATA ====

set.seed(123)
n <- 200

# Create correlated variables for more realistic PCA
# Start with some latent factors
academic_factor <- rnorm(n, 0, 1)
socioeconomic_factor <- rnorm(n, 0, 1)

fake_data <- data.frame(
  # School identifier
  school_id = 1:n,

  # Binary grouping variable (e.g., in program vs not)
  in_program = sample(c(0, 1), n, replace = TRUE, prob = c(0.6, 0.4)),

  # Academic outcomes (correlated via academic_factor)
  math_score = 500 + 50 * academic_factor + rnorm(n, 0, 20),
  ela_score = 510 + 45 * academic_factor + rnorm(n, 0, 18),
  attendance_rate = pmin(100, pmax(60, 85 + 5 * academic_factor + rnorm(n, 0, 8))),

  # Demographic percentages (correlated via socioeconomic_factor)
  pct_lowinc = pmin(100, pmax(0, 30 - 15 * socioeconomic_factor + rnorm(n, 0, 10))),
  pct_ell = pmin(100, pmax(0, 10 - 5 * socioeconomic_factor + rnorm(n, 0, 5))),
  pct_white = pmin(100, pmax(0, 60 + 20 * socioeconomic_factor + rnorm(n, 0, 15))),
  pct_sped = pmin(100, pmax(0, 15 + rnorm(n, 0, 5))),

  # Enrollment
  enrollment = rpois(n, lambda = 400),

  # Additional variables for PCA (with some correlation structure)
  chronic_absence = pmin(50, pmax(0, 15 - 8 * academic_factor + rnorm(n, 0, 5))),
  graduation_rate = pmin(100, pmax(50, 88 + 8 * academic_factor + rnorm(n, 0, 6)))
)

# Add some missing values to test robustness
fake_data$math_score[sample(n, 5)] <- NA
fake_data$ela_score[sample(n, 3)] <- NA

cat("Generated fake dataset with", nrow(fake_data), "observations\n")
cat("Variables:", paste(names(fake_data), collapse = ", "), "\n\n")


# ==== TEST CONFIGURATION ====

# Group comparison (Table 1) configuration
test_group_var <- "in_program"
test_compare_vars <- c("math_score", "ela_score", "attendance_rate", "pct_lowinc", "enrollment")
test_compare_labels <- c("Math Score", "ELA Score", "Attendance Rate (%)", "% Low Income", "Enrollment")

# Target variables for univariate analysis
test_target_vars <- c("math_score", "ela_score", "attendance_rate")
test_target_labels <- c("Math Score", "ELA Score", "Attendance Rate (%)")

# Variable pairs for bivariate analysis
test_var_pairs <- list(
  c("pct_lowinc", "math_score"),
  c("pct_ell", "ela_score"),
  c("chronic_absence", "graduation_rate")
)

# Background/control variables
test_background_vars <- c("enrollment", "pct_white", "pct_sped")

# Variables for PCA
test_pca_vars <- c("math_score", "ela_score", "attendance_rate",
                   "pct_lowinc", "chronic_absence", "graduation_rate")


# ==== RUN TESTS ====

cat("========================================\n")
cat("Testing Individual Functions\n")
cat("========================================\n\n")

# Test group comparison
cat("--- Testing group_comparison() ---\n")
grp_results <- group_comparison(
  df = fake_data,
  group_var = test_group_var,
  compare_vars = test_compare_vars,
  compare_labels = test_compare_labels
)
print(grp_results$tbl)

# Test univariate analysis
cat("\n--- Testing univariate_analysis() ---\n")
uni_results <- univariate_analysis(
  df = fake_data,
  vars = test_target_vars,
  labels = test_target_labels
)
print(uni_results$histogram)
print(uni_results$summary_table)

cat("\n--- Testing bivariate_analysis() ---\n")
bi_results <- bivariate_analysis(
  df = fake_data,
  var_pairs = test_var_pairs,
  background_vars = test_background_vars
)

cat("\n--- Testing multivariate_pca() ---\n")
pca_results <- multivariate_pca(
  df = fake_data,
  pca_vars = test_pca_vars
)
print(pca_results$scree_plot)
print(pca_results$scatter_plot)


# ==== TEST MAIN WRAPPER ====

cat("\n\n========================================\n")
cat("Testing Main Wrapper Function\n")
cat("========================================\n\n")

# Create temporary output directory for testing file saves
test_output_dir <- file.path(tempdir(), "paper_test_output")

all_results <- run_paper_analyses(
  df = fake_data,
  group_var = test_group_var,
  compare_vars = test_compare_vars,
  compare_labels = test_compare_labels,
  target_vars = test_target_vars,
  target_labels = test_target_labels,
  var_pairs = test_var_pairs,
  background_vars = test_background_vars,
  pca_vars = test_pca_vars,
  output_dir = test_output_dir
)

# Check what files were created
cat("\n\nFiles saved to", test_output_dir, ":\n")
if (dir.exists(test_output_dir)) {
  saved_files <- list.files(test_output_dir, recursive = TRUE)
  cat(paste(" -", saved_files, collapse = "\n"), "\n")
}


# ==== DISPLAY PLOTS ====

cat("\n\n========================================\n")
cat("Displaying Plots\n")
cat("========================================\n\n")

# Display all plots
print(all_results$univariate$histogram)
print(all_results$multivariate$scree_plot)
print(all_results$multivariate$scatter_plot)

cat("\nAll tests completed successfully!\n")
