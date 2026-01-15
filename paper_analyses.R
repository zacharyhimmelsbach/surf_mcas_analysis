# Load analytic data for focal year
df <- targets::tar_read(analysis_df) |> dplyr::filter(year_spring == 2024)

# Set up target variables ----
## Variables to compare surf with non-surf sample ----
surf_v_nonsurf_list <- list("avg_scaled_score_MATH" = "Avg MCAS Math (Scaled)",
                         "avg_scaled_score_ELA" =  "Avg MCAS ELA (Scaled)",
                         "sch_attendance_rate"   = "Attendance Rate",
                         "hs_total_enrl"        = "Total HS Enrollment",
                         "pct_white"            = "% White",
                         "pct_ell"              = "% ELL",
                         "pct_lowinc"           = "% Low Income",
                         "pct_sped"             = "% w/ Disability")
surf_v_nonsurf_vars <- names(surf_v_nonsurf_list)
surf_v_nonsurf_labels <- unname(unlist(surf_v_nonsurf_list))

## Variables from SURF ----
surf_varlist <- list(anyALC.Past31 = "% Use Alcohol",
                  anyCNN.Past31 = "% Use Cannabis",
                  anyNIC.Past31 = "% Use Nicotine",
                  sch_avg_phq4  = "Avg PHQ-4 Score",
                  sch_avg_si    = "Avg SI Score")
surf_vars <- names(surf_varlist)
surf_labels <- unname(unlist(surf_varlist))

## Pairwise analyses ----
dese_vars <- c("avg_scaled_score_MATH",
               "avg_scaled_score_ELA",
               "sch_attendance_rate")
bivariate_pairs <- lapply(surf_vars, function(x) {
  lapply(dese_vars, function(y) c(x, y))
}) |> unlist(recursive = FALSE)

### Control variables for adjusted estimates in bivariate analyses
cntrl_vars <- c("pct_white", "pct_ell", "pct_lowinc", "pct_sped")


# Load main wrapper function and run analyses
source("reproduce_paper_functions.R")
run_paper_analyses(
  df = df,
  group_var = "in_surf",
  compare_vars = surf_v_nonsurf_vars,
  compare_labels = surf_v_nonsurf_labels,
  target_vars = surf_vars,
  target_labels = surf_labels,
  var_pairs = bivariate_pairs,
  background_vars = cntrl_vars,
  pca_vars = c(surf_vars, dese_vars),
  output_dir = "results_2024"
)
