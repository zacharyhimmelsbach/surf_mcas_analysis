library(dplyr)
library(gtsummary)

# Load analytic data for focal year
df <- targets::tar_read(analysis_df) |> dplyr::filter(year_spring == 2024)


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


######################

# Bivariate Analyses ----


######################

# Multivariate Analyses ----


######################
