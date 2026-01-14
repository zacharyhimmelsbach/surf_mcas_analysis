merge_surf_mcas <- function(surf_df, mcas_df) {
  surf_df <- surf_df |>
    rename(year_spring = SSS.INT.SurveyYear,
           MGH.School.Code = SSS.INT.SchoolCode) |>
    mutate(in_surf = TRUE)

  base::merge(surf_df, mcas_df,
              by = c("MGH.School.Code", "year_spring"),
              all = TRUE) |>
    mutate(in_surf = ifelse(is.na(in_surf), FALSE, in_surf))
}

