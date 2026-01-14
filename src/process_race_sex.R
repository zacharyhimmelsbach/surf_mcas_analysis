
batch_process_race_sex <- function(race_sex_filepaths) {
  do.call(rbind, lapply(race_sex_filepaths, process_race_sex))
}

process_race_sex <- function(race_sex_filepath) {
  df <- readxl::read_xlsx(race_sex_filepath, skip = 1)
  # The year only exists in the file name
  year <- stringr::str_extract(fs::path_file(race_sex_filepath), "202\\d")
  df$year_spring <- as.numeric(year)

  # Keep only variables of interest
  df <- df |>
    dplyr::rename(school_name = `School Name`,
                  DESE.School.Code = `School Code`,
                  pct_indian_alaskan = `American Indian or Alaska Native`,
                  pct_asian = Asian,
                  pct_black = `Black or African American`,
                  pct_hispanic = `Hispanic or Latino`,
                  pct_multiracial = `Multi-Race, Not Hispanic or Latino`,
                  pct_white = White,
                  pct_female = Female,
                  pct_male = Male,
                  pct_nonbinary = Nonbinary
                  ) |>
    dplyr::mutate(dplyr::across(starts_with("pct_"), as.numeric))

  # # Merge on SURF school IDs
  # surf_xw <- read.csv("data/surf_school_code_xw.csv") |>
  #   dplyr::select(DESE.School.Code, MGH.School.Code,
  #                 MGH.District.Code, MGH.School.Type)
  #
  # df <- base::merge(df, surf_xw, by = "DESE.School.Code", all = TRUE) |>
  #   dplyr::filter(DESE.School.Code != "")

  return(df)
}
