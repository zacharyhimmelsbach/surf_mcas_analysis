
batch_process_enrl <- function(enrl_filepaths) {
  do.call(rbind, lapply(enrl_filepaths, process_enrl))
}

process_enrl <- function(enrl_filepath) {
  df <- readxl::read_xlsx(enrl_filepath, skip = 1) |>
    dplyr::select(`School Name`, `School Code`, `9`, `10`, `11`, `12`)
  # The year only exists in the file name
  year <- stringr::str_extract(fs::path_file(enrl_filepath), "202\\d")
  df$year_spring <- as.numeric(year)

  # Keep only variables of interest
  df <- df |>
    dplyr::rename(school_name = `School Name`,
                  DESE.School.Code = `School Code`,
                  grade_9_enrl = `9`,
                  grade_10_enrl = `10`,
                  grade_11_enrl = `11`,
                  grade_12_enrl = `12`) |>
    dplyr::mutate(
      dplyr::across(starts_with("grade_"), as.numeric)
    ) |>
    dplyr::mutate(hs_total_enrl = grade_9_enrl + grade_10_enrl + grade_11_enrl + grade_12_enrl) |>
    dplyr::filter(hs_total_enrl > 0)

  # # Merge on SURF school IDs
  # surf_xw <- read.csv("data/surf_school_code_xw.csv") |>
  #   dplyr::select(DESE.School.Code, MGH.School.Code,
  #                 MGH.District.Code, MGH.School.Type)
  #
  # df <- base::merge(df, surf_xw, by = "DESE.School.Code", all = TRUE) |>
  #   dplyr::filter(DESE.School.Code != "")

  return(df)
}
