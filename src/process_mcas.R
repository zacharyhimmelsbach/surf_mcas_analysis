
batch_process_mcas <- function(mcas_filepaths) {
  do.call(rbind, lapply(mcas_filepaths, process_mcas))
}

process_mcas <- function(mcas_filepath) {
  df <- readxl::read_xlsx(mcas_filepath, skip = 1)
  # The year only exists in the file name
  year <- stringr::str_extract(fs::path_file(mcas_filepath), "202\\d")
  df$year_spring <- year

  # Keep only variables of interest
  df <- df |> dplyr::select(`School Name`,
                            `School Code`,
                            Subject,
                            `No. of Students Included`,
                            `Part. Rate %`,
                            `Avg. Scaled Score`,
                            SGP,
                            year_spring) |>
    dplyr::rename(school_name = `School Name`,
                  DESE.School.Code = `School Code`,
                  part_rate = `Part. Rate %`,
                  avg_scaled_score = `Avg. Scaled Score`,
                  mcas_num_students = `No. of Students Included`,
                  sgp = SGP) |>
    mutate(avg_scaled_score = as.numeric(avg_scaled_score),
           sgp = as.numeric(sgp),
           mcas_num_students = as.numeric(mcas_num_students))

  # Get one row per school (raw is by school-subject)
  df <- df |> tidyr::pivot_wider(
      names_from = Subject,
      values_from = c(part_rate, avg_scaled_score, sgp, mcas_num_students)
    )

  # Merge on SURF school IDs
  surf_xw <- read.csv("data/surf_school_code_xw.csv") |>
    dplyr::select(DESE.School.Code, MGH.School.Code,
                  MGH.District.Code, MGH.School.Type)

  df <- base::merge(df, surf_xw, by = "DESE.School.Code", all = TRUE)

  return(df)
}
