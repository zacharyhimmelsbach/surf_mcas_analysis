
batch_process_attendance <- function(attendance_filepaths) {
  do.call(rbind, lapply(attendance_filepaths, process_attendance))
}

process_attendance <- function(attendance_filepath) {
  df <- readxl::read_xlsx(attendance_filepath, skip = 1)
  # The year only exists in the file name
  year <- stringr::str_extract(fs::path_file(attendance_filepath), "202\\d")
  df$year_spring <- year

  # Keep only variables of interest
  df <- df |> dplyr::select(`School Name`,
                            `School Code`,
                            `Attendance Rate`,
                            `Average # of Absences`,
                            `Absent 10 or more days`,
                            `Chronically Absent (10% or more)`,
                            `Unexcused > 9 days`,
                            year_spring) |>
    dplyr::rename(school_name = `School Name`,
                  DESE.School.Code = `School Code`,
                  sch_attendance_rate = `Attendance Rate`,
                  sch_avg_num_absnt = `Average # of Absences`,
                  sch_pct_absnt_10_plus = `Absent 10 or more days`,
                  sch_pct_chron_absnt = `Chronically Absent (10% or more)`,
                  sch_pct_unex_9_plus = `Unexcused > 9 days`) |>
    mutate(sch_attendance_rate = as.numeric(sch_attendance_rate))

  # Merge on SURF school IDs
  surf_xw <- read.csv("data/surf_school_code_xw.csv") |>
    dplyr::select(DESE.School.Code, MGH.School.Code,
                  MGH.District.Code, MGH.School.Type)

  df <- base::merge(df, surf_xw, by = "DESE.School.Code")

  return(df)
}
