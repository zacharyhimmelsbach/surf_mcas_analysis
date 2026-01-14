
batch_process_subpops <- function(subpops_filepaths) {
  do.call(rbind, lapply(subpops_filepaths, process_subpops))
}

process_subpops <- function(subpops_filepath) {
  df <- readxl::read_xlsx(subpops_filepath, skip = 1) |>
    select(-c(`Students with Disabilities#...11`,
              `Students with Disabilities%...12`))
  # The year only exists in the file name
  year <- stringr::str_extract(fs::path_file(subpops_filepath), "202\\d")
  df$year_spring <- as.numeric(year)

  # Keep only variables of interest
  df <- df |>
    dplyr::rename(school_name = `School Name`,
                  DESE.School.Code = `School Code`,
                  n_high_needs = `High Needs#`,
                  pct_high_needs = `High Needs%`,
                  n_ell = `English Learners#`,
                  pct_ell = `English Learners%`,
                  n_flne = `First Language Not English#`,
                  pct_flne = `First Language Not English%`,
                  n_lowinc = `Low Income#`,
                  pct_lowinc = `Low Income%`,
                  n_sped = `Students with Disabilities#...13`,
                  pct_sped = `Students with Disabilities%...14`) |>
    dplyr::mutate(dplyr::across(starts_with("pct_"), as.numeric),
                  dplyr::across(starts_with("n_"), as.numeric))

  # # Merge on SURF school IDs
  # surf_xw <- read.csv("data/surf_school_code_xw.csv") |>
  #   dplyr::select(DESE.School.Code, MGH.School.Code,
  #                 MGH.District.Code, MGH.School.Type) |>
  #   dplyr::filter(DESE.School.Code != "")
  #
  # df <- base::merge(df, surf_xw, by = "DESE.School.Code", all = TRUE)

  return(df)
}
