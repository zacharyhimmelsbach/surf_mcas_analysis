# Packages to load
my_packages <- c('targets', 'tarchetypes', 'dplyr')

# Load packages
lapply(my_packages, library, character.only=TRUE)

tar_source("src/")

tar_option_set(
  packages = c("dplyr", "camrprojects", "ggplot2")
)

# Targets ----
list(
  ## Raw Files ----
  tar_target(
    mcas_files,
    list.files("data/MCAS_school_level", full.names = TRUE),
    format = "file"
  ),

  tar_target(
    attendance_files,
    list.files("data/attendance_school_level", full.names = TRUE),
    format = "file"
  ),

  tar_target(
    enrollment_files,
    list.files("data/enrollments_dese", full.names = TRUE),
    format = "file"
  ),

  tar_target(
    race_gender_files,
    list.files("data/enrollments_race_gender_dese", full.names = TRUE),
    format = "file"
  ),

  tar_target(
    ell_lowinc_sped_files,
    list.files("data/ell_lowinc_sped_dese", full.names = TRUE),
    format = "file"
  ),

  ## Processed Files ----

  tar_target(
    school_mcas_df,
    batch_process_mcas(mcas_files)
  ),

  tar_target(
    school_enrl_df,
    batch_process_enrl(enrollment_files)
  ),

  tar_target(
    school_race_sex_df,
    batch_process_race_sex(race_gender_files)
  ),

  tar_target(
    school_subpops_df,
    batch_process_subpops(ell_lowinc_sped_files)
  ),

  tar_target(
    school_absnt_df,
    batch_process_attendance(attendance_files)
  ),

  tar_target(
    surf_data,
    read.csv(fs::path(config::get('dropbox'),
                      "SWA-Linked_data-Version_2510271238.csv"))
  ),

  tar_target(
    sch_lvl_surf,
    aggregate_surf(surf_data)
  ),

  ## Merged processed files ----
  tar_target(
    surf_mcas_df,
    merge_surf_mcas(sch_lvl_surf, school_mcas_df)
  ),

  tar_target(
    sch_demos_df,
    school_enrl_df |>
      left_join(
        school_race_sex_df,
        by = c("DESE.School.Code", "year_spring", "school_name")
      ) |>
      left_join(
        school_subpops_df,
        by = c("DESE.School.Code", "year_spring", "school_name")
      )
  ),

  tar_target(
    analysis_df,
    base::merge(surf_mcas_df, school_absnt_df |> select(DESE.School.Code, MGH.School.Code, year_spring, sch_attendance_rate),
                by = c("DESE.School.Code", "year_spring"),
                all.x = TRUE) |>
    base::merge(sch_demos_df,
                by = c("DESE.School.Code", "year_spring"),
                all = TRUE) |>
      dplyr::mutate(in_surf = ifelse(is.na(in_surf), FALSE, in_surf))
  )
)
