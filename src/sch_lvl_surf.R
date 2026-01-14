aggregate_surf <- function(df) {

  # Create combined nicotine use variable
  df <- df |> mutate(SBS.LGL.NIC.AnyPast31 =
                       SBS.INT.CGR.Past31.UseRating +
                       SBS.INT.SMK.Past31.UseRating +
                       SBS.INT.CIG.Past31.UseRating +
                       SBS.INT.VPS.Past31.UseRating > 4
                       )

  df |> group_by(SSS.INT.SchoolCode, SSS.INT.SurveyYear) |>
    summarize(
      N_SURF = n(),
      # Substance Use outcomes
      anyALC.Past31 = mean(SBS.INT.ALC.Past31.UseRating != 1, na.rm = TRUE),
      anyCNN.Past31 = mean(SBS.INT.CNN.Past31.UseRating != 1, na.rm = TRUE),
      anyNIC.Past31 = mean(SBS.LGL.NIC.AnyPast31, na.rm = TRUE),
      otherSUB.Lifetime = mean(SBS.LGC.OTH.Lifetime.Any, na.rm = TRUE),
      # Mental Health
      sch_avg_apss = mean(INV.DBL.APSS.Total, na.rm = TRUE),
      sch_avg_ers = mean(INV.INT.ERS.Total, na.rm = TRUE),
      sch_avg_phq4 = mean(INV.INT.PHQ4.Total, na.rm = TRUE),
      sch_avg_si = mean(INV.INT.SI.Total, na.rm = TRUE)
    )
}
