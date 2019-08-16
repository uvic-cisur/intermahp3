## code to prepare `imp` dataset goes here

imp = list(
  gamma_constant = list( # Means, Kehoe et al. (2012)
    "w" = 1.258,
    "m" = 1.171
  ),
  gamma_stderror = list( # Bounds on 95% CI, Kehoe et al. (2012)
    "w" = (1.293 - 1.223) / 2 / 1.96,
    "m" = (1.197 - 1.144) / 2 / 1.96
  ),
  pc_vars = c(
    "region",
    "year",
    "gender",
    "age_group",
    "population",
    "pcc_litres_year",
    "correction_factor",
    "relative_consumption",
    "p_la",
    "p_fd",
    "p_cd",
    "p_bd"
  ),
  mm_vars = c(
    "im",
    "region",
    "year",
    "gender",
    "age_group",
    "outcome",
    "count"
  )
)

usethis::use_data(imp, overwrite = TRUE)
