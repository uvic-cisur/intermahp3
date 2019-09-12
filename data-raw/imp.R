## code to prepare `imp` dataset goes here

imp = list(
  gamma_c = list( # means, Kehoe et al. (2012)
    "w" = 1.258,
    "m" = 1.171
  ),
  gamma_cs = list( # Squared means, Kehoe et al. (2012)
    "w" = 1.258**2,
    "m" = 1.171**2
  ),
  gamma_shape = list( # As a shape variable for gamma distribution
    "w" = 1/1.258/1.258,
    "m" = 1/1.171/1.171
  ),
  gamma_stderror = list( # Bounds on 95% CI, Kehoe et al. (2012)
    "w" = (1.293 - 1.223) / 2 / 1.96,
    "m" = (1.197 - 1.144) / 2 / 1.96
  ),
  rr_key_vars = c(
    "gender",
    "im",
    "outcome"
  ),
  pc_key_vars = c(
    "region",
    "year",
    "gender",
    "age_group"
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
  ),
  rr_choices = c(
    'csuch',

    'ihme',
    'ihme_plus',

    'ihme_zhao',
    'ihme_zhao_plus',

    'ihme_rehm',
    'ihme_rehm_plus'
  )
)

usethis::use_data(imp, overwrite = TRUE)
