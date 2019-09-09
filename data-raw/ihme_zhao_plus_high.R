## code to prepare `ihme_zhao_plus_high` dataset goes here
library(tidyverse)
library(intermahp3)

ihme_zhao_plus_high_rr = ihme_zhao_high_rr %>%
  mutate(risk = map(risk, ~pmax(.x, 0)))

usethis::use_data(ihme_zhao_plus_high_rr, overwrite = T)

