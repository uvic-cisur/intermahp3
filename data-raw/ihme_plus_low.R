## code to prepare `ihme_plus_low` dataset goes here

library(tidyverse)
library(intermahp3)

ihme_plus_low_rr = ihme_low_rr %>%
  mutate(risk = map(risk, ~pmax(.x, 0)))

usethis::use_data(ihme_plus_low_rr, overwrite = T)

