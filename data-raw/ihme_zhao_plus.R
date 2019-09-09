## code to prepare `ihme_zhao_plus` dataset goes here

library(tidyverse)
library(intermahp3)

ihme_zhao_plus_rr = ihme_zhao_rr %>%
  mutate(risk = map(risk, ~pmax(.x, 0)))

usethis::use_data(ihme_zhao_plus_rr, overwrite = T)
