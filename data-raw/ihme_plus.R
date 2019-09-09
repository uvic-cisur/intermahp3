## code to prepare `ihme_plus` dataset goes here

library(tidyverse)
library(intermahp3)

ihme_plus_rr = ihme_rr %>%
  mutate(risk = map(risk, ~pmax(.x, 0)))

usethis::use_data(ihme_plus_rr, overwrite = T)
