## code to prepare `ihme_zhao` dataset goes here

library(tidyverse)
library(intermahp3)

ihme = ihme_rr %>% filter(im != '_52' | gender == 'w')
zhao = csuch_rr %>% filter(im == '_52' & gender != 'w') %>%
  mutate(bingea = 0) %>%
  mutate(binge_risk = NA) %>%
  mutate(bingef = NA)

ihme_zhao_rr = bind_rows(ihme, zhao)

usethis::use_data(ihme_zhao_rr, overwrite = T)
