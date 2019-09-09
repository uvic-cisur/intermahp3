## code to prepare `ihme_zhao_low` dataset goes here

library(tidyverse)
library(intermahpr)
library(intermahp3)

zhao_low = intermahpr_sample_rr %>%
  filter(grepl('Z', im)) %>%
  mutate(im = gsub('Z', '2', im)) %>%
  mutate(condition = gsub(' - Zhao', '', condition)) %>%
  mutate(b6 = b6 - 1.96 * .000962) %>%
  prepareRR(ext = T) %>%
  makeFreeFactories() %>%
  mutate(im = gsub('.(.)...(.).', '_\\1\\2', im)) %>%
  mutate(bingea = 1) %>%
  mutate(wholly_attr = F) %>%
  mutate(gender = 'm') %>%
  mutate(r_fd = rr_fd - 1) %>%
  mutate(risk = map(ext_risk, ~as.numeric(.x(1:250)) - 1)) %>%
  mutate(bingea = 0) %>%
  mutate(binge_risk = NA) %>%
  mutate(bingef = NA) %>%
  select(im, gender, outcome, bingea, bingef, wholly_attr, r_fd, risk, binge_risk)

ihme_low = ihme_low_rr %>% filter(im != '_52' | gender == 'w')

ihme_zhao_low_rr = bind_rows(ihme_low, zhao_low)

usethis::use_data(ihme_zhao_low_rr, overwrite = T)
