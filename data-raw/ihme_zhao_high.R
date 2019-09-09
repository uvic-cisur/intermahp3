## code to prepare `ihme_zhao_high` dataset goes here

library(tidyverse)
library(intermahpr)
library(intermahp3)

zhao_high = intermahpr_sample_rr %>%
  filter(grepl('Z', im)) %>%
  mutate(im = gsub('Z', '2', im)) %>%
  mutate(condition = gsub(' - Zhao', '', condition)) %>%
  mutate(b6 = b6 + 1.96 * .000962) %>%
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

ihme_high = ihme_high_rr %>% filter(im != '_52' | gender == 'w')

ihme_zhao_high_rr = bind_rows(ihme_high, zhao_high)

usethis::use_data(ihme_zhao_high_rr, overwrite = T)
