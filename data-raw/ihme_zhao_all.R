## code to prepare `ihme_zhao_low` dataset goes here

library(tidyverse)
library(intermahpr)
library(intermahp3)

zhao_m = intermahpr_sample_rr %>%
  filter(grepl('Z', im)) %>%
  mutate(im = gsub('Z', '2', im)) %>%
  mutate(condition = gsub(' - Zhao', '', condition))

zhao_w = zhao_m %>%
  mutate(gender = 'Female') %>%
  mutate(b3 = -0.2449) %>%
  mutate(b6 = 0.03329)


zhao_all = bind_rows(zhao_m, zhao_w) %>%
  prepareRR(ext = T) %>%
  makeFreeFactories() %>%
  mutate(im = gsub('.(.)...(.).', '_\\1\\2', im)) %>%
  mutate(bingea = 1) %>%
  mutate(wholly_attr = F) %>%
  mutate(gender = ifelse(grepl('^M', gender), 'm', 'w')) %>%
  mutate(r_fd = 0) %>%
  mutate(risk = map(ext_risk, ~as.numeric(.x(1:150)) - 1)) %>%
  mutate(bingea = 0) %>%
  mutate(binge_risk = NA) %>%
  mutate(bingef = NA) %>%
  select(im, gender, outcome, bingea, bingef, wholly_attr, r_fd, risk, binge_risk)


ihme_noihd = ihme_rr %>% filter(im != '_52')

ihme_zhao_rr = bind_rows(ihme_noihd, zhao_all)

usethis::use_data(ihme_zhao_rr, overwrite = T)

ihme_zhao_plus_rr = ihme_zhao_rr %>%
  mutate(risk = map(risk, ~pmax(.x, 0)))

usethis::use_data(ihme_zhao_plus_rr, overwrite = T)
