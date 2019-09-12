## code to prepare `ihme_zhao_low` dataset goes here

library(tidyverse)
library(intermahpr)
library(intermahp3)

rehm = intermahpr_sample_rr %>%
  filter(grepl('(5...2|R)', im)) %>%
  mutate(im = gsub('R', '2', im)) %>%
  mutate(condition = gsub(' -.*', '', condition)) %>%
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

ihme_rehm_rr = bind_rows(ihme_noihd, rehm)

usethis::use_data(ihme_rehm_rr, overwrite = T)

ihme_rehm_plus_rr = ihme_rehm_rr %>%
  mutate(risk = map(risk, ~pmax(.x, 0)))

usethis::use_data(ihme_rehm_plus_rr, overwrite = T)
