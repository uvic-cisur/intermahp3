## code to prepare `csuch_rr` dataset goes here
library(tidyverse)
library(intermahpr)

csuch_rr0 = intermahpr_sample_rr %>%
  filter(!grepl('R', im)) %>%
  mutate(im = gsub('Z', '2', im)) %>%
  mutate(condition = gsub(' - Zhao', '', condition)) %>%
  prepareRR(ext = T)

csuch_rr_cal = csuch_rr0 %>%
  filter(form == 'Calibrated') %>%
  mutate(im = gsub('.(.)...(.).', '_\\1\\2', im)) %>%
  mutate(gender = ifelse(gender == 'Male', 'm', 'w')) %>%
  select(im, gender, outcome)

csuch_rr_free = csuch_rr0 %>%
  filter(form != 'Calibrated') %>%
  makeFreeFactories() %>%
  mutate(im = gsub('.(.)...(.).', '_\\1\\2', im)) %>%
  mutate(bingea = as.numeric(grepl('^.[789]', im) | grepl('schaemic', condition))) %>%
  mutate(wholly_attr = attributability == 'Wholly') %>%
  mutate(gender = ifelse(gender == 'Male', 'm', 'w')) %>%
  mutate(r_fd = rr_fd - 1)

csuch_rr_free$risk = map(csuch_rr_free$ext_risk, ~as.numeric(.x(1:250)) - 1)


csuch_rr_free$binge_risk = pmap(
  list(.x = csuch_rr_free$bingef, .y = csuch_rr_free$risk, .z = csuch_rr_free$bingea),
  function(.x, .y, .z) {
    pmax(.x * (.y + 1), .z) - 1
  }
)

csuch_rr = bind_rows(csuch_rr_free, csuch_rr_cal) %>%
  select(im, gender, outcome, bingea, bingef, wholly_attr, r_fd, risk, binge_risk)


usethis::use_data(csuch_rr, overwrite = TRUE)
