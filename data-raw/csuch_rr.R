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
  select(im, condition, gender, outcome)

csuch_rr_free = csuch_rr0 %>%
  filter(form != 'Calibrated') %>%
  makeFreeFactories() %>%
  mutate(im = gsub('.(.)...(.).', '_\\1\\2', im)) %>%
  mutate(bingea = as.numeric(grepl('^.[789]', im) | grepl('schaemic', condition))) %>%
  mutate(wholly_attr = attributability == 'Wholly') %>%
  mutate(gender = ifelse(gender == 'Male', 'm', 'w'))

csuch_rr_free$risk = map(csuch_rr_free$ext_risk, ~as.numeric(.x(1:250)) - 1)
csuch_rr_base = csuch_rr_free %>%
  filter(bingea == 0 & !wholly_attr) %>%
  select(im, gender, outcome, rr_fd, risk)
csuch_rr_base_scaled = csuch_rr_free %>%
  filter(bingea == 0 & wholly_attr) %>%
  select(im, gender, outcome, risk)

csuch_rr_free$binge_risk = pmap(
  list(.x = csuch_rr_free$bingef, .y = csuch_rr_free$risk, .z = csuch_rr_free$bingea),
  function(.x, .y, .z) {
    pmax(.x * (.y + 1), .z) - 1
  }
)
csuch_rr_binge = csuch_rr_free %>%
  filter(bingea == 1 & !wholly_attr) %>%
  select(im, gender, outcome, rr_fd, risk, binge_risk)
csuch_rr_binge_scaled = csuch_rr_free %>%
  filter(bingea == 1 & wholly_attr) %>%
  select(im, gender, outcome, risk, binge_risk)

csuch_rr = list(
  base = csuch_rr_base,
  binge = csuch_rr_binge,
  base_scaled = csuch_rr_base_scaled,
  binge_scaled = csuch_rr_binge_scaled,
  calibrated = csuch_rr_cal,
  im = c(
    csuch_rr_base$im,
    csuch_rr_binge$im,
    csuch_rr_base_scaled$im,
    csuch_rr_binge_scaled$im,
    csuch_rr_cal$im
  ) %>% unique() %>% sort()
)


# csuch_rr = csuch_rr0 %>%
#   select(im, gender, outcome, bingea, bingef, wholly_attr, rr_fd, risk, binge_risk)

#
# csuch_conditions = csuch_rr0 %>%
#   select(condition, im) %>%
#   unique()
#
# write_csv(csuch_conditions, file.path('data-full', 'csuch_conditions.csv'))
#
#
# str(csuch_rr0$risk[[40]])
#
# plot(1:250, csuch_rr$risk[[15]])
# plot(1:250, csuch_rr$binge_risk[[15]])


usethis::use_data(csuch_rr, overwrite = TRUE)
