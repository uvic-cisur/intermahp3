## code to prepare `csuch_rr` dataset goes here
library(tidyverse)
library(intermahpr)

csuch_rr0 = intermahpr_sample_rr %>%
  filter(!grepl('R', im)) %>%
  filter(form != 'Calibrated') %>%
  mutate(im = gsub('Z', '2', im)) %>%
  mutate(condition = gsub(' - Zhao', '', condition)) %>%
  prepareRR(ext = T) %>%
  makeFreeFactories() %>%
  mutate(im = gsub('.(.)...(.).', '_\\1\\2', im)) %>%
  mutate(bingea = 1 - as.numeric(grepl('^.[789]', im) | grepl('schaemic', condition))) %>%
  mutate(wholly_attr = attributability == 'Wholly') %>%
  mutate(gender = ifelse(gender == 'Male', 'm', 'w'))


csuch_rr0$risk = map(csuch_rr0$ext_risk, ~as.numeric(.x(1:250)) - 1)
csuch_rr0$binge_risk = pmap(
  list(.x = csuch_rr0$bingef, .y = csuch_rr0$risk, .z = csuch_rr0$bingea),
  function(.x, .y, .z) {
    .x * pmax(.y, 0 - .z)
  }
)

csuch_rr = csuch_rr0 %>%
  select(im, gender, outcome, bingea, bingef, wholly_attr, rr_fd, risk, binge_risk)



#
# csuch_conditions = csuch_rr0 %>%
#   select(condition, im) %>%
#   unique()
#
# write_csv(csuch_conditions, file.path('data-full', 'csuch_conditions.csv'))
#

str(csuch_rr0$risk[[40]])

plot(1:250, csuch_rr$risk[[15]])
plot(1:250, csuch_rr$binge_risk[[15]])


usethis::use_data(csuch_rr, overwrite = TRUE)
