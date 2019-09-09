## code to prepare `ihme_low` dataset goes here

library(tidyverse)

ihme_rr0 = read_csv(file.path('data-full', 'ihme_rr.csv')) %>%
  select(-mean_rr, -upper_rr) %>% ## TEMPORARY, NOT CERTAIN OF UE METHODS YET
  spread(exposure_grams_per_day, lower_rr) %>%
  rename(gender = sex) %>%
  mutate(gender = ifelse(gender == 'Male', 'm', 'w')) %>%
  crossing(tibble(outcome = c('Morbidity', 'Mortality'))) %>%
  select(-`0`)

crushed = ihme_rr0[, 3:152] %>%
  as.matrix() %>%
  split(1:54)

ihme_rr1 = ihme_rr0[, -(3:152)]
ihme_rr1$light_risk = crushed
ihme_rr2 = ihme_rr1 %>%
  mutate(y1 = map_dbl(light_risk, ~.x[100])) %>%
  mutate(y2 = map_dbl(light_risk, ~.x[150]))

ihme_rr2$heavy_risk = map2(.x = ihme_rr2$y1, .y = ihme_rr2$y2, ~.y + 0.02*(.y-.x)*(1:100))

ihme_rr2 = ihme_rr2 %>%
  mutate(risk = map2(light_risk, heavy_risk, c)) %>%
  mutate(risk = map(risk, ~.x - 1))

ihme_causes = read_csv(file.path('data-full', 'ihme_conditions.csv'))

ihme_low_rr = left_join(ihme_rr2, ihme_causes, by = c('cause_name' = 'condition')) %>%
  select(im, gender, outcome, risk) %>%
  mutate(bingea = 0, wholly_attr = FALSE, r_fd = 0, binge_risk = NA)

usethis::use_data(ihme_low_rr, overwrite = T)
