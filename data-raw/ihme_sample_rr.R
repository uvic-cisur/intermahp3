## code to prepare `ihme_sample_rr` dataset goes here

library(tidyverse)

ihme_rr_in = read_csv(file.path('data-full', 'ihme_rr.csv')) %>% rename(gender = sex)
ihme_rr_both = ihme_rr_in %>% filter(gender == "Both") %>% select(-gender) %>% crossing(tibble(gender = c('Male', 'Female')))
ihme_rr_specific = ihme_rr_in %>% filter(gender != "Both")

ihme_rr0 = bind_rows(ihme_rr_both, ihme_rr_specific) %>%
  gather('key', 'value', mean_rr:upper_rr) %>%
  crossing(tibble(outcome = c('Morbidity', 'Mortality'))) %>%
  # select(-mean_rr, -lower_rr) %>% ## TEMPORARY, NOT CERTAIN OF UE METHODS YET
  spread(exposure_grams_per_day, value) %>%
  mutate(gender = ifelse(gender == 'Male', 'm', 'w')) %>%
  select(-`0`)

crushed = ihme_rr0[, 5:154] %>%
  as.matrix() %>%
  split(1:276)

ihme_rr1 = ihme_rr0[, -(5:154)]
ihme_rr1$light_risk = crushed
ihme_rr2 = ihme_rr1 %>%
  mutate(y1 = map_dbl(light_risk, ~.x[100])) %>%
  mutate(y2 = map_dbl(light_risk, ~.x[150]))

ihme_rr2$heavy_risk = map2(.x = ihme_rr2$y1, .y = ihme_rr2$y2, ~.y + 0.02*(.y-.x)*(1:100))

ihme_rr2 = ihme_rr2 %>%
  mutate(risk = map2(light_risk, heavy_risk, c)) %>%
  mutate(risk = map(risk, ~.x - 1))

ihme_causes = read_csv(file.path('data-full', 'ihme_conditions.csv'))

ihme_sample_rr = left_join(ihme_rr2, ihme_causes, by = c('cause_name' = 'condition')) %>%
  select(im, gender, outcome, key, risk) %>%
  mutate(bingea = 0, wholly_attr = FALSE, r_fd = 0, binge_risk = NA) %>%
  spread(key, risk) %>%
  mutate(
    sd_plus = map2(mean_rr, upper_rr, ~(.y - .x)/1.96),
    sd_minus = map2(mean_rr, lower_rr, ~(.x - .y)/1.96)
  ) %>%
  select(-lower_rr, -upper_rr)

usethis::use_data(ihme_sample_rr, overwrite = T)
