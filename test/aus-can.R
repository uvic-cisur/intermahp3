library(tidyverse)
library(intermahp3)
library(ggplot2)

aus_pc0 = read_csv(file.path('U:', 'SamChurchill', 'From Adam to Sam', 'Australia', 'Australia prevcon 2013.csv'))
can_pc0 = read_csv(file.path('U:', 'SamChurchill', 'From Adam to Sam', 'Australia', 'Canada prevcon 2015.csv'))

pc0 = bind_rows(aus_pc0, can_pc0) %>%
  mutate(P_CD = P_CD / (P_CD + P_LA)) %>%
  mutate(P_LA = 1 - P_CD) %>%
  mutate(P_FD = 0) %>%
  mutate(Gender = ifelse(Gender == 'Male', 'm', 'w'))

ac_af <- NULL

for(.rr_choice in c(
  'ihme',
  'ihme_low',
  'ihme_high',

  'ihme_plus',
  'ihme_plus_low',
  'ihme_plus_high',

  'ihme_zhao',
  'ihme_zhao_low',
  'ihme_zhao_high',

  'ihme_zhao_plus',
  'ihme_zhao_plus_low',
  'ihme_zhao_plus_high')) {
  ac_mahp = mahp$new()
  ac_mahp$add_pc(pc0)
  ac_mahp$set_bb(list('w' = 50, 'm' = 60))
  ac_mahp$set_ub(150)
  ac_mahp$set_ext('')
  ac_mahp$set_scc(list('w' = 2/3, 'm' = 1/3))
  ac_mahp$choose_rr(.rr_choice)
  ac_mahp$init_fractions()
  # temp_af = ac_mahp$get_afs() %>% mutate(rr_choice = .rr_choice)
  ac_af = bind_rows(ac_af, ac_mahp$get_afs() %>% mutate(rr_choice = .rr_choice))
}


ac_af_only = ac_af %>%
  select(rr_choice, im, gender, outcome, region, year, age_group, af = af_entire_1.0000) %>%
  spread(rr_choice, af)


write_csv(ac_af_only, file.path('data-full', 'AUS-CAN-preliminary-af.csv'))
