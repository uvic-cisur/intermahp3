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
  ## In AUS-CAN project, we use IHME methods, disregarding binge effects.
  ## This means that change in consumption is implemented without need to
  ## renormalize the proportion of bingers above threshold
  print(.rr_choice)
  print(pc0$PCC_litres_year)
  pc1 = pc0
  if(grepl('low', .rr_choice)) {pc1 = mutate(pc1, PCC_litres_year = PCC_litres_year * 0.9)}
  if(grepl('high', .rr_choice)) {pc1 = mutate(pc1, PCC_litres_year = PCC_litres_year * 1.1)}
  print(pc1$PCC_litres_year)

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
  select(rr_choice, im, gender, outcome, region, age_group, af = af_entire_1.0000)

ac_af_wide = ac_af_only %>%
  spread(rr_choice, af)

write_csv(ac_af_wide, file.path('data-full', 'AUS-CAN-preliminary-af.csv'))


aus_mort0 = read_csv(file.path('U:', 'SamChurchill', 'From Adam to Sam', 'Australia', 'mort-2015-aus.csv'))
can_mort0 = read_csv(file.path('U:', 'SamChurchill', 'From Adam to Sam', 'Australia', 'mort-2015-can.csv'))
mort0 = bind_rows(aus_mort0, can_mort0) %>%
  mutate(ihmec = gsub('tract ', '',IHME_Condition)) %>%
  mutate(ihmec = gsub('Ischa', 'Isch', ihmec))

names(mort0) <- tolower(names(mort0))

mort1 = select(mort0, region, gender, age_group, ihmec, count)

mort100 = mort1 %>% filter(grepl('[aA]lcohol', ihmec)) %>%
  group_by(gender, region, age_group, ihmec) %>%
  summarise_all(sum) %>%
  ungroup() %>%
  rename(condition = ihmec)

rr_choices = tibble(
  rr_choice = c(
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
    'ihme_zhao_plus_high'
  )
)

mort_choices_100 = crossing(mort100, rr_choices) %>%
  mutate(count_dup = count) %>%
  spread(rr_choice, count_dup)


mort2 = mort1 %>% filter(!grepl('[aA]lcohol', ihmec) & !grepl('match', ihmec)) %>%
  group_by(gender, region, age_group, ihmec) %>%
  summarise_all(sum) %>%
  ungroup() %>%
  rename(condition = ihmec)

cdict = read_csv(file.path('data-full', 'ihme_conditions.csv'))

af1 = ac_af_only %>% left_join(cdict) %>% filter(grepl('Mort', outcome)) %>%
  mutate(gender = ifelse(gender == 'm', 'Male', 'Female'))

combine_all = full_join(af1, mort2) %>%
  mutate(acount = af * count) %>%
  select(-af, -im, -outcome) %>%
  spread(rr_choice, acount) %>%
  bind_rows(mort_choices_100)

write_csv(combine_all, file.path('data-full', 'CAN-AUS-project-mortality.csv'))
