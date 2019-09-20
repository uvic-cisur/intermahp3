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

rr_choices = rr_choices = tibble(
  rr_choice = c(
    'ihme',
    'ihme_plus',

    'ihme_zhao',
    'ihme_zhao_plus',

    'ihme_rehm',
    'ihme_rehm_plus')
)

ac_af <- NULL
# .rr_choice = 'ihme_rehm'
for(.rr_choice in rr_choices$rr_choice) {
  ac_mahp = mahp$new()
  ## In AUS-CAN project, we use IHME methods, disregarding binge effects.
  ## This means that change in consumption is implemented without need to
  ## renormalize the proportion of bingers above threshold
  print(.rr_choice)
  # print(pc0$PCC_litres_year)
  pc1 = pc0
  # if(grepl('low', .rr_choice)) {pc1 = mutate(pc1, PCC_litres_year = PCC_litres_year * 0.9)}
  # if(grepl('high', .rr_choice)) {pc1 = mutate(pc1, PCC_litres_year = PCC_litres_year * 1.1)}
  # print(pc1$PCC_litres_year)

  ac_mahp$add_pc(pc1)
  ac_mahp$set_bb(list('w' = 150, 'm' = 150))
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

totals = combine_all %>%
  group_by(region, gender) %>%
  summarise_if(is.numeric, sum, na.rm = T)

totals_noihd = combine_all %>%
  filter(condition != 'Ischemic heart disease') %>%
  group_by(region, gender) %>%
  summarise_if(is.numeric, sum, na.rm = T)

totals_just_is_diab = combine_all %>%
  filter(grepl('(Diab|Ischemic s)', condition)) %>%
  group_by(region, gender) %>%
  summarise_if(is.numeric, sum, na.rm = T)


write_csv(combine_all, na = '.', file.path('data-full', 'CAN-AUS-project-mortality.csv'))
write_csv(totals, na = '.', file.path('data-full', 'CAN-AUS-project-mortality-totals.csv'))

## Article tables
catd = read_csv(file.path('data-full', 'ihme_categories.csv'), col_types = 'cc')

T1 = combine_all %>%
  group_by(region, condition) %>%
  summarise_if(is.numeric, sum, na.rm = T) %>%
  left_join(cdict) %>%
  mutate(number = str_sub(im, 2, 2)) %>%
  left_join(catd) %>%
  arrange(category, condition, region)

T1w = T1 %>%
  ungroup() %>%
  mutate_if(is.numeric, function(.x) {round(.x, 1)})

write_csv(T1w, file.path('U:SamChurchill', 'From Adam to Sam', 'Australia', 'T1.csv'))

T1sub = T1 %>%
  ungroup %>%
  group_by(region, category) %>%
  summarise_if(is.numeric, sum)

T1subw = T1sub %>%
  ungroup() %>%
  mutate_if(is.numeric, function(.x) {round(.x, 1)})

write_csv(T1subw, file.path('U:SamChurchill', 'From Adam to Sam', 'Australia', 'T1sub.csv'))


T3g = combine_all %>%
  left_join(cdict) %>%
  mutate(number = str_sub(im, 2, 2)) %>%
  left_join(catd) %>%
  group_by(region, gender, category) %>%
  summarise_if(is.numeric, sum, na.rm = T)

T3c = T3g %>%
  ungroup() %>%
  group_by(region, category) %>%
  summarise_if(is.numeric, sum, na.rm = T) %>%
  mutate(gender = 'Combined')

T3 = bind_rows(T3g, T3c) %>%
  ungroup() %>%
  mutate_if(is.numeric, function(.x) {round(.x, 1)})

write_csv(T3, file.path('U:SamChurchill', 'From Adam to Sam', 'Australia', 'T3.csv'))

T3sg = combine_all %>%
  left_join(cdict) %>%
  mutate(number = str_sub(im, 2, 2)) %>%
  left_join(catd) %>%
  group_by(region, gender, condition) %>%
  summarise_if(is.numeric, sum, na.rm = T) %>%
  filter(grepl('c h', condition))

T3sc = T3sg %>%
  ungroup() %>%
  group_by(region, condition) %>%
  summarise_if(is.numeric, sum, na.rm = T) %>%
  mutate(gender = 'Combined')

T3s = bind_rows(T3sg, T3sc) %>%
  ungroup() %>%
  mutate_if(is.numeric, function(.x) {round(.x, 1)})


write_csv(T3s, file.path('U:SamChurchill', 'From Adam to Sam', 'Australia', 'T3ihd.csv'))

T3subg = combine_all %>%
  left_join(cdict) %>%
  mutate(number = str_sub(im, 2, 2)) %>%
  left_join(catd) %>%
  group_by(region, gender) %>%
  summarise_if(is.numeric, sum, na.rm = T)

T3subc = T3subg %>%
  ungroup() %>%
  group_by(region) %>%
  summarise_if(is.numeric, sum, na.rm = T) %>%
  mutate(gender = 'Combined')

T3sub = bind_rows(T3subg, T3subc) %>%
  ungroup() %>%
  mutate_if(is.numeric, function(.x) {round(.x, 1)})

write_csv(T3sub, file.path('U:SamChurchill', 'From Adam to Sam', 'Australia', 'T3sub.csv'))


## Composite curves
a.w.total = mort2 %>% filter(grepl('^F', gender) & grepl('^A', region)) %>% `$`(count) %>% sum()
c.w.total = mort2 %>% filter(grepl('^F', gender) & grepl('^C', region)) %>% `$`(count) %>% sum()
a.m.total = mort2 %>% filter(grepl('^M', gender) & grepl('^A', region)) %>% `$`(count) %>% sum()
c.m.total = mort2 %>% filter(grepl('^M', gender) & grepl('^C', region)) %>% `$`(count) %>% sum()
a.b.total = mort2 %>% filter(grepl('^A', region)) %>% `$`(count) %>% sum()
c.b.total = mort2 %>% filter(grepl('^C', region)) %>% `$`(count) %>% sum()

t.choice = paste0(rr_choices$rr_choice[[1]], '_rr')

mort3 = mort2 %>%
  group_by(gender, region, condition) %>%
  summarise_if(is.numeric, sum, na.rm = T)

t.eval = eval(sym(t.choice)) %>% unnest(risk) %>% mutate(x = rep(1:250, 23000/250)) %>%
  left_join(cdict) %>%
  filter(grepl('Mort', outcome)) %>%
  mutate(gender = ifelse(gender == 'm', 'Male', 'Female')) %>%
  left_join(mort3)

comp_master = bind_rows(
  lapply(
    rr_choices$rr_choice,
    function(.char) {
      .char %>%
        paste0('_rr') %>%
        sym() %>%
        eval() %>%
        mutate(rr_choice = .char)
      }
    )
  ) %>%
  mutate(risk = map(risk, ~.x[1:150])) %>%
  unnest(risk) %>%
  mutate(x = rep(1:150, 6*92)) %>%
  left_join(cdict) %>%
  filter(grepl('Mort', outcome)) %>%
  mutate(gender = ifelse(gender == 'm', 'Male', 'Female')) %>%
  left_join(mort3) %>%
  filter(!is.na(region))

gendered_comp = comp_master %>%
  group_by(region, gender, rr_choice, x) %>%
  summarise(y = sum(count * risk)/sum(count))

combined_comp = comp_master %>%
  group_by(region, rr_choice, x) %>%
  summarise(y = sum(count * risk)/sum(count)) %>%
  mutate(gender = 'Combined')

comp_final = bind_rows(gendered_comp, combined_comp) %>%
  unite('key', region, gender, rr_choice) %>%
  mutate(y = y + 1) %>%
  spread(key, y)

write_csv(comp_final, file.path('data-full', 'comp-curves.csv'))

gammas = ac_mahp$make_gamma() %>%
  unnest(base_gamma) %>%
  mutate(x = rep(1:150, 12)) %>%
  unite('key', region, gender, age_group) %>%
  select(key, x, base_gamma) %>%
  spread(key, base_gamma)

write_csv(gammas, file.path('data-full', 'pc-curves.csv'))

integrands = ac_mahp$af$base_paf %>%
  filter(outcome == 'Mortality', im == '_52') %>%
  select(region, gender, age_group, pf = integrand_1.0000) %>%
  unnest(pf) %>%
  mutate(x = rep(1:150, 1800/150)) %>%
  unite('key', region, gender, age_group) %>%
  spread(key, pf)


write_csv(integrands, file.path('data-full', 'pf-curves.csv'))
