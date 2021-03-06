library(tidyverse)
library(intermahp3)
library(ggplot2)

thing = mahp$new()
thing$init_sk()
thing$add_group('myname', list(m = 1, f = 2))
thing$add_scenario(1)
thing$add_scenario(2)
thing$sk

thing = mahp$new()
pc_checker = read_rds(file.path('U:SamChurchill', 'data', 'intermahpr_sample_pc.rds'))
pc_checker = mutate(pc_checker, extra_var = NA) %>% select(-correction_factor)
thing$add_pc(pc_checker)
pc_fixed = mutate(pc_checker, gender = ifelse(gender == 'Male', 'm', 'w'), correction_factor = 0.8) %>% select(-extra_var)
thing$add_pc(pc_fixed)
thing$pc

{
  thing = mahp$new()
  thing$add_pc(pc_fixed)
  thing$set_bb(list('w' = 50, 'm' = 60))
  thing$set_ub(150.5)
  thing$update_pc()
  View(thing$pc)
}

{
  thing = mahp$new()
  thing$choose_rr('csuch')
  pc_checker = read_rds(file.path('U:SamChurchill', 'data', 'intermahpr_sample_pc.rds'))
  pc_fixed = mutate(pc_checker, gender = ifelse(gender == 'Male', 'm', 'w'), correction_factor = 0.8)
  thing$add_pc(pc_fixed)
  thing$set_bb(list('w' = 50, 'm' = 60))
  thing$set_scc(list('w' = .66, 'm' = .33))
  thing$set_ub(150.5)
  # thing$update_pc()
  thing$init_paf()
  # View(thing$sk)
}

extra_thing = thing$paf %>% select(region, year, gender, age_group, im, outcome, af_entire_1)
write_csv(extra_thing, file.path('data-full', 'first-paff.csv'))


v2mort <- read_csv("~/New folder/InterMAHP Base Mortality.csv")
v2mort2 = v2mort %>%  mutate(gender = ifelse(gender == 'Male', 'm', 'w')) %>%
  mutate(im = gsub('.(.)...(.).', '_\\1\\2', im)) %>%
  select(region, year, gender, age_group, im, af_entire_2 = `AAF: Entire Population`)

v3mort = extra_thing %>% filter(outcome == "Mortality") %>% select(-outcome)

v5mort = left_join(v3mort, v2mort2) %>%
  filter(!(im %in% paste0("_", c(44, 46, 47, 85, 92)))) %>%
  mutate(diff = af_entire_1 - af_entire_2) %>%
  mutate(percent_diff = diff / af_entire_2)

hist(v5mort$percent_diff, breaks = (-40:40)/10)
(-40:40)/10


v5summary = v5mort %>%
  mutate(abs_diff = abs(diff)) %>%
  group_by(im) %>%
  summarise(mean_diff = mean(abs_diff), max_diff = max(abs_diff), min_diff = min(abs_diff))

v5istroke = v5mort %>% filter(im == '_56')

ggplot(v5summary, aes(x = im)) +
  geom_col(aes(y = max_diff)) +
  geom_col(aes(y = mean_diff), fill = 'red1') +
  geom_col(aes(y = min_diff), fill = 'cyan1')


{
  thing = mahp$new()
  thing$choose_rr('ihme')
  pc_checker = read_rds(file.path('U:SamChurchill', 'data', 'intermahpr_sample_pc.rds'))
  pc_fixed = mutate(pc_checker, gender = ifelse(gender == 'Male', 'm', 'w'), correction_factor = 0.8)
  thing$add_pc(pc_fixed)
  thing$set_bb(list('w' = 50, 'm' = 60))
  thing$set_scc(list('w' = .66, 'm' = .33))
  thing$set_ub(150.5)
  # thing$update_pc()
  thing$init_paf()
  # View(thing$sk)
}


{
  thing = mahp$new()
  thing$choose_rr('ihme')
  pc_checker = readr::read_rds(file.path('U:SamChurchill', 'data', 'intermahpr_sample_pc.rds'))
  pc_fixed = mutate(pc_checker, gender = ifelse(gender == 'Male', 'm', 'w'), correction_factor = 0.8)
  thing$add_pc(pc_fixed)
  thing$set_bb(list('w' = 50, 'm' = 60))
  thing$set_scc(list('w' = .66, 'm' = .33))
  thing$set_ub(150.5)
  # thing$update_pc()
  what = thing$make_gamma()
  # View(thing$sk)
}

{
  thing = mahp$new()
  thing$choose_rr('csuch')
  pc_checker = readr::read_rds(file.path('U:SamChurchill', 'data', 'intermahpr_sample_pc.rds'))
  pc_fixed = dplyr::mutate(pc_checker, gender = ifelse(gender == 'Male', 'm', 'w'), correction_factor = 0.8)
  thing$add_pc(pc_fixed)
  thing$set_bb(list('w' = 50, 'm' = 60))
  thing$set_scc(list('w' = .66, 'm' = .33))
  thing$set_ub(150.5)
  # thing$update_pc()
  thing$init_fractions()
  # View(thing$sk)
}



{
  thing = mahp$new()
  thing$set_ub(150.5)


  thing$choose_rr('csuch')
  pc_checker = readr::read_rds(file.path('U:SamChurchill', 'data', 'intermahpr_sample_pc.rds'))
  pc_fixed = dplyr::mutate(pc_checker, gender = ifelse(gender == 'Male', 'm', 'w'), correction_factor = 0.8)
  thing$add_pc(pc_fixed)
  thing$set_bb(list('w' = 50, 'm' = 60))
  thing$set_scc(list('w' = .66, 'm' = .33))
  thing$set_ub(150.5)
  # thing$update_pc()
  thing$init_fractions()
  thing$add_group('middle', list(w = c(20, 30), m = c(25, 40)))
  thing$add_scenario(0.95)
}



{
  thing = mahp$new()
  thing$set_ext('linear')
  thing$set_ub(250)
  thing$choose_rr('ihme')

  pc_checker = readr::read_rds(file.path('U:SamChurchill', 'data', 'intermahpr_sample_pc.rds'))
  pc_fixed = dplyr::mutate(pc_checker, gender = ifelse(gender == 'Male', 'm', 'w'), correction_factor = 0.8)
  broken = readr::read_rds(file.path('U:SamChurchill', 'data', 'intermahpr_sample_rr.rds'))
  thing$add_pc(broken)
  thing$add_pc(pc_fixed)

  thing$set_bb(list('w' = 50, 'm' = 60))
  thing$set_scc(list('w' = .66, 'm' = .33))
  thing$set_ext('linear')


  # thing$def_group('middle 43t89 &*%', list(w = c(20, 29, 39), m = c(39, 25)))
  thing$def_group('low', list(w = c(0, 19), m = c(0, 24)))
  thing$def_scenario(0.95)
  thing$init_fractions()

  thing$get_afs()
  thing$get_long_afs()

  # thing$def_group('hi', list(w = c(30, 250), m = c(40, 250)))
  # thing$def_group('sanity', list(w = c(0, 250), m = c(0, 250)))
  # thing$def_scenario(1.07)
  # thing$cmp_groups()
  # thing$cmp_scenarios()
  # thing$cmp_groups()
  # thing$cmp_scenarios()


  # thing$set_mcn(5)
  # thing$make_ue()
  # newthing = thing$sample_self()


  # thing$set_pc_sample_vars(c(imp$pc_vars, 'gamma_c'))
  # thing$add_pc(thing$sample_pc())
}


what_af <- NULL
for(i in 1:10) {
  thing = mahp$new()
  thing$set_ext('capped')
  thing$set_ub(150)
  thing$choose_rr('ihme')

  pc_checker = readr::read_rds(file.path('U:SamChurchill', 'data', 'intermahpr_sample_pc.rds'))
  pc_fixed = dplyr::mutate(pc_checker, gender = ifelse(gender == 'Male', 'm', 'w'), correction_factor = 0.8)
  thing$add_pc(pc_fixed)

  thing$set_bb(list('w' = 50, 'm' = 60))
  thing$set_scc(list('w' = .66, 'm' = .33))
  thing$set_ext('capped')

  thing$init_fractions()
  what_af = bind_rows(what_af, thing$get_afs() %>% mutate(run = paste0('run', i)))
  thing = NULL
}

formatted = what_af %>%
  select(-af_current_1.0000) %>%
  spread(run, af_entire_1.0000)



