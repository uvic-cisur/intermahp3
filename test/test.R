library(tidyverse)
library(intermahp3)

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
