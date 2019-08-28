## code to prepare `im_condition_dictionary` dataset goes here


library(tidyverse)
library(intermahpr)

csuch_causes = csuch_rr0 = intermahpr_sample_rr %>%
  filter(!grepl('R', im)) %>%
  mutate(im = gsub('Z', '2', im)) %>%
  mutate(condition = gsub(' - Zhao', '', condition)) %>%
  mutate(im = gsub('.(.)...(.).', '_\\1\\2', im)) %>%
  select(im, condition)

ihme_causes = read_csv(file.path('data-full', 'ihme_conditions.csv')) %>%
  filter(!(im %in% csuch_causes$im))


im_condition_dictionary = bind_rows(ihme_causes, csuch_causes) %>% unique()

usethis::use_data(im_condition_dictionary, overwrite = TRUE)
