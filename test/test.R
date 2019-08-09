library(intermahp3)

thing = mahp$new()
thing$init_sk()
thing$add_group('myname', list(m = 1, f = 2))
thing$add_scenario(1)
thing$add_scenario(2)
thing$sk
