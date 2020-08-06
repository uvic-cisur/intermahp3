## code to prepare `who_rr` dataset goes here
library(tidyverse)

## All real valued functions taken from GBD supplement or supplied R code as
## needed

## _24 is liver cancer ----
## p. 24
who_24_fn = function(x) {
  b1 = 0.005041

  ifelse(
    x < 0 | x > 150, 0,
    exp(b1 * x)
  )
}


## _52 is ischaemic heart disease ----
## p. 33
who_52_m_fn1 = function(x) {
  y1 = (x + 0.0099999997764826)/100
  y2 = (60 + 0.0099999997764826)/100

  b1 = 1.111874
  b2 = -0.4870068
  b3 = 1.550984
  b4 = 0.012

  ifelse(
    x < 0 | x > 150, 0,
    ifelse(
      x < 60,
      exp(b1 * ((b2 * sqrt(y1)) + (b3 * y1 * y1 * y1))),
      ifelse(
        x < 100,
        0.04571551 + exp(b1 * ((b2 * sqrt(y2)) + b3 * y2 * y2 * y2)),
        exp(b4 * (x - 100)) - 1 + 0.04571551
        +
          exp(b1 * ((b2 * sqrt(y2)) + (b3 * y2 * y2 * y2)))
      )
    )
  )
}

## p. 34
who_52_m_fn2 = function(x) {
  y1= (x + 0.0099999997764826)/100
  y2= (60 + 0.0099999997764826)/100

  b1 = 0.757104
  b2 = -0.4870068
  b3 = 1.550984
  b4 = 0.012

  ifelse(
    x < 0 | x > 150, 0,
    ifelse(
      x < 60,
      exp(b1 * ((b2 * sqrt(y1)) + (b3 * y1 * y1 * y1))),
      ifelse(
        x < 100,
        0.04571551 + exp(b1 * ((b2 * sqrt(y2)) + b3 * y2 * y2 * y2)),
        exp(b4 * (x - 100)) - 1 + 0.04571551 +
          exp(b1 * ((b2 * sqrt(y2)) + (b3 * y2 * y2 * y2)))
      )
    )
  )
}

## p. 35
who_52_m_fn3 = function(x) {
  y1= (x + 0.0099999997764826)/100
  y2= (30.3814 + 0.0099999997764826)/100

  b1 = 1.035623
  b2 = -0.4870068
  b3 = 1.550984
  b4 = 0.012

  ifelse(
    x < 0 | x > 150, 0,
    ifelse(
      x < 30.3814,
      exp(b1 * ((b2 * y1) + (b3 * y1 * log(y1)))),
      exp(b4 * (x - 30.3814)) - 1 +
        exp(b1 * ((b2 * y2) + (b3 * y2 * log(y2))))
    )
  )
}

## p. 36
who_52_w_fn1 = function(x) {
  y1= (x + 0.0099999997764826)/100
  y2= (30.3814 + 0.0099999997764826)/100

  b1 = 1.111874
  b2 = 1.832441
  b3 = 1.538557
  b4 = 0.01


  ifelse(
    x < 0 | x > 150, 0,
    ifelse(
      x < 30.3814,
      exp(b1 * ((b2 * y1) + (b3 * y1 * log(y1)))),
      exp(b4 * (x - 30.3814)) - 1 +
        exp(b1 * ((b2 * y2) + (b3 * y2 * log(y2))))
    )
  )
}

## p. 37
who_52_w_fn2 = function(x) {
  y1= (x + 0.0099999997764826)/100
  y2= (30.3814 + 0.0099999997764826)/100

  b1 = 1.035623
  b2 = 1.832441
  b3 = 1.538557
  b4 = 0.01


  ifelse(
    x < 0 | x > 150, 0,
    ifelse(
      x < 30.3814,
      exp(b1 * ((b2 * y1) + (b3 * y1 * log(y1)))),
      exp(b4 * (x - 30.3814)) - 1 +
        exp(b1 * ((b2 * y2) + (b3 * y2 * log(y2))))
    )
  )
}

## p. 38
who_52_w_fn3 = function(x) {
  y1= (x + 0.0099999997764826)/100
  y2= (30.3814 + 0.0099999997764826)/100

  b1 = 0.757104
  b2 = 1.832441
  b3 = 1.538557
  b4 = 0.01


  ifelse(
    x < 0 | x > 150, 0,
    ifelse(
      x < 30.3814,
      exp(b1 * ((b2 * y1) + (b3 * y1 * log(y1)))),
      exp(b4 * (x - 30.3814)) - 1 +
        exp(b1 * ((b2 * y2) + (b3 * y2 * log(y2))))
    )
  )
}

## _56 is ischaemic stroke ----
## p. 39
who_56_m_fn1 = function(x) {
  y1 = (1 + 0.0028572082519531)/100
  y2 = (x + 0.0028572082519531)/100

  b1 = 1.111874
  b2 = 0.4030081
  b3 = 0.3877538

  ifelse(
    x < 0 | x > 150, 0,
    ifelse(
      x < 1,
      1 - x * (1 - exp(b1 * (b2 * sqrt(y1) + b3 * sqrt(y1) * log(y1)))),
      exp(b1 * (b2 * sqrt(y2) + b3 * sqrt(y2) * log(y2)))
    )
  )
}

## p. 40
who_56_m_fn2 = function(x) {
  y1 = (1 + 0.0028572082519531)/100
  y2 = (x + 0.0028572082519531)/100

  b1 = 1.035623
  b2 = 0.4030081
  b3 = 0.3877538

  ifelse(
    x < 0 | x > 150, 0,
    ifelse(
      x < 1,
      1 - x * (1 - exp(b1 * (b2 * sqrt(y1) + b3 * sqrt(y1) * log(y1)))),
      exp(b1 * (b2 * sqrt(y2) + b3 * sqrt(y2) * log(y2)))
    )
  )
}

## p. 41
who_56_m_fn3 = function(x) {
  y1 = (1 + 0.0028572082519531)/100
  y2 = (x + 0.0028572082519531)/100

  b1 = 0.757104
  b2 = 0.4030081
  b3 = 0.3877538

  ifelse(
    x < 0 | x > 150, 0,
    ifelse(
      x < 1,
      1 - x * (1 - exp(b1 * (b2 * sqrt(y1) + b3 * sqrt(y1) * log(y1)))),
      exp(b1 * (b2 * sqrt(y2) + b3 * sqrt(y2) * log(y2)))
    )
  )
}

## p. 42
who_56_w_fn1 = function(x) {
  y1 = (1 + 0.0028572082519531)/100
  y2 = (x + 0.0028572082519531)/100

  b1 = 1.111874
  b2 = -2.48768
  b3 = 3.7087240


  ifelse(
    x < 0 | x > 150, 0,
    ifelse(
      x < 1,
      1 - x * (1 - exp(b1 * (b2 * sqrt(y1) + b3 * sqrt(y1)))),
      exp(b1 * (b2 * sqrt(y2) + b3 * y2))
    )
  )
}

## p. 43
who_56_w_fn2 = function(x) {
  y1 = (1 + 0.0028572082519531)/100
  y2 = (x + 0.0028572082519531)/100

  b1 = 1.035623
  b2 = -2.48768
  b3 = 3.7087240


  ifelse(
    x < 0 | x > 150, 0,
    ifelse(
      x < 1,
      1 - x * (1 - exp(b1 * ((b2 * sqrt(y1)) + (b3 * sqrt(y1) * log(y1))))),
      exp(b1 * ((b2 * sqrt(y2)) + (b3 * y2)))
    )
  )
}

## p. 44
who_56_w_fn3 = function(x) {
  y1 = (1 + 0.0028572082519531)/100
  y2 = (x + 0.0028572082519531)/100

  b1 = 0.757104
  b2 = -2.48768
  b3 = 3.7087240


  ifelse(
    x < 0 | x > 150, 0,
    ifelse(
      x < 1,
      1 - x * (1 - exp(b1 * ((b2 * sqrt(y1)) + (b3 * sqrt(y1) * log(y1))))),
      exp(b1 * ((b2 * sqrt(y2)) + (b3 * y2)))
    )
  )
}

## _62 is liver cirrhosis ----
## p. 47
who_62_m_fn = function(x) {
  y1 = (x+0.1699981689453125)/100

  b1 = 1.687111
  b2 = 1.106413

  ifelse(
    x < 0 | x > 150, 0,
    ifelse(
      x < 1,
      1 - x * exp((b1 + b2) * y1),
      exp((b1 + b2) * y1)
    )
  )
}

## p. 48
who_62_w_fn = function(x) {
  y1 = (x + 0.1699981689453125)/100

  b1 = 2.351821
  b2 = 0.9002139


  ifelse(
    x < 0 | x > 150, 0,
    ifelse(
      x < 1,
      1 - x * exp((b1 + b2) * sqrt(y1)),
      exp((b1 + b2) * sqrt(y1))
    )
  )
}

## _63 is acute pancreatitis, _64 is chronic pancreatitis. ----
## No distinction is made in WHO
## p. 49
who_634_m_fn = function(x) {
  b1 = 0.0173451

  ifelse(
    x < 0 | x > 150, 0,
    exp(b1 * x)
  )
}

## _71, is motor vehicle collisions ----
## p. 51
who_71_n_fn = function(x) {
  b1 = 0.00299550897979837

  ifelse(
    x < 0 | x > 150, 0,
    exp(b1 * x)
  )
}

who_71_b_fn = function(x) {
  b1 = 0.00299550897979837
  b2 = 0.959350221334602

  ifelse(
    x < 0 | x > 150, 0,
    exp(b1 * x + b2)
  )
}

## _8x, is unintentional injuries ----
## p. 52
who_8x_n_fn = function(x) {
  b1 = 0.00199800266267306

  ifelse(
    x < 0 | x > 150, 0,
    exp(b1 * x)
  )
}

who_8x_b_fn = function(x) {
  b1 = 0.00199800266267306
  b2 = 0.647103242058538

  ifelse(
    x < 0 | x > 150, 0,
    exp(b1 * x + b2)
  )
}

## _9x, is unintentional injuries ----
## p. 53
who_9x_n_fn = function(x) {
  b1 = 0.00199800266267306

  ifelse(
    x < 0 | x > 150, 0,
    exp(b1 * x)
  )
}

who_9x_b_fn = function(x) {
  b1 = 0.00199800266267306
  b2 = 0.647103242058538

  ifelse(
    x < 0 | x > 150, 0,
    exp(b1 * x + b2)
  )
}

## general functions for evaluation and linear extension ----
evaluate_rfn = function(rfn) {
  y1 = rfn(100)
  y2 = rfn(150)
  m = (y2 - y1) / 50

  fn_head = rfn(1:150)
  fn_tail = y2 + m * 1:100

  c(fn_head, fn_tail) - 1
}


## Building these conditions from scratch
who_24 = tibble(
  im = '_24',
  gender = c('m', 'w'),
  ## crossing outcome = m/m
  bingea = 0,
  bingef = 1,
  wholly_attr = FALSE,
  r_fd = c(1.23, 1.68),
  risk = list(evaluate_rfn(who_24_fn)),
  binge_risk = list(evaluate_rfn(who_24_fn))
) %>%
  crossing(tibble(who_age_group = c('15-34', '35-64', '65+')))

who_52_m = tibble(
  im = '_52',
  gender = 'm',
  who_age_group = c('15-34', '35-64', '65+'),
  ## crossing outcome = m/m
  bingea = 1,
  bingef = 1,
  wholly_attr = FALSE,
  r_fd = .25,
  risk = list(
    evaluate_rfn(who_52_m_fn1),
    evaluate_rfn(who_52_m_fn2),
    evaluate_rfn(who_52_m_fn3)),
  binge_risk = list(
    pmax(0, evaluate_rfn(who_52_m_fn1)),
    pmax(0, evaluate_rfn(who_52_m_fn2)),
    pmax(0, evaluate_rfn(who_52_m_fn3)))
)

who_52_w = tibble(
  im = '_52',
  gender = 'w',
  who_age_group = c('15-34', '35-64', '65+'),
  ## crossing outcome = m/m
  bingea = 1,
  bingef = 1,
  wholly_attr = FALSE,
  r_fd = .54,
  risk = list(
    evaluate_rfn(who_52_w_fn1),
    evaluate_rfn(who_52_w_fn2),
    evaluate_rfn(who_52_w_fn3)),
  binge_risk = list(
    pmax(0, evaluate_rfn(who_52_w_fn1)),
    pmax(0, evaluate_rfn(who_52_w_fn2)),
    pmax(0, evaluate_rfn(who_52_w_fn3)))
)

who_56_m = tibble(
  im = '_56',
  gender = 'm',
  who_age_group = c('15-34', '35-64', '65+'),
  ## crossing outcome = m/m
  bingea = 1,
  bingef = 1,
  wholly_attr = FALSE,
  r_fd = 0,
  risk = list(
    evaluate_rfn(who_56_m_fn1),
    evaluate_rfn(who_56_m_fn2),
    evaluate_rfn(who_56_m_fn3)),
  binge_risk = list(
    pmax(0, evaluate_rfn(who_56_m_fn1)),
    pmax(0, evaluate_rfn(who_56_m_fn2)),
    pmax(0, evaluate_rfn(who_56_m_fn3)))
)

who_56_w = tibble(
  im = '_56',
  gender = 'w',
  who_age_group = c('15-34', '35-64', '65+'),
  ## crossing outcome = m/m
  bingea = 1,
  bingef = 1,
  wholly_attr = FALSE,
  r_fd = 0,
  risk = list(
    evaluate_rfn(who_56_w_fn1),
    evaluate_rfn(who_56_w_fn2),
    evaluate_rfn(who_56_w_fn3)),
  binge_risk = list(
    pmax(0, evaluate_rfn(who_56_w_fn1)),
    pmax(0, evaluate_rfn(who_56_w_fn2)),
    pmax(0, evaluate_rfn(who_56_w_fn3)))
)

who_634_m = tibble(
  im = c('_63', '_64'),
  gender = 'm',
  ## crossing outcome = m/m
  bingea = 0,
  bingef = 1,
  wholly_attr = FALSE,
  r_fd = 1.2,
  risk = list(evaluate_rfn(who_634_m_fn)),
  binge_risk = list(evaluate_rfn(who_634_m_fn))
) %>%
  crossing(tibble(who_age_group = c('15-34', '35-64', '65+')))

who_71 = tibble(
  im = '_71',
  gender = c('m', 'w'),
  # crossing outcome = m/m
  bingea = 1,
  bingef = 1,
  wholly_attr = FALSE,
  r_fd = 0,
  risk = list(evaluate_rfn(who_71_n_fn)),
  binge_risk = list(evaluate_rfn(who_71_b_fn))
) %>%
  crossing(tibble(who_age_group = c('15-34', '35-64', '65+')))

who_8x = tibble(
  # crossing im = _8.12346
  gender = c('m', 'w'),
  # crossing outcome = m/m
  bingea = 1,
  bingef = 1,
  wholly_attr = FALSE,
  r_fd = 0,
  risk = list(evaluate_rfn(who_8x_n_fn)),
  binge_risk = list(evaluate_rfn(who_8x_b_fn))
) %>%
  crossing(tibble(who_age_group = c('15-34', '35-64', '65+'))) %>%
  crossing(tibble(im = paste0('_8', c(1,2,3,4,6))))

who_9x = tibble(
  # crossing im = _9.1345
  gender = c('m', 'w'),
  # crossing outcome = m/m
  bingea = 1,
  bingef = 1,
  wholly_attr = FALSE,
  r_fd = 0,
  risk = list(evaluate_rfn(who_9x_n_fn)),
  binge_risk = list(evaluate_rfn(who_9x_b_fn))
) %>%
  crossing(tibble(who_age_group = c('15-34', '35-64', '65+'))) %>%
  crossing(tibble(im = paste0('_9', c(1,3,4,5))))

who_diff = bind_rows(
  who_24,
  who_52_m,
  who_52_w,
  who_56_m,
  who_56_w,
  who_634_m,
  who_71,
  who_8x,
  who_9x) %>%
  crossing(tibble(outcome = c('Morbidity', 'Mortality')))

who_rest = cisur_rr %>%
  filter(!grepl('^_[789]', im) | (im %in% c('_85', '_92'))) %>%
  filter(!(im %in% c('_24', '_52', '_56'))) %>%
  filter(!(im %in% c('_63', '_64') & gender == 'm')) %>%
  crossing(tibble(who_age_group = c('15-34', '35-64', '65+')))

who_rr = bind_rows(who_diff, who_rest)

usethis::use_data(who_rr, overwrite = TRUE)
