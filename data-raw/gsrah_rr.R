## code to prepare `gsrah_rr` dataset goes here
library(tidyverse)

## All real valued functions taken from GBD supplement
## _52 is ischaemic heart disease
## p. 33
gsrah_52_m_fn1 = function(x) {
  y1= (x + 0.0099999997764826)/100
  y2= (60 + 0.0099999997764826)/100

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
gsrah_52_m_fn2 = function(x) {
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
gsrah_52_m_fn3 = function(x) {
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
gsrah_52_w_fn1 = function(x) {
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
gsrah_52_w_fn2 = function(x) {
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
gsrah_52_w_fn3 = function(x) {
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

## _56 is ischaemic stroke
## p. 39
gsrah_56_m_fn1 = function(x) {
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
gsrah_56_m_fn2 = function(x) {
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
gsrah_56_m_fn3 = function(x) {
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
gsrah_56_w_fn1 = function(x) {
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
      exp(b1 * (b2 * sqrt(y2) + b3 * sqrt(y2)))
    )
  )
}

## p. 43
gsrah_56_w_fn2 = function(x) {
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
      exp(b1 * ((b2 * sqrt(y2)) + (b3 * sqrt(y2) * log(y2))))
    )
  )
}

## p. 44
gsrah_56_w_fn3 = function(x) {
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
      exp(b1 * ((b2 * sqrt(y2)) + (b3 * sqrt(y2) * log(y2))))
    )
  )
}

## Building these conditions from scratch
gsrah_52_m = tibble(
  im = '_52',
  gender = 'm',
  age_group = c('15-34', '35-65', '65+'),
  ## crossing outcome = m/m
  # bingea = 0,
  # bingef = 1,
  # wholly_attr = FALSE,
  r_fd = .25,
  risk = list(gsrah_52_m_fn1, gsrah_52_m_fn2, gsrah_52_m_fn3)
  # binge_risk
)

gsrah_52_w = tibble(
  im = '_52',
  gender = 'w',
  age_group = c('15-34', '35-65', '65+'),
  ## crossing outcome = m/m
  # bingea = 0,
  # bingef = 1,
  # wholly_attr = FALSE,
  r_fd = .25,
  risk = list(gsrah_52_w_fn1, gsrah_52_w_fn2, gsrah_52_w_fn3)
  # binge_risk
)

gsrah_56_m = tibble(
  im = '_56',
  gender = 'm',
  age_group = c('15-34', '35-65', '65+'),
  ## crossing outcome = m/m
  # bingea = 0,
  # bingef = 1,
  # wholly_attr = FALSE,
  r_fd = .25,
  risk = list(gsrah_56_m_fn1, gsrah_56_m_fn2, gsrah_56_m_fn3)
  # binge_risk
)

gsrah_ischaemic = bind_rows(gsrah_52_m, gsrah_52_w, gsrah_56_m)

gsrah_rest = cisur_rr
