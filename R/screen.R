## intermahp3 - R package backend for the intermahp project
## Copyright (C) 2019 Canadian Institute for Substance Use Research

#' Screens a prevalence and consumption dataset for problematic measures
#'
#'
#'@export

screen_pc <- function(.data) {
  if(is.null(.data)) {stop('No prevalence and consumption dataset supplied')}
  names(.data) = str_to_lower(names(.data))
  missing_vars = setdiff(imp$pc_vars, names(.data))
  extra_vars = setdiff(names(.data), imp$pc_vars)
  msg = ""
  stop_flag = FALSE
  if(length(missing_vars) > 0) {
    msg = c(msg, "Prevalance and Consumption variables needed but not supplied:\n", paste0("\t", missing_vars, "\n"))
    stop_flag = TRUE
  }
  if(length(extra_vars) > 0) {
    msg = c(msg, "Prevalance and Consumption variables supplied but not needed:\n", paste0("\t", extra_vars, "\n"))
  }
  message(msg)
  if(stop_flag) {
    stop(msg)
  }
  msg = ""
  ## Past here, we have the correct variables for PC
  ## Most variables have no restrictions (i.e. year, region, age_group, etc)

  ## Due to the availability of dose-response relative risk functions, we
  ## require the gender variable to have one of 2 values: w or m.
  ## In the frontend, we will include an interactive component for distributing
  ## uploaded values for gender.
  supplied_g = unique(.data$gender)
  if(!setequal(c('w', 'm'), supplied_g)) {
    msg = c(msg, "Due to the availability of dose-response relative risk functions, InterMAHP requires the gender variable to take only 2 values: w or m.\n")
    stop_flag = TRUE
  }

  ## p_la + p_fd + p_cd must sum to 1
  p_check = .data$p_la + .data$p_fd + .data$p_cd
  where_diff = abs(p_check - 1) > 1e-4
  count_diff = sum(where_diff)
  if(count_diff > 0){
    msg = c(msg, "The proportions of Lifetime Abstainers, Current Drinkers, and Former Drinkers must sum to 1 (tolerance 1e-4).  They do not sum to 1 in rows:\n", paste0("\t", which(where_diff), "\n"))
    stop_flag = TRUE
  }
  message(msg)
  if(stop_flag) {
    stop(msg)
  }

  .data
}



