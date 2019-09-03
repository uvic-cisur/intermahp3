## intermahp3 - R package backend for the intermahp project
## Copyright (C) 2019 Canadian Institute for Substance Use Research

#' Screens a prevalence and consumption dataset for problematic measures
#'
#'
#'@export

screen_pc <- function(.data) {
  ## inititalize feedback variables
  msg = ""
  stop_flag = FALSE
  if(is.null(.data)) {stop('No prevalence and consumption dataset supplied')}

  ## Screen variables
  .data = screen_vars(.data, imp$pc_vars, "Prevalence and Consumption")

  ## Past here, we have the correct variables for PC
  ## Most variables have no restrictions (i.e. year, region, age_group, etc)

  ## Check gender uniformity
  g_list = screen_gender(.data$gender)
  msg = c(msg, g_list$msg)
  stop_flag = stop_flag | g_list$stop_flag

  ## p_la + p_fd + p_cd must sum to 1
  p_check = .data$p_la + .data$p_fd + .data$p_cd
  where_diff = abs(p_check - 1) > 1e-4
  count_diff = sum(where_diff)
  if(count_diff > 0){
    msg = c(msg, "The proportions of Lifetime Abstainers, Current Drinkers, and Former Drinkers must sum to 1 (tolerance 1e-4).  They do not sum to 1 in rows:\n", paste0("\t", which(where_diff), "\n"))
    stop_flag = TRUE
  }

  ## More screening can be put here

  message(msg)
  if(stop_flag) {
    stop(msg)
  }

  .data
}

#' Screens a morbidity and mortality dataset for problematic measures
#'
#'
#'@export

screen_mm = function(.data) {
  ## inititalize feedback variables
  msg = ""
  stop_flag = FALSE
  if(is.null(.data)) {stop('No morbidity and mortality dataset supplied')}
  .data = screen_vars(.data, imp$mm_vars, "Morbidity and Mortality")

  ## Check gender uniformity
  g_list = screen_gender(.data$gender)
  msg = c(msg, g_list$msg)
  stop_flag = stop_flag | g_list$stop_flag

  ## More screening can be put here

  message(msg)
  if(stop_flag) {
    stop(msg)
  }

  .data
}

#' Lowers case and checks for missing/extra variables
#'
#'@param .vars A character vector of variable names, all lower case.
#'
#'@export

screen_vars = function(.data, .vars, .name) {
  names(.data) = str_to_lower(names(.data))
  missing_vars = setdiff(.vars, names(.data))
  extra_vars = setdiff(names(.data), .vars)
  msg = ""
  stop_flag = FALSE
  if(length(missing_vars) > 0) {
    msg = c(msg, paste(.name, "variables needed but not supplied:\n"), paste0("\t", missing_vars, "\n"))
    stop_flag = TRUE
  }
  if(length(extra_vars) > 0) {
    msg = c(msg, paste(.name, "variables supplied but not needed:\n"), paste0("\t", extra_vars, "\n"))
  }
  if(msg != "") message(msg)
  if(stop_flag) stop(msg)
  .data
}

#' Screens supplied gender variable for InterMAHP permissable values
#'
#'Due to the availability of dose-response relative risk functions, we require
#'the gender variable to have one of 2 values: w or m. In the frontend, we will
#'include an interactive component for distributing uploaded values for gender.
#'
#'@export

screen_gender = function(.char) {
  supplied_g = unique(.char)
  if(!setequal(c('w', 'm'), supplied_g)) {
    return(
      list(
        msg = "Due to the availability of dose-response relative risk functions, InterMAHP requires the gender variable to take only 2 values: w and m.\n",
        stop_flag = TRUE
      )
    )
  } else {
    return(
      list(
        msg = "",
        stop_flag = FALSE
      )
    )
  }
}


#' Screens the supplied mahp instance for missing variables
#'
#'@export

screen_mahp = function(.mahp) {
  msg = ''
  stop_flag = FALSE
  if(is.null(.mahp$pc)) {
    msg = c(msg, 'Prevalence and Consumption dataset not provided.\n')
    stop_flag = TRUE
  }

  if(is.null(.mahp$rr)) {
    msg = c(msg, 'Relative risk source not chosen.\n')
    stop_flag = TRUE
  }

  if(is.null(.mahp$ub)) {
    msg = c(msg, 'Upper bound not set.\n')
    stop_flag = TRUE
  }

  if(is.null(.mahp$bb)) {
    msg = c(msg, 'Binge bounds not set.\n')
    stop_flag = TRUE
  }

  if(is.null(.mahp$scc) & '_22' %in% .mahp$rr$im) {
    msg = c(msg, 'Squamous cell carcinoma proportion of oesophageal cancers not set.\n')
    stop_flag = TRUE
  }

  message(msg)
  if(stop_flag) {
    stop(msg)
  }
}
