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
  stop_flag = stop_flag || g_list$stop_flag

  ## Ensure positivity of populations
  init_n = nrow(.data)
  .data = filter(.data, population > 0)
  rmvd_n = init_n - nrow(.data)
  if(rmvd_n) {
    msg = c(msg, paste0(rmvd_n, " rows with nonpositive populations removed from prevalence and consumption dataset."))
  }

  ## Proportions must be nonnegative.  We're feeling lazy today so this is the lazy solution
  ## Lifetime abstainers
  where_la_neg = .data$p_la < 0
  count_la_neg = sum(where_la_neg)
  if(count_la_neg > 0){
    msg = c(
      msg,
      "The proportion of Lifetime Abstainers must be nonnegative. Supplied values are negative in rows:\n",
      paste0(
        "\t",
        which(where_la_neg),
        "\nThese values will be set to 0."
      )
    )
    .data$p_la[where_la_neg] <- 0
  }

  ## Former drinkers
  where_fd_neg = .data$p_fd < 0
  count_fd_neg = sum(where_fd_neg)
  if(count_fd_neg > 0){
    msg = c(
      msg,
      "The proportion of Former Drinkers must be nonnegative. Supplied values are negative in rows:\n",
      paste0(
        "\t",
        which(where_fd_neg),
        "\nThese values will be set to 0."
      )
    )
    .data$p_fd[where_fd_neg] <- 0
  }

  ## Current drinkers
  where_cd_neg = .data$p_cd < 0
  count_cd_neg = sum(where_cd_neg)
  if(count_cd_neg > 0){
    msg = c(
      msg,
      "The proportion of Current Drinkers must be nonnegative. Supplied values are negative in rows:\n",
      paste0(
        "\t",
        which(where_cd_neg),
        "\nThese values will be set to 0."
      )
    )
    .data$p_cd[where_cd_neg] <- 0
  }


  ## Binge drinkers
  where_bd_neg = .data$p_bd < 0
  count_bd_neg = sum(where_bd_neg)
  if(count_bd_neg > 0){
    msg = c(
      msg,
      "The proportion of Binge Drinkers must be nonnegative. Supplied values are negative in rows:\n",
      paste0(
        "\t",
        which(where_bd_neg),
        "\nThese values will be set to 0."
      )
    )
    .data$p_bd[where_bd_neg] <- 0
  }

  ## p_la + p_fd + p_cd must sum to 1
  p_check = .data$p_la + .data$p_fd + .data$p_cd
  where_diff = abs(p_check - 1) > 1e-4
  where_zero = p_check == 0
  count_diff = sum(where_diff)
  count_zero = sum(where_zero)
  if(count_zero > 0){
    msg = c(
      msg,
      "Fatal Error: Proportions of Lifetime Abstainers, Current Drinkers, and Former Drinkers sum to 0 in the following rows:",
      paste0(
        "\t",
        which(where_zero),
        "\nThese values will be set to 0."
      )
    )
    stop_flag = TRUE
  }
  if(count_diff > 0){
    msg = c(
      msg,
      "The proportions of Lifetime Abstainers, Current Drinkers, and Former Drinkers must sum to 1 (tolerance 1e-4).  They do not sum to 1 in rows:\n",
      paste0(
        "\t",
        which(where_diff)
      ),
      "\nThese rows are being normalized."
    )
    .data$p_la[where_diff] <- .data$p_la[where_diff] / p_check[where_diff]
    .data$p_fd[where_diff] <- .data$p_fd[where_diff] / p_check[where_diff]
    .data$p_cd[where_diff] <- .data$p_cd[where_diff] / p_check[where_diff]
  }

  ## More screening can be put here


  if(stop_flag) {
    warning(c(msg, "Fatal errors detected, dataset not usable.  Please attend to these errors and try again."))
    return(NULL)
  } else if(length(msg > 0) && msg[[1]] != "") {
    message(msg)
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

  ## Ensure nonnegativity of counts
  init_n = nrow(.data)
  .data = filter(.data, count >= 0)
  rmvd_n = init_n - nrow(.data)
  if(rmvd_n) {
    msg = c(msg, paste0(rmvd_n, " rows with negative counts removed from morbidity and mortality dataset."))
  }

  ## Check gender uniformity
  g_list = screen_gender(.data$gender)
  msg = c(msg, g_list$msg)
  stop_flag = stop_flag || g_list$stop_flag

  ## Check IM format (must be _## or convertible to _##)
  im_subbed = str_sub(paste0('_', gsub('[^[:digit:]]', '', as.character(.data$im))), 1, 3)
  if(unique(str_length(im_subbed)) != 3) {
    msg = c(msg, "IM variable malformed, please see intermahp readme for details.")
    stop_flag = TRUE
  }

  ## More screening can be put here


  if(stop_flag) {
    warning(c(msg, "Fatal errors detected, dataset not usable.  Please attend to these errors and try again."))
    return(NULL)
  } else if(length(msg > 0) && msg[[1]] != "") {
    message(msg)
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
  # if(length(extra_vars) > 0) {
  #   msg = c(msg, paste(.name, "variables supplied but not needed:\n"), paste0("\t", extra_vars, "\n"))
  # }

  if(stop_flag) {
    warning(c(msg, "Fatal errors detected, dataset not usable.  Please attend to these errors and try again."))
    return(NULL)
  } else if(length(msg > 0) && msg[[1]] != "") {
    message(msg)
  }
  # if(msg != "") message(msg)
  # if(stop_flag) stop(msg)
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
    msg = c(msg, '\tPrevalence and Consumption dataset not provided.\n')
    stop_flag = TRUE
  }

  if(is.null(.mahp$rr)) {
    msg = c(msg, '\tRelative risk source not chosen.\n')
    stop_flag = TRUE
  }

  if(is.null(.mahp$ub)) {
    msg = c(msg, '\tUpper bound not set.\n')
    stop_flag = TRUE
  }

  if(is.null(.mahp$bb)) {
    msg = c(msg, '\tBinge bounds not set.\n')
    stop_flag = TRUE
  }

  if(is.null(.mahp$scc) & '_22' %in% .mahp$rr$im) {
    msg = c(msg, '\tSquamous cell carcinoma proportion of oesophageal cancers not set.\n')
    stop_flag = TRUE
  }

  if(stop_flag) {
    warning(c("Model construction incomplete due to the following errors:\n", msg, "Please attend to these errors and try again."))
    return(NULL)
  } else if(length(msg > 0) && msg[[1]] != "") {
    message(msg)
  }


  # message(msg)
  # if(stop_flag) {
  #   stop(msg)
  # }
}
