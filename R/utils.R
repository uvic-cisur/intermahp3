#' Add WHO conforming age groups naively
#'
#'
#' @param .data a tibble with 'age_group' variable
#' @param is_who T/F, are we using who?
#'
#' @return The dataset with maybe one more variable (who_age_group)
#'
#' @export

add_who_age_groups <- function(.data, is_who) {
  if(!is_who) {return(.data)}
  mutate(
    .data,
    who_age_group = as.character(
      cut(
        as.numeric(str_sub(age_group, 1, 2)),
        breaks = c(0, 14, 34, 64, 1000),
        labels = c("00-14", "15-34", "35-64", "65+")
      )
    )
  )
}

#### Imports -------------------------------------------------------------------

#' @importFrom R6 R6Class
#' @importFrom rlang sym
#' @importFrom magrittr %>% %<>%
#' @importFrom dplyr
#' mutate filter group_by ungroup inner_join select bind_rows left_join all_equal full_join setdiff distinct
#' @importFrom purrr map pmap map2_dbl map_dbl map2 pmap_dbl map_chr
#' @importFrom tibble tibble
#' @importFrom stringr str_to_lower str_sub
#' @importFrom tidyr gather spread
#' @importFrom nloptr nloptr
#' @keywords internal
foo <- function() {return("bar")}
