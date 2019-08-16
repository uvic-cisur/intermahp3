## intermahp3 - R package backend for the intermahp project
## Copyright (C) 2019 Canadian Institute for Substance Use Research

#' A model of alchohol harms and policies
#'
#' API for InterMAHP Shiny App
#'
#'@section Fields:
#'\describe{
#'  \item{pc}{A dataset of alcohol consumption and prevalence}
#'  \item{mm}{A dataset of mortality and morbidity counts}
#'  \item{rr}{A dataset of relative risk function evaluations}
#'  \item{sk}{A dataset skeleton used to perform aaf computations}
#'  \item{af}{A long dataset of attributable fractions}
#'  \item{dg}{A list of gender-stratified drinking groups}
#'  \item{bb}{Gender-stratified definition of binge drinking}
#'  \item{scc}{Gender-stratified propotion of squamous-cell carcinoma among
#'    oesophageal cancers (only SCC is alcohol-caused)}
#'  \item{ub}{Upper bound of alcohol consumption}
#'  \item{dg}{List of drinking groups (gender stratified)}
#'  \item{ext}{Boolean indicating whether relative risk functions are
#'    extrapolated linearly (TRUE) or capped (FALSE) after a consumption level
#'    of 150 grams per day}
#'  \item{rr_str}{String indicating which relative risk function source we use.
#'    his is used when the rr dataset needs updating via update_rr()}
#'}
#'
#'@section Data Methods:
#'\code{$new()} Creates a new model object.
#'
#'\code{$add_pc()} Prepares a prevalence and consumption dataset
#'
#'\code{$add_mm()} Prepares a morbidity and mortality dataset
#'
#'\code{$choose_rr()} Chooses a source for relative risk functions
#'
#'\code{$choose_project()} Chooses a predefined set of project settings
#'
#'\code{$set_ext()} Set risk extrapolation method (linear >> TRUE, capped >> FALSE)
#'
#'\code{$set_bb()} Sets binge consumption definitions
#'
#'\code{$set_ub()} Sets upper bound on consumption
#'
#'\code{$set_scc()} Sets squamous cell carcinoma proportions
#'
#'\code{$update_rr()} Updates the rr dataset according to the current values of
#'  relevant variables
#'
#'@section Evaluation Methods:
#'\code{$init_sk()} Initializes the skeleton computation dataset and evaluates
#'  at baseline consumption
#'
#'\code{$add_scenario()} Adds new scenario attributable fractions and relative
#'  attributable fractions to the skeleton dataset for each existing drinking
#'  group
#'
#'\code{$add_group()} Adds a new set of drinking groups for each existing
#'  scenario
#'
#'@importFrom R6 R6Class
#'@name mahp
NULL


#'@export
mahp <- R6Class(
  'mahp',
  public = list(
    ## Fields for datasets
    pc = NULL,
    mm = NULL,
    rr = NULL,
    sk = NULL,
    af = NULL,

    ## Fields for lists and scalars.
    ## bb and scc are gender stratified, ub is universal.
    ## dg is a list of gender stratified vectors
    ## ext is a boolean
    bb = NULL,
    scc = NULL,
    ub = NULL,
    dg = NULL,
    ext = NULL,
    rr_str = NULL,


    ## Data --------------------------------------------------------------------
    ## Functions in this grouping relate to intermahp inputs.  These include
    ## data sets such as relative risk function evaluations, prevalence,
    ## consumption, and harm observations.  This also includes scalar parameters
    ## such as upper bounds on consumption, binge level definitions, squamous
    ## cell carcinoma proportions, dose-response extrapolation method, etc.
    ## Data functions assign internal fields.

    ## Prepares a prevalence and consumption dataset
    add_pc = function(.data) {
      .data = screen_pc(.data)
      self$pc = .data
      invisible(self)
    },

    ## Prepares a morbidity and mortality dataset
    add_mm = function(.data) {
      .data = screen_mm(.data)
      self$mm = .data
      invisible(self)
    },

    ## Chooses a source for relative risk functions
    choose_rr = function(.char) {
      self$rr_str = eval(sym(.char))
      invisible(self)
    },

    ## Choose a predefined set of project settings
    choose_project = function(.char) {
      self$setbb(0)
      self$setub(0)
      self$setscc(0)
      invisible(self)
    },

    ## Set risk extrapolation method (linear >> TRUE, capped >> FALSE)
    set_ext = function(.char) {
      invisible(self)
    },

    ## Set binge consumption definitions
    set_bb = function(.numeric) {
      self$bb = .numeric
      invisible(self)
    },

    ## Set upper bound on consumption
    set_ub = function(.numeric) {
      self$ub = .numeric
      invisible(self)
    },

    ## Set squamous cell carcinoma proportions
    set_scc = function(.numeric) {
      self$scc = .numeric
      invisible(self)
    },

    ## Updates the rr dataset according to the current values of relevant
    ## variables
    update_rr = function() {

    },

    ## Updates the pc dataset according to the current values of relevant
    ## variables
    update_pc = function() {
      ## 'Magic' numbers
      yearly_to_daily_conv = 0.002739726
      litres_to_millilitres_conv = 1000
      millilitres_to_grams_ethanol_conv = 0.7893

      self$pc$ngamma = NULL
      self$pc = self$pc %>%
        group_by(region, year) %>%
        mutate(
          pcc_g_day =
            pcc_litres_year *
            litres_to_millilitres_conv *
            millilitres_to_grams_ethanol_conv *
            yearly_to_daily_conv *
            correction_factor,
          drinkers = population * p_cd
        ) %>% mutate(
          ## alcohol consumption over all age groups
          pcad = pcc_g_day * sum(population) / sum(drinkers)
        ) %>% mutate(
          ## mean consumption per age group
          pcc_among_drinkers = relative_consumption * pcad * sum(drinkers) /
            sum(relative_consumption*drinkers)
        ) %>%
        ungroup() %>%
        mutate(
          gamma_cs = as.numeric(imp$gamma_cs[gender]),
          bb = as.numeric(self$bb[gender])
        ) %>%
        mutate(
          gamma_shape = 1 / gamma_cs,
          gamma_scale = pcc_among_drinkers * gamma_cs
        ) %>%
        mutate(
          glb = pgamma(q = 0.03, shape = gamma_shape, scale = gamma_scale),
          gbb = pgamma(q = bb, shape = gamma_shape, scale = gamma_scale),
          gub = pgamma(q = self$ub, shape = gamma_shape, scale = gamma_scale)
        ) %>%
        mutate(
          nc = gub - glb
        ) %>%
        mutate(
          df = p_cd / nc
        ) %>%
        mutate(
          ngamma = pmap(
            list(.x = gamma_shape, .y = gamma_scale, .z = df),
            function(.x, .y, .z) {
              .z * dgamma(x = 1:ceiling(self$ub), shape = .x, scale = .y)
            }
          )
        ) %>%
        mutate(
          ## p_bat is "bingers above threshold", i.e. daily bingers on average.
          ## If p_bat >= p_bd, we must fix this by deflating the tail of the gamma
          ## distribution above the binge barrier and setting p_bat equal to p_bd.
          p_bat = df * (gub - gbb)
        ) %>%
        mutate(
          p_bat_error_correction = ifelse(p_bat > p_bd, p_bd / p_bat, 1),
          p_bat = ifelse(p_bat > p_bd, p_bd, p_bat),
          ## proportion of nonbingers and bingers "below threshold", i.e.
          ## that are not daily bingers on average.  Used for CSUCH ischaemic
          ## and injury RR's
          non_bingers = (p_cd - p_bd)  / (p_cd - p_bat),
          bingers = (p_bd - p_bat) / (p_cd - p_bat)
        )
    },

    ## Evaluation --------------------------------------------------------------
    ## Functions in this grouping relate to evaluation of intermahp inputs. This
    ## includes the computation of attributable fractions and uncertainty
    ## estimates.  Results of all computations are appended to the results field
    ## data sheet

    ## Initializes the skeleton computation dataset and evaluates at baseline
    ## consumption
    init_sk = function() {
      self$sk = tibble(x = 0)
      invisible(self)
    },

    ## Adds new scenario attributable fractions and relative attributable
    ## fractions to the skeleton dataset
    add_scenario = function(.numeric) {
      self$sk = mutate(self$sk, (!! paste0('s', .numeric)) := 0)
      invisible(self)
    },

    ## Adds a new set of drinking groups for each existing scenario
    ##
    add_group = function(.name, .list) {
      self$sk = mutate(self$sk, (!! .name) := 0)
      invisible(self)
    }

    ## Results -----------------------------------------------------------------
    ## Functions in this grouping relate to the presentation of results.  We
    ## output the data in long form, and also filter and shape the data for use
    ## in the Shiny App charting utility
  )
)
