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
#'\code{$choose_ext()} Chooses a risk extrapolation method
#'
#'\code{$choose_project()} Chooses a predefined set of project settings
#'
#'\code{$set_bb()} Sets binge consumption definitions
#'
#'\code{$set_ub()} Sets upper bound on consumption
#'
#'\code{$set_scc()} Sets squamous cell carcinoma proportions
#'
#'@section Evaluation Methods:
#'\code{$init_sk()}{ Initializes the skeleton computation dataset and evaluates
#'  at baseline consumption}
#'
#'\code{$add_scenario()}{Adds new scenario attributable fractions and relative
#'  attributable fractions to the skeleton dataset for each existing drinking
#'  group}
#'
#'\code{$add_group()}{Adds a new set of drinking groups for each existing
#'  scenario}
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
    bb = NULL,
    scc = NULL,
    ub = NULL,
    dg = NULL,

    ## Data --------------------------------------------------------------------
    ## Functions in this grouping relate to intermahp inputs.  These include
    ## data sets such as relative risk function evaluations, prevalence,
    ## consumption, and harm observations.  This also includes scalar parameters
    ## such as upper bounds on consumption, binge level definitions, squamous
    ## cell carcinoma proportions, dose-response extrapolation method, etc.
    ## Data functions assign internal fields.

    ## Prepares a prevalence and consumption dataset
    add_pc = function(.data) {
      screen_pc(.data)
      self$pc = .data
      invisible(self)
    },

    ## Prepares a morbidity and mortality dataset
    add_mm = function(.data) {
      self$mm = .data
      invisible(self)
    },

    ## Chooses a source for relative risk functions
    choose_rr = function(.char) {
      self$rr = eval(sym(.char))
      invisible(self)
    },

    ## Choose a risk extrapolation method
    choose_ext = function(.char) {
      invisible(self)
    },

    ## Choose a predefined set of project settings
    choose_project = function(.char) {
      self$setbb(0)
      self$setub(0)
      self$setscc(0)
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
