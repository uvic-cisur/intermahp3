## intermahp3 - R package backend for the intermahp project
## Copyright (C) 2019 Canadian Institute for Substance Use Research

#' A Reference Class to represent an international model of alchohol harms
#'
#'@field pc A dataset of alcohol consumption and prevalence
#'@field mm A dataset of mortality and morbidity counts
#'@field rr A dataset of relative risk function evaluations
#'@field results A dataset of results
#'@field bb Gender-stratified definition of binge drinking
#'@field scc Gender-stratified propotion of squamous-cell carcinoma among oesophageal cancers (only SCC is alcohol-caused)
#'@field ub Upper bound of alcohol consumption
imodel <- R6Class(
  'imodel',
  fields = list(
    ## Fields for datasets
    pc = 'tbl_df',
    mm = 'tbl_df',
    rr = 'tbl_df',
    results = 'tbl_df',

    ## Fields for scalars.  bb and scc are gender stratified, ub is universal
    bb = 'numeric',
    scc = 'numeric',
    ub = 'numeric'
  )
)
#### Data ----------------------------------------------------------------------
#### Functions in this grouping relate to intermahp inputs.  These include data
#### sets such as relative risk function evaluations, prevalence, consumption,
#### and harm observations.  This also includes scalar parameters such as upper
#### bounds on consumption, binge level definitions, squamous cell carcinoma
#### proportions, dose-response extrapolation method, etc. Results of data
#### functions are intermahp model objects.

#' Creates a new empty model object
#'
#'
makenewmodel = function() {}

#' Add/replace a prevalence and consumption dataset
#'
#'
addpc = function(.model, .data) {}

#' Add/replace a morbidity and mortality dataset
#'
#'
addmm = function(.model, .data) {}

#' Choose a relative risk source
#'
#'
chooserr = function(.model, .char) {}

#' Choose a risk extrapolation method
#'
#'
chooseext = function(.model, .char) {}

#' Choose a predefined set of project settings
#'
#'
chooseproject = function(.model, .char) {}

#' Set binge consumption definitions
#'
#'
setbb = function(.model, .numeric) {}

#' Set upper bound on consumption
#'
#'
setub = function(.model, .numeric) {}

#' Set squamous cell carcinoma proportions
#'
#'
setscc = function(.model, .numeric) {}

#' Prepare a model for evaluation
#'
#'
preparemodel = function(.model) {}

#### Evaluation ----------------------------------------------------------------
#### Functions in this grouping relate to evaluation of intermahp inputs.  This
#### includes the computation of attributable fractions and uncertainty
#### estimates.  Results of all computations are appended to the global results
#### sheet in the intermahp model object constructed by the functions in the
#### data grouping.

#' Evaluate
#'
#'
evalformer = function(.model) {}

evalcurrent = function(.model) {}

evaldenom = function(.model)

evalfull = function(.model) {}


#### Results -------------------------------------------------------------------
#### Functions in this grouping relate to
)
