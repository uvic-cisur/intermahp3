## intermahp3 - R package backend for the intermahp project
## Copyright (C) 2019 Canadian Institute for Substance Use Research

#' A model of alchohol harms and policies
#'
#' API for InterMAHP Shiny App
#'
#'@section Input Fields:
#'\describe{
#'  \item{pc}{A dataset of alcohol consumption and prevalence}
#'  \item{mm}{A dataset of mortality and morbidity counts}
#'  \item{rr}{A list of relative risk function evaluation datasets}
#'  \item{sk}{A dataset skeleton used to perform aaf computations}
#'  \item{dg}{A list of gender-stratified drinking groups}
#'  \item{bb}{Gender-stratified definition of binge drinking}
#'  \item{scc}{Gender-stratified propotion of squamous-cell carcinoma among
#'    oesophageal cancers (only SCC is alcohol-caused)}
#'  \item{ub}{Upper bound of alcohol consumption}
#'  \item{dg}{List of drinking groups (gender stratified)}
#'  \item{sn}{List of scenarios with names and mult. changes in consumption}
#'  \item{ext}{Boolean indicating whether relative risk functions are
#'    extrapolated linearly (TRUE) or capped (FALSE) after a consumption level
#'    of 150 grams per day}
#'  \item{cal}{Boolean indiciating whether to try to calibrate absolute risk
#'    curves for calibrable wholly attributable conditions}
#'  \item{rr_choice}{Character, relative risk source}
#'}
#'
#'@section Computed Fields:
#'\describe{
#'  \item{af}{A list of the wide fraction datasets described below}
#'  \item{base_paf}{A wide dataset of alcohol attributable fractions from
#'    partially attributable causes not affected by bingeing}
#'  \item{base_former_paf}{A wide dataset of alcohol attributable fractions from
#'    partially attributable causes not affected by bingeing that still affect
#'    former drinkers}
#'  \item{binge_paf}{A wide dataset of alcohol attributable fractions from
#'    partially attributable causes affected by bingeing}
#'  \item{binge_former_paf}{A wide dataset of alcohol attributable fractions
#'    from partially attributable causes affected by bingeing that still affect
#'    former drinkers}
#'  \item{scaled_base_waf}{A wide dataset of alcohol attributable fractions from
#'    wholly attributable causes not affected by binging whose attributable
#'    fractions are scaled forms of similar partially attributable forms}
#'  \item{scaled_binge_waf}{A wide dataset of alcohol attributable fractions
#'    from wholly attributable causes affected by binging whose attributable
#'    fractions are scaled forms of similar partially attributable forms}
#'  \item{calibrated_waf}{A wide dataset of alcohol attributable fractions from
#'    wholly attributable causes whose risk functions are calibrated from
#'    morbidity and mortality data}
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
#'\code{$update_rr()} Updates relative risk function evaluations with latest
#'  parameters
#'
#'\code{$make_gamma()} Makes base and binge gamma functions at the prescribed
#'  level of consumption
#'
#'@section Evaluation Methods:
#'\code{$init_fractions} Initialize and populate fraction sheets
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
    af = NULL,

    ## Fields for lists and scalars.
    ## bb and scc are gender stratified, ub is universal.
    ## dg is a list of gender stratified vectors
    ## ext is a boolean
    bb = NULL,
    scc = NULL,
    ub = NULL,
    dg = NULL,
    sn = NULL,
    ext = NULL,
    cal = NULL,
    rr_choice = NULL,


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

      ## First check that the source is valid, then set the source and update
      ## risk set
      if(is.null(.char)) {
        message('No relative risk source selected')
      } else if(!(.char %in% imp$rr_choices)) {
        message(paste0('Unknown relative risk source: ', .char))
      } else {
        self$rr_choice = .char
        self$update_rr()
        # self$rr = eval(sym(paste0(.char, '_rr')))
      }
      invisible(self)
    },

    ## Choose a predefined set of project settings
    choose_project = function(.char) {
      self$setbb(c('w' = 50, 'm' = 60))
      self$setub(250)
      self$setscc(c('w' = 1, 'm' = 1))
      invisible(self)
    },

    ## Set risk extrapolation method (linear >> TRUE, capped >> FALSE)
    set_ext = function(.char) {
      self$ext = .char

      self$update_rr()

      invisible(self)
    },

    ## Set binge consumption definitions
    set_bb = function(.numeric) {
      ## TODO: Ensure gender-stratified and within reasonable parameters
      self$bb = .numeric
      invisible(self)
    },

    ## Set upper bound on consumption
    set_ub = function(.numeric) {
      self$ub = .numeric

      if(.numeric > 250) {
        message('Maximum permitted upper bound is 250 grams-ethanol per day.')
      }
      self$ub = min(self$ub, 250)

      if(.numeric < 10) {
        message('Minimum permitted upper bound is 10 grams-ethanol per day.')
      }
      self$ub = max(self$ub, 10)

      self$update_rr()

      invisible(self)
    },

    ## Set squamous cell carcinoma proportions
    set_scc = function(.numeric) {
      ## TODO: Ensure gender-stratified and within reasonable parameters

      self$scc = .numeric

      self$update_rr()

      invisible(self)
    },

    ## Updates relative risk function evaluations with latest parameters
    update_rr = function() {
      if(!is.null(self$rr_choice)) {
        ## Start with the basic function evaluations 1:250
        temp_rr = eval(sym(paste0(self$rr_choice, '_rr')))

        ## If we've set scc proportions and 22 is part of the risk set being
        ## considered, we apply the proportions directly to the risks.
        temp_rr = if(!is.null(self$scc) && '_22' %in% self$rr$im) {
          rr_22 = temp_rr %>%
            filter(im == '_22') %>%
            ## Note, oesophageal cancer is not binge-affected in any risk source
            mutate(risk = map2(risk, gender, ~ .x * self$scc[[.y]]))
          bind_rows(
            filter(temp_rr, im != '_22'),
            rr_22
          )
        } else {
          temp_rr
        }

        ## Cap risk values if needed
        ## NOTE:: Order is important here, we implement capping of risk values
        ## AFTER applying SCC adjustment and BEFORE subsetting
        temp_rr = if(!is.null(self$ext) & !is.null(self$ub))
        {
          if(self$ext == 'capped' & self$ub > 150) {
            mutate(
              temp_rr,
              risk = map(
                risk,
                ~c(.x[1:150], rep(.x[150], ceiling(self$ub) - 150))
              ),
              binge_risk = map(
                binge_risk,
                ~c(.x[1:150], rep(.x[150], ceiling(self$ub) - 150))
              )
            )
          } else {
            temp_rr
          }
        } else {
          temp_rr
        }

        ## Subset the risk function evaluations now to match the length of the
        ## gamma vector evaluations
        temp_rr = if(!is.null(self$ub)) {
          mutate(
            temp_rr,
            risk = map(risk, ~.x[1:ceiling(self$ub)]),
            binge_risk = map(binge_risk, ~.x[1:ceiling(self$ub)])
          )
        } else {
          temp_rr
        }

        ## Categorize risk functions by methodology.
        ## Note that IHME fits entirely into the $base category, while CSUCH and
        ## related require all categories.  Wholly attributable categories
        ## should be able to be used by all.
        self$rr = list(
            base = temp_rr %>%
                filter(bingea == 0 & !wholly_attr & r_fd == 0 & !is.na(risk)) %>%
                select(im, gender, outcome, risk),
            base_former = temp_rr %>%
                filter(bingea == 0 & !wholly_attr & r_fd != 0 & !is.na(risk)) %>%
                select(im, gender, outcome, r_fd, risk),
            binge = temp_rr %>%
                filter(bingea == 1 & !wholly_attr & r_fd == 0 & !is.na(risk)) %>%
                select(im, gender, outcome, risk, binge_risk),
            binge_former = temp_rr %>%
                filter(bingea == 1 & !wholly_attr & r_fd != 0 & !is.na(risk)) %>%
                select(im, gender, outcome, r_fd, risk, binge_risk),
            base_scaled = temp_rr %>%
                filter(bingea == 0 & wholly_attr & !is.na(risk)) %>%
                select(im, gender, outcome, risk),
            binge_scaled = temp_rr %>%
                filter(bingea == 1 & wholly_attr & !is.na(risk)) %>%
                select(im, gender, outcome, risk, binge_risk),
            calibrated = temp_rr %>%
              filter(is.na(risk)) %>%
              select(im, gender, outcome),
            im = temp_rr$im %>% unique() %>% sort()
        )
      }
    },

    ## Makes base and binge gamma functions at the prescribed level of
    ## consumption
    make_gamma = function(consumption = 1,  binge_strat = FALSE) {
      msg = ''
      stop_flag = FALSE

      ## Ensure prevalence and consumption data has been supplied
      if(is.null(self$pc)) {
        msg = c(msg, 'Prevalence and consumption data needed')
        stop_flag = TRUE
      }

      if(stop_flag) {
        stop(msg)
      }

      ## 'Magic' numbers
      yearly_to_daily_conv = 0.002739726
      litres_to_millilitres_conv = 1000
      millilitres_to_grams_ethanol_conv = 0.7893

      ## Make the base gamma function
      base_gamma = self$pc %>%
        group_by(region, year) %>%
        mutate(
          ## Convert litres/year into grams/day and apply any scenario
          ## consumption delta
          pcc_g_day =
            pcc_litres_year *
            litres_to_millilitres_conv *
            millilitres_to_grams_ethanol_conv *
            yearly_to_daily_conv *
            correction_factor *
            consumption,
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
          ## Set gamma constants and binge levels (otherwise we have a loooot of
          ## vector lookups)
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
          ## The total area under the given gamma function within the specified
          ## bounds
          nc = gub - glb
        ) %>%
        mutate(
          ## Deflation factor --- the consumption measure must have area exactly
          ## p_cd within the specified bounds
          df = p_cd / nc
        ) %>%
        mutate(
          ## The base gamma is just a deflated gamma function.  We evaluate from
          ## 1 to the upper bound
          base_gamma = pmap(
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
          p_bat_ev = ifelse(p_bat > p_bd, p_bd / p_bat, 1)
        )

      ## Error correction is rare, so only do it if needed
      base_gamma = if(isTRUE(all_equal(1, base_gamma$p_bat_ev))) {
        base_gamma
      } else {
        ## Here we peel away gammas that need p_bat correction and recombine
        base_gamma_nec = filter(base_gamma, p_bat_ev == 1)
        base_gamma_ec = filter(base_gamma, p_bat_ev != 1) %>%
          mutate(
            ## When we need to rescale the tail beyond binge threshold (i.e.
            ## when the rescaling constant p_bat_ev is different from 1) we
            ## construct a vector of 1's up to binge and rescaling constant on
            ## the tail.
            p_bat_ec = map2(
              p_bat_ev, bb,
              ~ifelse(1:ceiling(self$ub) >= .y, 1, p_bat_ev)
            )
          ) %>%
          mutate(
            ## And then apply this rescaling vector to the base_gamma
            base_gamma = map2(base_gamma, p_bat_ec, `*`),
            ## With that fixed, p_bat comes back to p_bd levels at most, and we
            ## can use this quantity to split bingers and nonbingers below the
            ## binge threshold
            p_bat = ifelse(p_bat > p_bd, p_bd, p_bat)
          )

        bind_rows(base_gamma_nec, base_gamma_ec)
      }

      ## If we're building binge-stratified gammas, we split the base gamma here
      ## and return the result
      if(binge_strat) {
        base_gamma %>%
          mutate(
            ## proportion of nonbingers and bingers "below threshold", i.e.
            ## that are not daily bingers on average.  Used for CSUCH ischaemic
            ## and injury RR's
            non_bingers = (p_cd - p_bd)  / (p_cd - p_bat),
            bingers = (p_bd - p_bat) / (p_cd - p_bat)
          ) %>%
          mutate(
            ## The non-binge vector is the proportion of non-bingers within the
            ## base gamma function at each integer consumption level.  It is of
            ## course 0 above the binge threshold.
            non_binge_vector = map2(
              non_bingers, bb,
              ~ifelse(1:ceiling(self$ub) < .y, non_bingers, 0)
            ),
            ## The binge vector is the proportion of bingers within the base
            ## gamma function at each integer consumption level.  It is of
            ## course 1 above the binge threshold.
            binge_vector = map2(
              bingers, bb,
              ~ifelse(1:ceiling(self$ub) < .y, bingers, 1)
            )
          ) %>%
          mutate(
            ## Construct the corresponding binger and nonbingers consumption
            ## measures through multiplication with the binge and nonbinge
            ## proportionality vectors
            nonbinge_gamma = map2(base_gamma, non_binge_vector, `*`),
            binge_gamma = map2(base_gamma, binge_vector, `*`)
          ) %>%
          select(c(imp$pc_key_vars, 'p_fd', 'binge_gamma', 'nonbinge_gamma')) %>%
          return()
      } else {
        base_gamma %>%
          select(c(imp$pc_key_vars, 'p_fd', 'base_gamma')) %>%
          return()
      }
    },

    ## Evaluation --------------------------------------------------------------
    ## Functions in this grouping relate to evaluation of intermahp inputs. This
    ## includes the computation of attributable fractions and uncertainty
    ## estimates.  Results of all computations are appended to the results field
    ## data sheet

    ## Initialize and populate fraction sheets
    init_fractions = function() {
      screen_mahp(self)
      self$af = list()

      ## init gammas and binge gammas
      base_gamma = self$make_gamma(1, F)
      binge_gammas = self$make_gamma(1, T)

      ## Naming conventions for fractions:
      ##  1_2_#
      ##  1: the measure type
      ##  2: the measure group
      ##  #: the consumption level, multiplicative, 4 digits granularity

      ## init base fractions
      ## These are the simplest type and occur most frequently in our risk
      ## sources.  All of the IHME risk sources live here. Risk for former
      ## drinkers and bingers are unchanged.
      if(!is.null(self$rr$base)) {
        self$af$base_paf = full_join(base_gamma, self$rr$base, by = 'gender') %>%
          mutate(integrand_1.0000 = map2(risk, base_gamma, `*`)) %>%
          mutate(comp_current_1.0000 = map_dbl(integrand_1.0000, sum)) %>%
          mutate(denominator_1.0000 = 1 + comp_current_1.0000) %>%
          mutate(af_current_1.0000 = comp_current_1.0000 / denominator_1.0000) %>%
          mutate(af_entire_1.0000 = af_current_1.0000) %>%
          select(-p_fd, -base_gamma)
      }

      ## init base former fractions
      if(!is.null(self$rr$base_former)) {
        self$af$base_former_paf = full_join(base_gamma, self$rr$base_former, by = 'gender') %>%
          mutate(integrand_1.0000 = map2(risk, base_gamma, `*`)) %>%
          mutate(comp_current_1.0000 = map_dbl(integrand_1.0000, sum)) %>%
          mutate(comp_former = r_fd * p_fd) %>%
          mutate(denominator_1.0000 = 1 + comp_former + comp_current_1.0000) %>%
          mutate(af_current_1.0000 = comp_current_1.0000 / denominator_1.0000) %>%
          mutate(af_former_1.0000 = comp_former / denominator_1.0000) %>%
          mutate(af_entire_1.0000 = af_former_1.0000 + af_current_1.0000) %>%
          select(-p_fd, -base_gamma)
      }

      ## init binge fractions
      if(!is.null(self$rr$binge)) {
        self$af$binge_paf = full_join(binge_gammas, self$rr$binge, by = 'gender') %>%
          mutate(
            integrand_1.0000 = pmap(
              list(.w = risk, .x = binge_risk, .y = nonbinge_gamma, .z = binge_gamma),
              function(.w, .x, .y, .z) {(.w * .y) + (.x * .z)}
            )
          ) %>%
          mutate(comp_current_1.0000 = map_dbl(integrand_1.0000, sum)) %>%
          mutate(denominator_1.0000 = 1 + comp_current_1.0000) %>%
          mutate(af_current_1.0000 = comp_current_1.0000 / denominator_1.0000) %>%
          mutate(af_entire_1.0000 = af_current_1.0000) %>%
          select(-p_fd, -nonbinge_gamma, -binge_gamma)
      }

      ## init binge former fractions
      if(!is.null(self$rr$binge_former)) {
        self$af$binge_former_paf = full_join(binge_gammas, self$rr$binge_former, by = 'gender') %>%
          mutate(
            integrand_1.0000 = pmap(
              list(.w = risk, .x = binge_risk, .y = nonbinge_gamma, .z = binge_gamma),
              function(.w, .x, .y, .z) {(.w * .y) + (.x * .z)}
            )
          ) %>%
          mutate(comp_current_1.0000 = map_dbl(integrand_1.0000, sum)) %>%
          mutate(comp_former = r_fd * p_fd) %>%
          mutate(denominator_1.0000 = 1 + comp_former + comp_current_1.0000) %>%
          mutate(af_current_1.0000 = comp_current_1.0000 / denominator_1.0000) %>%
          mutate(af_former_1.0000 = comp_former / denominator_1.0000) %>%
          mutate(af_entire_1.0000 = af_former_1.0000 + af_current_1.0000) %>%
          select(-p_fd, -nonbinge_gamma, -binge_gamma)
      }
      ## init base scaled fractions
      if(!is.null(self$rr$base_scaled)) {
        self$af$base_scaled_waf = full_join(base_gamma, self$rr$base_scaled, by = 'gender') %>%
          mutate(integrand_1.0000 = map2(risk, base_gamma, `*`)) %>%
          mutate(comp_current_1.0000 = map_dbl(integrand_1.0000, sum)) %>%
          mutate(denominator_1.0000 = 1 + comp_current_1.0000) %>%
          ## Scaling factor is used post-hoc for drinking groups and scenarios.
          ## For current drinkers and entire population we just get values of 1
          mutate(scaling_factor_1.0000 = denominator_1.0000 / comp_current_1.0000) %>%
          mutate(af_current_1.0000 = 1) %>%
          mutate(af_entire_1.0000 = 1) %>%
          select(-p_fd, -base_gamma)
      }

      ## init binge scaled fractions
      if(!is.null(self$rr$binge_scaled)) {
        self$af$binge_scaled_waf = full_join(binge_gammas, self$rr$binge, by = 'gender') %>%
          mutate(
            integrand_1.0000 = pmap(
              list(.w = risk, .x = binge_risk, .y = nonbinge_gamma, .z = binge_gamma),
              function(.w, .x, .y, .z) {(.w * .y) + (.x * .z)}
            )
          ) %>%
          mutate(comp_current_1.0000 = map_dbl(integrand_1.0000, sum)) %>%
          mutate(denominator_1.0000 = 1 + comp_current_1.0000) %>%
          ## Scaling factor is used post-hoc for drinking groups and scenarios.
          ## For current drinkers and entire population we just get values of 1
          mutate(scaling_factor_1.0000 = denominator_1.0000 / comp_current_1.0000) %>%
          mutate(af_current_1.0000 = 1) %>%
          mutate(af_entire_1.0000 = 1) %>%
          select(-p_fd, -nonbinge_gamma, -binge_gamma)
      }
      ## init calibrated fractions
      if(!is.null(self$rr$calibrated)) {
        ## I dunno maybe set up some 1.00 base afs and postpone risk function
        ## calibration until asked for scenarios/drinking groups?
      }

      invisible(self)
    },

    ## Adds new scenario attributable fractions and relative attributable
    ## fractions to the partially attributable fraction dataset
    ##   .numeric is a multiplicative constant for relative scenario consumption
    ##     We truncate the value to 4 digits beyond the .
    add_scenario = function(.numeric) {
      # The change in consumption used to construct gammas
      .value = round(.numeric, digits = 4)
      # Change in consumption as a suffix for variable names
      .suffix = sprintf("%01.4f",.value)
      # Scenario integrand (summation values under this methodology)
      integrand = paste0('integrand_', .suffix)
      # AF component for current drinkers.  Sum of integrand
      comp_current = paste0('comp_current_', .suffix)
      # Denominator in AF computations, equal to 1 + FD component + CD component
      denominator = paste0('denominator_', .suffix)
      # 'True' AF before adjustments.  Made practical by hitting it with the
      # count adjustment
      af_entire = paste0('af_entire_', .suffix)
      # Count adjustment. Converts 'true' AF into something that can be used by
      # assuming the number of non-attributable harms are held constant between
      # scenarios
      adj_scenario = paste0('adj_scenario_', .suffix)
      # Scenario attributable fraction for current drinkers
      saf_current = paste0('saf_current_', .suffix)
      # Scenario attributable fraction for the entire population
      saf_entire = paste0('saf_entire_', .suffix)
      # Scenario attributable fraction for former drinkers
      saf_former = paste0('saf_former_', .suffix)
      # Scaling factor is used to normalize fractions for wholly attributable
      # conditions
      scaling_factor = paste0('scaling_factor_', .suffix)

      ## init scenario gammas and binge gammas
      base_gamma = self$make_gamma(.value, F)
      binge_gammas = self$make_gamma(.value, T)

      ## Naming conventions for fractions:
      ##  *_#
      ##  *: the measure type (saf = scenario attributable fraction = fraction
      ##  with count adjustment applied)
      ##  #: the consumption level, multiplicative, 4 digits granularity

      ## Add base scenario fractions
      if(!is.null(self$af$base_paf)) {
        self$af$base_paf = left_join(
          self$af$base_paf, base_gamma,
          by = c("region", "year", "gender", "age_group")) %>%
          mutate((!! integrand) := map2(risk, base_gamma, `*`)) %>%
          mutate((!! comp_current) := map_dbl(eval(sym(integrand)), sum)) %>%
          mutate((!! denominator) := 1 + eval(sym(comp_current))) %>%
          mutate((!! af_entire) := eval(sym(comp_current)) / eval(sym(denominator))) %>%
          mutate((!! adj_scenario) := (1 - af_entire_1.0000) / (1 - eval(sym(af_entire)))) %>%
          mutate((!! saf_entire) := eval(sym(adj_scenario)) * eval(sym(af_entire))) %>%
          mutate((!! saf_current) := eval(sym(saf_entire))) %>%
          select(-p_fd, -base_gamma)

        if(!is.null(self$dg)) {
          for(.name in names(self$dg)) {
            af_group = paste0('saf_', .name, '_', .suffix)
            self$af$base_paf = mutate(
              self$af$base_paf,
              (!! af_group) := pmap(
                list(.g = gender, .v = eval(sym(integrand)), .d = eval(sym(denominator))),
                function(.g, .v, .d) {
                  sum(.v[ceiling(self$dg[[.name]][[.g]][[1]]):ceiling(self$dg[[.name]][[.g]][[2]])])/.d
                }
              )
            )
          }
        }
      }

      ## Add base former scenario fractions
      if(!is.null(self$af$base_former_paf)) {
        self$af$base_former_paf = left_join(
          self$af$base_former_paf, base_gamma,
          by = c("region", "year", "gender", "age_group")) %>%
          mutate((!! integrand) := map2(risk, base_gamma, `*`)) %>%
          mutate((!! comp_current) := map_dbl(eval(sym(integrand)), sum)) %>%
          mutate((!! denominator) := 1 + eval(sym(comp_current))) %>%
          mutate((!! af_entire) := eval(sym(comp_current)) / eval(sym(denominator))) %>%
          mutate((!! adj_scenario) := (1 - af_entire_1.0000) / (1 - eval(sym(af_entire)))) %>%
          mutate((!! saf_entire) := eval(sym(adj_scenario)) * eval(sym(af_entire))) %>%
          mutate((!! saf_current) := eval(sym(saf_entire))) %>%
          mutate((!! saf_former) := eval(sym(adj_scenario)) * comp_former / eval(sym(denominator))) %>%
          select(-p_fd, -base_gamma)
        #TODO::  Evaluate over variable groups
      }

      ## Add binge scenario fractions
      if(!is.null(self$af$binge_paf)) {
        self$af$binge_paf = left_join(
          self$af$binge_paf, binge_gammas,
          by = c("region", "year", "gender", "age_group")) %>%
          mutate(
            (!! integrand) := pmap(
              list(.w = risk, .x = binge_risk, .y = nonbinge_gamma, .z = binge_gamma),
              function(.w, .x, .y, .z) {
                (.w * .y) + (.x * .z)
              }
            )
          ) %>%
          mutate((!! comp_current) := map_dbl(eval(sym(integrand)), sum)) %>%
          mutate((!! denominator) := 1 + eval(sym(comp_current))) %>%
          mutate((!! af_entire) := eval(sym(comp_current)) / eval(sym(denominator))) %>%
          mutate((!! adj_scenario) := (1 - af_entire_1.0000) / (1 - eval(sym(af_entire)))) %>%
          mutate((!! saf_entire) := eval(sym(adj_scenario)) * eval(sym(af_entire))) %>%
          mutate((!! saf_current) := eval(sym(saf_entire))) %>%
          select(-p_fd, -nonbinge_gamma, -binge_gamma)

        #TODO::  Evaluate over variable groups
      }

      ## Add binge former scenario fractions
      if(!is.null(self$af$binge_former_paf)) {
        self$af$binge_former_paf = left_join(
          self$af$binge_former_paf, binge_gammas,
          by = c("region", "year", "gender", "age_group")) %>%
          mutate(
            (!! integrand) := pmap(
              list(.w = risk, .x = binge_risk, .y = nonbinge_gamma, .z = binge_gamma),
              function(.w, .x, .y, .z) {(.w * .y) + (.x * .z)}
            )
          ) %>%
          mutate((!! comp_current) := map_dbl(eval(sym(integrand)), sum)) %>%
          mutate((!! denominator) := 1 + eval(sym(comp_current))) %>%
          mutate((!! af_entire) := eval(sym(comp_current)) / eval(sym(denominator))) %>%
          mutate((!! adj_scenario) := (1 - af_entire_1.0000) / (1 - eval(sym(af_entire)))) %>%
          mutate((!! saf_entire) := eval(sym(adj_scenario)) * eval(sym(af_entire))) %>%
          mutate((!! saf_current) := eval(sym(saf_entire))) %>%
          mutate((!! saf_former) := eval(sym(adj_scenario)) * comp_former / eval(sym(denominator))) %>%
          select(-p_fd, -nonbinge_gamma, -binge_gamma)
        #TODO::  Evaluate over variable groups
      }

      ## init base scaled fractions
      if(!is.null(self$af$base_scaled_waf)) {
        self$af$base_scaled_waf = left_join(
          self$af$base_scaled_waf, base_gamma,
          by = c("region", "year", "gender", "age_group")) %>%
          mutate((!! integrand) := map2(risk, base_gamma, `*`)) %>%
          mutate((!! comp_current) := map_dbl(eval(sym(integrand)), sum)) %>%
          mutate((!! denominator) := 1 + eval(sym(comp_current))) %>%
          ## Scaling factor is used post-hoc for drinking groups and scenarios.
          ## For current drinkers and entire population we just get values of 1
          mutate((!! scaling_factor) := eval(sym(denominator)) / eval(sym(comp_current))) %>%
          mutate((!! saf_current) := scaling_factor_1.0000 / eval(sym(scaling_factor))) %>%
          mutate((!! saf_entire) := eval(sym(saf_current))) %>%
          select(-p_fd, -base_gamma)
        #TODO::  Evaluate over variable groups
      }

      ## init binge scaled fractions
      if(!is.null(self$af$binge_scaled_waf)) {
        self$af$binge_scaled_waf = left_join(
          self$af$binge_scaled_waf, binge_gammas,
          by = c("region", "year", "gender", "age_group")) %>%
          mutate(
            (!! integrand) := pmap(
              list(.w = risk, .x = binge_risk, .y = nonbinge_gamma, .z = binge_gamma),
              function(.w, .x, .y, .z) {(.w * .y) + (.x * .z)}
            )
          ) %>%
          mutate((!! comp_current) := map_dbl(eval(sym(integrand)), sum)) %>%
          mutate((!! denominator) := 1 + eval(sym(comp_current))) %>%
          ## Scaling factor is used post-hoc for drinking groups and scenarios.
          ## For current drinkers and entire population we just get values of 1
          mutate((!! scaling_factor) := eval(sym(denominator)) / eval(sym(comp_current))) %>%
          mutate((!! saf_current) := scaling_factor_1.0000 / eval(sym(scaling_factor))) %>%
          mutate((!! saf_entire) := eval(sym(saf_current))) %>%
          select(-p_fd, -nonbinge_gamma, -binge_gamma)
        #TODO::  Evaluate over variable groups

      }
      # ## init calibrated fractions
      # if(!is.null(self$rr$calibrated)) {
      #   ## I dunno maybe set up some 1.00 base afs and postpone risk function
      #   ## calibration until asked for scenarios/drinking groups?
      # }
      #
      #
      #
      # ## Implement scenarios by constructing new integrand vectors, constructing
      # ## the scenario's integrand, current drinker component, and denominator,
      # ## then evaluating afs for drinking groups
      # self$paf = mutate(self$paf, (!! paste0('s', .numeric)) := 0)
      invisible(self)
    },

    ## Adds a new set of drinking groups for each existing scenario
    ##
    add_group = function(.name, .group) {

      ## TODO:: Screen name and group defn input, turn name into something
      ## suitable for tibble var name

      self$dg[[.name]] = .group

      ## Implement groups by adding a name to the list, then evaluating the new
      ## group for each scenario
      # self$paf = mutate(self$paf, (!! .name) := 0)
      invisible(self)
    }

    ## Results -----------------------------------------------------------------
    ## Functions in this grouping relate to the presentation of results.  We
    ## output the data in long form, and also filter and shape the data for use
    ## in the Shiny App charting utility
  )
)
