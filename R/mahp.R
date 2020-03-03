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
#'  \item{mcn}{Number of samples in Monte Carlo uncertainty estimation}
#'  \item{pc_sample_vars}{Which prevalence and consumption variables to sample}
#'  \item{dg}{List of all drinking groups (gender stratified)}
#'  \item{sn}{Vector of all scenarios as mult. changes in consumption.}
#'  \item{ext}{Character indicating whether relative risk functions are
#'    extrapolated linearly or capped after a consumption level
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
#'\code{$def_scenario()} Defines a scenario
#'\code{$rm_scenario}
#'\code{$cmp_scenario()} Adds new scenario attributable fractions and relative attributable fractions to the partially attributable fraction dataset
#'\code{$cmp_scenarios()}
#'
#'\code{$def_group()} Defines or redefines a group
#'\code{$rm_group}
#'\code{$cmp_group()}
#'\code{$cmp_groups()}
#'
#'
#'\code{$add_scenario()} Adds new scenario attributable fractions and relative
#'  attributable fractions to the skeleton dataset for each existing drinking
#'  group
#'
#'\code{$add_group()} Adds a new set of drinking groups for each existing
#'  scenario
#'
#'@section Results Methods:
#'\code{$get_afs} Provides all attributable fractions computed and evaluates scc
#'  correction if supplied
#'
#'\code{$get_long_afs} Invokes get_afs and formats as long
#'
#'\code{$get_long_counts} Invokes get_long_afs and applied afs to counts
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
    mcn = NULL,
    pc_sample_vars = NULL,
    sn = NULL,
    dg = list(),
    ext = NULL,
    cal = FALSE,
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

    rm_pc = function() {
      self$pc = NULL
      invisible(self)
    },

    ## Prepares a morbidity and mortality dataset
    add_mm = function(.data) {
      .data = screen_mm(.data)
      self$mm = .data
      invisible(self)
    },

    rm_mm = function() {
      self$mm = NULL
      invisible(self)
    },

    ## Chooses a source for relative risk functions
    choose_rr = function(.char) {

      ## First check that the source is valid, then set the source and update
      ## risk set
      if(is.null(.char)) {
        warning('No relative risk source selected')
      } else if(!(.char %in% imp$rr_choices)) {
        warning(paste0('Unknown relative risk source: ', .char))
      } else {
        self$rr_choice = .char
        # self$update_rr()
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

      # self$update_rr()

      invisible(self)
    },

    ## Set binge consumption definitions
    set_bb = function(.list) {
      ## TODO: Ensure gender-stratified and within reasonable parameters
      self$bb = .list
      invisible(self)
    },

    ## Set upper bound on consumption
    set_ub = function(.numeric) {
      self$ub = .numeric

      if(.numeric > 250) {
        warning('Maximum permitted upper bound is 250 grams-ethanol per day.')
      }
      self$ub = min(self$ub, 250)

      if(.numeric < 10) {
        warning('Minimum permitted upper bound is 10 grams-ethanol per day.')
      }
      self$ub = max(self$ub, 10)

      # self$update_rr()

      invisible(self)
    },

    ## Set squamous cell carcinoma proportions
    set_scc = function(.numeric) {
      ## TODO: Ensure gender-stratified and within reasonable parameters

      self$scc = .numeric

      # self$update_rr()

      invisible(self)
    },

    ## Set Monte Carlo uncertainty sample size
    set_mcn = function(.integer) {
      ## inititalize feedback variables
      msg = ""
      stop_flag = FALSE

      ## Sanitize the input.  At least 1, or the absolute value of the input, if
      ## the input can be coerced to an integer
      self$mcn = max(1, abs(as.integer(.integer)), na.rm = TRUE)

      invisible(self)
    },

    set_cal = function(.bool) {
      self$cal = .bool

      invisible(self)
    },

    ## Set pc variables to sample
    set_pc_sample_vars = function(.character) {
      ## inititalize feedback variables
      msg = ""
      stop_flag = FALSE

      self$pc_sample_vars = unique(.character)

      invisible(self)
    },

    ## Updates relative risk function evaluations with latest parameters
    ## Note: This function MUST be called manually before fractions are
    ## initialized.
    update_rr = function() {
      if(!is.null(self$rr_choice) && self$rr_choice %in% imp$rr_choices) {
        ## Start with the basic function evaluations 1:250
        temp_rr = eval(sym(paste0(self$rr_choice, '_rr')))

        ## If this is a sampling set of RRs we sample
        temp_rr = if(grepl('sample', self$rr_choice)) {
          temp_rr %>%
            mutate(
              sampled = rnorm(nrow(temp_rr))) %>%
            mutate(
              risk = pmap(
                list(.w = sampled, .x = mean_rr, .y = sd_plus, .z = sd_minus),
                function(.w, .x, .y, .z) {
                  .x + (.w > 0) * .w * .y + (.w < 0) * .w * .z
                }
              )
            )
        } else {
          temp_rr
        }

        ## Cap risk values if needed
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

        cal_rr = if(!is.null(self$mm) & !is.null(self$pc) & self$cal) {
          self$mm %>%
            filter(im %in% imp$rr_cal) %>%
            select(im, region, year, gender, age_group, outcome, count) %>%
            inner_join(self$make_gamma(1, F), by = imp$pc_key_vars) %>%
            mutate(
              threshold = ceiling(
                map2_dbl(
                  im,
                  gender,
                  ~if(str_sub(.x, 1, 2) == '_6') {
                    0
                  } else {
                    `[[`(self$bb, .y)
                  }
                )
              ),
              phat = count / drinkers
            ) %>%
            mutate(
              k = pmap_dbl(
                list(
                  .w = base_gamma, .x = ceiling(self$ub), .y = threshold, .z = phat
                ),
                function(.w, .x, .y, .z) {
                  .z / sum((1:(.x-.y)) * .w[(.y+1):.x])
                }
              )
            ) %>%
            # mutate(
            #   fn = pmap(
            #     list(.w = base_gamma, .x = ceiling(self$ub), .y = threshold, .z = count),
            #     function(.w, .x, .y, .z) {
            #       function(k) {
            #         sum((k * 1:(.x-.y)) * .w[(.y+1):.x]) - .z
            #       }
            #     }
            #   )
            # ) %>%
            # mutate(
            #   k = map_dbl(
            #     fn,
            #     ~nloptr(
            #       x0 = 0.01,
            #       eval_f = .x,
            #       lb = 0,
            #       ub = 1,
            #       opts = list(
            #         "algorithm" = "NLOPT_LN_COBYLA",
            #         "xtol_rel" = 1.0e-30
            #       )
            #     )$solution
            #   )
            # ) %>%
            mutate(
              risk = pmap(
                list(.w = 1/drinkers, .x = ceiling(self$ub), .y = threshold, .z = k),
                function(.w, .x, .y, .z) {
                  .w * c(rep(0, .y), (.z * 1:(.x-.y)))
                }
              )
            ) %>%
            select(c('im', imp$pc_key_vars, 'outcome', 'count', 'k', 'risk'))
        } else {
          NULL
        }


        ## Categorize risk functions by methodology.
        ## Note that IHME fits entirely into the $base category, while CSUCH and
        ## related require all categories.  Wholly attributable categories
        ## should be able to be used by all.
        self$rr = list(
          base = temp_rr %>%
            filter(bingea == 0 & !wholly_attr & r_fd == 0 & !is.null(risk)) %>%
            select(im, gender, outcome, risk),
          base_former = temp_rr %>%
            filter(bingea == 0 & !wholly_attr & r_fd != 0 & !is.null(risk)) %>%
            select(im, gender, outcome, r_fd, risk),
          binge = temp_rr %>%
            filter(bingea == 1 & !wholly_attr & r_fd == 0 & !is.null(risk)) %>%
            select(im, gender, outcome, risk, binge_risk),
          binge_former = temp_rr %>%
            filter(bingea == 1 & !wholly_attr & r_fd != 0 & !is.null(risk)) %>%
            select(im, gender, outcome, r_fd, risk, binge_risk),
          base_scaled = temp_rr %>%
            filter(bingea == 0 & wholly_attr & !is.null(risk)) %>%
            select(im, gender, outcome, risk),
          binge_scaled = temp_rr %>%
            filter(bingea == 1 & wholly_attr & !is.null(risk)) %>%
            select(im, gender, outcome, risk, binge_risk),
          calibrated = cal_rr, # TODO:: Calibrate absolute risk curves
          im = c(temp_rr$im, cal_rr$im) %>% unique() %>% sort()
        )
      }
    },

    ## Computes usable population metrics for Prevalence and Consumption
    compute_pm = function(consumption = 1) {
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

      self$pc %>%
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
          gamma_cs = if('gamma_cs' %in% names(.)) {gamma_cs} else {as.numeric(imp$gamma_cs[gender])},
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
          ## p_bat is "bingers above threshold", i.e. daily bingers on average.
          ## If p_bat >= p_bd, then we have an inconsistency.  The new method
          ## for fixing this is to assume that the user uploaded binge data is
          ## unreliable, so we raise p_bd to match p_bat
          p_bat = df * (gub - gbb)
        ) %>%
        mutate(
          p_bd = ifelse(p_bat > p_bd, p_bat, p_bd)
        )
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

      ## Adjust p_bd IF this is a scenario and IF we care about binging
      ## (for b_bat correction)
      metrics = if(consumption != 1 && binge_strat) {
        baseline = self$compute_pm(1)
        self$compute_pm(consumption) %>% mutate(
          p_bd = ifelse(
            baseline$p_bat > 0 & p_bat > 0,
            p_bd * p_bat / baseline$p_bat,
            p_bd
          )
        )
      } else {
        self$compute_pm(consumption)
      }



      ## Make the base gamma function
      base_gamma = metrics %>%
        mutate(
          ## The base gamma is just a deflated gamma function.  We evaluate from
          ## 1 to the upper bound
          base_gamma = pmap(
            list(.x = gamma_shape, .y = gamma_scale, .z = df),
            function(.x, .y, .z) {
              .z * dgamma(x = 1:ceiling(self$ub), shape = .x, scale = .y)
            }
          )
        )

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
          select(c(imp$pc_key_vars, drinkers, 'p_fd', 'base_gamma')) %>%
          return()
      }
    },

    ## Uncertainty Estimates ---------------------------------------------------
    ## Functions in this grouping deal with the implementation of Monte Carlo
    ## methods for the development of uncertainty estimates

    # sample_mahps,

    sample_self = function() {
      sampled = self$clone()
      sampled$rr_choice = paste0(self$rr_choice, '_sample')
      # sampled$update_rr()
      # sampled$pc = self$sample_pc()
      invisible(sampled)
    },

    make_ue = function() {
      for(i in 1:self$mcn) {
        sample_mahp = self$sample_self()

        sample_mahp$init_fractions()
      }
    },

    ## Constructs a sample of prevalence and consumption variables for use in
    ## developing uncertainty estimates for AFs
    sample_pc = function() {
      ## inititalize feedback variables
      msg = ""
      stop_flag = FALSE

      sampled = self$pc

      ## norman is the norm n
      norman = nrow(sampled)

      for(var in self$pc_sample_vars) {
        if(var == "pcc_litres_year") {
          if(!('pcc_litres_year_sd' %in% names(sampled))) {
            ## when not provided, standard deviation chosen so that 2*sd = 0.25 when estimate is 10
            ## NOTE:: no justification is provided for this value, please supply a sample standard deviation or do not sample this variable
            sampled %<>% mutate(pcc_litres_year_sd = sqrt(pcc_litres_year/640))
          }

          total_consumption <- sampled %>%
            select(region, year, pcc_litres_year, pcc_litres_year_sd) %>%
            distinct()

          short_norman = nrow(total_consumption)

          total_consumption %<>% mutate(
            pcc_litres_year = rnorm(short_norman, pcc_litres_year, pcc_litres_year_sd)
          )

          sampled <- select(sampled, -pcc_litres_year, -pcc_litres_year_sd) %>%
            left_join(total_consumption, by = c("region", "year"))

        } else if(var == "relative_consumption") {
          if(!('relative_consumption_sd' %in% names(sampled))) {
            ## when not provided, standard deviation chosen so that 2*sd = 0.025 when estimate is 1
            ## NOTE:: no justification is provided for this value, please supply a sample standard deviation or do not sample this variable
            sampled %<>% mutate(relative_consumption_sd = sqrt(relative_consumption/640))
          }

          sampled %<>% mutate(
            relative_consumption = rnorm(norman, relative_consumption, relative_consumption_sd)
          )

        } else if(grepl("^p_", var)) {
          var_sd = paste0(var, '_sd')

          if(!(var_sd %in% names(sampled))) {
            ## p_ variablesa are proportions.  We assume a sample size of 1000 and construct the standard variance.
            ## NOTE:: no justification is provided for this value, please supply a sample standard deviation or do not sample this variable
            sampled %<>% mutate((!! var_sd) := sqrt(0.001 * eval(sym(var)) * (1 - eval(sym(var)))))
          }

          sampled %<>% mutate(
            (!! var) := rnorm(norman, eval(sym(var)), eval(sym(var_sd)))
          )
        } else if(var == "gamma_c") {
          if(!('gamma_sd' %in% names(sampled))) {
            # SE from bounds on 95% CI, Kehoe et al. (2012)
            # NOTE:: see Kehoe et al., supply your own only if you know what you're doing
            sampled %<>% mutate(
              gamma_c = map_dbl(gender, ~`[[`(imp$gamma_c, .x)),
              gamma_sd = map_dbl(gender, ~`[[`(imp$gamma_sd, .x)))
          }

          sampled %<>% mutate(
            gamma_c = rnorm(norman, gamma_c, gamma_sd),
            gamma_cs = gamma_c^2,
          )
        }
      }

      invisible(sampled)
    },

    ## Evaluation --------------------------------------------------------------
    ## Functions in this grouping relate to evaluation of intermahp inputs. This
    ## includes the computation of attributable fractions and uncertainty
    ## estimates.  Results of all computations are appended to the results field
    ## data sheet

    ## Initialize and populate fraction sheets
    init_fractions = function() {
      # message('Initializing data')

      # message('\tScreening model object')
      screen_mahp(self)

      # message('\tResetting fraction data')
      self$af = list()

      ## init gammas and binge gammas
      # message('\tInitializing exposure distributions')
      base_gamma = self$make_gamma(1, F)
      binge_gammas = self$make_gamma(1, T)

      ## Naming conventions for fractions:
      ##  1_2_#
      ##  1: the measure type
      ##  2: the measure group
      ##  #: the consumption level, multiplicative, 4 digits precision

      # message('Computing Attributable Fractions')
      for(.type in c('base', 'base_former', 'binge', 'binge_former')) {
        .paf = paste0(.type, '_paf')
        if(!is.null(self$rr[[.type]]) & nrow(self$rr[[.type]]) > 0) {
          if(grepl('base', .type)) {
            self$af[[.paf]] = full_join(self$rr[[.type]], base_gamma, by = c("gender")) %>%
              mutate(integrand_1.0000 := map2(risk, base_gamma, `*`)) %>%
              select(-base_gamma)
          } else {
            self$af[[.paf]] = full_join(binge_gammas, self$rr[[.type]], by = 'gender') %>%
              mutate(
                integrand_1.0000 = pmap(
                  list(.w = risk, .x = binge_risk, .y = nonbinge_gamma, .z = binge_gamma),
                  function(.w, .x, .y, .z) {(.w * .y) + (.x * .z)}
                )
              ) %>%
              select(-nonbinge_gamma, -binge_gamma)
          }

          self$af[[.paf]] = mutate(self$af[[.paf]], comp_current_1.0000 = map_dbl(integrand_1.0000, sum))

          if(grepl('former', .type)) {
            self$af[[.paf]] = self$af[[.paf]] %>%
              mutate(comp_former = r_fd * p_fd) %>%
              mutate(denominator_1.0000 = 1 + comp_former + comp_current_1.0000) %>%
              mutate(af_former_1.0000 = comp_former / denominator_1.0000) %>%
              mutate(af_current_1.0000 = comp_current_1.0000 / denominator_1.0000)  %>%
              mutate(af_entire_1.0000 = af_former_1.0000 + af_current_1.0000)
          } else {
            self$af[[.paf]] = self$af[[.paf]] %>%
              mutate(denominator_1.0000 = 1 + comp_current_1.0000) %>%
              mutate(af_current_1.0000 = comp_current_1.0000 / denominator_1.0000) %>%
              mutate(af_entire_1.0000 = af_current_1.0000)
          }

          self$af[[.paf]] = self$af[[.paf]] %>% select(-p_fd)

          if(!is.null(self$dg)) {
            for(.name in names(self$dg)) {
              af_group = paste('af', .name, '1.0000', sep = '_')
              self$af[[.paf]] = mutate(
                self$af[[.paf]],
                (!! af_group) := pmap_dbl(
                  list(.g = gender, .v = integrand_1.0000, .d = denominator_1.0000),
                  function(.g, .v, .d) {
                    sum(.v[ceiling(self$dg[[.name]][[.g]][[1]]):ceiling(self$dg[[.name]][[.g]][[2]])])/.d
                  }
                )
              )
            }
          }
        }
      }

      ## init base scaled fractions
      if(!is.null(self$rr$base_scaled) & nrow(self$rr$base_scaled) > 0) {
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

        for(.name in names(self$dg)) {
          af_group = paste('af', .name, '1.0000', sep = '_')
          self$af$base_scaled_waf = mutate(
            self$af$base_scaled_waf,
            (!! af_group) := pmap_dbl(
              list(.g = gender, .v = integrand_1.0000, .c = comp_current_1.0000),
              function(.g, .v, .c) {
                sum(.v[ceiling(self$dg[[.name]][[.g]][[1]]):ceiling(self$dg[[.name]][[.g]][[2]])]) / .c
              }
            )
          )
        }
      }

      ## init binge scaled fractions
      if(!is.null(self$rr$binge_scaled) & nrow(self$rr$binge_scaled) > 0) {
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


        for(.name in names(self$dg)) {
          af_group = paste('af', .name, '1.0000', sep = '_')
          self$af$binge_scaled_waf = mutate(
            self$af$binge_scaled_waf,
            (!! af_group) := pmap_dbl(
              list(.g = gender, .v = integrand_1.0000, .c = comp_current_1.0000),
              function(.g, .v, .c) {
                sum(.v[ceiling(self$dg[[.name]][[.g]][[1]]):ceiling(self$dg[[.name]][[.g]][[2]])]) / .c
              }
            )
          )
        }
      }
      ## init calibrated fractions

      if(!is.null(self$rr$calibrated) & nrow(self$rr$calibrated) > 0) {
        self$af$calibrated_waf = full_join(base_gamma, self$rr$calibrated, by = imp$pc_key_vars) %>%
          mutate(integrand_1.0000 = map2(risk, base_gamma, `*`)) %>%
          ## Calibrated are pure absolute risk functions, don't produce fractions.
          ## Rather, safs are computed directly.
          mutate(af_current_1.0000 = 1) %>%
          mutate(af_entire_1.0000 = 1) %>%
          select(-base_gamma)

        for(.name in names(self$dg)) {
          af_group = paste('af', .name, '1.0000', sep = '_')
          self$af$calibrated_waf = mutate(
            self$af$calibrated_waf,
            (!! af_group) := pmap_dbl(
              list(.g = gender, .v = integrand_1.0000),
              function(.g, .v) {
                sum(.v[ceiling(self$dg[[.name]][[.g]][[1]]):ceiling(self$dg[[.name]][[.g]][[2]])])
              }
            )
          )
        }
      }

      self$cmp_scenarios()
      self$cmp_groups()

      invisible(self)
    },

    ## Remove attributable fraction sheet
    rm_af = function() {
      self$af = NULL
    },

    ## Defines a scenario
    def_scenario = function(.numeric) {
      ## sanitize the consumption into multiplicative factor
      .numeric = as.numeric(.numeric)
      .suffix = sprintf("%01.4f", .numeric)
      .sn_name = paste0(sprintf('%02.2+f', 100 * (.numeric - 1)), '%')

      if(.numeric <= 0) {
        warning('Scenario consumption change must be given multiplicatively. Values must be positive.')
        return(invisible(self))
      }

      # Don't accept 1
      if(.numeric == 1) {
        warning('Baseline consumption scenario is already defined.')
        return(invisible(self))
      }

      # jettison if scenario already exists
      if(.numeric %in% self$sn) {
        warning(paste0('Scenario ', .sn_name,' has already been added.'))
        return(invisible(self))
      }

      self$sn = union(self$sn, .numeric)

      ## Try to compute the scenario if fractions exist, but don't warn if not
      if(!is.null(self$af)) {
        self$cmp_scenario(.numeric)
      }

      invisible(self)

    },

    ## removes the given scenario if it exists
    rm_scenario = function(.numeric) {
      .numeric = as.numeric(.numeric)
      .suffix = sprintf("%01.4f", .numeric)
      # .sn_name = paste0(sprintf('%02.2+f', 100 * (.numeric - 1)), '%')

      re_match = paste0('_', .suffix, '$')

      self$sn = self$sn[self$sn != .numeric]
      for(af_name in names(self$af)) {
        sn_cols = grep(re_match, names(self$af[[af_name]]))
        self$af[[af_name]][sn_cols] = NULL
      }

      # warning(c('rm_scenario ', .sn_name))

      invisible(self)
    },

    ## Adds new scenario attributable fractions and relative attributable
    ## fractions to the partially attributable fraction dataset
    ##   .numeric is a multiplicative constant for relative scenario consumption
    ##     We truncate the value to 4 digits beyond the .
    cmp_scenario = function(.numeric) {
      ## If afs have been initialized then we compute scenario for existing
      ## groups.  Otherwise this method does nothing.
      if(!is.null(self$af)) {

        # The change in consumption used to construct gammas
        .value = round(.numeric, digits = 4)
        # Change in consumption as a suffix for variable names
        .suffix = sprintf("%01.4f",.value)

        # Don't accept 1
        if(.value == 1) {
          warning(paste0('Scenario ', .suffix,' has already been added.'))
          return(invisible(self))
        }

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

        # Add scenario to store if not present
        self$sn = union(self$sn, .value)

        ## init scenario gammas and binge gammas
        base_gamma = self$make_gamma(.value, F)
        binge_gammas = self$make_gamma(.value, T)

        ## Naming conventions for fractions:
        ##  *_#
        ##  *: the measure type (saf = scenario attributable fraction = fraction
        ##  with count adjustment applied)
        ##  #: the consumption level, multiplicative, 4 digits granularity

        for(.paf in c('base_paf', 'base_former_paf', 'binge_paf', 'binge_former_paf')) {
          if(!is.null(self$af[[.paf]])) {
            if(grepl('base', .paf)) {
              self$af[[.paf]] = left_join(
                self$af[[.paf]], base_gamma,
                by = c("region", "year", "gender", "age_group")) %>%
                mutate((!! integrand) := map2(risk, base_gamma, `*`)) %>%
                select(-p_fd, -base_gamma)
            } else {
              self$af[[.paf]] = left_join(
                self$af[[.paf]], binge_gammas,
                by = c("region", "year", "gender", "age_group")) %>%
                mutate(
                  (!! integrand) := pmap(
                    list(.w = risk, .x = binge_risk, .y = nonbinge_gamma, .z = binge_gamma),
                    function(.w, .x, .y, .z) {
                      (.w * .y) + (.x * .z)
                    }
                  )
                ) %>%
                select(-p_fd, -nonbinge_gamma, -binge_gamma)
            }

            self$af[[.paf]] = self$af[[.paf]] %>%
              mutate((!! comp_current) := map_dbl(eval(sym(integrand)), sum)) %>%
              mutate((!! denominator) := 1 + eval(sym(comp_current))) %>%
              mutate((!! af_entire) := eval(sym(comp_current)) / eval(sym(denominator))) %>%
              mutate((!! adj_scenario) := (1 - af_entire_1.0000) / (1 - eval(sym(af_entire)))) %>%
              mutate((!! saf_entire) := eval(sym(adj_scenario)) * eval(sym(af_entire))) %>%
              mutate((!! saf_current) := eval(sym(saf_entire)))


            if(grepl('former', .paf)) {
              self$af[[.paf]] = self$af[[.paf]] %>%
                mutate((!! saf_former) := eval(sym(adj_scenario)) * comp_former / eval(sym(denominator)))
            }

            ## When adding a scenario, compute for all existing drinking groups as well
            if(!is.null(self$dg)) {
              for(.name in names(self$dg)) {
                af_group = paste('saf', .name, .suffix, sep = '_')
                self$af[[.paf]] = mutate(
                  self$af[[.paf]],
                  (!! af_group) := pmap_dbl(
                    list(.g = gender, .v = eval(sym(integrand)), .d = eval(sym(denominator))),
                    function(.g, .v, .d) {
                      sum(.v[ceiling(self$dg[[.name]][[.g]][[1]]):ceiling(self$dg[[.name]][[.g]][[2]])])/.d
                    }
                  )
                )
              }
            }
          }
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

          ## When adding a scenario, compute for all existing drinking groups as well
          for(.name in names(self$dg)) {
            af_group = paste('saf', .name, .suffix, sep = '_')
            self$af$base_scaled_waf = mutate(
              self$af$base_scaled_waf,
              (!! af_group) := pmap_dbl(
                list(.g = gender, .v = eval(sym(integrand)), .c = eval(sym(comp_current)), .s = eval(sym(saf_current))),
                function(.g, .v, .c, .s) {
                  .s * sum(.v[ceiling(self$dg[[.name]][[.g]][[1]]):ceiling(self$dg[[.name]][[.g]][[2]])]) / .c
                }
              )
            )
          }
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

          ## When adding a scenario, compute for all existing drinking groups as well
          for(.name in names(self$dg)) {
            af_group = paste('saf', .name, .suffix, sep = '_')
            self$af$binge_scaled_waf = mutate(
              self$af$binge_scaled_waf,
              (!! af_group) := pmap_dbl(
                list(.g = gender, .v = eval(sym(integrand)), .c = eval(sym(comp_current)), .s = eval(sym(saf_current))),
                function(.g, .v, .c, .s) {
                  .s * sum(.v[ceiling(self$dg[[.name]][[.g]][[1]]):ceiling(self$dg[[.name]][[.g]][[2]])]) / .c
                }
              )
            )
          }
        }

        if(!is.null(self$rr$calibrated) & nrow(self$rr$calibrated) > 0) {
          self$af$calibrated_waf = left_join(
            self$af$calibrated_waf, base_gamma, by = imp$pc_key_vars) %>%
            mutate((!! integrand) := map2(risk, base_gamma, `*`)) %>%
            ## Calibrated are pure absolute risk functions, don't produce fractions.
            ## Rather, safs are computed directly.
            mutate((!! saf_current) := map_dbl(eval(sym(integrand)), sum)) %>%
            mutate((!! saf_entire) := eval(sym(saf_current))) %>%
            select(-base_gamma)

          for(.name in names(self$dg)) {
            af_group = paste('saf', .name, .suffix, sep = '_')
            self$af$calibrated_waf = mutate(
              self$af$calibrated_waf,
              (!! af_group) := pmap_dbl(
                list(.g = gender, .v = eval(sym(integrand))),
                function(.g, .v) {
                  sum(.v[ceiling(self$dg[[.name]][[.g]][[1]]):ceiling(self$dg[[.name]][[.g]][[2]])])
                }
              )
            )
          }
        }

        self$sn = union(self$sn, .value)

      } else {
        warning('Fractions must be initialized before groups may be computed')
      }

      invisible(self)
    },

    cmp_scenarios = function() {
      for(.scenario in self$sn) {
        self$cmp_scenario(.scenario)
      }
    },

    ## Defines or redefines a drinking group
    def_group = function(.name, .group) {
      ## sanitize the name into alphanumeric string
      .name = gsub('[^[:alnum:]]', '', as.character(.name))

      ## Ensure .group has the right structure
      if(!is.list(.group) ||
         is.null(.group$m) || !is.numeric(.group$m) || length(.group$m) != 2 || .group$m[2] <= .group$m[1] ||
         is.null(.group$w) || !is.numeric(.group$w) || length(.group$w) != 2 || .group$w[2] <= .group$w[1])  {
        warning('Group definition must conform to specifications in InterMAHP3 API manual')
        return(invisible(self))
      }

      if(!is.null(self$dg[[.name]])) {
        if(.name %in% c('entire', 'current', 'former')) {
          warning(c("Group name ", .name, " would override a base group."))
          return(invisible(self))
        }
        warning(c('Redefining group ', .name))
      }

      self$dg[[.name]] = .group

      ## Try to compute the group if fractions exist, but don't warn if not
      if(!is.null(self$af)) {
        self$cmp_group(.name)
      }

      invisible(self)
    },


    ## removes the given drinking group if it exists
    rm_group = function(.name) {
      ## sanitize the name into alphanumeric string
      .name = gsub('[^[:alnum:]]', '', as.character(.name))
      re_match = paste0('_', .name, '_')

      self$dg[[.name]] <- NULL
      for(af_name in names(self$af)) {
        dg_cols = grep(re_match, names(self$af[[af_name]]))
        self$af[[af_name]][dg_cols] = NULL
      }

      # warning(c('rm_group ', .name))

      invisible(self)
    },

    cmp_group = function(.name) {
      ## sanitize the name into alphanumeric string
      .name = gsub('[^[:alnum:]]', '', as.character(.name))

      ## If afs have been initialized then we compute groups for existing
      ## scenarios.  Otherwise this method does nothing.
      if(!is.null(self$af)) {
        for(.value in c(1, self$sn)) {
          ## When adding a drinking group, only add for already computed scenarios
          ## to ensure necessary values are computed in sheets.

          # Change in consumption as a suffix for variable names
          .suffix = sprintf("%01.4f",.value)

          .af_type = if(.value == 1) {'af'} else {'saf'}
          af_group = paste(.af_type, .name, .suffix, sep = '_')

          # Scenario integrand (summation values under this methodology)
          integrand = paste0('integrand_', .suffix)
          # AF component for current drinkers.  Sum of integrand
          comp_current = paste0('comp_current_', .suffix)
          # Denominator in AF computations, equal to 1 + FD component + CD component
          denominator = paste0('denominator_', .suffix)
          # Scenario attributable fraction for current drinkers
          saf_current = paste0(.af_type, '_current_', .suffix)

          ## init scenario gammas and binge gammas
          base_gamma = self$make_gamma(.value, F)
          binge_gammas = self$make_gamma(.value, T)

          ## Naming conventions for fractions:
          ##  *_#
          ##  *: the measure type (saf = scenario attributable fraction = fraction
          ##  with count adjustment applied)
          ##  #: the consumption level, multiplicative, 4 digits granularity

          for(.paf in c('base_paf', 'base_former_paf', 'binge_paf', 'binge_former_paf')) {
            if(!is.null(self$af[[.paf]])) {
              ## When adding a paf drinking group, only add for already computed
              ## scenarios to ensure that Values for integrand and denominator
              ## have already been initialized
              self$af[[.paf]] = mutate(
                self$af[[.paf]],
                (!! af_group) := pmap_dbl(
                  list(.g = gender, .v = eval(sym(integrand)), .d = eval(sym(denominator))),
                  function(.g, .v, .d) {
                    sum(.v[ceiling(self$dg[[.name]][[.g]][[1]]):ceiling(self$dg[[.name]][[.g]][[2]])])/.d
                  }
                )
              )
            }
          }

          for(.waf in c('base_scaled_waf', 'binge_scaled_waf')) {
            if(!is.null(self$af[[.waf]])) {
              ## When adding a waf drinking group, only add for already computed
              ## scenarios to ensure that Values for integrand, current component,
              ## and current attributable fraction have already been initialized
              self$af[[.waf]] = mutate(
                self$af[[.waf]],
                (!! af_group) := pmap_dbl(
                  list(.g = gender, .v = eval(sym(integrand)), .c = eval(sym(comp_current)), .s = eval(sym(saf_current))),
                  function(.g, .v, .c, .s) {
                    .s * sum(.v[ceiling(self$dg[[.name]][[.g]][[1]]):ceiling(self$dg[[.name]][[.g]][[2]])]) / .c
                  }
                )
              )
            }
          }

          # Calibrated
          if(!is.null(self$af$calibrated_waf)) {
            self$af$calibrated_waf = mutate(
              self$af$calibrated_waf,
              (!! af_group) := pmap_dbl(
                list(.g = gender, .v = eval(sym(integrand))),
                function(.g, .v) {
                  sum(.v[ceiling(self$dg[[.name]][[.g]][[1]]):ceiling(self$dg[[.name]][[.g]][[2]])])
                }
              )
            )
          }
        }
      } else {
        warning('Fractions must be initialized before groups may be computed')
      }

      invisible(self)
    },

    cmp_groups = function() {
      for(.name in names(self$dg)) {
        self$cmp_group(.name)
      }
    },

    ## Results -----------------------------------------------------------------
    ## Functions in this grouping relate to the presentation of results.  We
    ## output the data in long form, and also filter and shape the data for use
    ## in the Shiny App charting utility

    ## Provides all attributable fractions computed and performs post-hoc
    ## squamous cell carcinoma AAF correction
    get_afs = function() {
      .temp_af <- NULL
      for(.af in self$af) {
        .af_names = names(.af)[grep('^(saf|af)', names(.af))]
        .af = .af %>%
          select(c("im", "region", "year", "gender", "age_group", "outcome", .af_names))

        .af <- if(!is.null(self$scc) && '_22' %in% .af$im) {
          .af_22 = .af %>%
            filter(im == '_22') %>%
            gather('key', 'value', .af_names) %>%
            mutate(value = map2_dbl(value, gender, ~ .x * self$scc[[.y]])) %>%
            spread(key, value)
          .af_xx = .af %>%
            filter(im != '_22')

          bind_rows(.af_xx, .af_22)
        } else {
          .af
        }

        .temp_af <- bind_rows(.temp_af, .af)
      }
      .temp_af
    },


    ## Invokes get_afs and formats as long
    get_long_afs = function() {
      .wide_af = self$get_afs()
      .af_names = names(.wide_af)[grep('^(saf|af)', names(.wide_af))]

      .long_af = .wide_af %>% gather(key = 'af_key', value = 'af_value', .af_names)
      .long_af
    },

    ## Invokes get_long_afs and applied afs to counts
    get_long_counts = function() {
      if(is.null(self$mm)) {
        warning('Morbidity/mortality data required for this operation')
      } else {
        .long_af = self$get_long_afs()
        .long_ac = inner_join(
          .long_af,
          self$mm,
          by = c("im", "region", "year", "gender", "age_group", "outcome")
        ) %>%
          mutate(ac_value = af_value * count) %>%
          ## In the app we prefer separate status & scenarios variables over
          ## rewriting a lot of code
          mutate(status = gsub('^...?_([[:alnum:]]*).*', '\\1', af_key)) %>%
          mutate(sn = as.numeric(gsub('.*(.{6})$', '\\1', af_key))) %>%
          mutate(scenario = ifelse(sn == 1, 'Baseline', paste0(sprintf('%02.2+f', 100 * (sn - 1)), '%'))) %>%
          mutate(cc = str_sub(im, 2, 2)) %>%
          inner_join(
            tibble(
              # This is the canonical CC designation for the InterMAHP shiny app
              # if this is needed LITERALLY anywhere else, we need to include it
              # as a separate data object, or weave it onto the RR data
              cc = as.character(1:9),
              condition_category = c(
                "Communicable",
                "Cancer",
                "Endocrine",
                "Neuro",
                "Cardio",
                "Digestive",
                "Collisions",
                "Unintentional",
                "Intentional"
              )
            ),
            by = "cc"
          )
      }
    }
  )
)
