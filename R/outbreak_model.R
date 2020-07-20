
#' Run a single instance of the branching process model
#' @author Joel Hellewell
#' @inheritParams outbreak_step
#' @param num.initial.cases How many cases to start with.
#' @param inc_meanlog shape of distribution for incubation period.
#' @param inc_sdlog scale of distribution for incubation period.
#' @param cap_max_days Max number of days to run the simulation.
#' @param cap_cases After reaching this cap, assume the epidemic continues to grow.
#' @param delay_shape Probability of adherence to isolation after symptom onset when not tracked.
#' @param delay_scale Doesnt do anything and should be removed.
#' @param
#' @return data.table of cases by week, cumulative cases, and the effective reproduction number of the outreak
#' @export
#'
#' @importFrom data.table rbindlist
#' @importFrom purrr partial
#' @examples
#'
#'\dontrun{
#' incfn <- dist_setup(dist_param1 = 2.322, dist_param2 = 6.492, dist_type = 'weibull')
#' # delay distribution sampling function
#' delayfn <- dist_setup(2, 4, 'weibull')
#' # generate initial cases
#' case_data <- outbreak_setup(num.initial.cases = 5,
#'                             incfn=incfn,
#'                             delayfn = delayfn,
#'                             prop.asym=0)
#' # generate next generation of cases
#' case_data <- outbreak_step(case_data = case_data,
#'                            disp.iso = 1,
#'                            disp.com = 0.16,
#'                            r0isolated = 0,
#'                            r0community = 2.5,
#'                            prop.asym = 0,
#'                            incfn = incfn,
#'                            delayfn = delayfn,
#'                            inf_rate = 2,
#'                            inf_shape = 2,
#'                            inf_shift = 3,
#'                            prop.ascertain = 0,
#'                            quarantine = FALSE)
#'}

outbreak_model <- function(num.initial.cases = NULL, prop.ascertain = NULL,
                           cap_max_days = NULL, cap_cases = NULL,
                           r0isolated = NULL, r0community = NULL,
                           disp.iso = NULL, disp.com = NULL,
                           delay_shape = NULL, delay_scale = NULL,
                           inc_meanlog = NULL, inc_sdlog = NULL,
                           prop.asym = NULL, inf_shape = NULL,
                           inf_rate = NULL, inf_shift = NULL,
                           min_quar_delay = 1, max_quar_delay = NULL,
                           test_delay = NULL, sensitivity = NULL,
                           precaution = NULL, self_report = NULL,
                           quarantine = NULL, testing = NULL,
                           earlyOut = NULL, iso_adhere = NULL) {

  # Set up functions to sample from distributions
  # incubation period sampling function
  incfn <- dist_setup(dist_param1 = inc_meanlog,
                      dist_param2 = inc_sdlog,
                      dist_type = 'lognormal')
  # incfn <- dist_setup(dist_shape = 3.303525,dist_scale = 6.68849) # incubation function for ECDC run
  # onset to isolation delay sampling function
  delayfn <- dist_setup(delay_shape,
                        delay_scale,
                        "adherence")

  # Set initial values for loop indices
  total.cases <- num.initial.cases
  latest.onset <- 0
  extinct <- FALSE

  # Initial setup
  case_data <- outbreak_setup(num.initial.cases = num.initial.cases,
                              incfn = incfn,
                              prop.asym = prop.asym,
                              delayfn = delayfn,
                              sensitivity = sensitivity,
                              precaution = precaution,
                              test_delay = test_delay,
                              self_report = self_report,
                              testing = testing,
                              iso_adhere = iso_adhere)

  # Preallocate
  effective_r0_vect <- c()
  cases_in_gen_vect <- c()


  # Model loop
  while (latest.onset < cap_max_days & total.cases < cap_cases & !extinct) {

    out <- outbreak_step(case_data = case_data,
                         disp.iso = disp.iso,
                         disp.com = disp.com,
                         r0isolated = r0isolated,
                         r0community = r0community,
                         incfn = incfn,
                         delayfn = delayfn,
                         inf_shape = inf_shape,
                         inf_rate = inf_rate,
                         inf_shift = inf_shift,
                         prop.ascertain = prop.ascertain,
                         quarantine = quarantine,
                         prop.asym = prop.asym,
                         min_quar_delay = min_quar_delay,
                         max_quar_delay = max_quar_delay,
                         sensitivity = sensitivity,
                         precaution = precaution,
                         test_delay = test_delay,
                         self_report = self_report,
                         iso_adhere = iso_adhere,
                         testing = testing)


    case_data <- out[[1]]
    effective_r0_vect <- c(effective_r0_vect, out[[2]])
    cases_in_gen_vect <- c(cases_in_gen_vect, out[[3]])
    total.cases <- nrow(case_data)
    latest.onset <- max(case_data$onset)
    extinct <- all(case_data$isolated)
  }

  if(earlyOut==TRUE){
    first_iso <- Inf
    early_missed <- nrow(case_data)
    if(any(case_data$missed==FALSE & case_data$isolated_time<Inf)){
      # time of first case detection
      first_iso <- case_data[missed==FALSE,][isolated_time==min(isolated_time),isolated_time]
      # number of cases exposed before first case detection
      early_missed <- nrow(case_data[exposure<first_iso,])
    }
  }

  #calculate time to test (from exposure) for each tested case
  #timetotest <- case_data[(isolated_time < Inf & !is.na(test_result)),(isolated_time - exposure) + test_delay,]

  # Prepare output, group into weeks
  weekly_cases <- case_data[, week := floor(onset / 7)
                            ][, .(weekly_cases = .N, tested = numTested(test_result),
                                  positive = numPositive(test_result),
                                  isolated = numOccur(isolated_time),
                                  released = numOccur(isolated_end)), by = week
                              ]

  # maximum outbreak week
  max_week <- floor(cap_max_days / 7)
  # weeks with 0 cases in 0:max_week
  missing_weeks <- (0:max_week)[!(0:max_week %in% weekly_cases$week)]

  # add in missing weeks if any are missing
  if (length(missing_weeks > 0)) {
    weekly_cases <- data.table::rbindlist(list(weekly_cases,
                                               data.table(week = missing_weeks,
                                                          weekly_cases = 0,
                                                          tested = 0,
                                                          positive = 0,
                                                          isolated = 0,
                                                          released = 0)))
  }
  # order and sum up
  weekly_cases <- weekly_cases[order(week)
                               ][, cumulative := cumsum(weekly_cases)]
  # cut at max_week
  weekly_cases <- weekly_cases[week <= max_week]

  # Add effective R0
  weekly_cases <- weekly_cases[, `:=`(effective_r0 = mean(effective_r0_vect,
                                                          na.rm = TRUE),
                                      cases_per_gen = list(cases_in_gen_vect))]#,
                                      #timetotest = ifelse(length(timetotest)<1, 0,
                                      #                    c(list(timetotest),rep(0,max_week))))]

  if(earlyOut==TRUE){
    weekly_cases <- weekly_cases[, `:=`(first_iso = first_iso,
                                        early_missed = early_missed)]
  }

  # return
  return(weekly_cases)
}

numTested <- function(results) {
  return(length(which(!is.na(results))))
}

numPositive <- function(results){
  return(length(which(results[which(!is.na(results))] == TRUE)))
}

numOccur <- function(results){
  return(length(which(results < Inf)))
}
