#' Set up initial cases for branching process
#' @author Emma Davis and Tim Lucas (from Joel Hellewell)
#' @inheritParams outbreak_step
#' @param num.initial.cases Integer number of initial cases
#' @param incfn function that samples from incubation period Weibull distribution; generated using dist_setup
#' @param delayfn function generated using dist_setup = 1 or Inf (adherence to isolation)
#' @param prop.asym Numeric proportion of cases that are sublinical (between 0 and 1)
#'
#' @return data.table of cases in outbreak so far
#' @export
#' @importFrom data.table data.table as.data.table
#'
#' @examples
#'
#'\dontrun{
#' # incubation period sampling function
#' incfn <- dist_setup(dist_shape = 2.322737,dist_scale = 6.492272)
#' # delay distribution sampling function
#' delayfn <- dist_setup(delay_shape, delay_scale)
#' outbreak_setup(num.initial.cases = 5,incfn,delayfn,k=1.95,prop.asym=0)
#'}
outbreak_setup <- function(num.initial.cases, incfn, delayfn, prop.asym, sensitivity, precaution, test_delay, self_report, testing) {

  # Column names used in nonstandard eval. These should go in globaVariables in scenario_sim.R
  test_result <- isolated_end <- infector_iso_end <- delays <- NULL
  delays_traced <- test <- time_to_test <- test_result <- isolated_end <- NULL

  # Set up table of initial cases
  inc_samples <- incfn(num.initial.cases)

  case_data <- data.table(exposure = rep(0, num.initial.cases), # Exposure time of 0 for all initial cases
                          asym = purrr::rbernoulli(num.initial.cases, prop.asym),
                          caseid = 1:(num.initial.cases), # set case id
                          infector = 0,
                          onset = inc_samples,
                          new_cases = NA,
                          missed = NA,
                          test_result = NA)

  # precalculate who will self-isolated if symptomatic and how long after onset, returns Inf if never isolate
  adhere <- delayfn(num.initial.cases)

  # you never isolate if asymptomatic, but isolate at time 'adhere' after onset if symptomatic
  case_data <- case_data %>% mutate(isolated_time = ifelse(asym==FALSE,
                                                           onset + adhere,
                                                           Inf))

  # you are missed if you never isolate, but if you isolate you'll be detected (not missed) with probability self_report
  case_data <- case_data %>%
    mutate(missed = ifelse(isolated_time < Inf,
                           rbernoulli(sum(isolated_time<Inf), 1 - self_report),
                           TRUE))

  # if testing is happening then detected (not missed) cases are tested, missed cases are not
  if(testing==TRUE){
    case_data <- case_data %>%
      mutate(test_result = ifelse(missed==FALSE,
                                   purrr::rbernoulli(sum(!missed), sensitivity),
                                   NA))
  }

  # end isolation never (Inf) if test positive or missed (isolation start also Inf)
  # if test negative then end isolation a precautionary period ('precaution') after isolation start
  case_data <- case_data %>%
    mutate(isolated_end = isolated_time+ifelse(vect_isTRUE(test_result) | missed==T,Inf,precaution)) %>%
    mutate(isolated = FALSE) # initialise so all cases are initially able to transit (effectively isolate once their secondary cases have been calculated and assigned)

  case_data <- as.data.table(case_data)
  # return
  return(case_data)
}
