#' Boiler-plate hard coded inital spin-up values per orignal MEND authors
#'
#' @param fileInput // input file for time series data
#'
#' @return out // standard list array of parameters, states, ranges for solving, calibrating, optimizing with 'mendSolve()'
#' @export
#'
#' @examples
#' varName <-  mendInitApproximate(fileInput)  #load inital spin-up parameter values to variable for implementation in mendSolve()
#'
mendInitApproximate <- function(fileInput) {
## inital spin up values provided by MEND original authors
## inital states and parameters ###########################################################################
  Q = 0.1; D = 1; M = 5; P = 10; B = 2; EP = 10^(-5); EM = 10^(-5)  ## initial pool values (mg C/g soil)
  P.lignin = P/2; P.cellulose = P/2
  T = Q + D + M + P + B + EP + EM
  statesOut <- c(Q = Q, D = D, M = M, P.lignin = P.lignin, P.cellulose = P.cellulose, P = P, B = B, EP = EP, EM = EM, C = 0, T = T) ##, K = T.K)
###########################################################################################################
  parameters <- mendParameters()
###########################################################################################################
## parameter ranges #######################################################################################
  ranges <- mendStats(parameters)
##########################################################################################################
## output ################################################################################################
  if(!is.null(fileInput)) {
    timeSeries <- fileInput
    timeSeries <- timeSeries %>%
      excel_sheets() %>%
      set_names() %>%
      map(read_excel, path = timeSeries)
  } else timeSeries <- fileInput

  parametersOut <- list(parameters = parameters, ranges = ranges)

  out <- list()
  out[[1]] <- list(states = statesOut, parameters = parametersOut, timeSeries = timeSeries)
return(out)
}
