#' Boiler-plate hard coded inital spin-up values per orignal MEND authors
#'
#' @param temp      // initial reference temperature paramters
#' @param fileInput // input file for time series data
#'
#' @return out // standard list array of parameters, states, ranges for solving, calibrating, optimizing with 'mendSolve()'
#' @export
#'
#' @examples
#' varName <- mendInitAnalytical(temp, fileInput)
#'
mendInitAnalytical <- function(temp, fileInput) {
## inital parameters ######################################################################################
parameters <- mendParameters()
###########################################################################################################
## parameter statistics####################################################################################
ranges <- mendStats(parameters)
##########################################################################################################
## inital states #########################################################################################
# via steady-state analytical calculations
statesOut <- mendSteadyState(parameters = parameters, temp = temp)
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

