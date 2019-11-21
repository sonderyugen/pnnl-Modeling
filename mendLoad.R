#' Load command for integrating user specified data - will look for files named 'paramters', 'states', and 'timeseries'
#'
#' @param fileInput         // User specified file input source // can be passed as 'NULL' to ignore
#' @param fileTimeSeries    // User specified time series data input source for calibration // can be passed as 'NULL' to ignore
#' @param temp              // User specified value/range of values for temperature sensitivity
#' @param times             // time sequence
#' @param conditions        // initial temp/pH conditions
#' @param CUE               // User specified boolean for CUE
#'
#' @return out // standard input list array for parameters, states, statisitcs, etc for implementation in solver, calibration, sensitivity
#' @export
#' @importFrom readxl read_excel excel_sheets
#' @importFrom purrr map invoke_map set_names
#' @importFrom dplyr %>%
#'
#' @examples
#' varName <- mendLoad(file, sensitivity, temp, times, conditions, CUE)
#'
mendLoad <- function(fileInput, fileTimeSeries, temp, times, conditions, CUE) {
## standard starting values #################################################################################################
  out <- mendInitAnalytical(temp = temp, fileInput = fileInput)
#############################################################################################################################
## excel file import ########################################################################################################
  if(!is.null(fileInput)) {
  sourceInput <- mendRead(fileInput = fileInput)
  }
  parms <- c(sourceInput$parameters)
  states <- c(sourceInput$states)

  timeSeries <- list()
  if (!is.null(fileTimeSeries)){
    timeSeriesInput <- mendRead(fileInput = fileTimeSeries)
    for (i in 1:length(timeSeriesInput)){
      timeSeries[i] <- as.list(timeSeriesInput[i])
    }
    names(timeSeries) <- names(timeSeriesInput) }
  else { timeSeries <- fileTimeSeries }
#############################################################################################################################
  ## name/value assignments #################################################################################################
  st <- map(states, mean); sts <- out[[1]]$states
  statesOut <- mendLoadStates(st = st, sts = sts)

  pars <- out[[1]]$parameters
  parameters <- mendLoadPars(pars = pars,parms = parms)

  out[[1]]$states = statesOut
  out[[1]]$parameters = parameters
  out[[1]]$timeSeries = timeSeries
  return(out)
}
