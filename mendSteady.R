#' Steady-state solver for 'mendMod()' systems of equations function
#'
#' @param times         // time sequence
#' @param states        // inital pool state values
#' @param parameters    // intial paramter values including model parameters, environmental conditions (contained in paramters$parameters), and ranges (contained in parameters$ranges)
#'
#' @return  modMendOut
#' @export
#' @importFrom rootSolve steady
#'
#' @examples
#' varName <- mendSteady(times, states, parameters)
#'
mendSteady <- function(times, states, parameters) {
  mendModOut <- list()

  time <- c(times[[1]], times[[length(times)]])
  rootData <- rootSolve::steady(y = states, func = mendMod, times = time, parms = parameters, method = "runsteady")
  states <- rootData$y

  solveData <- deSolve::ode(y = states, times = times, func = mendMod, parms = parameters, method = "lsode", maxsteps = 100,000)

  mendModOut$solve <- solveData; mendModOut$states <- states; mendModOut$roots <- rootData
  return(mendModOut)
}
