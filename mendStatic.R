#' Mean Value system solver for 'mendMod()'
#'
#' @param times        // time sequence
#' @param states       // inital pool state values
#' @param parameters   // intial parameter values
#'
#' @return modMendOut
#' @export
#' @importFrom deSolve ode
#'
#' @examples
#' varName <- mendState(times, states, parameters)
#'
mendStatic <- function (times, states, parameters) {

  mendModOut <- deSolve::ode(y = states, times = times, func = mendMod, parms = parameters, method = "lsode", maxsteps = 100,000)

return(mendModOut)
}
