#' Function for otpimx systems optimizer 'func' argument
#'
#'
#' @param times         // inital time sequence
#' @param states        // inital pool state values
#' @param parameters    // intial parameter values not optimized
#' @param namelist      // optimx doesn't return the evaluated parameters w/names, required by reference in mendMod
#' @param avg           // reference data for objective function
#' @param model         // user specified model output value(s)
#' @param func          // MEND ordinary differential equations function mendMod() for implementation with optimx
#'
#' @return ret // cost function value
#' @export
#' @importFrom deSolve ode
# #' @importFrom rootSolve rootSolve? // optional method to allow optimizing against steady state output // optional approach to evaluate from steady state output // not currently implemented0
#'
#' @examples
#' mendOpt(times, states, parameters, namelist, avg, model, func)
#'
mendOpt <- function(ranges, times, states, parameters, namelist, avg, model, func) {
  rng <- ranges
  names(rng) <- namelist
  parms <- c(as.list(parameters), as.list(rng))

  max <-  deSolve::ode(y = states, times = times, func = func, parms = parms, method = "lsode", maxsteps = 100,000)

  if(model == "maximum") {
   ret = sum((as.vector(max) - as.vector(avg))^2) }
  else if (model == "Maximum") {
    ret = sum((as.vector(max) - as.vector(avg))) }
  else if (model == "Minimum") {
   ret = sum((as.vector(avg) - as.vector(max))) }
  else {print("Error in 'mendOpt': incorrect value for 'model' argument.")}  ## exit option?

  return(ret)
}

## possible objective function alternatives?
#  max <-  rootSolve::rootsolve(y = states, times = times, func = func, parms = parms, method = "lsode")
#  max <-  deSolve::ode(y = states, times = times, func = func, parms = pars, method = "lsode")
#  ret <- sum((max - avg)^2)
