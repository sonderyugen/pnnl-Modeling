#' Solver root for implementing solver with established states, parameters, and conditions
#'
#' @param times         // time sequence
#' @param states        // inital pool state values
#' @param parameters    // intial paramter values including model parameters, environmental conditions (contained in paramters$parameters), and ranges (contained in parameters$ranges)
#' @param model         // user specified model output value(s)
#' @param maxOpt        // boolean for optimizing the parameter values for mendMax or using statistical range approximations
#' @param parMax        // optional parameter set for swifter evaluation of 'maxOpt = TRUE' via 'mendMax()' of multiple temperature models
#'
#' @return solveMendOut // list of return data for all solution sets minimum, average, maximum per initial argument 'model'
#' @export
#'
#' @examples
#' varName <- mendSolve(times, states, paramters, model, maxOpt, parMax)
#'
mendSolve <- function(times, states, parameters, model, maxOpt, parMax) {
  params = c(as.list(parameters$parameters), as.list(parameters$conditions), CUE = parameters$CUE)
  ranges = parameters$ranges

  solveMendOut <-  switch(model,
                      "Maximum" = ,
                      "Minimum" = mendMax(times = times, states = states, parameters = params, ranges = ranges, model = model, maxOpt = maxOpt, parMax = parMax),
                      "Average" = mendStatic(times = times, states = states, parameters = params),
                      "Steady" = mendSteady(times = times, states = states, parameters = params),
                      "Ranges" = mendRange(times = times, states = states, parameters = params, ranges = ranges, maxOpt = maxOpt, parMax = parMax),
                      "All" = mendAll(times = times, states = states, parameters = params, ranges = ranges, maxOpt = maxOpt, parMax = parMax),
                      print("Incorrect value for 'model' argument.")  ## exit option?
  )
  return(solveMendOut)
}
