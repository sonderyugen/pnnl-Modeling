#' Average, minimum, and maximum optimizer/solver for mendMod
#'
#' @param times        // time sequence
#' @param states       // inital pool state values
#' @param parameters   // intial parameter values
#' @param ranges       // intial state/parameter/condition standard deviation values
#' @param maxOpt       // boolean for optimizing ranges (TRUE) or approximation (FALSE)
#' @param parMax       // updated starting values for temperature specific maximum/minimum optimization based on initial reference temperature value for efficiency
#'
#' @return mendModOut  // list of standard deSolve objects
#' @export
#'
#' @examples
#' varName <- mendRange(times, states, paramters, ranges, maxOpt, parMax)
#'
mendRange <- function(times, states, parameters, ranges, maxOpt, parMax) {

  if(!is.null(parMax)) {
    parametersMin <- parMax$min
    parametersMax <- parMax$max
  } else {
    parametersMin <- parameters
    parametersMax <- parameters
  }

  avg = mendStatic(times = times, states = states, parameters = parameters)
  min = mendMax(times = times, states = states, parameters = parametersMin, ranges = ranges, model = "Minimum", avg = avg, maxOpt = maxOpt, parMax = parMax)
  max = mendMax(times = times, states = states, parameters = parametersMax, ranges = ranges, model = "Maximum", avg = avg, maxOpt = maxOpt, parMax = parMax)

  mendModOut = list(avg = avg, min = min, max = max)
  return(mendModOut)
}
