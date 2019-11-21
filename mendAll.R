#' Solver for mendMod with all results returned
#'
#' @param times        // time sequence
#' @param states       // inital state pool values
#' @param parameters   // intial paramter values
#' @param ranges       // intial statistical value for paramters
#' @param maxOpt       // boolean for paramter optimization (TRUE) or approximation (FALSE)
#' @param parMax       // updated starting values for temperature specific maximum/minimum optimization based on initial reference temperature value for efficiency
#'
#' @return mendModOut // a list containing the steady state, maximum, minimum, and average solution for this paramter set
#' @export
#'
#' @examples
#' varName <- mendAll(times, states, paramters, ranges, maxOpt, parMax)
#'
mendAll <- function(times, states, parameters, ranges, maxOpt, parMax) {
  steady <- mendSteady(times = times, states = states, parameters = parameters)
  states <- steady$states

  ranges <- mendRange(times = times, states = states, parameters = parameters, ranges = ranges, maxOpt = maxOpt, parMax = parMax)

  mendModOut <- list(steady = steady, avg = ranges$avg, min = ranges$min, max = ranges$max)
  return(mendModOut)
}
