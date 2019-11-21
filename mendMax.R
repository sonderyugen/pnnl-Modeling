#' Maximum/minimum value optimzer/solver for mendMod
#'
#' @param times        // time sequence
#' @param states       // inital pool state values
#' @param parameters   // inital parameter values
#' @param ranges       // intial parameter/condition/state standard deviation values
#' @param model        // user specified model output value(s)
#' @param maxOpt       // user specified boolean for optimizing range model(s)
#' @param avg          // user specified reference data values
#' @param parMax       // updated starting values for temperature specific maximum/minimum optimization based on initial reference temperature value for efficiency
#'
#' @return modMendOut  // standard deSolve objecgt
#' @importFrom optimx optimx
#' @importFrom deSolve ode
#' @export
#'
#' @examples
#' varName <- mendMax(times, states, paramters, ranges, parMax)
#' varName <- mendMax(times, states, parameters, ranges, model, maxOpt, avg = dataVar, parMax)  //mendMax will run mendMod output through the solver unless a reference data set is passed using 'data' argument
#'
mendMax <- function(times, states, parameters, ranges, model = "Maximum", maxOpt = FALSE, avg = NULL, parMax) {
  mendModOut <- list()
  rngMean <- ranges$mean; nameList <- names(rngMean)
  rngStd <- ranges$sd; names(rngStd) <- nameList
  rngMin <- ranges$minimum; names(rngMin) <- nameList
  rngMax <- ranges$maximum; names(rngMax) <- nameList

  rngMax <- pmax(rngMax, (rngMean + rngStd))
  if (any(rngMax < 0)) {
    rngSwap <- pmin(rngMax, (rngMean + rngStd))
    rngMax[rngMax < 0] <- rngSwap[names(rngSwap) %in% names(rngMax[rngMax < 0])]
    if(any(rngMax < 0)) {
      rngSwap <- pmin(rngMean[names(rngMean) %in% names(rngMax[rngMax < 0])], abs(rngMax[rngMax < 0]))
      rngSwap <- pmax(rngSwap, min(rngMax[rngMax > 0]))
      names(rngSwap) <- names(rngMax[rngMax < 0])
      rngMax[rngMax < 0] <- rngSwap[names(rngSwap) %in% names(rngMax[rngMax < 0])]
    }
  }

  rngMin <- pmin(rngMin, (rngMean - rngStd))
  if (any(rngMin < 0)) {
    rngSwap <- pmax(rngMin, (rngMean - rngStd))
    rngMin[rngMin < 0] <- rngSwap[names(rngSwap) %in% names(rngMin[rngMin < 0])]
    if(any(rngMin < 0)) {
      rngSwap <- pmin(rngMean[names(rngMean) %in% names(rngMin[rngMin < 0])], abs(rngMin[rngMin < 0]))
      rngSwap <- pmin(rngSwap, min(rngMin[rngMin > 0]))
      names(rngSwap) <- names(rngMin[rngMin < 0])
      rngMin[rngMin < 0] <- rngSwap[names(rngSwap) %in% names(rngMin[rngMin < 0])]
    }
  }

  nameList <- names(rngMean)

  if(maxOpt && is.null(parMax)) {
    if (is.null(avg)) {
      avg <-  deSolve::ode(y = states, times = times, func = mendMod, parms = parameters, method = "lsode", maxsteps = 100,000)
    }

    par <- parameters[!names(parameters) %in% nameList]
    maxOut <-  optimx::optimx(par = as.vector(rngMean), fn = mendOpt, gr = NULL, hess = NULL, lower = as.vector(rngMin), upper = as.vector(rngMax), method = "L-BFGS-B", itnmax = NULL, hessian = FALSE, control = list(trace = 2, maximize = TRUE),
                            times = times, states = states, parameters = par, namelist = nameList, avg = avg, model = model, func = mendMod)

    maxPar <-  c(maxOut[1:length(nameList)])
    names(maxPar) <-  nameList

    optPars <- parameters
    for (i in 1:length(nameList)) {
      optPars[[nameList[i]]] <- maxPar[[nameList[i]]]
    }
    solveOpt <- deSolve::ode(y = states, parms = optPars, times = times, func = mendMod, method = "lsode", maxsteps = 100,000)

    solveMax <- list()
    solveMax$parameters <- optPars
    solveMax$solve <- solveOpt
  } else if (maxOpt && !is.null(parMax)){
      if(model == "Maximum") {
        maxPar <- parMax$max }
      else if (model == "Minimum") {
        maxPar <- parMax$min }
      else {print("Error in 'mendMax': incorrect value for 'model' argument.")}  ## exit option?

      solveOpt <- deSolve::ode(y = states, parms = maxPar, times = times, func = mendMod, method = "lsode", maxsteps = 100,000)

      solveMax <- list()
      solveMax$parameters <- parMax; solveMax$solve <- solveOpt
  } else{solveMax <- NULL}


  if(model == "Maximum") {
    maxPar <- rngMax }
  else if (model == "Minimum") {
    maxPar <- rngMin }
  else {print("Error in 'mendMax': incorrect value for 'model' argument.")}  ## exit option?

  pars <- parameters
  for (i in 1:length(nameList)) {
      pars[[nameList[i]]] <- maxPar[[nameList[i]]]
  }
  solveData <- deSolve::ode(y = states, parms = pars, times = times, func = mendMod, method = "lsode", maxsteps = 100,000)
  solveStd <- list(); solveStd$solve <- solveData; solveStd$parameters <- pars

  mendModOut$solve <-  solveStd; mendModOut$opt <- solveMax
  return(mendModOut)
}
