#' Senseitivity range analysis for parameters with statistics.  Returns senstivity range plots via 'FME::sensRange()'
#'
#' @param times       // initial time sequence
#' @param states      // initial carbon pool state values
#' @param parameters  // initial parameter values for model function 'mendMod()'
#' @param ranges      // initial parameter statistics including mean, sd, min, max
#' @param sensePar    // User specified boolean for individial parameter sensitity analysis via 'FME::sensRange()'
#'
#' @return parSense // list object containing sensitivity output for each parameter vai 'FME:sensRange()', 'FME::sensFun()', and 'FME::collin()'
#' @export
#' @importFrom FME sensRange sensFun collin
#'
#' @examples
#' varName <- mendSense(times, states, paramters, ranges, sensePar)
#'
mendSense <- function(times, states, parameters, ranges, sensePar) {
  mendSenseFunc <- function(pars, states, times) {
    return(deSolve::ode(y = states, times = times, func = mendMod, parms = pars))
  }

  rngMean <- ranges$mean; nameList <- names(rngMean)
  rngStd <- ranges$sd; names(rngStd) <- nameList
  rngMin <- ranges$minimum; names(rngMin) <- nameList
  rngMax <- ranges$maximum; names(rngMax) <- nameList

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

  parRanges <- data.frame(min = c(rngMin), max = c(rngMax))
  rownames(parRanges) <- nameList

  nameStates <- names(states)

  if(sensePar) {
    parSense <- list();
    for (i in 1:length(rngMean)) {
      parSense[i] <- FME::sensRange(func = mendSenseFunc, parms = as.list(parameters), dist = "grid", sensvar = nameStates, parRange = parRanges[i,], num = 100, states = states, times = times)
    }
  } else {parSense <- NULL}

  parSenses <- FME::sensRange(func = mendSenseFunc, parms = as.list(parameters), dist = "latin", sensvar = nameStates, parRange = parRanges, num = 100, states = states, times = times)

  parFun <- sensFun(func = mendSenseFunc, parms = parameters, sensvar = nameStates, varscale = 1, parscale = 1, states = states, times = times)
  sf <- parFun; sf <- sf[,-c(1,2)]; sf <- sf[,-(which(colSums(sf) == 0))]

  parCol <- collin(sf)

  senseList <- list()
  senseList$sese <- parSense; senseList$senses <- parSenses; senseList$fun <- parFun; senseList$col <- parCol
  return(senseList)
}
#sensVar <- c(names(ranges$mean))
#parRanges <- data.frame(min = c(ranges$minimum); max = c(ranges$maximum)); rownames(parRanges) <- c(senseVar)
#sR <- FME::sensRange(func = mendSolveFull, parms = pars, dist = "grid", sensevar = senseVar, parRange = parRanges, num = 50)
##return(sR)
