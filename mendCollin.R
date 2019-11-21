#' function uniting deSolve package with FME package for complete model sensntivity analysis, calibration, and model output against user specified time-series data
#'
#' @param times     // initial time sequence for model analysis in 'mendModFull()'
#' @param states    // initial carbon-pool state values
#' @param pars      // initial model parameter values for 'mendModFull()'
#' @param ranges    // initial parameter statistics including mean, sd, min, max
#' @param timeData  // reference time-series data for 'FME::modFit()' via 'FME::modCost()'
#'
#' @return out // list object containing standard deSolve object from 'ode::deSolve' and updated list of parameter values used in creating output deSolve object
#' @export
#' @importFrom FME sensFun collin modCost modFit
#' @importFrom deSolve ode
#'
#' @examples
#' varName <- mendCollin(times, states, pars, ranges, timeData)
#'
mendCollin <- function(times, states, pars, ranges, timeData) {
  mendSenseObj <- function(parms, times, states, pars, timeData) {
    parms <- as.list(c(parms, pars))
    mendSolveFull <- function(times, states, parms) {
      return(deSolve::ode(y = states, times = times, func = mendMod, parms = parms, method = 'lsode', maxsteps = 100,000))
    }
    modOut <- mendSolveFull(times = times, states = states, parms = parms)
    return(FME::modCost(obs = timeData, model = modOut))
  }
  tData <- as.data.frame(timeData[[1]]); nameStates <- names(states[names(states) %in% colnames(tData)])
  tData <- tData[names(tData) %in% nameStates]; tData <- cbind(time = 1:nrow(tData), tData)

  rngMean <- ranges$mean
  parTest <- pars[names(pars) %in% names(rngMean)]; parLeft <- pars[!names(pars) %in% names(rngMean)]

  sF <- FME::sensFun(func = mendSenseObj, parms = parTest, sensvar = nameStates, varscale = 1, parscale = 1, times = times, states = states, pars = parLeft, timeData = tData)
  sf <- sF; sf <- sf[,-c(1,2)]; sf <- sf[,-(which(colSums(sf) == 0))]

  uncleC <- FME::collin(sf)

  col <- uncleC[uncleC[,"collinearity"] <20,]
  colMax <- col[col[,"N"] == max(col[,"N"]),]
  colMin <- colMax[colMax[, "collinearity"] == min(colMax[,"collinearity"]),]
  colPars <- c(names(colMin[,colMin == 1]))

  rngMin <- ranges$minimum; rngMax <- ranges$maximum; rngStd = ranges$sd
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
  ranges$mean <- rngMean; ranges$sd <- rngStd; ranges$minimum <- rngMin; ranges$maximum <- rngMax

  meanPars <- rngMean[names(rngMean) %in% colPars]
  minPars <- rngMin[names(rngMin) %in% colPars]; maxPars <- rngMax[names(rngMax) %in% colPars]

  parRight <- pars[!names(pars) %in% names(meanPars)]
  parData <- modFit(f = mendSenseObj, p = c(meanPars), times = times, states = states, pars = parRight, timeData = tData, lower = c(minPars), upper = c(maxPars), method = "L-BFGS-B")

  par <- pars; parFit <- parData$par; namePar <- names(parFit)
  for (i in 1:length(namePar)) {
      par[[namePar[[i]]]] <- parFit[[namePar[[i]]]]
  }

  out <- list()
  sense <- list(sf = sf, col = uncleC); out$sense <- sense
  parameters <- list(parameters = par, ranges = ranges); out$parameters <- parameters
  return(out)
}
