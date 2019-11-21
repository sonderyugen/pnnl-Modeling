#' Least Squares Minimization and Calibration of Model along experimental data
#'
#' @param timeData      // time series data along which solveModel will be calibrated
#' @param times         // time sequence
#' @param states        // inital state pool values
#' @param parameters    // initial paramter values
#' @param ranges        // initial parameter statistics
#' @param calOpt        // booloean for optimizing the parameters
#'
#' @return calData      // object containing updated parameters, parameter statistics, and calibration data via 'mendCollin()'
#' @export
#' @importFrom optimx optimx
#'
#' @examples
#' varName <- mendMin(timeData, times, states, parameters, ranges, calOpt)
#'
mendMin <- function(timeData, times, states, parameters, ranges, calOpt) {
  mendModOut <- list()
  rngMean <- ranges$mean; nameList <- names(rngMean)
  rngStd <- ranges$sd; names(rngStd) <- nameList
  rngMax <- ranges$maximum; names(rngMax) <- nameList
  rngMin <- ranges$minimum; names(rngMin) <- nameList

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

  pars <- parameters
  if (calOpt) {
    nameList <- names(rngMean)
    p <- c(parameters[[1]]); pars <- p[!names(p) %in% nameList]
    names(pars) <- names(p[!names(p) %in% nameList])

    minOut <-  optimx::optimx(par = as.vector(rngMean), fn = mendOpt, gr = NULL, hess = NULL, lower = as.vector(rngMin), upper = as.vector(rngMax), method = "L-BFGS-B", itnmax = NULL, hessian = FALSE, control = list(trace = 2, maximize = FALSE),
                              times = times, states = states, parameters = pars, namelist = nameList, avg = timeData[[1]], model = "maximum", func = mendMod)

    minPar <-  c(minOut[1:length(nameList)])
    names(minPar) <-  nameList

    for (i in 1:length(nameList)) {
        pars[[nameList[[i]]]] <- minPar[[nameList[[i]]]]
    }
  }

  calData <- mendCollin(times = times, states = states, pars = pars, ranges = ranges, timeData = timeData)

  return(calData)
}
#testCalPlot <- testCalCorrect$solve[[1]]
#testCalCal <- testCalCorrect$calibrate[[1]]
## optional argument 'minCal' not currently implemented due to functions #########################################################
#  calibrate <- list()
#  rangePar <- as.matrix(data.frame(minimum = rngMin, maximum = rngMax)); names(rangePar) <- names(rngMean)
#  len <- min(length(timeData), length(solveData))
#  for (i in 1:len) {
#    sData <- solveData[[i]]; tData <- as.list(timeData[[i]])
#    design <- apply(sData, 2, as.list); design <-  design[names(design) %in% names(tData)]; design <- lapply(design, as.data.frame)
#    obs <- tData[names(tData) %in% names(design)]
#    c <- RobustCalibration::rcalibration_MS(design = design, observations = obs, index_theta = parameters, p_theta = length(parameters),
#                                              have_trend = rep(FALSE, length(design)), simul_type = c(rep(0, length(design))),
#                                              math_model = NULL, theta_range = rangePar)
##c <- RobustCalibration::rcalibration_MS(design = design, observations = obs, index_theta = parameters, p_theta = length(parameters),
##                                         have_trend = rep(FALSE, length(design)), simul_type = c(rep(1, length(design))),
##                                         math_model = mendMod, theta_range = rangePar)
#    calibrate[[i]] <- c
#  }
