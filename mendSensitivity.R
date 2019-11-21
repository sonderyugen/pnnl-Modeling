## #' Parameteric ODE sensitivity solver for mendMod()
## #'
## #' @param times       // time sequence
## #' @param states      // initial carbon-pool state values
## #' @param parameters  // initial parameter values
## #' @param ranges      // specified/available parameter ranges
## #'
## #' @return ranges // modified ranges based on sensitivity analysis
## #' @export
## #' @importFrom ODEsensitivity ODEmorris
## #'
## #' @examples
## #' varName <- mendSensitivityODE(times, states, parameters, ranges)
## #'
#mendSensitivityODE <- function(times, states, parameters, ranges) {
#  means <- ranges$mean; sd <- ranges$sd; minimum <- ranges$minimum; maximum <- ranges$maximum
#
#  ##  test and replace for negative range values
#  maximum <- pmin(maximum, (means + sd))
#  if (any(maximum < 0)) {
#    rngSwap <- pmax(maximum, (means + sd))
#    maximum[maximum < 0] <- rngSwap[names(rngSwap) %in% names(maximum[maximum < 0])]
#    if(any(maximum < 0)) {
#      rngSwap <- pmin(means[names(means) %in% names(maximum[maximum < 0])], abs(maximum[maximum < 0]))
#      rngSwap <- pmax(rngSwap, min(maximum[maximum > 0]))
#      names(rngSwap) <- names(maximum[maximum < 0])
#      maximum[maximum < 0] <- rngSwap[names(rngSwap) %in% names(maximum[maximum < 0])]
#    }
#  }

#  minimum <- pmin(minimum, (means - sd))
#  if (any(minimum < 0)) {
#    rngSwap <- pmax(minimum, (means - sd))
#    minimum[minimum < 0] <- rngSwap[names(rngSwap) %in% names(minimum[minimum < 0])]
#    if(any(minimum < 0)) {
#      rngSwap <- pmin(means[names(means) %in% names(minimum[minimum < 0])], abs(minimum[minimum < 0]))
#      rngSwap <- pmin(rngSwap, min(minimum[minimum > 0]))
#      names(rngSwap) <- names(minimum[minimum < 0])
#      minimum[minimum < 0] <- rngSwap[names(rngSwap) %in% names(minimum[minimum < 0])]
#    }
#  }
##################################################################################################################################
## sensitivity analysis ##########################################################################################################
#  nameListAll <-names(parameters)
#  maxs <- mean(maximum); mins <-  min(minimum)
#  outputSensitivityAll <- ODEsensitivity::ODEmorris(mod = mendMod, pars = nameListAll, state_init = states, times = times, binf = mins, bsup = maxs,
#                                   ode_method = "lsode", maxsteps = 100000)

#  return(ranges)
##################################################################################################################################
## ODEmorris framework for evaluating only parameters for which statistical data is available ####################################
#  nameListRanges = names(means)
#  pars <- parameters[!names(means) %in% names(parameters)]
#  outputSensitivityRanges <- ODEsensitivity::ODEmorris(mod = mendModSensitivity, pars = nameListRanges, state_init = states, times = times, binf = minimum, bsup = maximum,
#                                      ode_method = "lsode", maxsteps = 100000,
#                                      vars = pars)
#}
##################################################################################################################################




##################################################################################################################################
## alternative sensitivity analysis methods ######################################################################################
# #' State variable sensitivity analysis via r-package 'multisensi' command 'multisensi()'
# #'
# #' @param times      // time sequence
# #' @param states     // initial carbon-pool state values
# #' @param paramters  // initial paramter values
# #'
# #' @return sensitivity // standard list of identified state sensitivity values
# #' @export
# #' @importFrom multisensi multisensi
# #'
# #' @examples
# #' varName <- mendSensitivityState(times, states, parameters)
# #'
#mendSensitivityState <- function(times, states, parameters) {
#  outputSensitivity <- multisensi::multisensi(design = expand.grid, model = mendOpt,
#                                              reduction = basis.ACP, dimension = NULL, analysis = analysis.sensitivity,
#                                              basis.args = list(), analysis.args = list(keep.outputs = TRUE), times = times, states = states, parameters = parameters)
#  return(outputSensitivity)
#}
##################################################################################################################################
# #'  Parametric variable sensitivity analysis via r-package 'sensitivity' command 'delsa()'
# #'
# #' @param times        // time sequence
# #' @param states       // initial carbon-pool state values
# #' @param parameters   // initial parameter values
# #' @param ranges       // specified/available parameter ranges
# #'
# #' @return ranges //
# #' @export
# #' @importFrom sensitivity delsa
# #'
# #' @examples
# #' varName <- mendSensitivityPar(times, states, parameters, ranges)
# #'
#mendSensitivityPar <- function(times, states, parameters, ranges) {
#  means <- ranges$mean; sd <- ranges$sd; minimum <- ranges$minimum; maximum <- ranges$maximum

  ##  test and replace for negative range values
#  maximum <- pmax(maximum, (mean + sd))
#  if (any(maximum < 0)) {
#    rngSwap <- pmin(maximum, (mean + sd))
#    maximum[maximum < 0] <- rngSwap[names(rngSwap) %in% names(maximum[maximum < 0])]
#    if(any(maximum < 0)) {
#      rngSwap <- pmin(mean[names(mean) %in% names(maximum[maximum < 0])], abs(maximum[maximum < 0]))
#      rngSwap <- pmax(rngSwap, min(maximum[maximum > 0]))
#      names(rngSwap) <- names(maximum[maximum < 0])
#      maximum[maximum < 0] <- rngSwap[names(rngSwap) %in% names(maximum[maximum < 0])]
#    }
#  }

#  minimum <- pmin(minimum, (mean - sd))
#  if (any(minimum < 0)) {
#    rngSwap <- pmax(minimum, (mean - sd))
#    minimum[minimum < 0] <- rngSwap[names(rngSwap) %in% names(minimum[minimum < 0])]
#    if(any(rngMin < 0)) {
#      rngSwap <- pmin(mean[names(mean) %in% names(minimum[minimum < 0])], abs(minimum[minimum < 0]))
#      rngSwap <- pmin(rngSwap, min(minimum[minimum > 0]))
#      names(rngSwap) <- names(minimum[minimum < 0])
#      minimum[minimin < 0] <- rngSwap[names(rngSwap) %in% names(minimum[minimum < 0])]
#    }
#  }
  ##################

#  nameList <- names(means)
#  rangePar <- list()
#  for (i in 1:length(means)) {
#    rangePar[[i]] <- c(minimum = minimum[[i]], maximum = maximum[[i]])
#  }

#  names(rangePar) <- nameList
#  pars <- parameters[!names(rangePar) %in% names(parameters)]; names(pars) <- names(parameters[!names(rangePar) %in% names(parameters)])
#  outputSensitivityTotal <- sensitivity::delsa(model = mendModSensitivity, par.ranges = rangePar, samples = 1, method = "grid", times = times, states = states, parameters = pars)
#  for (i in 1:length(means)) {
#    parPar <- rangePar[[i]]; names(parPar) <- nameList[[i]];
#    pars <- parameters[!names(parPar) %in% names(parameters)];  names(pars) <- names(parameters[!names(parPar) %in% names(parameters)])
#    outPut <- sensitivity::delsa(model = mendModSensitivity, par.ranges = parPar, samples = 100, method = "grid", times = times, states = states, parameters = pars)
#    outputSensitivityParms[[i]] <- outPut
#    names(outputSensitivityParms[[i]]) <- nameList[[i]]
#  }


#  ranges <- list(mean = parMean, sd = sd, minimum = minimum, maximum = maximum)
#  return(ranges)
#}
