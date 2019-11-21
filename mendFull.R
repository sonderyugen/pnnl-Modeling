#' Shortuct 'mendRoot()' argument path (via 'run = full')to evaluate a parameter set and time series for fully analysed and calibrated output.  Functionally the same as arugment path via 'calibrate = TRUE' without the option of further optimization.
#'
#' @param fileInput      // input source for 'run' arugment 'load' or 'object', can be NULL
#' @param fileTimeSeries // time-series input source required for calibration
#' @param times          // initial time sequence for evaluation by 'mendMod()'
#' @param conditions     // intial temperature and pH values
#' @param CUE            // User specified boolean for dynamic Carbon Use Efficiency (CUE) for use in evaluation by 'mendMod()'
#' @param model          // User specified argument to evaluate model along "Steady", Average", "Maximum", or "Minimum" values, or "Ranges", or "All"
#' @param maxOpt         // User specified boolean to optimize the range of the model according to given function arguement 'model'
#'
#' @return solveData list object containing standard deSolve object via 'deSolve::ode()' and parameter list specific to model output via 'mendMod()'
#' @export
#'
#' @examples
#' varName <- mendFull(fileIn, fileTime, times, conditions, CUE, model, maxOpt)
#'
mendFull <- function(fileInput = fileInput, fileTimeSeries = fileTimeSeries, times = times, conditions = conditions, CUE = CUE, model = model, maxOpt = maxOpt) {
  if(is.null(fileTimeSeries)) {
    stop("Time-series file data must be provided for complete analysis.  Exiting now.")
  }

  initialConditions <- conditions[[1]]; temp <- unname(initialConditions["Temp.init.C"])
  inputData <- switch(select.list(c("Initial", "Analytical", "Load", "Object"), preselect = "Initial", title = "Please choose a parameter set to run:", graphics = TRUE),
         "Initial" = mendInitApproximate(file = fileTimeSeries),
         "Analytical" = mendInitAnalytical(temp = temp, file = fileTimeSeries),
         "Load" = mendLoad(fileInput = fileInput, fileTimeSeries = fileTimeSeries, temp = temp, times = times, conditions = initialConditions, CUE = CUE), ## // sensitivity not currently implemented
         "Object" = mendLoadObj(fileInput = fileInput, fileTimeSeries = fileTimeSeries, temp = temp),
         stop("Please choose argument value for 'Load', 'Object', Inital' or 'Analytical'. Exiting now."))

  ins <- inputData[[1]]
  p <- ins$parameters; parameters = p$parameters; pars <- c(parameters, initialConditions, CUE = CUE)
  states = ins$states; ranges = p$ranges; timeData <- ins$timeSeries

  calData <- mendCollin(times = times, states = states, pars = pars, ranges = ranges, timeData = timeData)

  parms <- list()
  parameters <- calData$parameters
  pars <- parameters$parameters
  for (i in 1:length(conditions)) {
    parms[[i]] = as.list(c(pars, conditions[[i]], CUE = CUE))
  }

  parMax <- NULL
  for (i in 1:length(parms)) {
    solveData[[i]] <- mendSolve(times = times, states = states, parameters = parms[[i]], model = model, maxOpt = maxOpt, parMax = parMax)
    if(i < length(conditions) && maxOpt) {
      if((model == "All" || model == "Ranges")) {
        if (i == 1) {
          testDataSolve <- solveData[[i]]
          testMinSolve <- testDataSolve$min; minPar <- testMinSolve$parameters
          testMaxSolve <- testDataSolve$max; maxPar <- testMaxSolve$parameters
        }
        c <- conditions[[i+1]]; nameC <- names(c)
        for(j in length(nameC)) {
          testMin[[nameC[[j]]]] <- c[[nameC[[j]]]]
          testMax[[nameC[[j]]]] <- c[[nameC[[j]]]]
        }
        parMax <- list()
        parMax$min <- testMin; parMax$max <- testMax
      }
    }
  }

  return(solveData)
}
