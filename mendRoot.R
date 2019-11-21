#' Root command for pnnlMEND enables compelete model formulation and analysis with options
#'
#' @param input          User specifed boolean for entered input for model processing.  With no input -> code runs example files for Inital or Analytical starting paramter values.  'input = FALSE' also allows user to run other mendRoot options using user specified environment variable(s) as input
#' @param fileInput      Initial parameter value and state value input source via 'mendInput()' - either NULL, a specified path, or user dialogue 'file.choose()'
#' @param fileTimeSeries Time-Series data input source used for calibration via 'mendMin()'- either NULL, a specified path, or user dialogue 'file.choose()'
#' @param fileOut        Output path for saved plot(s) via 'mendPlot()' - either NULL, a specified path, or user dialogue 'file.choose()'
#' @param run            User specified 'run' argument for default value initialization - includes'Initial' for basic spin up (inteded to be reassigned by/for enduser purposes), 'Analytical' for analytical emprical starting state, parameter, and range values, 'Load' for end user external data supply (CSV/Excel etc), and 'Object' for an existing RAM data object type:list() containing minimum primary initial item type: list() $input - $states(11 standard states) and $parameters($parameters and $ranges each with 36 stadard parameter values)
#' @param solve          User specified boolean to run solver on model system
#' @param CUE            User specified boolean to run Temp Sensitive Carbon Use Efficiency (CUE)
#' @param sensitivity    User specified option to evaluate input data for model/paramter sensitivty analysis.  not referenced when 'input = FALSE' // currently not implemented
#' @param sensePar       User specified option to evaluate each parameter for sensitivity ranges
#' @param calibrate      User specified option to calibrate model with time-series data
#' @param calOpt         User specified boolean to calibrate calibration paramters along user provided time-series data set
# #' @param plotOut        User specified boolean to plot final model output(s) // not currently implemented
# #' @param save           User specified boolean for saving plot output //*
#' @param model          User specified argument to evaluate model along "Steady", Average", "Maximum", or "Minimum" values, or "Ranges", or "All"
#' @param maxOpt         User specified boolean to optimize the range of the model according to given function arguement 'model'
#' @param times          User specified time sequence default 10 years
#' @param temp           User specified Temp/Temp Sequence
#' @param pH             User specified pH values
#' @param kineticsRef    User specified reference temperature for kinetics evaluations
#'
#' @return out // Standard output object containting argument resolved output for series of parameter sets provided to 'mendRoot()'
#' @export
#'
#' @examples
#' mendRoot()
#' mendRoot(run = "Analytical", model = "Maximum")
#' mendRoot(run = "Initial", model = "Minimum")
#' mendRoot(model = "Ranges")
#' mendRoot(fileInput = fileIn, fileTimeSeries = NULL, run = "Load", sensitivity = TRUE)
#' mendRoot(fileInput = NULL, fileTimeSeries = fileTime, run = "Load", calibrate = TRUE)
#' mendRoot(run = "Analytical", solve = TRUE, model = "Steady")
#' mendRoot(input = TRUE, fileInput = fileIn, fileTimeSeries = fileTime, run = "Load", solve = TRUE, model = "All", maxOpt = TRUE, sensitivity = TRUE, sensePar = TRUE, calibrate = "TRUE", calOpt = TRUE, temp = seq(from = 13, to = 17, by = 1))
#'
mendRoot <- function(input = FALSE,  fileInput = NULL, fileTimeSeries = NULL, fileOut = NULL, run = "Full",                                             ## // fileInput = file.choose() //fileTimeSeries = file.choose //fileOut = file.choose()
                     solve = FALSE, CUE = FALSE, calibrate = FALSE, sensitivity = FALSE, sensePar = FALSE, calOpt = FALSE, plotOut = FALSE, save = FALSE,                 ## // minCal = FALSE// currently not implemented
                     model = "Average", maxOpt = FALSE, times = seq(1, 87600, 1), temp = seq(from = 12, to = 12, by = 1), pH = 6, kineticsRef = 12) {
## input and processing functions ######################################################################
  conditions <- mendConditionsLoad(temp = temp, pH = pH, kin = kineticsRef)
  initialConditions <- conditions[[1]]; temp <- unname(initialConditions["Temp.init.C"])

if (input) {
  ##raw data processsing
    inputData <- mendInput(run = run, fileInput = fileInput, fileTimeSeries = fileTimeSeries, temp = temp, initialConditions = initialConditions)        ## asks user for data source input choice
} else {
  ## example output
    inputData <-  switch(run,
                         "Initial" = mendInitApproximate(fileInput = fileTimeSeries),                             ## contains inital spin up state and parameter values/ranges
                         "Analytical" = mendInitAnalytical(temp = temp, fileInput = fileTimeSeries),              ## cointains state values determined via analytical steady state
                         "Load" = mendLoad(fileInput = fileInput, fileTimeSeries = fileTimeSeries, temp = temp, times = times, conditions = initialConditions, CUE = CUE), #user specified external parameter/state, and time series data // sensitivity not currently implemented
                         "Object" = mendLoadObj(fileInput = fileInput, fileTimeSeries = fileTimeSeries, temp = temp),                                 ## used for alternative data object sourcing for use in alternate package function calls/objectives
                         "Full" = mendFull(fileInput = fileInput, fileTimeSeries = fileTimeSeries, times = times, conditions = conditions, CUE = CUE, model = model, maxOpt = maxOpt),
                         "Input"  = switch(select.list(c("Initial", "Analytical", "Load", "Object", "Full"), preselect = "Initial", title = "Please choose a parameter set to run:", graphics = TRUE),
                                        "Initial" = mendInitApproximate(file = fileTimeSeries),
                                        "Analytical" = mendInitAnalytical(temp = temp, file = fileTimeSeries),
                                        "Load" = mendLoad(fileInput = fileInput, fileTimeSeries = fileTimeSeries, temp = temp, times = times, conditions = initialConditions, CUE = CUE), ## // sensitivity not currently implemented
                                        "Object" = mendLoadObj(fileInput = fileInput, fileTimeSeries = fileTimeSeries, temp = temp),
                                        "Full" = mendFull(fileInput = fileInput, fileTimeSeries = fileTimeSeries, times = times, conditions = conditions, CUE = CUE, model = model, maxOpt = maxOpt),
                                        stop("Please choose argument value for 'Load', 'Object', Inital' or 'Analytical'. Exiting now.")
                                      ),
                         stop("Please set 'run' argument value for 'Load', 'Object', Inital' or 'Analytical'. Exiting now.")
                  )
}
out <- list();
for (x in 1:length(inputData)) {
  ins <- inputData[[x]]
  input <- list(states = ins$states, parameters = ins$parameters, timeSeries = ins$timeSeries, times = times, model = model)

  output <- list()
  output$input <- input
########################################################################################################
## inputs and processing functions yield initial parameter & state values for modeling input ###########
  parameters <- list()
  for (i in 1:length(conditions)) {
    parameters[[i]] <- c(input$parameters, conditions = list(conditions[[i]]), CUE = CUE)
  }
  states <- input$states
  p <- input$parameters; par <- p$parameters; ranges <- p$ranges
  pars <- list()
  for (i in 1:length(parameters)) {
    pars[[i]] <- c(parameters[[i]]$parameters, parameters[[i]]$conditions, CUE = parameters[[i]]$CUE)
  }
########################################################################################################
  if (sensitivity) {
    parSense <- mendSense(times = times, parameters = pars[[1]], states = states, ranges = ranges, sensePar = sensePar)
    output$sensitivity <- parSense
  }
########################################################################################################
  if (solve) {
    solveData <- list(); parMax <-  NULL
    for (i in 1:length(parameters)) {
      p <- parameters[[i]]
      solveData[[i]] <- mendSolve(times = times, states = states, parameters = p, model = model, maxOpt = maxOpt, parMax = parMax)

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

    output$solve <- solveData

    if(model == "Steady" || model == "All"){
      testData <- solveData[[1]]
      states <- testData$states
      inputData$states <- states
    }
  }
########################################################################################################
  if (calibrate) {
    timeData <- input$timeSeries
    if(is.null(timeData)) {
      timeData <- file.choose()
    }

    calData <- mendMin(timeData = timeData, times = times, states = states, parameters = pars[[1]], ranges = ranges, calOpt = calOpt) ## // 'minCal'calibration boolean for optional implementation

    parms <- list()
    ps <- calData$parameters; p <- ps$parameters
    for (i in 1:length(conditions)) {
      parms[[i]] = as.list(c(p, conditions, CUE = CUE))
    }

    parMax <- NULL
    for (i in 1:length(parms)) {
      calibrateData[[i]] <- mendSolve(times = times, states = states, parameters = parms[[i]], model = model, maxOpt = maxOpt, parMax = parMax)
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

    output$calibrate <- calibrateData

    if(model == "Steady" || model == "All"){
      testData <- solveData[[1]]
      states <- testData$states
      inputData$states <- states
    }
  }
#######################################################################################################
  out[[x]] <- output
}
########################################################################################################
## final model plot/output options #####################################################################
#if (plotOut) {
#  mendPlot(out = out, save = save, fileOut = fileOut)
#}
########################################################################################################
return(out)
}

## optional inverted state-parameters sensitivity analysis##############################################
#if (sensitivity) {
#  for (i in 1:length(parameters)) {
#    sensitivity[[i]] <- mendSensitivityState(times = times, states = states, parameters = parameters[[i]], ranges = ranges)
#  }
#}
#  output$sensitivity <- sensitivity
########################################################################################################

