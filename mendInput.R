#' Manual input of values - states, parameters, ranges
#'
#' @param run               // User specified 'run' argument for default value initialization - includes'Initial' for basic spin up (inteded to be reassigned by/for enduser purposes), 'Analytical' for analytical emprical starting state, parameter, and range values, 'Load' for end user external data supply (CSV/Excel etc), and 'Object' for an existing RAM data object type:list() containing minimum primary initial item type: list() $input - $states(11 standard states) and $parameters($parameters and $ranges each with 36 stadard parameter values)
#' @param fileInput         // User specified file or file location, or by default 'fileInput = file.choose()'
#' @param fileTimeSeries    // User specified file or file location, or default 'fileTimeSeries = file.choose()'
#' @param temp              // initial model temperature as provided by the user for 'mendInitAnalytical()' -> 'mendSteadyState()' calculations
#' @param initialConditions // User specified or default initial conditions for 'mendLoad()'
#'
#' @return out // standard MEND 'input' data structure including parameters(parametes and ranges), states, and timeSeries data
#' @export
#' @importFrom svDialogs dlg_input
#'
#' @examples
#' varName <- mendInput(run, fileInput, fileTimeSeries, temp, initialConditions)
#'
mendInput <- function(run, fileInput, fileTimeSeries, temp, initialConditions) { ##//sensitivity 'mendSensitivity()' for parameteric sensitivity analysis currently not implemented
## space holder variable assignment ######################################################################
out <-  switch(run,
               "Initial" = mendInitApproximate(fileInput = fileTimeSeries),                             ## contains inital spin up state and parameter values/ranges
               "Analytical" = mendInitAnalytical(temp = temp, fileInput = fileTimeSeries),              ## cointains state values determined via analytical steady state
               "Load" = mendLoad(fileInput = fileInput, fileTimeSeries = fileTimeSeries, temp = temp, times = times, conditions = initialConditions, CUE = CUE), #user specified external parameter/state, and time series data // sensitivity not currently implemented
               "Object" = mendLoadObj(fileInput = fileInput, fileTimeSeries = fileTimeSeries, temp = temp),                                 ## used for alternative data object sourcing for use in alternate package function calls/objectives
               "Input"  = switch(select.list(c("Initial", "Analytical", "Load", "Object"), preselect = "Initial", title = "Please choose a parameter set to run:", graphics = TRUE),
                                 "Initial" = mendInitApproximate(file = fileTimeSeries),
                                 "Analytical" = mendInitAnalytical(temp = temp, file = fileTimeSeries),
                                 "Load" = mendLoad(fileInput = fileInput, fileTimeSeries = fileTimeSeries, temp = temp, times = times, conditions = initialConditions, CUE = CUE), ## // sensitivity not currently implemented
                                 "Object" = mendLoadObj(fileInput = fileInput, fileTimeSeries = fileTimeSeries, temp = temp),
                                 stop("Please set 'run' argument value for 'Load', 'Object', Inital' or 'Analytical'. Exiting now.")
               ),
               stop("Please set 'run' argument value for 'Load', 'Object', Inital' or 'Analytical'. Exiting now.")
)
states <- out[[1]]$states
nameSt <- names(states)

parameters <- out[[1]]$parameters$parameters
namePar <- names(parameters)

ranges <- out[[1]]$parameters$ranges
nameRng <- names(ranges$mean)

fileTimeSeries <- out[[1]]$fileTimeSeries
##########################################################################################################
## data entry/input/acquisition/analysis states ###########################################################
stList <- as.list(c("All", "None", nameSt))
nameSelectSt <- select.list(stList, multiple = TRUE, title = "Please select one or more states for initial value input.", graphics = getOption("menu.graphics"))
if(any(nameSelectSt == "All")) {
  nameSelectSt <- names(states)
}
if (!any(nameSelectSt == "None")) {
  str <- "Enter a value for state: '"
  str2 <- "' (-1 to exit)"
  for (i in 1:length(nameSelectSt)) {
    msg <- paste(str, nameSelectSt[[i]], str2)
    default <-  states[[nameSelectSt[[i]]]]
    response <- svDialogs::dlg_input(message = msg, default = default)$res

    if(length(response) && !is.na(as.numeric(response))){
      states[[nameSelectSt[[i]]]] <- response
    }
  }
}

## data entry/input/acquisition/analysis parameters ######################################################
parList <- as.list(c("All", "None", namePar))
nameSelectPar <- select.list(parList, multiple = TRUE, title = "Please select one or more parameters for initial value input.")
if(any(nameSelectPar == "All")) {
  nameSelectPar <- names(parameters)
}
if (!any(nameSelectPar == "None")) {
  str <- "Enter a value for parameter: '"
  str2 <- "' (-1 to exit)"
  for (i in 1:length(nameSelectPar)) {
    msg <- paste(str, nameSelectPar[[i]], str2)
    default <-  parameters[[nameSelectPar[[i]]]]
    response <- svDialogs::dlg_input(message = msg, default = default)$res

    if(length(response) && !is.na(as.numeric(response))){
          val <- as.matrix(response)
          parameters[[nameSelectPar[[i]]]] <- val
    }
  }
}

## data entry/input/acquisition/analysis ranges ##########################################################
rng <- as.list(ranges$mean);
sd <- as.list(ranges$sd); maxi <- as.list(ranges$maximum); mini <- as.list(ranges$minimum)
names(sd) <- nameRng; names(maxi) <- nameRng; names(mini) <- nameRng
rngList <- as.list(c("All", "None", namePar))
nameSelectRng <- select.list(rngList, multiple = TRUE, title = "Please select one or more parameters for range input.")
if(any(nameSelectRng == "All")) {
  nameSelectRng <- names(states)
}
if (!any(nameSelectRng == "None")) {
  str <- "Enter a value for range: '"
  str2 <- "' (-1 to exit)"
  for (i in 1:length(nameSelectRng)) {
    msg <- paste(str, nameSelectRng[[i]], str2)
    default <-  maxi[[nameSelectRng[[i]]]] - mini[[nameSelectRng[[i]]]]
    response <- svDialogs::dlg_input(message = msg, default = default)$res

    if(length(response) && !is.na(as.numeric(response))){
      val <- as.numeric(response)

      maxi[[nameSelectRng[[i]]]] <- rng[[nameSelectRng[[i]]]] + (val / 2)
      mini[[nameSelectRng[[i]]]] <- rng[[nameSelectRng[[i]]]] - (val / 2)
      sd[[nameSelectRng[[i]]]] <- (val / 6)
    }
  }
}
##########################################################################################################

pars <- list(parameters = parameters, ranges = ranges)
out[[1]] <- list(states = states, parameters = pars, timeSeries <- fileTimeSeries)
return(out)
}
