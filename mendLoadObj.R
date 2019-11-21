#' Data object method for state, parameter, time-series data loading
#'
#' @param fileInput      // User specified input file containing parameter and state data
#' @param fileTimeSeries // User specified time-series data for implementation in calibration via 'mendCollin()'
#' @param temp           // initial reference temp for steady state analysis via 'mendSteady()'
#'
#' @return out // a list object containing series of data sets
#' @export
#'
#' @examples
#' varName <- mendLoadObj(fileIn, fileTime, temp)
#'
mendLoadObj <- function(fileInput, fileTimeSeries, temp) {
  init <- mendInitAnalytical(temp = temp, fileInput = fileTimeSeries)
  out <- list()

  for (i in 1:length(fileInput)) {
    trialFile <- fileInput[[i]]
    tempOut <- init

    if(exists(trialFile$parameters)) {
      parFile <- trialFile$parameters
      parData <- tempOut$parameters
      if (exists(parFile$parameters)) {
        parF <- parFile$parameters; parD <- parData$parameters
        namesPar <- names(parF); namePar <- names(parD[names(parD) %in% namePar])
        for (j in 1:length(namePar)) {
          parD[[namePar[[j]]]] <- parF[[namePar[[j]]]]
        }
      }
      parData$parameters <- parD

      if (exists(parFile$ranges)) {
        rngFile <- parFile$ranges; rngData <- parData$rangesl
        namesRng <- names(rngFile); nameRng <- names(rngData[[names(rngData) %in% namesRng]])
        for (j in 1:length(nameRng)) {
          tempFile <- rngFile[[j]]; tempData <- rngData[[nameRng[[j]]]]
          namesTemp <- names(tempFile); nameTemp <- names(tempFile[[names(tempFile) %in% namesRng]])
          for (k in 1:length(nameTemp)) {
            tempData[[nameTemp[[k]]]] <- tempFile[[nameTemp[[k]]]]
          }
          rngData[[nameRng[[j]]]] <- tempData
        }
      }
      parData$ranges <- rngData

      tempOut$parameters <- parData
    }

    if(exists(trialFile$states)) {
      stFile <- trialFile$states; StData <- tempOut$states
      namesSt <- names(stFile); nameSt <- names(stData[names(stData) %in% namesSt])
      for (j in 1:length(nameSt)){
        stData[[nameSt[[j]]]] <- stFile[[nameSt[[j]]]]
      }
      tempOut$states <- stData
    }

    if (exists(trialFile$timeSeries)) {
      tempOut$timeSeries <- trialFile$timeSeries
    } else {tempOut$timeSeries <- fileTimeSeries }

    out[[i]] <- tempOut
  }

  return(out)
}
