#mendPlot <- function(out, save, fileOut) {
#  if(is.null(fileOut)) {
#    fileOut <- file.choose
#  }

#  choiceSeries <- c("Temperature", "Parameter")
#  cSeries <- select.list(choice = choiceSeries, title = "Please choose plot series:")
#  c1 <- match(cSeries, choiceSeries)

#  choiceSet <- c(choiceSeries[!c1])
#  cSet <- select.list(choice = choiceSet, title = "Please choose plot set:")
#  c2 <- match(cSet, choiceSeries)

#  choicePlot <- switch(c2,
#                       {},
#                       {})




#  sPars <- "Parameter set: "
#  parsChoice <- list();
#  for (i in 1:length(out)) {
#    strPar <- paste(sPars, i)
#    parsChoice[i] <- strPar
#  }
#  parChoices <- c(parsChoice)
#  rangeChoices <- c("Average", "Maxmium", "Minimum", "Range", "All")
#  stateChoices <-


#  if(save) {
#    for (i in 1:length(nameList)){
#       str <- paste(s, nameList[[i]], ".png")
#       png(filename = str)
#       plot(plotList, which = nameList, select = nameList, xlab = "time (hours)", ylab = "g C / g Soil")
#       dev.off()
#    }
#  }
#}
########################################################################################################################################
#s <- "C:/Users/hiet494/Desktop/tempOut/"
#save = FALSE
#for (i in 1:length(nameList)){
#     str <- paste(s, nameList[[i]], ".png")
#     png(filename = str)
#     plot(testAvg, testMin, testMax, which = nameList[[i]], select = nameList[[i]], xlab = "time (hours)", ylab = "g C / g Soil")
#     dev.off()
#  }
#tCalPlot <- testCalPlot[c(seq(1, length(testCalPlot), by = round(length(testCalPlot) / length(testCalTS))))]
#tCalCal <- testCalCal[c(seq(1, length(testCalCal), by = round(length(testCalPlot) / length(testCalTS))))]
#tCalTS <- testCalTS[c(seq(1, ceiling(length(testCalPlot) / ceiling(length(testCalPlot) / length(testCalTS))), by = 1))]

#str <- paste(s, "T13.png")
#png(filename = str)
#plot(testCalPlot, testCalCal, which = "T", xlab = "time (hours)", ylab = "g C / g Soil")
#lines(c(rep(states[["T]], length(testCalPlot))), col = "blue")
#dev.off()
