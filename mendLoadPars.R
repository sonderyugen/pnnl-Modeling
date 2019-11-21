#' external data name accomodation // can be modified to accomodate different naming protocols regarding initial model parameter values
#'
#' @param pars  // external data paramter values
#' @param parms // placeholder parameter values
#'
#' @return pars // standard updated parameter and range list
#' @export
#' @importFrom purrr map
#'
#' @examples
#' varName <- mendLoadPars(pars, parms)
#'
mendLoadPars <- function(pars, parms) {
## can call mendNamePars(nameList) once the issue is resolved
  parameters <- pars$parameters; ranges <- pars$ranges; means <- ranges$mean
  names <- names(means); nameParms <- names(parms)
  m <- as.numeric(unlist(map(parms, mean))); names(m) <- names(parms)
  sd <- as.numeric(unlist(map(parms, sd))); minimum <- as.numeric(unlist(map(parms, min))); maximum <- as.numeric(unlist(map(parms, max)))

  if (any(!names(means) %in% names(parms))){
    nameList <- names(means[!names(means) %in% names(parms)])
    sds <- ranges$sd; maxs <- ranges$maximum; mins <- ranges$minimum;
    for (i in 1:length(nameList)){
      m[[nameList[[i]]]] <- means[[nameList[[i]]]]
      sd[[nameList[[i]]]] <- sds[[nameList[[i]]]]
      maximum[[nameList[[i]]]] <- maxs[[nameList[[i]]]]
      minimum[[nameList[[i]]]] <- mins[[nameList[[i]]]]
    }
  }
  ranges <- list(mean = m, sd = sd, minimum = minimum, maximum = maximum)


  par <- m
  if(length(par) < length(parameters)) {
    nameList <- names(parameters[!names(parameters) %in% names(par)])
    for (i in 1:length(nameList)){
      par[[nameList[[i]]]] <- parameters[[nameList[[i]]]]
    }
    parameters <- par
  }

  pars$parameters <- parameters; pars$ranges <- ranges
  return(pars)
}
